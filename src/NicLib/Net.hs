{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE
  FlexibleInstances
, FlexibleContexts
, LambdaCase
, MultiWayIf
, NamedFieldPuns
, NumDecimals
, OverloadedStrings
, RankNTypes
, ViewPatterns
#-}

-- | Miscellanary of Network-based IO functions, namely creating & handling HTTP requests & responses in ExceptT, with cookie jar support
module NicLib.Net
( commonRequest
, fetch'
, fetch
, handleResponse
, httpOrBust
, CookieJar
, createCookieJar
, evictExpiredCookies
) where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.ListLike (ListLike)
import Data.ListLike.String (StringLike, fromString, toString, unlines)
import Data.Streaming.Zlib (ZlibException(..))
import Data.Time.Calendar
import Data.Time.Clock
import Network.HTTP.Client hiding (httpLbs, httpNoBody, withResponse) -- for cookie stuff
import Network.HTTP.Conduit (HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Simple -- in package http-conduit. Contrast with Network.HTTP.Conduit (in http-conduit too) and Network.HTTP.Client (in the http-client package)
import Network.HTTP.Types.Status (Status(statusMessage))
import NicLib.IO
import NicLib.NStdLib
import NicLib.URL as URL
import System.IO
import Prelude hiding (unlines)
import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as BS'
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T'
import NicLib.Text
import Data.String (IsString)

{- other useful funcs
   setRequestBodyURLEncoded :: [(ByteString, ByteString)] -> Request -> Request -- sets content-type to application/x-www-form-urlencoded and sets request body as URL-encoded data -- cf. <https://hackage.haskell.org/package/http-client-0.5.7.0/docs/Network-HTTP-Client-MultipartFormData.html>?
   setRequestbodyFile :: FilePath -> Request -> Request
   setRequestBodyJSON :: ToJSON a => a -> Request -> Request -- sets content-type to application/json too
   setRequestBodyLBS :: ByteString -> Request -> Request
   setRequestHeaders :: [(HeaderName :: CI ByteString, ByteString)] -- see <http://hackage.haskell.org/package/http-types-0.9.1/docs/Network-HTTP-Types-Header.html#t:HeaderName>. also addRequestHeader
   httpJSONEither
-}

-- UTCTime Binary instance used in Cookie's Binary instance, for Cookie's expiry time, creation time,...record fields
instance Bin.Binary UTCTime where
    put (UTCTime {utctDay, utctDayTime}) = do
        Bin.put (toModifiedJulianDay utctDay)
        Bin.put (truncate utctDayTime :: Integer)
    get = do
        modJulDay <- Bin.get :: Bin.Get Integer
        dayTimeInt <- Bin.get :: Bin.Get Integer
        return $ UTCTime (ModifiedJulianDay modJulDay) (fromInteger dayTimeInt)

instance Bin.Binary Cookie where
    put (Cookie {cookie_name, cookie_value, cookie_expiry_time, cookie_domain, cookie_path, cookie_creation_time, cookie_last_access_time, cookie_persistent, cookie_host_only, cookie_secure_only, cookie_http_only}) = do
        Bin.put cookie_name
        Bin.put cookie_value
        Bin.put cookie_expiry_time
        Bin.put cookie_domain
        Bin.put cookie_path
        Bin.put cookie_creation_time
        Bin.put cookie_last_access_time
        Bin.put cookie_persistent
        Bin.put cookie_host_only
        Bin.put cookie_secure_only
        Bin.put cookie_http_only
    get = Cookie <$> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get

instance Bin.Binary CookieJar where
    put = Bin.put . destroyCookieJar
    get = liftM createCookieJar Bin.get

commonRequest :: URL -> Request
commonRequest u = let https = scheme u == "https:" in
    setRequestMethod "GET"
    . setRequestHost (iso $ URL.domain u)
    . setRequestPath (iso $ URL.path u)
    . setRequestSecure https
    . setRequestPort ({- fromMaybe -} (if https then 443 else 80) {- (readMaybe . BS'.unpack . bstail' . iso $ URL.port u) -})
    . setRequestQueryString (getQuery $ query u)
    $ defaultRequest

-- | fetch an HTTP resource based on given HTTP request
fetch' :: (MonadIO m, MonadCatch m, ListLike str Char, IsString str, StringLike str, Semigroup str) => Request -> StateT CookieJar (ExceptT str m) BS.ByteString
fetch' req = do
    jar <- get
    now <- liftIO getCurrentTime
    let (req', updJar) = insertCookiesIntoRequest req jar now -- cf. updateCookieJar
    resp <- lift . httpOrBust 3 5e6 $ httpLBS req'
    let (updJar', resp') = updateCookieJar resp req' now updJar
    put updJar'
    return $ getResponseBody resp'

-- | fetch a resource using a sensible HTTP request based on given URL
fetch :: (MonadIO m, MonadCatch m, ListLike str Char, IsString str, StringLike str, Semigroup str) => URL -> StateT CookieJar (ExceptT str m) BS.ByteString
fetch = fetch' . commonRequest

-- | returns a function in which you can handle the response code and body
-- example: handleResponse (httpOrBust 3 2e6 (httpLBS someRequest)) $ \(code, body) -> ...
handleResponse :: forall m b str. (MonadCatch m, MonadIO m) => ExceptT str m (Response BS.ByteString) -> ((Int, BS.ByteString) -> ExceptT str m b) -> ExceptT str m b
handleResponse t f = fmap (getResponseStatusCode &&& getResponseBody) t >>= f

-- | exception handler. Wraps an EIO, returning Lefts for various HTTP errors
-- i is current iteration state. If the EIO produces an error that suggests that retrying would help, then it's retried. n is max number of attempts at the given EIO before giving-up. For example, httpOrBust retries a connection attempt that timed-out, but not one that had too many redirects.
-- if all attempts fail, the error message of the most recent attempt is returned in the ExceptT's Left value
-- wait time is in microseconds (1s = 1e6 μs)
httpOrBust :: forall m a str. (MonadCatch m, MonadIO m, ListLike str Char, IsString str, StringLike str, Semigroup str) => Int -> Int -> ExceptT str m a -> ExceptT str m a
httpOrBust n waitTime e = go 1 e where
    go i e' = case compare i n of -- YES, we DO NEED go to take e'! e' IS NOT NECESSARILY EQUAL TO e! Namely, go2 passes an endomorphism of e to go potentially many times.
                                  -- related note: go and go2 are mutually recursive. Ideally I'd have written go2 as a lambda variable in a CPS call inside a normally recursive go.
        LT -> liftIO (threadDelay waitTime) >> go2 i -- delay a bit, in hopes that a temporary network failure will fix itself, then retry
        EQ -> go2 i -- try one last time (no thread sleeping, since we aren't going to try again, and so won't wait for the network to go back up)
        GT -> e' -- stop retrying; return ExceptT immediately. base case #1 for go & go2
    go2 i = catch e $ exceptionToErrorMsg i >>> \(retry, message) ->
        let !ex = ExceptT . return . Left $ message in do
            liftIO (hPutStrLn stderr $ toString message) -- IO () -> m (). TODO: So here we want to update the user on progress in real-time (during the connection attempt process) rather than afterward; thus either WriterT or ExceptT is inappropriate, since we want to output messages in this function, right here, rather than accumulating a value to be returned....
            if retry then go (succ i) ex else ex -- base case #2 for go & go2
    exceptionToErrorMsg i = \case
        HttpExceptionRequest req content -> (second (\message -> unlines ["HTTP Exception: (" <> pnn i <> " attempt)", indent 4 message, "for the following request:", indent 4 . fromString $ show req]))
            (case content of
                ConnectionClosed                       -> (False, "Attempted to use an already closed Network.HTTP.Client.Internal Connection")
                ConnectionFailure cfe                  -> (True, "Connection failure: " <> fromString (show cfe)) -- TODO: not sure if this should be False
                ConnectionTimeout                      -> (True, "Timed-out trying to connect to server")
                HttpZlibException (ZlibException code) -> (False, "Problem inflating response body (code " <> (fromString $ show code) <> "). Consult zlib(3) or other docs.")
                IncompleteHeaders                      -> (False, "Incomplete set of headers") -- how does it know it's incomplete?
                InternalException ie                   -> (False, "Internal exception: " <> fromString (show ie))
                InvalidChunkHeaders                    -> (False, "Invalid chunk header!")
                InvalidDestinationHost h               -> (False, "Tried connecting to an invalid host (" <> fromString (BS'.unpack h) <> ")")
                InvalidHeader s                        -> (False, "Could not parse header: " <> fromString (BS'.unpack s))
                InvalidProxySettings msg               -> (False, fromString $ T'.unpack msg)
                InvalidProxyEnvironmentVariable a b    -> (False, "Invalid proxy envvar: " <> fromString (T'.unpack a) <> "=" <> fromString (T'.unpack b))
                InvalidStatusLine s                    -> (False, "Unknown response status: " <> fromString (BS'.unpack s)) -- a status line is the HTTP response status code plus the "reason phrase", e.g. "OK" or "Not Found"
                NoResponseDataReceived                 -> (True, "No response data from server at all. Was a connection closed prematurely?")
                OverlongHeaders                        -> (False, "Header too long in server response")
                ProxyConnectException bs code status   -> (False, "HTTP response " <> fromString (show code) <> " (" <> fromString (BS'.unpack (statusMessage status)) <> ") when trying to connect to proxy\n" <> fromString (BS'.unpack bs))
                ResponseBodyTooShort a b               -> (False, "Request body unexpected size (too short); expected " <> fromString (show a) <> " but received" <> fromString (show b))
                ResponseTimeout                        -> (True, "Timed-out waiting for server's response")
                StatusCodeException resp bs            -> (False, unlines ["non-2** response:", indent 4 $ fromString (show resp), indent 4 (fromString $ BS'.unpack bs)])
                TlsNotSupported                        -> (False, "Manager doesn't support TLS. Are you using tlsManagerSettings from http-client-tls?")
                TooManyRedirects resps                 -> (False, "Too Many Redirects (" <> (fromString . show $ length resps) <> "):" <> fst (foldr (\(fromString . show -> resp) (b,acc) -> ("Response #" <> (fromString $ show acc) <> ":" <> (indent 4 resp) <> b, succ acc :: Int)) (mempty,0) resps))
                WrongRequestBodyStreamSize a b         -> (False, "Request body unexpected size; expected " <> (fromString $ show a) <> " but received " <> (fromString $ show b))
            )
        InvalidUrlException url reason -> (False, (fromString url) <> " is malformed because: " <> (fromString reason))
    pnn :: (StringLike str) => Int -> str
    pnn (last . show -> n) = fromString $ n : case n of
        '1' -> "st"
        '2' -> "nd"
        '3' -> "rd"
        _   -> "th"
