{-# OPTIONS_GHC -Wno-orphans #-}
module NicLib.Net where

-- base
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import Data.String (IsString(..))
-- import qualified Data.Trie as Trie
-- import Data.Trie (Trie)
import Data.Either (either)
import Data.Foldable (foldl')
import Data.Char (toLower)

-- text
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)

import Control.Monad.Trans.Except -- transformers
import Network.HTTP.Req hiding (parseUrl) -- req
import URI.ByteString -- uri-bytestring

-- NicLib
import NicLib.Errors
import NicLib.NStdLib (both)

-- bytestring
import qualified Data.ByteString as BS'
import qualified Data.ByteString.Char8 as BSC'

instance (MonadIO m, IsString e, Monoid w) => MonadHttp (BugT e w m) where
    handleHttpException = err . fromString . show
{-
    getHttpConfig = def {httpConfigRetryPolicy = _, httpConfigRetryJudge = _}
        where
    exceptionToErrorMsg :: Network.HTTP.Client.HttpException -> (Bool, Text)
    exceptionToErrorMsg i = \case
        HttpExceptionRequest req content -> second (\message -> T'.unlines ["HTTP Exception: (" <> pnn i <> " attempt)", indent 4 message, "for the following request:", indent 4 . T'.pack $ show req])
            (case content of
                ConnectionClosed                       -> (False, "Attempted to use an already closed Network.HTTP.Client.Internal Connection")
                ConnectionFailure cfe                  -> (True, "Connection failure: " <> T'.pack (show cfe)) -- TODO: not sure if this should be False
                ConnectionTimeout                      -> (True, "Timed-out trying to connect to server")
                HttpZlibException (ZlibException code) -> (False, "Problem inflating response body (code " <> T'.pack (show code) <> "). Consult zlib(3) or other docs.")
                IncompleteHeaders                      -> (False, "Incomplete set of headers") -- how does it know it's incomplete?
                InternalException ie                   -> (False, "Internal exception: " <> T'.pack (show ie))
                InvalidChunkHeaders                    -> (False, "Invalid chunk header!")
                InvalidDestinationHost h               -> (False, "Tried connecting to an invalid host (" <> decodeUtf8 h <> ")")
                InvalidHeader s                        -> (False, "Could not parse header: " <> decodeUtf8 s)
                InvalidProxySettings msg               -> (False, msg)
                InvalidProxyEnvironmentVariable a b    -> (False, "Invalid proxy envvar: " <> a <> "=" <> b)
                InvalidStatusLine s                    -> (False, "Unknown response status: " <> decodeUtf8 s) -- a status line is the HTTP response status code plus the "reason phrase", e.g. "OK" or "Not Found"
                NoResponseDataReceived                 -> (True, "No response data from server at all. Was a connection closed prematurely?")
                OverlongHeaders                        -> (False, "Header too long in server response")
                ProxyConnectException bs code status   -> (False, "HTTP response " <> T'.pack (show code) <> " (" <> decodeUtf8 (statusMessage status) <> ") when trying to connect to proxy\n" <> decodeUtf8 bs)
                ResponseBodyTooShort a b               -> (False, "Request body unexpected size (too short); expected " <> T'.pack (show a) <> " but received" <> T'.pack (show b))
                ResponseTimeout                        -> (True, "Timed-out waiting for server's response")
                StatusCodeException resp bs            -> (False, T'.unlines ["non-2** response:", indent 4 $ T'.pack (show resp), indent 4 (decodeUtf8 bs)])
                TlsNotSupported                        -> (False, "Manager doesn't support TLS. Are you using tlsManagerSettings from http-client-tls?")
                TooManyRedirects resps                 ->
                    let v = fst $ foldr (\(T'.pack . show -> resp) (b,acc) -> ("Response #" <> T'.pack (show acc) <> ":" <> (indent 4 resp) <> b, succ acc :: Int)) (mempty,0) resps
                    in (False, "Too Many Redirects (" <> (T'.pack . show $ length resps) <> "):" <> v)
                WrongRequestBodyStreamSize a b         -> (False, "Request body unexpected size; expected " <> T'.pack (show a) <> " but received " <> T'.pack (show b))
            )
        InvalidUrlException url reason -> (False, (T'.pack url) <> " is malformed because: " <> (T'.pack reason))
    pnn :: Int -> Text
    pnn (show -> n) = T'.pack $ n <> case last n of
        '1' -> "st"
        '2' -> "nd"
        '3' -> "rd"
        _   -> "th"
-}

-- common Options:
-- "queryParam" =: value -- value :: ToHttpApiData a.
-- basicAuth "uname" "pass"
-- TODO: continue here! c:

-- | Given the number of things I'll be creating ExceptT's for, maybe I should have [forall a. Show a => a] as the ExceptT parameter.
getSample :: BugT String [String] IO ()
getSample = do
    exampleBase <- withExceptT show $ (\case Left e -> e) <$> parseURI' "https://google..com/"
    url <- withBugAccum pure . withExceptT show $ parseRelTo exampleBase "http://wiki.c2.com/?WhyWeLoveLisp"
    case url of
        Left x -> f x
        Right x -> f x
    where
        f :: (MonadIO m, Monoid w, IsString e) => (Url scheme, Option scheme) -> BugT e w m ()
        f (u,o) = do
            resp <- req GET u NoReqBody bsResponse o
            liftIO . BSC'.putStrLn $ responseBody resp

-- | The preferred normalization. Convenience function.
normalize :: URIRef a -> BS'.ByteString
normalize = normalizeURIRef' aggressiveNormalization

-- | Fails on non-http(s) schemes.
absURIToUrl :: (Monad m, Monoid w) => URIRef Absolute -> BugT URIParseError w m (Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https))
absURIToUrl u@(URI {..}) =
    let p :: Url a -> Url a -- type signatures given to keep polymorphism
        p z = foldl' (\b a -> b /: decodeUtf8 a) z (BSC'.split '/' uriPath)
        q :: Option a
        q = foldMap (uncurry (=:) . both decodeUtf8) (queryPairs uriQuery)
    in case BSC'.map toLower $ schemeBS uriScheme of
        "https" -> pure $ Right (p (https $ authToText uriAuthority), q)
        "http" -> pure $ Left (p (http $ authToText uriAuthority), q)
        _ -> err $ OtherError $ "absURIToUrl failed on \"" <> BSC'.unpack (serializeURIRef' u) <> "\"; we don't support non-http(s) schemes."
    where
        authToText :: Maybe Authority -> Text
        authToText = decodeUtf8 . hostBS . authorityHost . fromJust

-- | The most convenient way to parse a URI for a scraper. Combines @parseURI'@ and @relTo@.
parseRelTo :: (Monoid w, Monad m, IsString w) => URIRef Absolute -> BS'.ByteString -> BugT URIParseError w m (Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https))
parseRelTo base x = parseURI' x >>= either pure (`relTo` base) >>= absURIToUrl

-- produce an absolute URL from a @parseURI'@ value
-- for JH scraper, should use with bytestring-trie? Prob., since all ASCII chars.
relTo :: (Monoid w, Monad m, IsString w) => URIRef a -> URIRef Absolute -> BugT e w m (URIRef Absolute)
parsed `relTo`  (URI {..}) = case parsed of
    RelativeRef {..} -> do
        maybe (return ()) (warn . ("rrAuthority filled: " <>) . fromString . show) rrAuthority
        return $ URI uriScheme uriAuthority rrPath rrQuery rrFragment
    a@(URI _ _ _ _ _) -> return a

-- Made to parse href elements from HTML. Does not account for hand-written URLs like "site.com/page1", as this would not work in a browser.
parseURI' :: (Monad m, Monoid w) => BS'.ByteString -> BugT URIParseError w m (Either (URIRef Absolute) (URIRef Relative))
parseURI' s = case parseURI laxURIParserOptions s of
    Left (MalformedScheme MissingColon) -> either err (pure . Right) $ parseRelativeRef laxURIParserOptions s
    Left o -> err o
    Right r -> pure $ Left r
