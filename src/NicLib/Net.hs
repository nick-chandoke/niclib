{-# OPTIONS_GHC -Wno-orphans #-}

-- | A more approachable API to @uri-bytestring@ and @req@. Import this instead of @uri-bytestring@ or @req@. And nota bene: /req/ is both the name of a package, and the premier function it exports.
--
-- The expected progression of morphisms is @ByteString@ → @URIRef@ → @Url@ → add options → 'req'. @req@ does not export the constructor of @Url@; thus there are no morphisms @Url@ → @URIRef@. This means that @Url@s are just isomorphisms of @URIRef@s that can be passed to @req@. That's why this module exports versions of @req@ and friends that accept @URIRef Absolute@s rather than @Url@s.
--
-- === 'URIRef' vs. 'Url'
--
-- * @URIRef@ is defined in @uri-bytestring@. @Url@ is defined in @req@
-- * Because this library uses both @req@ and @uri-bytestring@, but @req@ uses only http and https schemes despite @uri-bytestring@ using any @ByteString@ as a scheme, we must limit schemes to the intersection of these two libraries: http and https.
--     * NicLib.Net re-exports @req@'s @Scheme@ and not @uri-bytestring@'s
--
-- Both are binary GADT coproducts:
--
-- * @URIRef@: @Absolute@ | @Relative@
-- * @Url@: @Http@ | @Https@
--
-- Becasue every @Url@ has a scheme, all @Urls@ created from a @URIRef@ are computed from at least one @URIRef Absolute@.
--
-- Also, the req 'Options' opaque monoid is parameterized by a scheme, but the only functions that use a specific scheme are
--
-- * basicAuth
-- * oAuth2Bearer
-- * oAuth2Token
--
-- and all of these are where @scheme ~ 'Https@. It turns-out in practice that you'll only need to use @Option@s that work for both schemes; 'basicAuth' is handled by parsing those details in the @URIRef@ (see 'parseURI'), and if you're using oAuth you'll want to construct @Url@s by hand via the @(/~)@ and @(/:)@, in which case you'd import @Network.HTTP.Req@ separately from @NicLib.Net@. That's why these three functions aren't re-exported by this module.
module NicLib.Net
( -- * Parse
  parseURI
, parseRelTo
-- * URI/URL Endos
, relTo
-- * Render
, normalize
-- * Exception Type
, BadUrlException(..)
-- * Re-exports from req or req-conduit
, Url
-- ** Custom Monad Config
, MonadHttp(..)
, HttpConfig(..)
-- ** Request
, req
, reqBc
, req'
, Req
, runReq
-- *** Request Bodies
, FormUrlEncodedParam
, NoReqBody(..)
, ReqBodyJson(..)
, ReqBodyFile(..)
, ReqBodyBs(..)
, ReqBodyLbs(..)
, ReqBodyUrlEnc(..)
, ReqBodySource(..)
, ReqBodyMultipart
, reqBodyMultipart
-- ** Options
, Option
, responseTimeout
, httpVersion
, header
, cookieJar
, decompress
, basicProxyAuth
-- *** Response Accessors
, responseBody
, responseStatusCode
, responseStatusMessage
, responseHeader
, responseCookieJar
-- **** Response Body Accessor Proxies/Hints
, IgnoreResponse
, ignoreResponse
, JsonResponse
, jsonResponse
, BsResponse
, bsResponse
-- ** HTTP Methods
, GET(..)
, POST(..)
, HEAD(..)
, PUT(..)
, DELETE(..)
-- ** Exceptions
, HttpException(..)
, CanHaveBody(..)
, Scheme(..)
-- * Re-exports from uri-bytestring
-- ** Types
, Host(..)
, Port(..)
, Authority(..)
, UserInfo(..)
, Query(..)
, URIRef(..)
, Absolute
, Relative
-- ** Error Types
, SchemaError(..)
, URIParseError(..)
-- ** URI Parsing Options
, URIParserOptions(..)
, strictURIParserOptions
, laxURIParserOptions
, URINormalizationOptions(..)
, noNormalization
, rfc3986Normalization
, httpNormalization
, aggressiveNormalization
, httpDefaultPorts
-- ** URI Serializing
, serializeURIRef
, serializeURIRef'
-- ** URI Utilities
, urlDecode
, urlDecodeQuery
, urlEncodeQuery
, urlEncodePath
, urlEncode
-- * Debugging Functions
, unsafeParseURI
) where

import RIO

-- base
import Control.Arrow
import Control.Monad (foldM)
import Data.Char (toLower)
import Data.Foldable (foldl')
import Data.Proxy
import qualified Data.Bifunctor as BiF
import Control.Applicative (liftA2)

-- text
import RIO.Text (Text, decodeUtf8')
import qualified RIO.Text as T

-- NicLib
import NicLib.NStdLib (both, liftME, (<||>))
import NicLib.FileSystem (concatPaths)

-- bytestring
import qualified RIO.ByteString as BS'
import qualified Data.ByteString.Char8 as BSC'

-- misc
import Control.Exception.Safe -- safe-exceptions
import Control.Monad.Catch as Catch -- exceptions. Apparently this is necessary, as instancing MonadThrow cannot be done with importing just safe-exceptions
import Conduit hiding (throwM) -- conduit

-- uri-bytestring
import URI.ByteString hiding (parseURI, Scheme(..))
import qualified URI.ByteString as U

-- req and req-conduit
import Network.HTTP.Client (Manager, Request)
import Network.HTTP.Req hiding (req, req', reqBr)
import Network.HTTP.Req.Conduit
import qualified Network.HTTP.Req as Req

data BadUrlException
    = NonHTTP
    | MalformedURL String
    deriving (Show, Typeable)

instance Exception BadUrlException

instance MonadThrow Req where
    throwM = liftIO . Catch.throwM

{-
instance MonadHttp _ where
    handleHttpException = Left . fromString . show
    getHttpConfig = def {httpConfigRetryPolicy = _, httpConfigRetryJudge = _}
        where
    exceptionToErrorMsg :: Network.HTTP.Client.HttpException -> (Bool, Text)
    exceptionToErrorMsg i = \case
        HttpExceptionRequest req content -> second (\message -> T.unlines ["HTTP Exception: (" <> pnn i <> " attempt)", indent 4 message, "for the following request:", indent 4 . T.pack $ show req])
            (case content of
                ConnectionClosed                       -> (False, "Attempted to use an already closed Network.HTTP.Client.Internal Connection")
                ConnectionFailure cfe                  -> (True, "Connection failure: " <> T.pack (show cfe)) -- TODO: not sure if this should be False
                ConnectionTimeout                      -> (True, "Timed-out trying to connect to server")
                HttpZlibException (ZlibException code) -> (False, "Problem inflating response body (code " <> T.pack (show code) <> "). Consult zlib(3) or other docs.")
                IncompleteHeaders                      -> (False, "Incomplete set of headers") -- how does it know it's incomplete?
                InternalException ie                   -> (False, "Internal exception: " <> T.pack (show ie))
                InvalidChunkHeaders                    -> (False, "Invalid chunk header!")
                InvalidDestinationHost h               -> (False, "Tried connecting to an invalid host (" <> decodeUtf8' h <> ")")
                InvalidHeader s                        -> (False, "Could not parse header: " <> decodeUtf8' s)
                InvalidProxySettings msg               -> (False, msg)
                InvalidProxyEnvironmentVariable a b    -> (False, "Invalid proxy envvar: " <> a <> "=" <> b)
                InvalidStatusLine s                    -> (False, "Unknown response status: " <> decodeUtf8' s) -- a status line is the HTTP response status code plus the "reason phrase", e.g. "OK" or "Not Found"
                NoResponseDataReceived                 -> (True, "No response data from server at all. Was a connection closed prematurely?")
                OverlongHeaders                        -> (False, "Header too long in server response")
                ProxyConnectException bs code status   -> (False, "HTTP response " <> T.pack (show code) <> " (" <> decodeUtf8' (statusMessage status) <> ") when trying to connect to proxy\n" <> decodeUtf8' bs)
                ResponseBodyTooShort a b               -> (False, "Request body unexpected size (too Left); expected " <> T.pack (show a) <> " but received" <> T.pack (show b))
                ResponseTimeout                        -> (True, "Timed-out waiting for server's response")
                StatusCodeException resp bs            -> (False, T.unlines ["non-2** response:", indent 4 $ T.pack (show resp), indent 4 (decodeUtf8' bs)])
                TlsNotSupported                        -> (False, "Manager doesn't support TLS. Are you using tlsManagerSettings from http-client-tls?")
                TooManyRedirects resps                 ->
                    let v = fst $ foldr (\(T.pack . show -> resp) (b,acc) -> ("Response #" <> T.pack (show acc) <> ":" <> (indent 4 resp) <> b, succ acc :: Int)) (mempty,0) resps
                    in (False, "Too Many Redirects (" <> (T.pack . show $ length resps) <> "):" <> v)
                WrongRequestBodyStreamSize a b         -> (False, "Request body unexpected size; expected " <> T.pack (show a) <> " but received " <> T.pack (show b))
            )
        InvalidUrlException url reason -> (False, (T.pack url) <> " is malformed because: " <> (T.pack reason))
    pnn :: Int -> Text
    pnn (show -> n) = T.pack $ n <> case last n of
        '1' -> "st"
        '2' -> "nd"
        '3' -> "rd"
        _   -> "th"
-}

-- | The preferred normalization. Convenience function.
normalize :: URIRef a -> BS'.ByteString
normalize = normalizeURIRef' aggressiveNormalization

-- | Made to parse href elements from HTML. Remember that hand-written URLs like "site.com/page1" are incorrectly parsed as relative url paths.
parseURI :: BS'.ByteString -> Either URIParseError (Either (URIRef Relative) (URIRef Absolute))
parseURI s = let rel = either Left (pure . Left) $ parseRelativeRef laxURIParserOptions s in case U.parseURI laxURIParserOptions s of
    Left (MalformedScheme MissingColon) -> rel
    Left (MalformedScheme NonAlphaLeading) -> rel
    Left o -> Left o
    Right r -> pure . Right $ r {uriPath = replIf BSC'.null (uriPath r) "/"}

-- | Produce an absolute URL from a @parseURI@ value
relTo :: URIRef a -> URIRef Absolute -> URIRef Absolute
parsed `relTo` (URI {..}) = case parsed of -- URI and RelativeRef have different field names, so wildcards are safe
    RelativeRef {..} ->
        let withDots = replIf (not . (("./" `BSC'.isPrefixOf`) <||> ("../" `BSC'.isPrefixOf`))) rrPath ("../" <> rrPath)
            !rrPath' = replIf (not . ("/" `BSC'.isPrefixOf`)) rrPath
                     $ concatPaths 0x2F uriPath withDots
        in URI uriScheme uriAuthority rrPath' rrQuery rrFragment
    a@(URI _ _ _ _ _) -> a -- parsed was absolute already; keep as-is

-- | Inherit scheme and/or domain from a given absolute url. Convenience function built atop 'parseURI' and 'relTo'.
parseRelTo :: URIRef Absolute -> BS'.ByteString -> Either URIParseError (URIRef Absolute)
parseRelTo base bs
    | BSC'.null bs = Left (OtherError "parsing a null relative path is nonsensical")
    | otherwise =
        let bs' = replIf ("//" `BS'.isPrefixOf`) bs (U.schemeBS (uriScheme base) <> ":" <> bs)
        in case parseURI bs' of
            Left l -> Left l
            Right x -> pure $ either (`relTo` base) id x

-- | Useful for 'bsResponse' or 'ignoreResponse'. Remember that a response includes HTTP status, headers, and cookies as well as the body; use 'responseBody' to get the body.
--
-- Do not use 'jsonResponse'; its implementation of 'getHttpResponse' uses 'throwIO' on JSON parse failure (and uses lazy @eitherDecode@.) Instead, it's better to use @eitherDecode' . bsResponse@ yourself.
--
-- Also, do not use @lbsResponse@; use 'reqBc' instead.
req :: (MonadHttp m, MonadThrow m, HttpMethod method, HttpBody body, HttpResponse response, HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
    => method
    -> URIRef Absolute
    -> body
    -> Proxy response
    -> (forall scheme. Option scheme)
    -> m response
req m uri body p opts = reqHelper opts uri $ \(u,o) -> Req.req m u body p o

-- | Consume HTTP response body via Conduit (wrapper around req-conduit's 'responseBodySource', that's easier to use, and obviously, that works with 'URIRef's)
reqBc :: (MonadHttp m, MonadThrow m, HttpMethod method, HttpBody body, HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
      => method
      -> URIRef Absolute
      -> body
      -> (forall scheme. Option scheme)
      -> (forall f. MonadResource f => ConduitT BS'.ByteString Void f a) -- ^ e.g. 'sinkFile'
      -> m a
reqBc m uri body opts consumer = reqHelper opts uri $ \(u,o) ->
    Req.reqBr m u body (o <> opts) (\respBodyReader -> runConduitRes $ responseBodySource respBodyReader .| consumer)

req' :: (MonadHttp m, MonadThrow m, HttpMethod method, HttpBody body, HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
     => method
     -> URIRef Absolute
     -> body
     -> (forall scheme. Option scheme)
     -> (Request -> Manager -> m a)
     -> m a
req' m uri body opts f = reqHelper opts uri $ \(u,o) -> Req.req' m u body (o <> opts) f

-- ** Helper/static functions (do not export)

type UO s = (Url s, Option s)
type EUO = Either (UO 'Http) (UO 'Https)
newtype GOpt = GOpt { getOpts :: forall (s :: Scheme). Option s }
instance Semigroup GOpt where GOpt a <> GOpt b = GOpt $ a <> b
instance Monoid GOpt where mempty = GOpt mempty

-- common function to all the req* functions
reqHelper :: MonadThrow m
          => (forall scheme. Option scheme)
          -> URIRef Absolute
          -> (forall scheme. UO scheme -> m a)
          -> m a
reqHelper o u f = uriToUrl u >>= (f ||| f) . BiF.bimap (second (<>o)) (second (<>o))

-- | May throw 'BadUrlException' or 'UnicodeException'. Disregards URI fragments
uriToUrl :: forall m. MonadThrow m => URIRef Absolute -> m EUO
uriToUrl (URI {uriScheme, uriAuthority, uriPath, uriQuery}) = case uriAuthority of
    Nothing -> throw $ MalformedURL "Authority"
    Just (Authority {authorityUserInfo, authorityHost = ah, authorityPort}) ->
        let -- apply to a url after giving it a scheme via http or https functions
            addPath :: Url scheme -> m (Url scheme)
            addPath z =
                if BS'.null uriPath then
                    pure z
                else
                    foldM (\b -> fmap (b /:) . decode) z (BSC'.split '/' (BSC'.tail uriPath))

            wrap :: EUO -> m EUO
            wrap (Left (x, y)) = Left . (,y) <$> addPath x
            wrap x = pure x
        in do
            host <- decode (hostBS ah)
            gOpts <- (GOpt (maybe mempty (port . portNumber) authorityPort) <>)
                <$> foldMapM
                    (uncurry (liftA2 (\k v -> GOpt $ if T.null v then queryFlag k else k =: v)) . both decode)
                    (queryPairs uriQuery)
            case authorityUserInfo of
                Nothing ->
                    case BSC'.map toLower $ U.schemeBS uriScheme of
                        "http"  ->   wrap $ Left  (http  host, getOpts gOpts)
                        "https" ->   wrap $ Right (https host, getOpts gOpts)
                        _       ->   throw NonHTTP
                Just (UserInfo user pass) ->
                    wrap $ Right (https host, basicAuth user pass <> getOpts gOpts)
  where
    decode :: BSC'.ByteString -> m Text
    decode = either throw pure . decodeUtf8'

-- | Runs 'parseURI' then pulls-out uri, assuming that it's absolute and well-formed
unsafeParseURI :: BSC'.ByteString -> URIRef Absolute
unsafeParseURI = (\case Right (Right x) -> x) . parseURI

-- | Replace a value if predicate holds over it; else return the same object
--
-- Useful for incremental modifications to a variable to ensure that it is well-formed
replIf :: (a -> Bool) -> a -> a -> a
replIf p a e = if p a then e else a
{-# INLINE replIf #-}
