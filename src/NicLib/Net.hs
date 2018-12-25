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
-- Both are GADT binary coproducts:
--
-- * @URIRef@: @Absolute@ | @Relative@
-- * @Url@: @Http@ | @Https@
--
-- Becasue every @Url@ has a scheme, all @Urls@ created from a @URIRef@ are computed from at least one @URIRef Absolute@.
--
-- Also, the req 'Options' opaque monoid is predicated over a scheme, but the only functions that use a specific scheme are
--
-- * basicAuth
-- * oAuth2Bearer
-- * oAuth2Token
--
-- and all of these are where @scheme ~ 'Https@. This means that @Option 'Http@ ⊂ @Option 'Https@. Thus all http @Option@s are assumed as http or https options.
module NicLib.Net
( -- * Parse
  parseURI
, parseRelTo
-- * URI/URL Endos
, relTo
-- * Render
, normalize
-- * Exception Type
, NetException(..)
-- * Re-exports from req or req-conduit
, Url
-- ** Custom Monad Config
, MonadHttp(..)
, HttpConfig(..)
-- ** Request
, req
, reqBr
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
-- ** Options & Query
, Option
, responseTimeout
, httpVersion
, header
, cookieJar
, decompress
, basicProxyAuth
, oAuth1
-- *** HTTPS-Exclusive Options
, basicAuth
, oAuth2Bearer
, oAuth2Token
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
, responseBodySource
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
) where

-- base
-- import Data.Trie (Trie)
-- import qualified Data.Trie as Trie
import Data.Char (toLower)
import Data.Proxy
import Data.Either (either)
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Control.Applicative (empty)

-- text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

-- NicLib
import NicLib.NStdLib (both)
import NicLib.AccumShort (liftME)

-- bytestring
import qualified Data.ByteString as BS'
import qualified Data.ByteString.Char8 as BSC'

-- misc
import Control.Exception.Safe -- safe-exceptions
import URI.ByteString hiding (parseURI, Scheme(..)) -- uri-bytestring
import qualified URI.ByteString as U

-- req and req-conduit
import Network.HTTP.Client (Manager, Request, Response, BodyReader)
import Network.HTTP.Req hiding (req, req', reqBr)
import Network.HTTP.Req.Conduit
import qualified Network.HTTP.Req as Req

data NetException
    = NonHTTP
    deriving (Show, Typeable)

instance Exception NetException

{-
instance MonadHttp _ where
    handleHttpException = Left . fromString . show
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
                ResponseBodyTooShort a b               -> (False, "Request body unexpected size (too Left); expected " <> T'.pack (show a) <> " but received" <> T'.pack (show b))
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

{- Function for testing this library!
-- | Given the number of things I'll be creating ExceptT's for, maybe I should have [forall a. Show a => a] as the ExceptT parameter.
getExample :: _
getExample = do
    exampleBase <- parseURI "https://google..com/"
    url <- parseRelTo exampleBase "http://wiki.c2.com/?WhyWeLoveLisp"
    case url of
        Left x -> f x
        Right x -> f x
    where
        f :: (MonadIO m, Monoid w, IsString e) => (Url scheme, Option scheme) -> BugT e w m ()
        f (u,o) = do
            resp <- req GET u NoReqBody bsResponse o
            liftIO . BSC'.putStrLn $ responseBody resp
-}

-- | The preferred normalization. Convenience function.
normalize :: URIRef a -> BS'.ByteString
normalize = normalizeURIRef' aggressiveNormalization

-- | Made to parse href elements from HTML. Does not account for hand-written URLs like "site.com/page1", as this would not work in a browser.
--
-- Note that although such URIs are parsed correctly into authority and path, they're still of the @URIRef Realtive@ type because they are without a scheme.
--
-- Overrides 'Network.HTTP.Req.parseURI', supporting both absolute and relative 'URIRef's, rather than absolute only.
parseURI :: BS'.ByteString -> Either URIParseError (Either (URIRef Relative) (URIRef Absolute))
parseURI s = case U.parseURI laxURIParserOptions s of
    Left (MalformedScheme MissingColon) -> either Left (pure . Left) $ parseRelativeRef laxURIParserOptions s
    Left o -> Left o
    Right r -> pure $ Right r

-- TODO: parse basic authentication (user:pass@domain) and include as (Option 'Https) via basicAuth
-- | Returns @Nothing@ on non-http(s) schemes.
uriToUrl :: URIRef Absolute -> Maybe (Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https))
uriToUrl (URI {..}) =
    let p :: Url a -> Url a -- type signatures given to keep polymorphism
        p z = foldl' (\b a -> b /: decodeUtf8 a) z (BSC'.split '/' uriPath)
        q :: Option a
        q = foldMap (uncurry (=:) . both decodeUtf8) (queryPairs uriQuery)
        authText = decodeUtf8 . hostBS . authorityHost $ fromJust uriAuthority
        g :: e ~ Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https)
          => ((Url a, Option a) -> e)
          -> (Text -> Url a)
          -> Maybe e
        g side f = pure $ side (p (f authText), q)
    in case BSC'.map toLower $ U.schemeBS uriScheme of
        "http"  -> g Left http
        "https" -> g Right https
        _ -> empty

-- | Produce an absolute URL from a @parseURI@ value
relTo :: URIRef a -> URIRef Absolute -> URIRef Absolute
parsed `relTo` (URI {..}) = case parsed of -- URI and RelativeRef have different field names, so wildcards are safe
    RelativeRef {..} -> URI uriScheme uriAuthority rrPath rrQuery rrFragment -- make parsed relative to the absolute URIRef
    a@(URI _ _ _ _ _) -> a -- parsed was absolute already; keep as-is

-- | The most convenient way to parse a URI for a scraper (considering that a scraper is seeded with an initial URL, and stays within that URL's domain.) Combines @parseURI@, @relTo@, and @uriToUrl@.
--
-- @Left Nothing@ means non-https scheme; other @Left@'s are @URIParseError@s
parseRelTo :: URIRef Absolute -> BS'.ByteString -> Either (Maybe URIParseError) (Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https))
parseRelTo base bs = case parseURI bs of
    Left l -> Left (Just l)
    Right (either (`relTo` base) id -> absOrRel) -> liftME Nothing $ uriToUrl absOrRel

-- | Make a request from an absolute URI. Given how req handles @Url@, there're no compile-time-safe ways to pass-in @Option@s; use _ if you want to pass @Option@s that apply to both @'Http@ and @'Https@ schemes.
req :: (MonadHttp m, MonadThrow m, HttpMethod method, HttpBody body, HttpResponse response, HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
    => method
    -> URIRef Absolute
    -> body
    -> Proxy response
    -> (forall scheme. Option scheme)
    -> m response
req m uri body proxy opts = reqHelper (\(u,o) -> Req.req m u body proxy (o <> opts)) uri

reqBr :: (MonadHttp m, MonadThrow m, HttpMethod method, HttpBody body, HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
      => method
      -> URIRef Absolute
      -> body
      -> (forall scheme. Option scheme)
      -> (Response BodyReader -> IO a)
      -> m a
reqBr m uri body opts f = reqHelper (\(u,o) -> Req.reqBr m u body (o <> opts) f) uri

req' :: (MonadHttp m, MonadThrow m, HttpMethod method, HttpBody body, HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
     => method
     -> URIRef Absolute
     -> body
     -> (forall scheme. Option scheme)
     -> (Request -> Manager -> m a)
     -> m a
req' m uri body opts f = reqHelper (\(u,o) -> Req.req' m u body (o <> opts) f) uri

-- | CPS helper (do not export)
reqHelper :: (MonadHttp m, MonadThrow m)
          => (forall scheme. (Url scheme, Option scheme) -> m response)
          -> URIRef Absolute
          -> m response
reqHelper f = maybe (throw NonHTTP) (either f f) . uriToUrl
