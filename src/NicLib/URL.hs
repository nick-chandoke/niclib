{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE
  DeriveDataTypeable
, DeriveGeneric
, FlexibleInstances
, GeneralizedNewtypeDeriving
, LambdaCase
, MultiWayIf
, MultiParamTypeClasses
, NamedFieldPuns
, OverloadedStrings
, NoOverloadedLists
, TupleSections
, ViewPatterns
#-}

-- TODO (unnecessary:)
--   * fix // occuring in some paths (e.g. str2url "/wiki/Special:Search" (str2url "http://google.com" Nothing)) by fixing dots, rather than using the T'.replace "//" "/" hack
--   * general delimiters & other reserved characters (e.g. ';' for delimiting query params)
--   * IPv6 address domains
-- check with <https://www.skorks.com/2010/05/what-every-developer-should-know-about-urls/> to ensure that my library is complete

-- As name implies, this is for URLs specifically; URNs or general URIs are not handled here.
-- see RFC 3986 <https://tools.ietf.org/html/rfc3986> for details on URLs
-- percent-encoded URLs are considered different URLs from their unencoded equivalents
module NicLib.URL
( URL(..)
, URLParserState(..)
, Query(..)
, query2BS
, str2url
, ppURL
, makeQuery
, reduce
) where
import Combinator.Booly
import Data.Binary (Binary)
import Data.Data
import Data.Foldable (find)
import Data.Function
import GHC.Generics (Generic)
import NicLib.Aeson
import NicLib.NStdLib
import NicLib.Parser (parse)
import Prelude hiding (FilePath)
import qualified Data.ByteString.Char8 as BS'
import qualified Data.Map.Lazy as M
import qualified Data.Ord as Ord
import qualified Data.Text as T'

-- | helper function. mostly used for isomorphisms and equality/ordering
-- does not affect scheme, query, nor fragment, as these aren't used in any functions anyway; they exist solely for use with client programs
-- idempotent automorphism
-- I don't want to put this in str2url; I want URLs to retain their exact information as parsed from a string, but have the URL act in most situations like its reduced version. If anyone wants to not use reduce, they can wrap URL in newtype and write new instances.
-- remember that reduce is needed in morphisms from URL to other things; as the name implies, reduce does not permit an inverse
reduce :: URL -> URL
reduce u@(URL {domain, path}) = u {scheme = mempty, domain = fromMaybe domain (T'.stripPrefix "www." domain), path = rmTrailing '/' path, query = Query [], fragment = mempty}

data URLParserState = SchemeUPS | {- UsernameUPS | PasswordUPS | -} DomainUPS | {- PortUPS | -} PathUPS | QueryUPS | FragmentUPS deriving (Eq, Enum, Bounded, Data, Generic)
instance Show URLParserState where show = (\s -> take (length s - 3) s) . showConstr . toConstr

data URL = URL { scheme :: T'.Text
--             , username :: T'.Text
--             , password :: T'.Text
               , domain :: T'.Text
--             , port :: T'.Text
               , path :: T'.Text
               , query :: Query
               , fragment :: T'.Text
               } deriving Show

-- NB. URL equality & isomorphism:
--   1) we assume all URLs are http- or https-schemed; we assume https if no scheme is given, for instance in the context of relative URLs
--   2) we assume that the following are meaningless: queries, fragments, and trailing slashes in URL paths
instance Ord URL where
    compare u v =
        let u' = reduce u -- lazy let block means these are evaluated only if the first condition below fails. Note in the first condition, (==) already reduces both URLs, so there's no point in doing it here AND there
            v' = reduce v
        in if | u == v -> Ord.EQ
              | on (<) domain u' v' -> if on (<) path u' v' then Ord.LT else Ord.GT
              | otherwise -> Ord.GT

-- | URLs equal iff they have matching paths (regardless of trailing slashes) and same domain (not including login info or port)
instance Eq URL where (==) = curry (uncurry (on (==) (domain &&& path)) . both reduce)

-- | Isomorphism in the context of crawling. Preserves equality and order: iso u1 == iso u2 <=> u1 == u2. Thus a reduced, "canonicalized" form is created; a URL as a String will lose information considered inessential: query, fragment, scheme, port, and user info
instance Isomorphism URL String where
    iso (reduce -> URL {domain, path}) = T'.unpack $ domain <> path
    dual t = fromMaybe (error $ "trying to parse malformed URL to string: \"" ++ t ++ "\"") . flip str2url Nothing $ T'.pack t

-- | On many systems '/' is path seperator. Each URL should have a filename without needing to create new directories, so in this isomorphism we replace '/' with an innocuous character
-- NOTE: some URL queries may form illegally large pathnames, depending on which filesystem you're using, and the parent directory of the URL-resultant filepath
instance Isomorphism URL FilePath where
    iso  = FilePath                . replaceAll '/' '\US' . (iso :: URL -> String) . reduce
    dual = (dual :: String -> URL) . ("http://" <>) . replaceAll '\US' '/' . unwrapFilePath

newtype Query = Query { getQuery :: [(BS'.ByteString, Maybe BS'.ByteString)] } deriving (Ord, Eq, Binary, Semigroup, Monoid)
instance {-# OVERLAPPING #-} Show Query where show = BS'.unpack . query2BS

query2BS (getQuery -> []) = mempty
query2BS (getQuery -> q) = BS'.cons '?' . BS'.init {- to remove trailing ampersand -} $ foldMap (\(k, mv) -> k <> (maybe mempty (BS'.cons '=') mv) <> "&") q

instance ToJSON URL where
    toJSON url     = object ["url" .= ppURL url]
    toEncoding url = pairs  ("url" .= ppURL url)

instance FromJSON URL where -- non-exhaustive patterns: (Array _), (String _),...; can I use NicLib.Aeson to make it better?
    parseJSON (Object v) = (dual :: String -> URL) <$> v .: "url"

-- | display a URL as a string. URL's show instance verbosely displays a URL as a Haskell record type, so this is "pretty-print" compared to that
ppURL :: URL -> T'.Text
ppURL (URL {scheme, {- username, password, -} domain, {- port, -} path, query, fragment}) = mconcat
    [ bool' (\s -> s <> if "http" `T'.isPrefixOf` s then "://" else ":") id T'.null scheme
--  , bool' (\u -> u <> bool' (T'.cons ':') id T'.null password <> "@") id T'.null username
    , domain
--  , bool' (T'.cons ':') id T'.null port
    , path
    , T'.pack $ show query
    , bool' (T'.cons '#') id T'.null fragment
    ]

-- | Non-strict parsing of well-formed http(s) URLs. Undefined behavior for malformed URLs; a malformed URL may produce either Nothing or a URL object with unsensible record values. A non-http(s) URL will parse into scheme and the remainder of the URL will be stored in path.
-- str2url does not follow relative URL parsing as described in §5.2.2 of the RFC. Instead, simply, the relative URL we're parsing will inherit any missing fields (execpting query and fragment) from the referrer, if a referrer is given, and if those missing fields are non-empty in the referrer. A relative URI without a referrer will always produce Nothing.
-- The merging of paths and removal of dot segments (§5.2.3 and §5.2.4 respectively) are implemented with functions equivalent to the algorithms given in the RFC. (Or at least they're supposed to be; see below.)
-- str2url mempty _ = Nothing
-- The second parameter is an absolute URL used to absolutify the URL we're trying to parse, if that URL is relative (i.e. its domain is null). Trying to parse a relative URL without a referer will produce Nothing. If parsing an absolute URL, the referer has no effect.
-- Ambiguity resolution:
-- * "google.com" vs. "index.html": parsed as path if a referrer is given; otherwise domain
-- * "user:pass@domain.com" vs. "mailto:user@domain.com": assumes latter (scheme, user, and domain, except that, because this is not http(s), it'll actually be scheme and path)
-- "nic@com.com" produces a URL whose only non-null field is domain, with "nic@com.com" as its value. In the first versions of this parser, I would assume nic as username, and com.com as domain. However, after considering the mailto scheme, I realized that such a URL is really ambiguous, depending on which scheme I assume! Thus I return a garbage URL. Perhaps I should return Nothing or Left "ambiguous".... There are many URLs of such a form (where it looks like a sensible URL, but when you examine it in enough detail that you're able to parse it, you find that you actually cannot parse it,) and I'm quite uncertain about my ability to debug them.
-- str2url was written for a web scraper. Although I took some effort to make it work for URLs in general, testing that it works for all kinds of URLs is tricky. I can say that works for all properly-formed sensible http(s) URLs, and all URLs whose schemes do not have double slashes. In hindsight, I see that it handles a great many nonsense cases: when parsing login credentials, port, and domain, http URLs where the http scheme does not have double-slashes - which, obviously, does not exist!
-- Compare the differences (marked with an asterisk) of str2url to the expected values as given in §5.4.1 of the RFC:
-- mapM_ (\(str, url) -> TIO.putStrLn $ str <> " -> " <> maybe "Nothing" showURL url) $ (id &&& flip str2url (str2url "http://a/b/c/d;p?q" Nothing)) <$> ["g:h", "g", "./g", "g/", "//g", "?y", "g?y", "#s", "g#s", "g?y#s", ";x", "g;x", "g;x?y#s", "", ".", "./", "..", "../", "../g", "../..", "../../", "../../g"]
-- "g:h"      -> "g://h"                  *
-- "g"        -> "https://a/b/c/g"
-- "./g"      -> "https://a/b/c/g"
-- "g/"       -> "https://a/b/c/g/"
-- "//g"      -> "https://a/b/c///g"      *
-- "?y"       -> "https://a/b/c/?y"       *
-- "g?y"      -> "https://a/b/c/g?y"
-- "#s"       -> "https://a/b/c/#s"       *
-- "g#s"      -> "https://a/b/c/g#s"
-- "g?y#s"    -> "https://a/b/c/g?y#s"
-- ";x"       -> "https://a/b/c/;x"
-- "g;x"      -> "https://a/b/c/g;x"
-- "g;x?y#s"  -> "https://a/b/c/g;x?y#s"
-- ""         -> Nothing                  *
-- "."        -> "https://a/b/c/"
-- "./"       -> "https://a/b/c/"
-- ".."       -> "https://a/b/"
-- "../"      -> "https://a/b/"
-- "../g"     -> "https://a/b/g"
-- "../.."    -> "https://a/"
-- "../../"   -> "https://a/"
-- "../../g"  -> "https://a/g"
-- basically, this isn't a perfect function, but it works well enough for parsing most URLs (i.e. those without login creds or port specification), so who cares?
-- T'.foldl' (\b@(colonCount, q, f) a -> case a of ':' -> (succ colonCount, q, f); '?' -> (colonCount, True, f); '#' -> (colonCount, q, True); _ -> b) (0, False, False)
str2url :: T'.Text -> Maybe URL -> Maybe URL
str2url (T'.null -> True) _ = Nothing
str2url t mref = first (b '?') (b '#' t) & \((t', q), f) -> -- pull-off fragment, then query, from right side. Always works assuming URL has no more than one '?' or '#', and '#' doesn't preceed '?'
    find (\(URL {scheme}) -> not (T'.any (=='/') scheme)) (SchemeUPS:bool [] [PathUPS] (isJust mref) >>= \s0 -> parse parser s0 (URL "" "" "" (makeQuery q) f) t') >>= \url -> scheme url & \scheme' ->
        if T'.null scheme' || "http" `T'.isPrefixOf` scheme' then
            if (T'.null . domain) url then -- inherit missing fields from referer
                mref <&> \ref -> url
                    { scheme = scheme' <|< scheme ref <|< "https"
--                  , username = username url <|< username ref
--                  , password = password url <|< password ref
                    , domain = domain ref
--                  , port = port url <|< port ref
                    , path = T'.replace "//" "/" . dots $ merge (path ref) (path url)
                    }
            else
                return $ url {scheme = scheme url <|< "https", path = dots $ path url}
        else
            fold (sequenceA [{- username, password, -} domain, {- port, -} path] url) & \str -> return $ url {scheme = scheme', {- username = mempty, password = mempty, -} domain = mempty, {- port = mempty, -} path = str}
    where
        parser =
            [ (SchemeUPS, \url text -> case takeFirst ["://", ":"] text of Nothing -> [(url, text, Just PathUPS)]; Just (scheme', rest) -> [(url {scheme = scheme'}, rest, Just DomainUPS)])
            , (DomainUPS, \url text -> T'.break (=='/') text & \(domain', rest) -> [(url {domain = domain'}, rest, Just PathUPS)])
            , (PathUPS, \url text -> [(url {path = text}, undefined, Nothing)])
            ]

        -- | breaks text on first occuring delimiter
        takeFirst :: [T'.Text] -> T'.Text -> Maybe (T'.Text, T'.Text)
        takeFirst delims text = find (not . T'.null . snd) ((\delim -> T'.breakOn delim text & \(p1,p2) -> (p1, T'.drop (T'.length delim) p2)) <$> delims)

        {- | parse what information we can from a URL; we don't fill-in missing information with defaults here; that's done in the context inside str2url but outside parse. In particular, the referer should be absolute, and so should have every field available to populate any missing ones in what's returned from parse
        -- notably, scheme and path may be empty
        -- all the "_ -> Nothing"'s herein are for pattern-matching completeness. They are unreachable, though.
        parseURL :: T'.Text -> URL -> Maybe URL
        parseURL p0 u0 = case T'.splitOn "://" p0 of
            [scheme', r] ->
                {- if isJust $ T'.find (=='@') r then -- at-sign has semantic significance only if scheme uses double slashes
                    bimap (b ':') (first (b ':') . T'.break (=='/')) (b '@' r) & \((user, pass), ((domain', port'), path')) ->
                        return $ u0 {scheme = scheme', username = user, password = pass, domain = domain', port = port', path = path'}
                else u0 {username = mempty, password = mempty} & \u0 -> -}
                    first (b ':') (T'.break (=='/') r) & \((domain', port'), path') ->
                        return $ u0 {scheme = scheme', domain = domain', {- port = port', -} path = path'}
            [_] -> {- u0 {username = mempty, password = mempty} & \u0 -> -} case T'.split (==':') p0 of -- either there's a scheme that doesn't use double slashes, or there's no scheme. Regardless of whether there's a scheme, we need to parse domain, port, and path
                [scheme', domain', portNPath] -> T'.break (=='/') portNPath & \(port', path') -> return $ u0 {scheme = scheme', domain = domain', {- port = port', -} path = path'}
                [α, d] -> T'.break (=='/') d & \(β, path') -> return $ if T'.all (inRange ('0','9')) β then u0 {scheme = mempty, domain = α, {- port = β, -} path = path'} else u0 {scheme = α, domain = β, {- port = mempty, -} path = path'}
                [d] -> b '/' p0 & \(domain', path') -> case mref of
                    Nothing -> return $ u0 {scheme = mempty, domain = domain', {- port = mempty, -} path = path'}
                    Just ref -> return $ u0 {scheme = mempty, domain = mempty, {- port = mempty, -} path = d}
                _ -> Nothing
            _ -> Nothing

        -}
        b c = second tail' . T'.break (==c) -- break on a delimiter character, discarding that delimiter

        -- | merge base and relative path, as described in RFC §5.2.3
        merge :: T'.Text -> T'.Text -> T'.Text
        merge basePath referencePath {- i.e. relativePath -} = if T'.null basePath then T'.cons '/' referencePath else T'.dropWhileEnd (/= '/') basePath <> referencePath

        -- | remove dot segments, as described in §5.2.4 of the RFC
        -- TODO: algorithmically seriously tune prefix checking and replacement: discover implications of conditionals (e.g. begins with ../ => doesn't begin with ./) to ensure that correct ordering and non-redundancy of conditionals is. Even better is putting them all as a fold or FSM
        dots :: T'.Text -> T'.Text
        dots inBuf = go inBuf mempty where
            go inBuf outBuf =
                if T'.null inBuf then outBuf
                else if | "./" `T'.isPrefixOf` inBuf -> go (T'.drop 2 inBuf) outBuf -- A.
                        | "../" `T'.isPrefixOf` inBuf -> go (T'.drop 3 inBuf) outBuf -- A.
                        | "/./" `T'.isPrefixOf` inBuf -> go (T'.cons '/' $ T'.drop 3 inBuf) outBuf -- B.
                        | "/." == inBuf -> go (T'.cons '/' $ T'.drop 2 inBuf) outBuf -- B.
                        | "/../" `T'.isPrefixOf` inBuf -> go (T'.drop 3 inBuf) (bool' T'.init id T'.null $ T'.dropWhileEnd (/= '/') outBuf) -- C.
                        | "/.." == inBuf -> go (T'.cons '/' $ T'.drop 3 inBuf) (bool' T'.init id T'.null $ T'.dropWhileEnd (/= '/') outBuf) -- C.
                        | ".." == inBuf -> go (T'.drop 2 inBuf) outBuf -- D.
                        | "." == inBuf -> go (T'.tail inBuf) outBuf -- D.
                        | otherwise -> (if "/" `T'.isPrefixOf` inBuf then first (T'.cons '/') . T'.break (== '/') $ T'.tail inBuf else T'.break (== '/') inBuf) & \(firstSegment, rest) -> go rest (outBuf <> firstSegment) -- E.
        
        -- | dropWhileEnd (/= '/') for ByteStrings
        {-dropAfterLastSlash :: BS'.ByteString -> BS'.ByteString
        dropAfterLastSlash bs = maybe mempty (flip BS'.take bs . succ) (BS'.elemIndexEnd '/' bs)-}

-- | parses query string into a Query from text
makeQuery :: T'.Text -> Query
makeQuery = Query . M.toAscList . M.delete mempty . M.fromList . fmap ((\(k:v) -> (k, listToMaybe v)) . (BS'.split '=')) . filter (not . BS'.null) . BS'.split '&' . BS'.pack . iso
