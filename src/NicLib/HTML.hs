{-# LANGUAGE
  FlexibleInstances
, FlexibleContexts
, LambdaCase
, MonadComprehensions
, NamedFieldPuns
, OverloadedStrings
 #-}

-- | general HTML manipulation. Re-exports Taggy. Very useful for scraping.
module NicLib.HTML
( module Text.Taggy
, dispNode
, dispDOM
, parseDOM'
, scrapeURLs
, getText
, nonRenderTags
, domFromURL
, pageLinks
, findByTags
, NodeFilter(..)
, tagFilter
, (∈)
, (|:)
, fileToDOM
, NodeDisplay -- do not export constructor; force user to use methods withTags, withText, &c
, withTags
, withText
) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Catch
import Data.Foldable (find)
import Data.ListLike (ListLike)
import Data.ListLike.String
import Data.String (IsString)
import Data.Text.Lazy.Encoding (decodeUtf8')
import Data.Tree as Tree
import NicLib.IO
import NicLib.NStdLib
import NicLib.Net
import NicLib.URL
import Text.Taggy
import qualified Data.Bifunctor as BiF
import qualified Data.ByteString as BS'
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as S
import qualified Data.Text as T'
import qualified Data.Text.Lazy.IO as TLIO

-- | convenience method for GHCi testing
fileToDOM :: String -> IO Node
fileToDOM = fmap (head . parseDOM True) . TLIO.readFile

-- | helper method (mostly for debugging in GHCi) for getting a DOM root Node from a URL
-- uses dummy empty cookie jar
-- the result monad is the monad inside EIO (∀m. ExceptT ErrorMsg m); remember to lift as needed
domFromURL :: (MonadIO m, MonadCatch m, StringLike s, IsString s, ListLike s Char, Semigroup s) => URL -> m (Either s Node)
domFromURL = fmap (>>= parseDOM') . runExceptT . flip evalStateT (createCookieJar []) . fetch

-- | scape a page's <body> for its scrapable links (e.g. <a>, <link>, <script>)
-- parameter is URL to parse
-- uses empty cookie jar
pageLinks :: (MonadIO m, MonadCatch m, IsString s, StringLike s, Semigroup s, ListLike s Char) => T'.Text -> m (Either s (S.Set T'.Text, S.Set URL))
pageLinks u = runExceptT $ do
    url <- ExceptT . return . liftME (fromString "URL parse error") $ str2url u Nothing
    dom <- ExceptT $ domFromURL url
    return $ scrapeURLs dom (Just url)

-- !! do something special for <del>, <s>, and <strike>; these are "anti-text"
-- do something special for tables too
-- nonRenderTags and breakingTags consider only descendents of <html> (<head>'s descendentsaren't considered)

-- neither these elements nor descendents thereof are renderable
nonRenderTags :: S.Set T'.Text
nonRenderTags = S.fromList ["area", "audio", "button", "canvas", "colgroup", "embed", "img", "map", "meta", "meter", "noscript", "object", "param", "picture", "progress", "script", "source", "svg", "textarea", "track", "video"]

-- essentially text elements that display block. mutually exclusive with nonRenderTags
breakingTags :: S.Set T'.Text
breakingTags = S.fromList ["address", "blockquote", "br", "caption", "cite", "code", "dd", "details", "dialog", "div", "dl", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "li", "menu", "menuitem", "ol", "option", "p", "pre", "q", "samp", "section", "select", "summary", "table", "thead", "tfoot", "tr", "ul"]

-- DOM display combinators
-- maybe add non-text content later

data NodeDisplay = NodeDisplay {tags :: Bool, withAttrs :: Bool, text :: Bool}
instance Semigroup NodeDisplay where
    NodeDisplay a b c <> NodeDisplay x y z = NodeDisplay (a || x) (b || y) (c || z) -- assuming that NodeDisplay constructors are mutually exclusive, with non-constructor NodeDisplay attributes defaulting to False, (||) is good

-- | bool is whether to display attributes or not
withTags :: Bool -> NodeDisplay
withTags wa = NodeDisplay True wa False

withText :: NodeDisplay
withText = NodeDisplay False False True

dispNode :: NodeDisplay -> Node -> T'.Text
dispNode (NodeDisplay {tags, withAttrs, text}) = \case NodeElement e -> if tags then element2text e else mempty; NodeContent t -> if text then T'.strip t else mempty
    where
        element2text (Element {eltName, eltAttrs}) = "<" <> eltName <> (if withAttrs && not (HM.null eltAttrs) then " " <> attrs2text eltAttrs <> ">" else ">")
        attrs2text = T'.intercalate " " . HM.foldlWithKey' (\a k v -> (k <> "=\"" <> v <> "\""):a) mempty

dispDOM :: NodeDisplay -> Tree Node -> IO ()
dispDOM disp = putStrLn . Tree.drawTree . fmap (T'.unpack . dispNode disp)

-- | just a little syntactic sugar method
-- assumes there's just one DOM root; if multiple, it'll take the first one only
-- TODO: use with <http://hackage.haskell.org/package/text-icu>
-- parseDOM' returns (Right []) => given bytestring was null or had opening tag bracket without any closing brackets. Even a string of garble will return [NodeContent "garble"]. However, "<oou", "oo<u", &c will return [].
parseDOM' :: StringLike s => BS.ByteString -> Either s Node
parseDOM' = liftME (fromString "Empty DOM") . listToMaybe <=< liftM (parseDOM True) . (BiF.first (fromString . show) . decodeUtf8')

-- | searches a node's descendants for URLs, as per tag2URL. Returns (malformed URLs, valid URLs)
-- Referrer URL is needed to parse relative URLs that may appear in DOM. You'll likely want to pass just the domain name itself, e.g. (str2url "https://site.com" Nothing)
-- The node you pass must be a DOM root (doctype); for searching general DOM nodes, see [write a function and put it here]
scrapeURLs :: Node -> Maybe URL -> (S.Set T'.Text, S.Set URL)
scrapeURLs dom m_uref = case ((flip str2url Nothing <=< (\case NodeElement e -> "href" `HM.lookup` eltAttrs e)) =<< findByTags ["doctype", "html", "head", "base"] dom) <|> m_uref of
    m_uref -> case find (\case NodeElement e -> T'.toLower (eltName e) == "body"; _ -> False) (unfoldTree (id &&& nodeChildren) dom) of -- only pull scrapables from children of <body>, not <head>
        Nothing -> mempty
        Just body -> flip foldMap ((iso :: Node -> Tree Node) body) $ \node -> case tag2URL node m_uref of -- can get away with two unfolds like this only in a non-strict language such as Haskell!
            Nothing -> mempty
            Just eiURL -> case eiURL of
                Left  malformed  -> (S.singleton $ T'.pack (show malformed), mempty)
                Right wellformed -> (mempty, S.singleton wellformed)
    where
        -- | tries to pull URLs from a NodeElement's attribute(s) (does not scrape NodeContext text for URL-like patterns)
        -- if the Node does not have an attribute from which a URL can be derived, then Nothing is returned. If it does, we try to parse it into a URL object; if parsing fails (i.e. the URL is malformed), its text value is returned in a Left; otherwise a correct URL object is returned in Right - the behavior we're hoping for
        tag2URL :: Node -> Maybe URL -> Maybe (Either T'.Text URL)
        tag2URL (NodeElement (Element {eltName, eltAttrs})) m_uref = [maybe (Left url) Right $ str2url url m_uref | url <- HM.lookup eltName tagattrs >>= (`HM.lookup` eltAttrs)] where
            -- I'm unsure the extent to which this should be configurable
            tagattrs :: HM.HashMap T'.Text T'.Text
            tagattrs = HM.fromList [("a", "href"){-,("script","src"), ("form", "action")-}]
        tag2URL _ _ = Nothing

-- | Concat of all NodeContent for all children of given node
-- NB. if you pass a non-renderable node (like a <script>), then the text is extracted from it; this allows explicitly getting text from otherwise non-textworthy nodes, but breakes monoidal structure
getText :: Node -> [T'.Text]
getText (NodeElement p) = fold [getText c | c <- eltChildren p, case c of NodeElement e -> (T'.toLower $ eltName e) `S.notMember` nonRenderTags; _ -> True]
getText (NodeContent t) = pure t

-- | find [root, child, grandchild,...] by their tag names. List elements must be immediately related in DOM
-- list elements must be all lowercase
findByTags :: [T'.Text] -> Node -> Maybe Node
findByTags tags n = case n of
    NodeContent _ -> Nothing
    NodeElement e -> case tags of
        [] -> Just n
        [tag] -> if tag == T'.toLower (eltName e) then Just n else Nothing
        (tag:tags) -> if tag == T'.toLower (eltName e) then foldr (\a b -> b <|> findByTags tags a) Nothing (eltChildren e) else Nothing

-- Filters
-- remember that <&&> and <||> compose filters

-- | the predicate should assume that the tag (i.e. eltName) is all lowercase
tagFilter :: (T'.Text -> Bool) -> (Element -> Bool)
tagFilter = (. T'.toLower . eltName)

-- | key is member of node element's attributes
infixr 3 ∈
(∈) :: T'.Text -> (Element -> Bool)
k ∈ e = HM.member k (eltAttrs e)

-- | key is member of node element's attributes, with predicate about its value
-- example: mp3Filter = nodeFilter $ tagFilter (=="a") <&&> "href" |: (".mp3" `T'.isSuffixOf`). filterNode mp3Filter (iso domRoot)
infixr 5 |:
(|:) :: T'.Text -> (T'.Text -> Bool) -> (Element -> Bool)
k |: p = maybe False p . HM.lookup k . eltAttrs

class NodeFilter f where nodeFilter :: f -> (Node -> Bool)
instance NodeFilter (Element -> Bool) where nodeFilter f = \case NodeElement e -> f e; _ -> False
instance NodeFilter (T'.Text -> Bool) where nodeFilter f = \case NodeContent t -> f t; _ -> False -- we need tagFilter because it's just over eltName; it may be combined with eltAttrs. However, for text nodes, nodeFilter is sufficient, because there's only one argument to NodeContent.
