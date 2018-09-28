{-# OPTIONS_GHC -Wall #-}
-- | production-ready scraper framework
-- How to sequence scrapers:
-- do
--     scraper₁
--         scraper₂
--     scraper₃
-- will execute scraper₁, then execute scraper₂ over all the URLs that scraper₁ produced/put. Then runs scraper₃. In other words, scraper₂ is a subpart of scraper₁, but the running of scraper₁ is unrelated to the running of scraper₃.
-- The WriterT of Text logs warnings; one may suppose then that errors cause the scraper to terminate, and are encoded by the Left value of the ExceptT
module NicLib.Scraper
( Scraper
, Scraper'
, DOMURL
, runScraper
, assumingThat
, download
, withDOM
, withPage'sURLs
)
where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except -- remember it's not Control.Monad.Except! Use transformer!
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.Char (toLower)
import Data.Foldable (find)
import Data.ListLike (ListLike)
import Data.ListLike.String
import Data.Random
import Data.Random.Source.DevRandom
import Network.URI.Encode (decode) -- package uri-encode
import NicLib.HTML
import NicLib.Errors
import NicLib.List
import NicLib.NStdLib
import NicLib.Net
import NicLib.URL
import Prelude hiding (FilePath)
import System.Exit
import System.IO
import qualified Data.ByteString.Char8 as BS8'
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S
import qualified Data.Text as T'
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import Data.String (IsString)

-- to use mkcd, or mkdir, put (lift . lift) [mkdir | mkcd]

-- something along the lines of: collect -> sort/size-up -> consider each in set (elements, unions and intersections,...some subset of the power set, identified by some predicates, or trying things to see if they make sense or work or fail.)

type Scraper a = forall e m s. (MonadIO m, MonadCatch m, StringLike s) => StateT (S.Set URL) (BugT e s m) a
type Scraper' s m a = StateT (S.Set URL) (BugT s m) a -- for using multiple Scraper's in type signature with common monad
type DOMURL = (Node, URL) -- used to describe a resource by value and reference

-- | runScraper calls runWriterT, and makes the pattern ((x,y),z) into (x,y,z)
runScraper :: (MonadIO m, MonadCatch m, StringLike s) => Scraper' s m a -> S.Set URL -> m (Either s (a, S.Set URL, T'.Text))
runScraper = (fmap . fmap) (\((x, y), z) -> (x, y, z)) . runExceptT . runWriterT <% runStateT
-- print errors from WriterT if any: when (not $ T'.null errors) $ TIO.hPutStrLn stderr ("Errors:\n" <> errors)

-- | test an assumption; if assumption fails, user will be asked whether they want to continue; returns True if the user wants to continue; False if they don't
-- you'll likely want to write a handlers for each case of 1) the user wanting to continue; 2) not wanting to continue
-- for example, if an assumption about the DOM fails, return, print, or write the DOM to a file for user's inspection; if continuing, perhaps try a different strategy for scraping (i.e. a different predicate that should also extract exactly the desired data)
assumingThat :: MonadIO m => a -> (String, a -> Bool) -> (a -> m b) -> (a -> m b) -> m b
assumingThat a (desc, p) failed ok = if p a then ok a else liftIO (putStrLn $ "Assumption failed: " ++ desc) >> failed a

-- | save file to disk in current directory. Filename is determined by URL path's last segment. You can pass a function to make the filename a function of this trailing path segment.
download :: (MonadIO m, MonadCatch m, ListLike s Char, StringLike s, IsString s, Semigroup s) => (String -> String) -> URL -> Scraper' s m ()
download f url@(ppURL -> ppurl) = case f . decode . snd . fromJust $ breakAtLast '/' (T'.unpack ppurl) of
    filename -> lift . lift $ evalStateT (fetch url) mempty >>= liftIO . BS.writeFile filename -- TODO: create cookie and HTTP header combinators, for particular sending of headers and cookies

-- | the most basic scraper; one must use this
withDOM :: (MonadIO m, MonadCatch m, ListLike s Char, StringLike s, IsString s, Semigroup s) => (DOMURL -> Scraper' s m a) -> Scraper' s m [a]
withDOM f = do
    (S.toList -> urls) <- get -- TODO: usually use a Trie instead of a Set, for URLs.
    forM urls $ \url -> do
        dom <- lift . lift . ExceptT $ domFromURL url
        f (dom, url)

-- | do stuff with URLs scraped from DOM
-- please remember that these are the URLs scraped from DOM! They are not the URLs from state! (i.e. the URLs at which the DOMs are located)
withPage'sURLs :: (MonadIO m, MonadCatch m, StringLike s) => DOMURL -> (S.Set URL -> Scraper' s m a) -> Scraper' s m a
withPage'sURLs (dom, url) f = case scrapeURLs dom (Just url) of
    (S.toList -> malformed, wellformed) -> do
        when (not $ null malformed) (lift . tell $ "Malformed URLs found on " <> ppURL url <> ":\n" <> T'.unlines malformed <> "\n")
        f wellformed

-- Consider various catamorphisms over data that keeps the original data (Tree Node) the same, but the object returned is digested in different ways, e.g. a fold for extracting text all in line, or a fold for extracting Text with links (i.e. (Text, [(Word64, Word64, URL)]), which is a 2-tuple of parsed-out text, including that from <a>, and the list of triples for marking the beginning and end positions of link text, and the URL of that link. Prob. make semigroups over these folds.
-- Consider that I'll want to extract various text patterns, e.g. dates, numbers (incl. decimals), URLs (though hopefully not, as these'll all be in href's), keywords
-- Replace find with fromJust . find `assumingThat` length == 1
-- TODO: periodically write StateT to file; create combinator about ^C to write state then exit; on next run, read state from file >>= runStateT statefulFunction
-- use Default with newtype (e.g. for newtype LoginCreds = forall m. (MonadCatch m, MonadIO m) => m (T'.Text, T'.Text), instance Default LoginCreds where LoginCreds = TIO.readFile prefsFile.keychain)
   
-- one should be able to crawl entire site using StateT, forever, and guard.
-- scraping w/interactive partitioning: partition into ({Partitioned}, {Unpartitioned})
-- seriously need to make lifting behind-the-scenes

-- consider using this for tumblr: searching tumblr likes pages for text, then downloading that post

-- btw, streaming & processing AJAX is a producer/consumer model
-- seperately, consider the pixiv case of crawling/scraping: there's something fishy going-on with downloading images. However, it clearly happens at least once, since the browser displays the picture. This may be accomplished with any combination of authentication, AJAX, HTTP GET.
-- DSL: filters/finds across multiple pages is powerful; for each page, show page's links to human; human will filter and ad-hoc sort by liklihood of desired predicate match (i.e. best place to look next for whatever-we're-looking-for.)
