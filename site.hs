---------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend, (<>))
import           Data.List                       (intersperse, find, sortOn, reverse, delete, sortBy)
import           Data.List.Split                 (splitOn)
import           Data.Ord                        (comparing)
import           Data.Maybe                      (fromMaybe, fromJust)
import           Control.Applicative             ((<|>))
import           Control.Monad                   (foldM, (<=<), forM_)
import           Control.Arrow                   (second)
import           Data.Text                       (Text)
import qualified Data.Map                        as M
import qualified Data.Yaml                       as Y
import qualified Data.HashMap.Strict             as HM
import qualified Data.Vector                     as V

import           Hakyll
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match "fonts/*" $ do
        route   idRoute

        compile copyFileCompiler
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.org"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/basic.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    --tagsRules tags $ \tag identifiers -> do
    tagsRulesVersioned tags $ \tag identifiers -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            --posts <- recentFirst =<< loadAll pat
            posts <- loadAll $ fromList $ map (setVersion $ Just "meta") identifiers
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    ---- get post meta for use in related context
    --posts <- buildPostsMeta "posts/*"

    --match "posts/*" $ do
    match "posts/*" $ version "meta" $ do
        route   $ setExtension "html"
        compile getResourceBody

    match "posts/*" $ version "html" $ do
        route $ setExtension "html"
        compile $ do
          -- hasNoVersion: created two versions of posts to avoid circular
          -- dependencies with relatedPosts. found the my tags pages generated
          -- by tagsRules did not match any posts. Tried to apply version; this
          -- did not work. Found eventually that tagsRules has problems with
          -- versioned items. easiest workaround: version main displayed posts
          -- as "html", match on that version in index and post functions, and
          -- to match on the seperate set of post items for related posts, use
          -- "hasNoVersion" so that the html set is excluded, avoiding
          -- redundancy and (crucially) circular dependency.
          -- ps <- (loadAll ("posts/*" .&&. hasNoVersion) :: Compiler [Item String])
          ps <- loadAll ("posts/*" .&&. hasVersion "meta") :: Compiler [Item String]
          --let ctx = tagsCtx tags <> postCtx <> relatedCtx posts <> listField "posts" postCtx (return ps)
          --let ctx = tagsCtx tags <> postCtx <> listField "posts" postCtx (return ps)
          let ctx = tagsCtx tags <> postCtx <> listField "posts" postCtx (return ps) <> relatedPostsCtx ps
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
    -- running loadall inside of main match posts section caused circular
    -- dependency error. Is it the case that loadall is actually, by some
    -- magic, loading the objects that are created here? the monad would handle
    -- order of operations. reference here: https://jaspervdj.be/hakyll/tutorials/04-compilers.html

    -- changing the loaded posts to meta appears to fix the circular
    -- dependencies issue.

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "html")
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ makeItem $ Redirect "blog.html"

    match "blog.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "html")
            let blogCtx =
                    listField "latest_posts" (postCtxWithTags tags) (return $ take 3 posts) `mappend`
                    listField "later_posts" postCtx (return $ drop 3 posts) `mappend`
                    listField "alltags" tagCtx (return $ tagList tags) `mappend`
                    constField "title" "Blog"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
-- Post Context

postCtx :: Context String
postCtx
  =  dateField "date" "%B %e, %Y"
  <> defaultContext


--------------------------------------------------------------------------------
-- Tags

postCtxWithTags :: Tags -> Context String
postCtxWithTags t = tagsCtx t <> postCtx

tagsCtx :: Tags -> Context String
tagsCtx = tagsFieldCustom "tags"

tagCtx :: Context (String, String, String)
tagCtx = tagNameField <> tagUrlField <> tagCountField
  where
      tagNameField = field "name" $ (\(name,_,_) -> return name) . itemBody
      tagUrlField = field "url" $ (\(_,url,_) -> return url) . itemBody
      tagCountField = field "count" $ (\(_,_,count) -> return count) . itemBody

tagList :: Tags -> [Item (String, String, String)]
tagList tags = map (Item "tags" . preprocess) (tagsMap tags)
  where
    preprocess t = (tagName, tagUrl, tagCount)
      where
        tagName = fst t
        tagUrl = show $ tagsMakeId tags $ fst t
        tagCount = show $ length $ snd t

tagsFieldCustom :: String     -- ^ Destination key
                -> Tags       -- ^ Tags
                -> Context a  -- ^ Context
tagsFieldCustom =
  tagsFieldWith getTags renderLink (mconcat . intersperse " ")


renderLink _   Nothing         = Nothing
renderLink tag (Just filePath) =
  Just $ H.a ! A.class_ "tag" ! A.href (toValue $ toUrl filePath) $ toHtml tag

--------------------------------------------------------------------------------
-- Related Posts

getMetaTags :: Metadata -> [Text]
getMetaTags = fromMaybe [] . (Y.parseMaybe Y.parseJSON <=< HM.lookup "tags")

relatedPostsCtx :: [Item String] -> Context String
relatedPostsCtx xs = listFieldWith "related" postCtx selectPosts
  where
    matchPath x y = not $ eqOn (toFilePath . itemIdentifier) x y
    rateItem :: [String] -> Item String-> Compiler Int
    rateItem ts i = (length . filter (`elem` ts)) <$> (splitOn "," <$> getTags i)
    getTags :: Item String -> Compiler String
    getTags x = fromMaybe "" <$> getMetadataField (itemIdentifier x) "tags"
    selectPosts :: Item String -> Compiler [Item String]
    selectPosts s = do
      postTags <- splitOn "," <$> getTags s
      let trimmedItems = filter (matchPath s) xs --exclude current post
      take 2 . reverse <$> sortOnM (rateItem postTags) trimmedItems

-- compare two arguments for equality based on their transformations to
-- comparable types
eqOn :: Eq b => (a -> b) -> a -> a -> Bool
eqOn f x y = f x == f y

-- sorton a monadic function
sortOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
sortOnM f xs = (map fst . sortBy (comparing snd) . zip xs) <$> mapM f xs

--------------------------------------------------------------------------------
-- TagsRulesVersioned

-- inspired by http://hakyll.narkive.com/RqvLp93d/setversion-and-a-pattern
-- since fromList and versions don't mix easily as per the docs of that function,
-- "fromList". This means that in the body of the tagsRules' section,
-- posts <- recentFirst =<< loadAll pat
-- becomes
-- posts <- loadAll $ fromList $ map (setVersion $ Just "meta") identifiers

tagsRulesVersioned :: Tags -> (String -> [Identifier] -> Rules ()) -> Rules ()
tagsRulesVersioned tags rules =
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        rulesExtraDependencies [tagsDependency tags] $
            create [tagsMakeId tags tag] $
                rules tag identifiers
