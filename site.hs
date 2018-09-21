---------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid                     (mappend, (<>))
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
import           Text.Pandoc

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    match "fonts/*" $ route idRoute >> compile copyFileCompiler
    match "images/*" $ route idRoute >> compile copyFileCompiler
    match "css/*" $ route idRoute >> compile compressCssCompiler
    match "templates/*" $ compile templateCompiler
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "index.html" $ do
        route idRoute
        compile $ makeItem $ Redirect "blog.html"

    match (fromList ["about.org", "projects.org"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/basic.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tagsRulesVersioned tags $ \tag identifiers -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- loadAll $ fromList $ map (setVersion $ Just "meta") identifiers
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ version "meta" $ do
        route   $ setExtension "html"
        compile getResourceBody

    match "posts/*" $ version "html" $ do
        route $ setExtension "html"
        compile $ do
          ident <- getUnderlying
          toc <- getMetadataField ident "toc"
          let compiler = case toc of
                Just _ -> pandocCompilerWith defaultHakyllReaderOptions withToc
                Nothing -> pandocCompiler
          ps <- loadAll ("posts/*" .&&. hasVersion "meta") :: Compiler [Item String]
          let ctx = tagsCtx tags <> postCtx <> listField "posts" postCtx (return ps) <> relatedPostsCtx ps
          compiler
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "blog.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "html")
            let blogCtx =
                    listField "latest_posts" (postCtxWithTags tags) (return $ take 3 posts) `mappend`
                    listField "later_posts" (postCtxWithTags tags) (return $ drop 3 posts) `mappend`
                    listField "alltags" tagCtx (return $ tagList tags) `mappend`
                    constField "title" "Blog"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls
  where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate        = Just "<p><h1>Contents</h1></p><div class=\"toc\">$toc$</div>\n$body$"
        }

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

relatedPostsCtx :: [Item String] -> Context String
relatedPostsCtx xs = listFieldWith "related" postCtx selectPosts
  where
    matchPath x y = not $ eqOn (toFilePath . itemIdentifier) x y
    rateItem :: [String] -> Item String-> Compiler Int
    rateItem ts i = length . filter (`elem` ts) <$> (splitOn "," <$> getTags i)
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
sortOnM f xs = map fst . sortBy (comparing snd) . zip xs <$> mapM f xs

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
