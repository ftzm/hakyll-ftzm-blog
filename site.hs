{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

import           Data.Monoid                     ((<>))
import           Data.List                       (intersperse, reverse, sortBy)
import           Data.Ord                        (comparing)
import           Control.Monad                   (forM_)

import           Hakyll
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc

main :: IO ()
main = hakyll $ do

    match "fonts/*" $ route idRoute >> compile copyFileCompiler
    match "images/*" $ route idRoute >> compile copyFileCompiler
    match "css/*" $ route idRoute >> compile compressCssCompiler
    match "templates/*" $ compile templateCompiler

    match "index.html" $ do
        route idRoute
        compile $ makeItem $ Redirect "articles.html"

    match (fromList ["about.org", "projects.org", "resume.org", "404.org"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/basic.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRulesVersioned tags $ \tag identifiers -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- loadAll $ fromList $ map (setVersion $ Just "meta") identifiers
            let ctx = constField "title" title
                   <> listField "posts" postCtx (return posts)
                   <> defaultContext
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
          let ctx = tagsCtx tags <> postCtx <> relatedPostsCtx ps 2
          compiler
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "articles.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "html")
            let blogCtx =
                  listField "posts" (postCtxWithTags tags) (return posts)
                    <> listField "alltags" tagCtx (return $ tagList tags)
                    <> constField "title" "Blog"
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls

  where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate        = Just "<div class=\"toc\"><h1>Contents</h1>$toc$</div>\n$body$"
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
tagList tags = map (Item "tags" . tagTup) (tagsMap tags)
  where
    tagTup (name, (urls)) = (name, tagUrl, tagCount)
      where
        tagUrl = show $ tagsMakeId tags name
        tagCount = show $ length urls

tagsFieldCustom :: String     -- ^ Destination key
                -> Tags       -- ^ Tags
                -> Context a  -- ^ Context
tagsFieldCustom =
  tagsFieldWith getTags renderLink (mconcat . intersperse " ")


renderLink :: H.ToMarkup a => a -> Maybe FilePath -> Maybe H.Html
renderLink _   Nothing         = Nothing
renderLink tag (Just filePath) =
  Just $ H.a ! A.class_ "tag" ! A.href (toValue $ toUrl filePath) $ toHtml tag

--------------------------------------------------------------------------------
-- Related Posts

-- |A context that adds related posts under "related"
relatedPostsCtx
  :: [Item String] -- ^ List of post items
  -> Int           -- ^ Number of related posts to collect
  -> Context String
relatedPostsCtx posts n = listFieldWith "related" postCtx selectPosts
  where
    rateItem ts i = length . filter (`elem` ts) <$> (getTags $ itemIdentifier i)
    selectPosts s = do
      postTags <- getTags $ itemIdentifier s
      let trimmedItems = filter (not . matchPath s) posts --exclude current post
      take n . reverse <$> sortOnM (rateItem postTags) trimmedItems

-- |Compare two items for equality based on their filepaths
matchPath :: Item String -> Item String -> Bool
matchPath x y = eqOn (toFilePath . itemIdentifier) x y

-- |Compare two arguments for equality based on their transformations to
-- comparable types
eqOn :: Eq b => (a -> b) -> a -> a -> Bool
eqOn f x y = f x == f y

-- |Sort on a monadic function
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
