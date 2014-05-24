{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Applicative
import           Control.Monad hiding (forM_)
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Monoid
import           Data.Time
import           Hakyll
import           System.FilePath
import           System.Locale
import           System.Process
import           Text.Blaze.Html ((!), toHtml, toValue)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (preEscapedString)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc

main :: IO ()
main = hakyllWith config $ do
    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Static files
    match (     "images/*.jpg"
           .||. "images/*.png"
           .||. "images/*.gif"
           .||. "favicon.ico"
           .||. "files/**"
          ) $ do
        route   idRoute
        compile copyFileCompiler

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    -- Formula images
    match "images/*.tex" $ do
        route   $ setExtension "png"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/formula.tex" defaultContext
            >>= pdflatex
            >>= pdfToPng

    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Render the /tmp index page
    match "tmp/index.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= relativizeUrls

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "label/*")

    -- Render each and every post
    -- Match all files under posts directory and its subdirectories.
    -- Turn posts into wordpress style url: year/month/date/title/index.html
    forM_ [("posts/*",
            "templates/post.html",
            "templates/postfooter.html"
           ),
           ("pages/*",
            "templates/page.html",
            "templates/pagefooter.html")] $ \(p, t, f) ->
        match p $ do
            route $ wordpressRoute
            compile $ do
                let allCtx =
                        field "recent" (\_ -> recentPostList) `mappend`
                        defaultContext

                customPandocCompiler
                    >>= saveSnapshot "teaser"
                    >>= loadAndApplyTemplate t (postCtx tags)
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate f (postCtx tags)
                    >>= loadAndApplyTemplate "templates/default.html" allCtx
                    >>= wordpressifyUrls

    -- Build special pages
    forM_ ["index.markdown", "404.markdown", "search.markdown"] $ \p ->
        match p $ do
            route   $ setExtension "html"
            compile $ do
                let allCtx =
                        field "recent" (\_ -> recentPostList) `mappend`
                        defaultContext

                customPandocCompiler
                    >>= loadAndApplyTemplate "templates/page.html" (postCtx tags)
                    >>= loadAndApplyTemplate "templates/default.html" allCtx
                    >>= wordpressifyUrls

    -- Labels
    tagsRules tags $ \tag pattern -> do
        let title = "Posts with label " ++ " &#8216;" ++ tag ++ "&#8217;"
        route labelRoute
        compile $ do
            let allCtx =
                    field "recent" (\_ -> recentPostList) `mappend`
                    defaultContext

            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" allCtx
                >>= wordpressifyUrls

    paginate 2 $ \index maxIndex itemsForPage -> do
        let id = fromFilePath $ "blog/page/" ++ (show index) ++ "/index.html"
        create [id] $ do
            route idRoute
            compile $ do
                let allCtx =
                        field "recent" (\_ -> recentPostList) `mappend`
                        defaultContext
                    loadTeaser id = loadSnapshot id "teaser"
                                        >>= loadAndApplyTemplate "templates/teaser.html" (teaserCtx tags)
                                        >>= wordpressifyUrls
                item1 <- loadTeaser (head itemsForPage)
                item2 <- loadTeaser (last itemsForPage)
                let body1 = itemBody item1
                    body2 = if length itemsForPage == 1 then "" else itemBody item2
                    postsCtx =
                        constField "posts" (body1 ++ body2) `mappend`
                        field "navlinkolder" (\_ -> return $ indexNavLink index 1 maxIndex) `mappend`
                        field "navlinknewer" (\_ -> return $ indexNavLink index (-1) maxIndex) `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/blogpage.html" postsCtx
                    >>= loadAndApplyTemplate "templates/default.html" allCtx
                    >>= wordpressifyUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedContext posts

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = cached "Main.customPandocCompiler" $
    fmap fixAfter
        . writePandocWith wopt
        . fmap (Pandoc.walk (removeBirdTracks . hiddenBlocks))
        <$> readPandocWith ropt . fmap fixBefore
        <$> getResourceBody
  where
    fixBefore = fixCodeBlocks . fixHeaders . fixLineEndings
    fixAfter  = fixupTables

    ropt = defaultHakyllReaderOptions
    wopt = defaultHakyllWriterOptions
        { Pandoc.writerReferenceLinks = True
        , Pandoc.writerHTMLMathMethod =
            Pandoc.MathJax $ "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
                          ++ "?config=TeX-AMS-MML_HTMLorMML"
        }

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

teaserCtx :: Tags -> Context String
teaserCtx tags =
    field "teaser" teaserBody `mappend`
    (postCtx tags)

teaserBody :: Item String -> Compiler String
teaserBody item = do
    let body = itemBody item
    return $ extractTeaser . maxLengthTeaser . compactTeaser $ body
  where
    extractTeaser :: String -> String
    extractTeaser [] = []
    extractTeaser xs@(x : xr)
        | "<!-- more -->" `isPrefixOf` xs = []
        | otherwise                       = x : extractTeaser xr

    maxLengthTeaser :: String -> String
    maxLengthTeaser s = if findIndex (isPrefixOf "<!-- more -->") (tails s) == Nothing
                            then unwords (take 60 (words s))
                            else s

    compactTeaser :: String -> String
    compactTeaser =
        (replaceAll "<iframe [^>]*>" (const "")) .
        (replaceAll "<img [^>]*>" (const "")) .
        (replaceAll "<p>" (const "")) .
        (replaceAll "</p>" (const "")) .
        (replaceAll "<blockquote>" (const "")) .
        (replaceAll "</blockquote>" (const "")) .
        (replaceAll "<strong>" (const "")) .
        (replaceAll "</strong>" (const "")) .
        (replaceAll "<ol>" (const "")) .
        (replaceAll "</ol>" (const "")) .
        (replaceAll "<ul>" (const "")) .
        (replaceAll "</ul>" (const "")) .
        (replaceAll "<li>" (const "")) .
        (replaceAll "</li>" (const "")) .
        (replaceAll "<h[0-9][^>]*>" (const "")) .
        (replaceAll "</h[0-9]>" (const "")) .
        (replaceAll "<pre.*" (const "")) .
        (replaceAll "<a [^>]*>" (const "")) .
        (replaceAll "</a>" (const ""))

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh -p 2222' _site/* jaspervdj@jaspervdj.be:jaspervdj.be"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Lost in Technopolis"
    , feedDescription = "RSS feed for John Wiegley's blog"
    , feedAuthorName  = "John Wiegley"
    , feedAuthorEmail = "johnw@newartisans.com"
    , feedRoot        = "http://newartisans.com"
    }

pdflatex :: Item String -> Compiler (Item TmpFile)
pdflatex item = do
    TmpFile texPath <- newTmpFile "pdflatex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        _ <- system $ unwords ["pdflatex", "-halt-on-error",
            "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
        return ()

    makeItem $ TmpFile pdfPath

pdfToPng :: Item TmpFile -> Compiler (Item TmpFile)
pdfToPng item = do
    let TmpFile pdfPath = itemBody item
        pngPath         = replaceExtension pdfPath "png"
    unsafeCompiler $ do
        _ <- system $ unwords ["convert", "-density", "150", "-quality", "90",
                pdfPath, pngPath]
        return ()
    makeItem $ TmpFile pngPath

labelRoute :: Routes
labelRoute = setExtension ".html"
    `composeRoutes` gsubRoute "." adjustLink
    `composeRoutes` gsubRoute "/" (const "")
    `composeRoutes` gsubRoute "^label" (const "label/")
    `composeRoutes` gsubRoute "-html" (const "/index.html")

adjustLink = (filter (not . isSlash)) . (map (toLower . replaceWithDash))

replaceWithDash :: Char -> Char
replaceWithDash c =
    if c == '.' || c == ' '
        then '-'
        else c

isSlash :: Char -> Bool
isSlash '/' = True
isSlash _   = False

wordpressRoute :: Routes
wordpressRoute = gsubRoute "posts/" (const "")
    `composeRoutes` gsubRoute "pages/" (const "")
    `composeRoutes` gsubRoute "^[0-9]{4}-[0-9]{2}-"
                        (map replaceWithSlash)
    `composeRoutes` gsubRoute ".md" (const "/index.html")
    `composeRoutes` gsubRoute ".lhs" (const "/index.html")
  where
    replaceWithSlash c = if c == '-' || c == '_' then '/' else c

wordpressifyUrls :: Item String -> Compiler (Item String)
wordpressifyUrls item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap wordpressifyUrlsWith item

wordpressifyUrlsWith :: String  -- ^ HTML to wordpressify
                     -> String  -- ^ Resulting HTML
wordpressifyUrlsWith = withUrls convert
  where
    convert x = replaceAll "/index.html" (const "/") x

-- | If any line that begins with "\#", change it to "#".  This is necessary
--   because Markdown uses # for headers, but Literate Haskell doesn't accept
--   them.
fixHeaders :: String -> String
fixHeaders = unlines . map (\l -> if "\\#" `isPrefixOf` l
                                  then drop 1 l else l) . lines

fixCodeBlocks :: String -> String
fixCodeBlocks =
    unlines . map fixup . lines
  where
    fixup l
        | "``` " `isPrefixOf` l = "<pre class=\"brush:" ++ drop 4 l ++ "\">"
        | "```" == l = "</pre>"
        | otherwise = l

fixupTables :: String -> String
fixupTables txt = replace txt "<table>"
                              "<table style=\"width: 70%; margin: 20px\">"

-- | Replace a sublist with another list.
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace l@(x:xs) i j = if i `isPrefixOf` l
                       then j ++ replace (drop (length i) l) i j
                       else x : replace xs i j

-- | Turn @CRLF@ pairs into a single @LF@.  This is necessary since
--   'readMarkdown' is picky about line endings.
fixLineEndings :: String -> String
fixLineEndings [] = []
fixLineEndings ('\r':'\n':cs) = '\n':fixLineEndings cs
fixLineEndings (c:cs) = c:fixLineEndings cs

-- | Remove Literate Haskell's bird-tracks from the output.  I prefer not to see
--   them.
removeBirdTracks :: Pandoc.Block -> Pandoc.Block
removeBirdTracks (Pandoc.RawBlock "html" src) =
  Pandoc.RawBlock "html"
    $ (\x -> replace x "\n<span style=\"color: gray;\">ghci"
                      "<span style=\"color: gray;\">ghci")
    $ (\x -> replace x "<span class=\"fu\"></span><span class=\"ot\"> "
                      "<span class=\"fu\"></span><span class=\"ot\">")
    $ (\x -> replace x "<span class=\"fu\">&gt;</span>"
                      "<span class=\"fu\"></span>")
    $ (\x -> replace x "<span class=\"fu\">&gt;</span> "
                      "<span class=\"fu\"></span>")
    src
removeBirdTracks b = b

-- | If a code block contains "-- HIDE" anywhere within it, drop that block
--   from the output.
hiddenBlocks :: Pandoc.Block -> Pandoc.Block
hiddenBlocks b@(Pandoc.CodeBlock _ src) =
  if "-- HIDE" `isInfixOf` src then Pandoc.Null else b
hiddenBlocks b = b

toWordPressUrl :: FilePath -> String
toWordPressUrl url =
    replaceAll "/index.html" (const "/") (toUrl url)

wpUrlField :: String -> Context a
wpUrlField key = field key $
    fmap (maybe "" toWordPressUrl) . getRoute . itemIdentifier

feedContext :: Context String
feedContext = mconcat
    [ rssBodyField "description"
    , rssTitleField "title"
    , wpUrlField "url"
    , dateField "date" "%B %e, %Y"
    ]

rssTitleField :: String -> Context a
rssTitleField key = field key $ \i -> do
    value <- getMetadataField (itemIdentifier i) "title"
    let value' = liftM (replaceAll "&" (const "&amp;")) value
    maybe empty return value'

rssBodyField :: String -> Context String
rssBodyField key = field key $
    return .
    (replaceAll "<iframe [^>]*>" (const "")) .
    (withUrls wordpress) .
    (withUrls absolute) .
    itemBody
  where
    wordpress x = replaceAll "/index.html" (const "/") x
    absolute x = if (head x) == '/' then (feedRoot feedConfiguration) ++ x else x

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    itemTpl <- loadBody "templates/postitem.html"
    posts   <- preprocess' =<< loadAll (pattern .&&. hasNoVersion)
    applyTemplateList itemTpl (postCtx tags) posts

recentPostList :: Compiler String
recentPostList = do
    posts   <- fmap (take 10) . recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/indexpostitem.html"
    list    <- applyTemplateList itemTpl defaultContext posts
    return list

recentPosts :: Compiler [Item String]
recentPosts = do
    identifiers <- getMatches "posts/*"
    return [Item identifier "" | identifier <- identifiers]

indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if (refPage == "") then ""
              else H.a ! A.href (toValue $ toUrl $ refPage) $
                   (preEscapedString lab)
        lab = if (d > 0) then "Older Entries &raquo;" else "&laquo; Newer Entries"
        refPage = if (n + d < 1 || n + d > maxn) then ""
                  else case (n + d) of
                    1 -> "/blog/page/1/"
                    _ -> "/blog/page/" ++ (show $ n + d) ++ "/"

paginate:: Int -> (Int -> Int -> [Identifier] -> Rules ()) -> Rules ()
paginate itemsPerPage rules = do
    identifiers <- getMatches "posts/*"

    let sorted = reverse $ sortBy byDate identifiers
        chunks = chunk itemsPerPage sorted
        maxIndex = length chunks
        pageNumbers = take maxIndex [1..]
        process i is = rules i maxIndex is
    zipWithM_ process pageNumbers chunks
        where
            byDate id1 id2 =
                let fn1 = takeFileName $ toFilePath id1
                    fn2 = takeFileName $ toFilePath id2
                    parseTime' fn = parseTime defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
                in compare ((parseTime' fn1) :: Maybe UTCTime) ((parseTime' fn2) :: Maybe UTCTime)

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
    where (ys,zs) = splitAt n xs
