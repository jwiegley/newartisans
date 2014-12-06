{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative hiding ((<|>), many)
import           Control.Arrow (first)
import qualified Control.Foldl as L
import           Control.Lens hiding (Context, pre)
import           Control.Monad hiding (forM_)
import           Control.Monad.Catch hiding (try)
import           Data.Attoparsec.Text hiding (take, takeWhile, match)
import           Data.Char
import           Data.Foldable hiding (elem)
import           Data.List hiding (concatMap, any, all)
import           Data.List.Split hiding (oneOf)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Lazy (unpack)
import           Data.Time
import           Hakyll
import           Pipes as P
import           Pipes.Attoparsec as P
import qualified Pipes.Group as P
import qualified Pipes.Prelude as P
import           Pipes.Safe hiding (try)
import           Pipes.Shell
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text
import qualified Pipes.Text.Encoding as Text
import           Prelude hiding (concatMap, any, all)
import           System.Directory
import           System.FilePath
import           System.IO hiding (utf8)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Locale
import           System.Process
import           Text.Blaze.Html ((!), toValue)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (preEscapedString)
import           Text.Pandoc (Block (CodeBlock), Pandoc, bottomUpM)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc
import qualified Text.ParserCombinators.Parsec as Parsec

--import Debug.Trace

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
    match "css/*.css" $ do
        route idRoute
        -- compile compressCssCompiler
        compile yuiCompressor

    -- Compress and minify JavaScript
    match "js/*.js" $ do
        route idRoute
        compile yuiCompressor

    -- Compress and minify Blueprint CSS
    forM_ [ "blueprint-css/blueprint/*.css"
          , "blueprint-css/blueprint/plugins/fancy-type/*.css"
          ] $ \p -> match p $ do
        route idRoute
        compile yuiCompressor

    -- Render the /tmp index page
    match "tmp/index.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= relativizeUrls

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "label/*")

    -- Render each and every post
    -- Match all files under posts directory and its subdirectories.
    -- Turn posts into wordpress style url: year/month/title/index.html
    forM_ [("posts/*",
            "templates/post.html",
            "templates/postfooter.html"
           ),
           ("pages/*",
            "templates/page.html",
            "templates/pagefooter.html")] $ \(p, t, f) ->
        match p $ do
            route wordpressRoute
            compile $ do
                let allCtx =
                        field "recent" (const recentPostList) <>
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
                        field "recent" (const recentPostList) <>
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
                    field "recent" (const recentPostList) <>
                    defaultContext

            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title <>
                            constField "posts" list <>
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" allCtx
                >>= wordpressifyUrls

    paginate 6 $ \idx maxIndex itemsForPage -> do
        let ident
                | show idx == "1" = fromFilePath "index.html"
                | otherwise = fromFilePath $ "blog/page/" ++ show idx ++ "/index.html"
        create [ident] $ do
            route idRoute
            compile $ do
                let allCtx =
                        field "recent" (const recentPostList) <>
                        defaultContext
                    loadTeaser ident' = loadSnapshot ident' "teaser"
                        >>= loadAndApplyTemplate "templates/teaser.html"
                            (-- constField "title" "Lost in Technopolis" <>
                             teaserCtx tags)
                        >>= wordpressifyUrls
                items  <- mapM loadTeaser itemsForPage
                let postsCtx = constField "posts" (concatMap itemBody items)
                        <> field "navlinkolder"
                            (const $ return $ indexNavLink idx 1 maxIndex)
                        <> field "navlinknewer"
                            (const $ return $ indexNavLink idx (-1) maxIndex)
                        <> defaultContext

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

-- | My own variant of pandocCompile, which does some touchups to the text
--   before, during and after, and applies BlogLiterately's handling of ghci
--   blocks.
customPandocCompiler :: Compiler (Item String)
customPandocCompiler = cached "Main.customPandocCompiler" $ do
    path <- getResourceFilePath
    body <- fmap fixBefore <$> getResourceBody
    let doc  = readPandocWith ropt body
        doc' = Pandoc.walk (removeBirdTracks . hiddenBlocks) doc
    return $ fixAfter <$> writePandocWith wopt (fmtGhci path body <$> doc')
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

    fmtGhci path body
        | "[ghci]" `isInfixOf` itemBody body
            = unsafePerformIO . formatInlineGhci path
        | otherwise = id

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

teaserCtx :: Tags -> Context String
teaserCtx tags = field "teaser" teaserBody <> postCtx tags

teaserBody :: Item String -> Compiler String
teaserBody item = do
    let body = itemBody item
    return $ extractTeaser . maxLengthTeaser . compactTeaser $ body
  where
    extractTeaser :: String -> String
    extractTeaser [] = []
    extractTeaser xs@(x : xr)
        | "<!--more-->" `isPrefixOf` xs = []
        | otherwise = x : extractTeaser xr

    maxLengthTeaser :: String -> String
    maxLengthTeaser s
        | isNothing (findIndex (isPrefixOf "<!--more-->") (tails s))
            = unwords (take 60 (words s))
        | otherwise = s

    compactTeaser :: String -> String
    compactTeaser
        = replaceAll "<iframe [^>]*>" (const "")
        . replaceAll "<img [^>]*>" (const "")
        . replaceAll "<p>" (const "")
        . replaceAll "</p>" (const "")
        . replaceAll "<blockquote>" (const "")
        . replaceAll "</blockquote>" (const "")
        . replaceAll "<strong>" (const "")
        . replaceAll "</strong>" (const "")
        . replaceAll "<ol>" (const "")
        . replaceAll "</ol>" (const "")
        . replaceAll "<ul>" (const "")
        . replaceAll "</ul>" (const "")
        . replaceAll "<li>" (const "")
        . replaceAll "</li>" (const "")
        . replaceAll "<h[0-9][^>]*>" (const "")
        . replaceAll "</h[0-9]>" (const "")
        . replaceAll "<pre.*" (const "")
        . replaceAll "<a [^>]*>" (const "")
        . replaceAll "</a>" (const "")

config :: Configuration
config = defaultConfiguration { deployCommand = "./deploy" }

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

adjustLink :: String -> String
adjustLink = filter (not . isSlash) . map (toLower . replaceWithDash)

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
    `composeRoutes` gsubRoute "^[0-9]{4}-[0-9]{2}-[0-9]{2}-"
        ((\x -> take 8 x ++ drop 11 x) . map replaceWithSlash)
    `composeRoutes` gsubRoute ".md" (const "/index.html")
    `composeRoutes` gsubRoute ".lhs" (const "/index.html")
  where
    replaceWithSlash c = if c == '-' || c == '_' then '/' else c

wordpressifyUrls :: Item String -> Compiler (Item String)
wordpressifyUrls item = do
    rt <- getRoute $ itemIdentifier item
    return $ case rt of
        Nothing -> item
        Just _  -> fmap wordpressifyUrlsWith item

wordpressifyUrlsWith :: String  -- ^ HTML to wordpressify
                     -> String  -- ^ Resulting HTML
wordpressifyUrlsWith = withUrls convert
  where
    convert = replaceAll "/index.html" (const "/")

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
        | "``` {.sourceCode .literate .haskell}" `isPrefixOf` l
            = "<pre class=\"brush:haskell\">"
        | "``` " `isPrefixOf` l = "<pre class=\"brush:" ++ drop 4 l ++ "\">"
        | "```" == l = "</pre>"
        | otherwise = l

fixupTables :: String -> String
fixupTables txt = replace txt "<table>" "<table style=\"width: 70%; margin: 20px\">"

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
rssBodyField key =
    field key
        $ return
        . replaceAll "<iframe [^>]*>" (const "")
        . withUrls wordpress
        . withUrls absolute
        . itemBody
  where
    wordpress = replaceAll "/index.html" (const "/")

    absolute x = if head x == '/'
                 then feedRoot feedConfiguration ++ x
                 else x

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    itemTpl <- loadBody "templates/postitem.html"
    posts   <- preprocess' =<< loadAll (pattern .&&. hasNoVersion)
    applyTemplateList itemTpl (postCtx tags) posts

recentPostList :: Compiler String
recentPostList = do
    posts   <- fmap (take 6) . recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/indexpostitem.html"
    applyTemplateList itemTpl defaultContext posts

recentPosts :: Compiler [Item String]
recentPosts = do
    identifiers <- getMatches "posts/*"
    return [Item identifier "" | identifier <- identifiers]

indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where
    ref = if refPage == ""
          then ""
          else H.a ! A.href (toValue $ toUrl refPage) $ preEscapedString lab
    lab = if d > 0
          then "Older Entries &raquo;"
          else "&laquo; Newer Entries"
    refPage = if n + d < 1 || n + d > maxn
              then ""
              else case n + d of
                1 -> "/"
                _ -> "/blog/page/" ++ show (n + d) ++ "/"

paginate:: Int -> (Int -> Int -> [Identifier] -> Rules ()) -> Rules ()
paginate itemsPerPage rules = do
    identifiers <- getMatches "posts/*"
    let sorted      = sortBy (flip byDate) identifiers
        chunks      = chunksOf itemsPerPage sorted
        maxIndex    = length chunks
        pageNumbers = take maxIndex [1..]
        process i   = rules i maxIndex
    zipWithM_ process pageNumbers chunks
  where
    byDate id1 id2 =
        let fn1 = takeFileName (toFilePath id1)
            fn2 = takeFileName (toFilePath id2)
            parseTime' fn
                = parseTime defaultTimeLocale "%Y-%m-%d"
                $ intercalate "-"
                $ take 3
                $ splitAll "-" fn
        in compare (parseTime' fn1 :: Maybe UTCTime)
                   (parseTime' fn2 :: Maybe UTCTime)

yuiCompressor :: Compiler (Item String)
yuiCompressor = do
    path <- getResourceFilePath
    makeItem $ unsafePerformIO $ do
        home <- getHomeDirectory
        let javaCmd = "java -jar "
                   ++ (home </> ".nix-profile/lib/yuicompressor.jar")
                   ++ " "
                   ++ path
        -- Where there is no decoding failure, the return value of the text
        -- stream will be an empty byte stream followed by its own return
        -- value.  In all cases you must deal with the fact that it is a
        -- 'ByteString' producer that is returned, even if it can be thrown
        -- away with 'Control.Monad.void'
        runSafeT $
            unpack <$> Text.toLazyM (void (producerCmd' javaCmd ^. Text.utf8))

{------------------------------------------------------------------------------}

unTag :: String -> (Maybe String, String)
unTag s = either (const (Nothing, s)) id $ Parsec.parse tag "" s
  where
    tag = do
      tg <- Parsec.between (Parsec.char '[') (Parsec.char ']')
          $ Parsec.many $ Parsec.noneOf "[]"
      skipMany $ Parsec.oneOf " \t"
      _   <- Parsec.string "\r\n" Parsec.<|> Parsec.string "\n"
      txt <- Parsec.many Parsec.anyToken
      Parsec.eof
      return (Just tg, txt)

onTag :: String -> (Pandoc.Attr -> String -> a) -> (Block -> a) -> Block -> a
onTag t f def b@(CodeBlock attr@(_, as, _) s)
  | lowercase t `elem` map lowercase (maybe id (:) tag as)
    = f attr src
  | otherwise = def b
  where (tag, src) = unTag s
onTag _ _ def b = def b

lowercase :: String -> String
lowercase = map toLower

-- | Information about a running process: stdin, stdout, stderr, and a
--   handle.
type ProcessInfo = (Handle, Handle, Handle, ProcessHandle)

-- | An input to ghci consists of an expression/command, possibly
--   paired with an expected output.
data GhciInput  = GhciInput String (Maybe String)
  deriving Show

-- | An output from ghci is either a correct output, or an incorrect
--   (unexpected) output paired with the expected output.
data GhciOutput = OK String
                | Unexpected String String
  deriving Show

-- | A @GhciLine@ is a @GhciInput@ paired with its corresponding @GhciOutput@.
data GhciLine = GhciLine GhciInput GhciOutput
  deriving Show

-- | Evaluate an expression using an external @ghci@ process.
ghciEval :: GhciInput -> (GhciInput, String)
ghciEval input@(GhciInput expr _) =
    let script = "putStrLn " ++ show magic ++ "\n"
              ++ expr ++ "\n"
              ++ "putStrLn " ++ show magic ++ "\n"
    in (input, script)

stripGhciOutput :: GhciLine -> GhciLine
stripGhciOutput (GhciLine input@(GhciInput _ expected) (OK out)) =
    let !out' = strip out
    in GhciLine input $ case expected of
      Nothing -> OK out'
      Just e
          | out' == e -> OK out'
          | otherwise -> Unexpected out' e
stripGhciOutput x = x

-- | Start an external ghci process, run a computation with access to
--   it, and finally stop the process.
ghciProcess :: (MonadCatch m, MonadSafe m)
            => FilePath -> Pipe (GhciInput, String) GhciLine m ()
ghciProcess path = do
    isLit <- lift $ runEffect $
        P.any ("> " `T.isPrefixOf`)
              (L.purely P.folds L.mconcat
                        (Text.readFile path ^. Text.lines))
    for cat $ \(input, str) -> do
        let cmd = ["ghci", "-v0", "-ignore-dot-ghci"] ++ [ path | isLit ]
        -- P.parsed ghciParser
        --     (Text.decodeUtf8
        --         (Text.encodeUtf8 (T.pack str)
        --              >-> P.map Just
        --              >-> pipeCmd' (unwords cmd)))
        --     >-> P.map (GhciLine input . OK)
        return ()
  where
    magic' = T.pack magic

    ghciParser = manyTill anyChar (try (string magic'))
              *> manyTill anyChar (try (string magic'))
              <* takeLazyText

-- $extract
-- To extract the answer from @ghci@'s output we use a simple technique
-- which should work in most cases: we print the string @magic@ before
-- and after the expression we are interested in. We assume that
-- everything that appears before the first occurrence of @magic@ on the
-- same line is the prompt, and everything between the first @magic@ and
-- the second @magic@ plus prompt is the result we look for.

-- | There is nothing magic about the magic string.
magic :: String
magic = "!@#$^&*"

extract' :: GhciLine -> String
extract' (GhciLine _ (OK l)) = l
extract' _ = ""
  --   fmap (extract . unlines) (readMagic 2)
  -- where
  --   readMagic :: Int -> IO [String]
  --   readMagic 0 = return []
  --   readMagic n = do
  --     l <- hGetLine h
  --     let n' | (null . snd . breaks (isPrefixOf magic)) l = n
  --            | otherwise = n - 1
  --     (l:) <$> readMagic n'

extract :: String -> String
extract s = v
    where
      (t, u) =  breaks (isPrefixOf magic) s
      -- t contains everything up to magic, u starts with magic
      -- |u'                      =  tail (dropWhile (/='\n') u)|
      pre =  reverse . takeWhile (/='\n') . reverse $ t
      prelength = if null pre then 0 else length pre + 1
      -- pre contains the prefix of magic on the same line
      u' = drop (length magic + prelength) u
      -- we drop the magic string, plus the newline, plus the prefix
      (v, _) = breaks (isPrefixOf (pre ++ magic)) u'
      -- we look for the next occurrence of prefix plus magic

breaks :: ([a] -> Bool) -> [a] -> ([a], [a])
breaks _ [] =  ([], [])
breaks p as@(a : as')
    | p as = ([], as)
    | otherwise = first (a:) $ breaks p as'

-- | Given the path to the @.lhs@ source and its representation as a
--   @Pandoc@ document, @formatInlineGhci@ finds any @[ghci]@ blocks
--   in it, runs them through @ghci@, and formats the results as an
--   interactive @ghci@ session.
--
--   Lines beginning in the first column of the block are interpreted
--   as inputs.  Lines indented by one or more space are interpreted
--   as /expected outputs/.  Consecutive indented lines are
--   interpreted as one multi-line expected output, with a number of
--   spaces removed from the beginning of each line equal to the
--   number of spaces at the start of the first indented line.
--
--   If the output for a given input is the same as the expected
--   output (or if no expected output is given), the result is typeset
--   normally.  If the actual and expected outputs differ, the actual
--   output is typeset first in red, then the expected output in blue.
formatInlineGhci :: FilePath -> Pandoc -> IO Pandoc
formatInlineGhci path = bottomUpM go
  where
    go = onTag "ghci" formatGhciBlock return

    formatGhciBlock _attr src = do
        results <- runSafeT $ P.toListM $ P.each (parseGhciInputs src)
            >-> P.map ghciEval
            >-> ghciProcess path
            >-> P.map (formatGhciResult . stripGhciOutput)
        return $ Pandoc.RawBlock "html"
               $ "<pre><code>" ++ intercalate "\n" results ++ "</code></pre>"

parseGhciInputs :: String -> [GhciInput]
parseGhciInputs
    = map mkGhciInput
    . split
      ( dropInitBlank
      . dropFinalBlank
      . keepDelimsL
      $ whenElt (not . (" " `isPrefixOf`))
      )
    . lines

mkGhciInput :: [String] -> GhciInput
mkGhciInput []       = GhciInput "" Nothing
mkGhciInput [i]      = GhciInput i Nothing
mkGhciInput (i:expr) = GhciInput i (Just . unlines' . unindent $ expr)

unlines' :: [String] -> String
unlines' = intercalate "\n"

strip :: String -> String
strip = f . f
  where f = dropWhile isSpace . reverse

unindent :: [String] -> [String]
unindent [] = []
unindent (x:xs) = map (drop indentAmt) (x:xs)
  where indentAmt = length . takeWhile (==' ') $ x

indent :: Int -> String -> String
indent n = unlines' . map (replicate n ' '++) . lines

colored, coloredBlock :: String -> String -> String
colored color txt = "<span style=\"color: " ++ color ++ ";\">" ++ txt ++ "</span>"
coloredBlock color = unlines' . map (colored color) . lines

ghciPrompt :: String
ghciPrompt = colored "gray" "ghci&gt; "

formatGhciResult :: GhciLine -> String
formatGhciResult (GhciLine (GhciInput input _) (OK output))
  | all isSpace output = ghciPrompt ++ esc input
  | otherwise = ghciPrompt ++ esc input ++ "\n" ++ indent 2 (esc output) ++ "\n"
formatGhciResult (GhciLine (GhciInput input _) (Unexpected output expr))
  = ghciPrompt
        ++ esc input
        ++ "\n" ++ indent 2 (coloredBlock "red" (esc output))
        ++ "\n" ++ indent 2 (coloredBlock "blue" (esc expr))
        ++ "\n"

esc :: String -> String
esc = concatMap escapeOne
  where
    escapeOne '<' = "&lt;"
    escapeOne '>' = "&gt;"
    escapeOne  c  = [c]
