{-# LANGUAGE CPP, OverloadedStrings #-}
module Main where
import Data.Char (toLower)
import Control.Monad
import System.Exit
import System.Directory
import System.FilePath
import Data.Maybe (fromMaybe)
import Text.Printf
import System.IO
import Data.Monoid (mempty)
import Text.Printf
import Text.Highlighting.Kate
import Data.Text.Lazy (Text, pack, unpack)
import Data.Algorithm.Diff
import Control.Applicative
import System.Environment (getArgs)
import Lucid

data TestResult = Pass | Fail | Error
                  deriving (Eq, Show)

main = do
  inputs <- map ("tests" </>) <$>
            filter isTestFile <$> getDirectoryContents "tests"
  args <- getArgs
  let regen = "--regenerate" `elem` args
  results <- forM inputs (runTest regen)
  let numfailures = length $ filter (== Fail) results
  let numerrors = length $ filter (== Error) results
  exitWith $ if numfailures == 0 && numerrors == 0
                then ExitSuccess
                else ExitFailure $ numfailures + numerrors

isTestFile :: FilePath -> Bool
isTestFile f = case drop 1 $ takeExtension f of
                    x -> x `elem` map (map toLower) languages &&
                          null (takeExtension (dropExtension f))

err :: String -> IO ()
err = hPutStrLn stderr

runTest regen inpFile = do
  code <- readFile inpFile
  let lang = drop 1 $ takeExtension inpFile
  let actual = formatHtml $ highlightAs lang code
  when regen $
    writeFile (inpFile <.> "html") actual
  expectedString <- readFile (inpFile <.> "html")
  if expectedString == actual
     then do
       putStrLn $ "[PASSED] " ++ inpFile
       return Pass
     else do
       putStrLn $ "[FAILED] " ++ inpFile
       putStrLn $ "--- " ++ inpFile <.> "html"
       putStrLn $ "+++ actual"
       printDiff expectedString actual
       return Fail

formatHtml toks =
  unpack $ renderText $ head_ (metadata >> css) >> body_ (fragment)
  where css = style_ [ type_ "text/css"] $ pack $ styleToCss pygments
        fragment = formatHtmlBlock opts toks
        metadata = meta_ [ httpEquiv_ "Content-Type", content_ "text/html; charset=UTF-8" ]
        opts = defaultFormatOpts{ titleAttributes = True }

vividize :: Diff String -> String
vividize (Both s _) = "  " ++ s
vividize (First s)  = "- " ++ s
vividize (Second s) = "+ " ++ s

printDiff :: String -> String -> IO ()
printDiff expected actual = do
  mapM_ putStrLn $ map vividize $ getDiff (lines expected) (lines actual)
