module Main (main) where
import System.Environment
import WXR2Pandoc (processFile_xmlconduit)

main :: IO ()
main = do
  args <- getArgs
  case args of
    filename:_ -> processFile_xmlconduit filename
    [] -> putStrLn "wxr2pandoc <filename.xml>"
