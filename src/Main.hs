module Main where

import System.FilePath
import System.Directory
import Control.Monad


templatesDir :: FilePath
templatesDir =  ".gitignore"

extension    :: String
extension    =  "gitignore"


listAvailableTemplates :: IO [FilePath]
listAvailableTemplates = do
    home  <- getHomeDirectory
    files <- getDirectoryContents $ home </> templatesDir
    return $ filter isTemplate files
        where isTemplate   :: FilePath -> Bool
              isTemplate p =  takeExtension p == '.' : extension


getFullPathTemplate :: FilePath -> FilePath -> FilePath
getFullPathTemplate home file = home </> templatesDir </> file


getFullPathTemplates :: FilePath -> [FilePath] -> [FilePath]
getFullPathTemplates home = map (getFullPathTemplate home)


main :: IO ()
main = do
    home      <- getHomeDirectory
    templates <- listAvailableTemplates
    let full = getFullPathTemplates home templates
    forM_ full putStrLn
    return ()

