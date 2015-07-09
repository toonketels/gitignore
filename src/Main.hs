module Main where

import System.FilePath
import System.Directory
import Control.Monad
import System.IO.Error

templatesDir :: FilePath
templatesDir =  ".gitignore"

extension    :: String
extension    =  "gitignore"


listAvailableTemplates :: IO (Either String [FilePath])
listAvailableTemplates = do
    home   <- getHomeDirectory
    result <- tryIOError (getDirectoryContents $ home </> templatesDir)
    case result of
        Left _      -> return $ Left "Problem getting files from templates directory"
        Right files -> return $ Right $ filter isTemplate files
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
    case templates of
        Left e      -> putStrLn e
        Right files ->
            forM_ files' putStrLn
                where files' = getFullPathTemplates home files
    return ()
