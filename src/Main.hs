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


getTemplatesContent :: [FilePath] -> IO (Either String [String])
getTemplatesContent paths = do
    result <- tryIOError (forM paths readFile)
    case result of
        Left _         -> return $ Left "Problem loading templates"
        Right contents -> return $ Right $ lines $ foldr (++) "" contents


main :: IO ()
main = do
    home      <- getHomeDirectory
    templates <- listAvailableTemplates
    case templates of
        Left e      -> putStrLn e
        Right files -> do
            result <- getTemplatesContent $ getFullPathTemplates home files
            case result of
                Left e         -> putStrLn e
                Right contents -> forM_ contents putStrLn
    return ()
