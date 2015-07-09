module Main where

import Prelude hiding (readFile)
import System.FilePath
import System.Directory
import Control.Monad
import System.IO.Error
import Data.List ((\\))
import System.IO.Strict (readFile)   -- Needed because we read and write to the same file
                                     -- which is not possible lazily

type Rule = String

templatesDir :: FilePath
templatesDir =  ".gitignore"

gitignoreFile :: FilePath
gitignoreFile = ".gitignore"

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


getTemplatesContent :: [FilePath] -> IO (Either String [Rule])
getTemplatesContent paths = do
    result <- tryIOError (forM paths readFile)
    case result of
        Left _         -> return $ Left "Problem loading templates"
        Right contents -> return $ Right $ lines $ foldr (++) "" contents


getGitignoreFileContents :: IO (Either String [Rule])
getGitignoreFileContents = do
    result <- tryIOError (readFile gitignoreFile)
    case result of
        Left e        -> if   isDoesNotExistError e
                         then return $ Right []
                         else return $ Left "Problem loading .gitignore"
        Right content -> return $ Right $ lines content


rulesToAdd :: [Rule] -> [Rule] -> [Rule]
rulesToAdd existing new = new \\ existing


addRules :: [Rule] -> IO (Either String ())
addRules toAdd = do
    result <- tryIOError (appendFile gitignoreFile $ unlines toAdd)
    case result of
        Left _  -> return $ Left "Problem writing to .gitignore file"
        Right _ -> return $ Right ()


getSummary :: [Rule] -> String
getSummary rules = if   null rules
                   then "Nothing to add to the .gitignore file"
                   else "Added " ++ (show $ length rules) ++ " rules to the .gitignore file"


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
                Right allRules -> do
                    result' <- getGitignoreFileContents
                    case result' of
                        Left e              -> putStrLn e
                        Right existingRules -> do
                            -- forM_ toAdd putStrLn
                             let toAdd = rulesToAdd existingRules allRules
                             result'' <- addRules toAdd
                             case result'' of
                                Left e    -> putStrLn e
                                Right _   -> putStrLn $ getSummary toAdd
    return ()
