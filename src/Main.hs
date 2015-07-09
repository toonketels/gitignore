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


showMessage :: (Either String String) -> IO ()
showMessage = either showError showSuccess
    where showError   m = putStrLn $ "Error: " ++ m
          showSuccess m = putStrLn $ "Success: " ++ m


main :: IO ()
main = do
    home      <- getHomeDirectory
    existing  <- getGitignoreFileContents
    templates <- listAvailableTemplates
    allRules  <- case templates of
                    Left e      -> return $ Left e
                    Right files -> getTemplatesContent $ getFullPathTemplates home files
    toAdd     <- return $ liftM2 rulesToAdd existing allRules
    added     <- case toAdd of
                    Left e      -> return $ Left e
                    Right rules -> addRules rules
    showMessage $ getMessage added toAdd
        where
            getMessage added' toAdd' = case added' of
                Left e  -> Left e
                Right _ -> liftM getSummary toAdd'
