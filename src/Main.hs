module Main where

import Prelude hiding     (readFile)
import Data.List          (partition, (\\))
import Control.Monad      (forM, liftM, liftM2, join)
import System.FilePath    (addExtension, takeExtension, dropExtension, (</>))
import System.Directory   (getHomeDirectory, getDirectoryContents)
import System.Environment (getArgs)
import System.IO.Error    (tryIOError, isDoesNotExistError)
import System.IO.Strict   (readFile)   -- Needed because we read and write to the same file
                                       -- which is "not?" possible lazily

type Rule = String

templatesDir :: FilePath
templatesDir =  ".gitignore"

gitignoreFile :: FilePath
gitignoreFile =  ".gitignore"

extension :: String
extension =  "gitignore"


listChosenTemplates :: [String] -> [FilePath] -> Either String [FilePath]
listChosenTemplates [] _            = Left "No template names provided"
listChosenTemplates names templates =
    if   someTemplatesMissing
    then Left  ("Templates missing for " ++ unwords missing)
    else Right (addExtensions present)
            where namesOnly              = fmap dropExtension
                  addExtensions          = fmap (flip addExtension extension)
                  someTemplatesMissing   = not (null missing)
                  group desired existing = partition (flip elem existing) desired
                  (present, missing)     = group names (namesOnly templates)


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


formatMessage :: (Either String String) -> String
formatMessage (Left  m) = "Error: "   ++ m
formatMessage (Right m) = "Success: " ++ m



main :: IO ()
main = do
    args      <- getArgs
    home      <- getHomeDirectory
    existing  <- getGitignoreFileContents
    templates <- listAvailableTemplates
    chosen    <- return $ join (liftM (listChosenTemplates args) templates)
    allRules  <- case chosen of
                    Left e      -> return $ Left e
                    Right files -> getTemplatesContent $ getFullPathTemplates home files
    toAdd     <- return $ liftM2 rulesToAdd existing allRules
    added     <- case toAdd of
                    Left e      -> return $ Left e
                    Right rules -> addRules rules
    putStrLn $ formatMessage $ getMessage added toAdd
        where
            getMessage added' toAdd' = case added' of
                Left e  -> Left e
                Right _ -> liftM getSummary toAdd'
