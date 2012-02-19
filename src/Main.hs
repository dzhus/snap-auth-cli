{-|

Command-line tool to manage 'Snap.Snaplet.Auth.AuthManager' database.
Currently supports only 'Snap.Snaplet.Auth.Backends.JsonFile' backend.

Run without arguments to get usage info.

-}

module Main

where

import Data.Functor

import qualified Data.Text as T (pack)
import qualified Data.ByteString.UTF8 as B (fromString)

import System.Console.GetOpt
import System.Environment

import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Web.ClientSession


-- | Possible modes of operation.
data Mode = Create | Delete
          deriving Show


-- | Holds all options passed from command-line.
data Options = Options
    {
      optMode :: Maybe Mode
    , optLogin :: Maybe String
    , optPassword :: Maybe String
    , optJson :: String
    }
    deriving Show


-- | Default program options (no-op).
defaultOptions :: Options
defaultOptions = Options
    {
      optMode = Nothing
    , optLogin = Nothing
    , optPassword = Nothing
    , optJson = "users.json"
    }


-- | Save new user in auth backend given user login and password
mgrSaveUser :: IAuthBackend r =>
               r
               -> (String, String)
               -> IO AuthUser
mgrSaveUser amgr (l, p) =
    let
        login = T.pack l
        pass = B.fromString p
        user' = defAuthUser{userLogin = login}
    in
      do
        user <- setPassword user' pass
        save amgr user


-- | Try to delete user in auth backend given user login
mgrDeleteUser :: IAuthBackend r => r -> String -> IO ()
mgrDeleteUser amgr l =
    let
        login = T.pack l
    in
      do
        user <- lookupByLogin amgr login
        case user of
          Just found -> destroy amgr found
          Nothing -> ioError $ userError $
                     l ++ ": user not found"


main :: IO ()
main =
    let
        options :: [OptDescr (Options -> Options)]
        options =
            [
              Option ['c'] ["create"]
              (NoArg $ \opts -> opts{optMode = Just Create})
              "Create new user"
            , Option ['d'] ["delete"]
              (NoArg $ \opts -> opts{optMode = Just Delete})
              "Delete user"
            , Option ['u'] ["user", "name"]
              (ReqArg (\u opts -> opts{optLogin = Just u}) "USER")
              "User login"
            , Option ['p'] ["password"]
              (ReqArg (\p opts -> opts{optPassword = Just p}) "PWD")
              "User password"
            , Option ['j'] ["json"]
              (ReqArg (\j opts -> opts{optJson = j}) "JSONFILE")
              "JsonFile backend storage file"
            ]
    in
      do
        -- Parse command-line args into Options opts
        getopts <- getOpt Permute options <$> getArgs
        opts <- case getopts of
          (o, _, []) -> return $ foldl (flip id) defaultOptions o
          (_, _, errs) -> ioError $ userError $
                          concat errs ++ usageInfo header options
              where header = "Usage: snap-auth-cli [OPTIONS]"

        -- Load JSON database using file specified in -j
        amgr <- mkJsonAuthMgr (optJson opts)

        -- Operate depending on mode selected
        case (optMode opts, optLogin opts, optPassword opts) of
          (Nothing, _, _) -> ioError (userError "No operation mode selected")
          (_, Nothing, _) -> ioError $ userError "No user selected"
          (Just Delete, Just l, _) -> mgrDeleteUser amgr l
          (Just Create, Just l, Just p) -> mgrSaveUser amgr (l, p)
                                           >> return ()
          (Just Create, _, Nothing) -> ioError $ userError "No password set"
