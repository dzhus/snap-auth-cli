{-# LANGUAGE Rank2Types #-}

{-|

Main module.

-}

module Main

where

import Data.Functor

import Data.Aeson.Encode.Pretty

import qualified Data.Text as T (pack)
import qualified Data.ByteString.Lazy as LB (putStr)
import qualified Data.ByteString.UTF8 as B (fromString)

import System.Console.GetOpt
import System.Environment

import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Web.ClientSession


-- | Possible modes of operation.
data Mode = Create | Delete | Show'
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
mgrNewUser :: IAuthBackend r =>
               r
               -> (String, String)
               -> IO AuthUser
mgrNewUser amgr (l, p) =
    let
        login = T.pack l
        pass = B.fromString p
        user' = defAuthUser{userLogin = login}
    in
      do
        user <- setPassword user' pass
        save amgr user


type AuthUserAction = IAuthBackend r => r -> AuthUser -> IO ()

-- | Get user from backend by login and apply 'AuthUserAction' to it.
mgrOldUser :: IAuthBackend r => r -> String -> AuthUserAction -> IO ()
mgrOldUser amgr l f =
    let
        login = T.pack l
    in
      do
        user <- lookupByLogin amgr login
        case user of
          Just found -> f amgr found
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
            , Option ['w'] ["show"]
              (NoArg $ \opts -> opts{optMode = Just Show'})
              "Show user"
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
        header = "Usage: snap-auth-cli [OPTIONS]"
    in
      do
        -- Parse command-line args into Options opts
        getopts <- getOpt Permute options <$> getArgs
        opts <- case getopts of
          ([], _, _) -> ioError $ userError $ usageInfo header options
          (o, _, []) -> return $ foldl (flip id) defaultOptions o
          (_, _, errs) -> ioError $ userError $
                          concat errs ++ usageInfo header options

        -- Load JSON database using file specified in -j
        amgr <- mkJsonAuthMgr (optJson opts)
                
        -- Operate depending on mode selected
        case (optMode opts, optLogin opts, optPassword opts) of
          (Nothing, _, _) -> ioError (userError "No operation mode selected")
          (_, Nothing, _) -> ioError $ userError "No user selected"

          (Just Show', Just l, _) -> mgrOldUser amgr l 
                                     (\_ u -> LB.putStr $ encodePretty u)

          (Just Delete, Just l, _) -> mgrOldUser amgr l destroy

          (Just Create, Just l, Just p) -> mgrNewUser amgr (l, p)
                                           >> return ()
          (Just Create, _, Nothing) -> ioError $ userError "No password set"
