{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Main module.

-}

module Main

where

import Data.Functor

import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Types as A

import qualified Data.Text as T (pack)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as LB (putStr)
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.HashMap.Lazy as M

import System.Console.CmdArgs.Implicit

import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile

import Web.ClientSession


-- | Rank-2 type for action applicable to AuthManager and AuthUser.
type AuthUserAction = IAuthBackend r => r -> AuthUser -> IO ()


-- | Save new user in auth backend given user login, password and roles
mgrNewUser :: IAuthBackend r => r
           -> (String, String, [String], [(String, String)])
           -> IO AuthUser
mgrNewUser amgr (l, p, rs, mt) =
    let
        login = T.pack l
        pass = BU.fromString p
        roles = map (Role . BU.fromString) $ rs
        meta = M.fromList $
               map (\(k, v) -> (T.pack k, A.String $ T.pack v)) mt
        au' = defAuthUser{ userLogin = login
                         , userRoles = roles
                         , userMeta = meta}
    in
      do
        au <- setPassword au' pass
        save amgr au


-- | Get user from backend by login and apply 'AuthUserAction' to it.
mgrOldUser :: IAuthBackend r => r
           -> String
           -> AuthUserAction
           -> IO ()
mgrOldUser amgr l f =
    let
        login = T.pack l
    in
      do
        au <- lookupByLogin amgr login
        case au of
          Just found -> f amgr found
          Nothing -> ioError $ userError $
                     l ++ ": user not found"


-- | Possible modes of operation.
data OpMode = Create | Read | Delete
              deriving (Show, Data, Typeable)

-- | Default instance for CmdArg.
instance Default OpMode where
    def = Read


-- | Holds all options passed from command-line.
data Options = Options
    { mode :: OpMode
    , user :: Maybe String
    , password :: Maybe String
    , json :: String
    , role :: [String]
    , key :: [String]
    , value :: [String]
    }
    deriving (Show, Data, Typeable)


main :: IO ()
main =
    let
        sample = Options
                 { mode = enum [Read &= help "Read existing user"
                               , Create &= help "Create new user"
                               , Delete &= help "Delete user"]
                 , user = def &= help "User login"
                 , password = def
                 , role = def &= name "r"
                   &= help "User role. May be specified multiple times"
                 , key = def &= name "k"
                   &= help "User meta key. Must be followed by value option"
                 , value = def &= name "v"
                   &= help "User meta value."
                 , json = "users.json"
                   &= typFile
                   &= help "Path to JsonFile database"
                 }
                 &= program "snap-auth-cli"
    in do
      -- RecordWildCards
      Options{..} <- cmdArgs $ sample
      amgr <- mkJsonAuthMgr json
      case (mode, user, password) of
        (_, Nothing, _) -> ioError $ userError "No user selected"
        (Read, Just l, _) -> mgrOldUser amgr l
                             (\_ au -> LB.putStr $ encodePretty au)
        (Delete, Just l, _) -> mgrOldUser amgr l destroy
        (Create, Just l, Just pw) -> mgrNewUser amgr (l, pw, role, meta)
                                     >> return ()
                                  where meta = zip key value
        (Create, _, Nothing) -> ioError $ userError "No password set"
