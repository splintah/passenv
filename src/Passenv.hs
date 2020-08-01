{-
passenv
Copyright (C) 2020 Splinter Suidman

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}

{-|
Module      : Passenv
Description : The main passenv module.
Copyright   : (c) Splinter Suidman, 2020
License     : GPL-3
Portability : POSIX

passenv is a tool and Haskell library for fetching secrets from a password store
managed by @pass@ and adding them to your environment.

See <https://github.com/splintah/passenv> for more information.
-}

module Passenv
  ( passenv
  , passenvWith
  , PassenvOptions (..)
  , PassenvException (..)
    -- ** Re-exports
    -- *** From "Passenv.Pass"
  , pass
  , passKV
  , passKVWith
  , passWithParser
    -- *** From "Passenv.Env"
  , env
    -- *** From "Passenv.Builder"
  , HasPassword (..)
  , HasKeyValues (..)
    -- *** From external modules
  , Default (..)
  ) where

import           Passenv.Builder      (Builder, HasKeyValues (..),
                                       HasPassword (..), runBuilder)
import           Passenv.Env          (Env (getEnv), env)
import           Passenv.Pass         (pass, passKV, passKVWith, passWithParser)

import           Control.Exception    (Exception)
import           Control.Monad.Catch  (throwM)
import           Data.Default         (Default (def))
import           System.Environment   (getArgs)
import           System.Posix.Env     (getEnvironment)
import           System.Posix.Process (executeFile)

-- | An exception for passenv functions.
data PassenvException
  = ArgumentsError String
    -- ^ Wrong or not enough arguments were supplied. The 'String' describes the
    -- intended usage.
  deriving anyclass (Exception)

instance Show PassenvException where
  show (ArgumentsError usage) = unwords
    [ "Wrong or not enough arguments were supplied."
    , "Usage:"
    , usage
    ]

-- | Options for 'passenv'.
--
-- There is a 'Default' instance for 'PassenvOptions'; you can use
-- 'def' to get the default options.
data PassenvOptions = PassenvOptions
  { passenvSearchPath :: Bool
    -- ^ Search the current path for the command.
    --
    -- See 'executeFile' for more information.
    --
    -- 'Default': 'True'.
  } deriving stock (Show)

instance Default PassenvOptions where
  -- |
  -- > def = PassenvOptions
  -- >   { passenvSearchPath = True
  -- >   }
  def = PassenvOptions
    { passenvSearchPath = True
    }

-- | Does the same as 'passenv', but does not get the arguments of the process,
-- and instead uses the command and arguments that are passed to the function.
passenvWith :: String -- ^ Command to run.
            -> [String] -- ^ Arguments for the command.
            -> PassenvOptions
            -> Builder IO Env
            -> IO void
passenvWith command args options builder = do
  oldEnv <- getEnvironment
  newEnv <- getEnv <$> runBuilder builder
  executeFile command (passenvSearchPath options) args (Just (oldEnv <> newEnv))

-- | Run the default passenv function:
--
-- * Get the process' arguments which describe the command to execute. Throws an
--   'ArgumentsError' when not enough arguments were supplied.
--
-- * Run the given builder, which builds an 'Env' in the 'IO' monad.
--
-- * Execute the command in the new environment.
--
-- This function will never finish, because the passenv process will be replaced
-- by the executed process.
passenv :: PassenvOptions -> Builder IO Env -> IO void
passenv options builder = do
  (command, args) <- getArgs >>= \case
    []   -> throwM $ ArgumentsError "pass command to execute in the new environment"
    c:as -> pure (c, as)

  passenvWith command args options builder
