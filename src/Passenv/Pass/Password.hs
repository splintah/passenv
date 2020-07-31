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

{-# LANGUAGE DerivingStrategies #-}

{-|
Module      : Passenv.Password
Description : A file format with a password on the first line.
Copyright   : (c) Splinter Suidman, 2020
License     : GPL-3
Portability : portable

A file format with a password on the first line.
-}

module Passenv.Pass.Password
  ( PasswordFile (..)
    -- ** Parser
  , passwordFile
  , Parser
    -- ** Re-exports
  , HasPassword (..)
  ) where

import           Control.Monad.Reader (asks)
import           Data.Void            (Void)
import           Passenv.Builder      (HasPassword (password))
import           Passenv.Util         (line)
import           Text.Megaparsec      (Parsec)

-- | The type of the parser.
type Parser = Parsec Void String

-- | A 'PasswordFile' is a file that contains a password on the first
-- line. The rest of the contents will not be parsed.
--
-- This file format can be parsed using the 'passwordFile' parser.
--
-- === Example
--
-- > myverygoodpassword
-- > key: value
-- > Foo
--
-- > # Bar
--
-- Parsing this file will return:
--
-- > PasswordFile
-- >   { pwFilePassword = "myverygoodpassword"
-- >   }
data PasswordFile = PasswordFile
  { pwFilePassword :: String
    -- ^ The password.
  } deriving stock (Show)

instance HasPassword PasswordFile where
  password = asks pwFilePassword

-- | Parse a 'passwordFile'.
passwordFile :: Parser PasswordFile
passwordFile = PasswordFile <$> line
