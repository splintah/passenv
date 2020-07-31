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
{-# LANGUAGE OverloadedStrings  #-}

{-|
Module      : Passenv.KeyValue
Description : A file format with a password and key-value pairs.
Copyright   : (c) Splinter Suidman, 2020
License     : GPL-3
Portability : portable

A file format with a password and key-value pairs.
-}

module Passenv.Pass.KeyValue
  ( KeyValueFile (..)
  , KeyValueFileOptions (..)
    -- ** Parsers
  , keyValueFile
  , keyValueFileWith
  , Parser
    -- ** Re-exports
  , HasPassword (..)
  , HasKeyValues (..)
  ) where

import           Control.Applicative        hiding (many, some)
import           Control.Monad.Reader       (asks)
import           Data.Char                  (isSpace)
import           Data.Default               (Default (def))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Void                  (Void)
import           Passenv.Builder            (HasKeyValues (valueMaybe),
                                             HasPassword (password))
import           Passenv.Util               (isEol)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- | The type of the parser.
type Parser = Parsec Void String

-- | A key-value file is a password file format that contains:
--
-- * On the first line: a password.
--
-- * On all other lines: key-value pairs in the format @key: value@.
--   The spaces before and after the @:@ will be ignored; this means
--   that keys cannot contain spaces, and values cannot start with
--   spaces.
--
-- This file format is often used by browser extensions for @pass@.
--
-- This file format can be parsed using the 'keyValueFile' and
-- 'keyValueFileWith' parsers.
--
-- The parser also supports comments, starting with whatever character
-- you like. You can configure this character using
-- 'kvFileOptionsComment'. Comments must appear on their own line, so
-- the comment character /can/ appear inside values, as can be seen in
-- the following example.
--
-- === Example
--
-- > myverygoodpassword
-- > username: myusername
-- > email:user@example.com
-- > # A line comment, and an empty line:
-- >
-- > key         : value
-- > test: this is not # a comment
--
-- With the default options, parsing this file will return:
--
-- > KeyValueFile
-- >   { kvFilePassword = "myverygoodpassword"
-- >   , kvFileMap = 'Map.fromList'
-- >     [ ("email","user@example.com")
-- >     , ("key","value")
-- >     , ("test", "this is not # a comment")
-- >     , ("username","myusername")
-- >     ]
-- >   }
data KeyValueFile = KeyValueFile
  { kvFilePassword :: String
    -- ^ The password.
  , kvFileMap      :: Map String String
    -- ^ The key-value pairs.
  } deriving stock (Show)

instance HasPassword  KeyValueFile where
  password = asks kvFilePassword

instance HasKeyValues KeyValueFile where
  valueMaybe key = asks $ Map.lookup key . kvFileMap

-- | The options for parsing a 'KeyValueFile'.
--
-- This type has a 'Default' instance, so you can use 'def' as the
-- default options.
data KeyValueFileOptions = KeyValueFileOptions
  { kvFileOptionsComment :: Maybe Char
    -- ^ The character that starts comments. If 'Nothing', comments
    -- will not be parsed. If @'Just' c@ for some @c :: 'Char'@, the
    -- character @c@ will start comments.
    --
    -- See 'KeyValueFile' for more information about comments.
    --
    -- 'Default': @'Just' \'#\'@.
  } deriving (Show, Read, Eq)

-- |
-- > def = KeyValueFileOptions
-- >   { kvFileOptionsComment = Just '#'
-- >   }
instance Default KeyValueFileOptions where
  def = KeyValueFileOptions
    { kvFileOptionsComment = Just '#'
    }

spaceConsumer :: KeyValueFileOptions -> Parser ()
spaceConsumer KeyValueFileOptions { kvFileOptionsComment = Nothing } =
  Lexer.space space1 empty empty
spaceConsumer KeyValueFileOptions { kvFileOptionsComment = Just c } =
  Lexer.space space1 (Lexer.skipLineComment [c]) empty

lexeme :: KeyValueFileOptions -> Parser a -> Parser a
lexeme opts = Lexer.lexeme $ spaceConsumer opts

-- | Parse a 'KeyValueFile' with 'def'ault 'KeyValueFileOptions'.
keyValueFile :: Parser KeyValueFile
keyValueFile = keyValueFileWith def

-- | Parse a 'KeyValueFile' with the given 'KeyValueFileOptions'.
keyValueFileWith :: KeyValueFileOptions -> Parser KeyValueFile
keyValueFileWith opts  = KeyValueFile <$> lexeme opts line <*> keyValueMap opts

-- | Parse the key-value pairs.
keyValueMap :: KeyValueFileOptions -> Parser (Map String String)
keyValueMap opts = Map.fromList <$> many (keyValuePair opts)

-- | Parse a single key-value pair.
keyValuePair :: KeyValueFileOptions -> Parser (String, String)
keyValuePair opts = do
  key <- lexeme opts $ takeWhile1P Nothing (\c -> c /= ':' && not (isSpace c))
  _ <- lexeme opts $ char ':'
  val <- lexeme opts line
  pure (key, val)

line :: Parser String
line = takeWhileP Nothing (not . isEol)
