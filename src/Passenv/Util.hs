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

{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Passenv.Password
Description : A file format with a password on the first line.
Copyright   : (c) Splinter Suidman, 2020
License     : GPL-3
Portability : portable

A file format with a password on the first line.
-}

module Passenv.Util
  ( line
  , isEol
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | Parses a line with an optional end of line.
line :: (MonadParsec e s m, Stream s, Token s ~ Char) => m (Tokens s)
line = takeWhile1P Nothing (not . isEol)
    <* optional eol

-- | Returns whether the given 'Char' is an end of line character.
--
-- > isEol c = c == '\n' || c == '\r'
isEol :: Char -> Bool
isEol c = c == '\n' || c == '\r'
