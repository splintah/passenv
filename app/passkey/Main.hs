{-
passkey
Copyright (C) 2020 Splinter Suidman

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE LambdaCase #-}

{-|
Module      : Main
Description : An example usage of the passenv library, retrieving a
              value from a key-value file.
Copyright   : (c) Splinter Suidman, 2020
License     : GPL-3
Portability : POSIX

An example usage of the passenv library, retrieving a value from a
key-value file.

Usage:

> passkey file key
-}

module Main where

import           Control.Exception  (throwIO)
import           Passenv.Builder    (runBuilder, value)
import           Passenv.Pass       (passKV)
import           System.Environment (getArgs)

passkey :: String -- ^ File
        -> String -- ^ Key
        -> IO String
passkey file key = runBuilder $ passKV file (value key)

main :: IO ()
main =
  getArgs >>= \case
    (f:k:_) -> passkey f k >>= putStrLn
    _ -> throwIO . userError $
      "Usage: passkey FILE KEY."
