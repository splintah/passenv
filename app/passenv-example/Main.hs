{-
passenv-example
Copyright (C) 2020 Splinter Suidman

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : An example usage of the passenv library.
Copyright   : (c) Splinter Suidman, 2020
License     : GPL-3
Portability : POSIX

An example usage of the passenv library. This example is explained in detail at
<https://github.com/splintah/passenv#readme>.

Usage:

> passkey command [arguments ...]
-}

import           Passenv

main :: IO ()
main = passenv def
  $ pass "github.com" (env
      [ ("GITHUB_PASSWORD", password)
      ])
 <> passKV "mail" (env
      [ ("MAIL_PASSWORD", password)
      , ("MAIL_IMAP_HOST", value "imap")
      ])
 <> passKV "example.com" (env
      [ ("EXAMPLE_AUTH", value "username" <> ":" <> password)
      ])

