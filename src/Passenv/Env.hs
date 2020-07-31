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

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|
Module      : Passenv.Env
Description : Environment builders.
Copyright   : (c) Splinter Suidman, 2020
License     : GPL-3
Portability : portable

Environment builders.

The 'env' 'Builder' is used to build an environment.
-}

module Passenv.Env
  ( Var
  , Env (..)
  , env
  ) where

import           Control.Applicative (liftA2)
import           Passenv.Builder     (Builder)

-- | An environment variable. This type is used for both the
-- variable's name and the variable's value.
type Var = String

-- | An environment. This is a list of pairs of 'Var's.
newtype Env = Env { getEnv :: [(Var, Var)] }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

-- | Create an 'Env' 'Builder' from a list of pairs of 'Var'
-- 'Builder's.
env :: Applicative m
    => [(Builder m Var, Builder m Var)]
    -> Builder m Env
env = fmap Env . env'

-- | More general version of 'env'.
env' :: Applicative m => [(m a, m b)] -> m [(a, b)]
env' = traverse (uncurry $ liftA2 (,))
