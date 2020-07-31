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

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|
Module      : Passenv.Builder
Description : A builder for monoidal values in a monadic context.
Copyright   : (c) Splinter Suidman, 2020
License     : GPL-3
Portability : portable

A builder for monoidal values in a monadic context.

This module contains the 'Builder' type that is used for building the
'Passenv.Env.Env', but also for the underlying 'Passenv.Env.Var's. And
it can be used for any 'Monoid'al value, and in any 'Monad'ic context
(an 'Applicative' context is often enough).

=== 'IsString'

This module also exports an 'IsString' instance for 'Builder's, which
is useful when building 'String's or other types with an 'IsString'
instance.

For example, if we have a builder

@
stringBuilder :: 'Applicative' m => 'Builder' m 'String'
@

then we can compose it with a 'String' literal:

@
stringBuilder '<>' "some string" :: 'Applicative' m => 'Builder' m 'String'
@

For this you must enable the @OverloadedStrings@ language extension,
by adding the following to the top of your file:

> {-\# LANGUAGE OverloadedStrings \#-}
-}

module Passenv.Builder
  ( -- ** Builder
    Builder
  , mkBuilder
  , runBuilder
    -- ** @Has@ classes
  , HasPassword (..)
  , HasKeyValues (..)
  , KeyValueException (..)
  ) where

import           Control.Exception    (Exception)
import           Control.Monad.Catch  (MonadCatch (catch), MonadThrow (throwM))
import           Control.Monad.Except (MonadError (catchError, throwError))
import           Control.Monad.Reader (MonadReader (ask, local))
import           Control.Monad.State  (MonadState (get, put))
import           Control.Monad.Trans  (MonadIO (liftIO), MonadTrans (lift))
import           Control.Monad.Writer (MonadWriter (..))
import           Data.Monoid          (Ap (Ap, getAp))
import           Data.String          (IsString (..))

----------------------------------------
-- Builder -----------------------------

-- | A builder for monoidal values in a monadic context.
--
-- 'Builder's are 'Monoid's if @a@ is a 'Monoid' and @m@ is an
-- 'Applicative', and can be composed with @('<>')@. 'Builder's are
-- also 'Monad's if @m@ is a 'Monad'.
newtype Builder m a = Builder { _runBuilder :: Ap m a }
  deriving stock (Show, Read)
  deriving newtype (Semigroup, Monoid, Functor, Applicative, Monad)

-- | Make a 'Builder' from a monadic action.
mkBuilder :: m a -> Builder m a
mkBuilder = Builder . Ap

-- | Run a 'Builder', returning the underlying monadic action.
runBuilder :: Builder m a -> m a
runBuilder = getAp . _runBuilder

----------------------------------------
-- Builder instances -------------------

instance MonadTrans Builder where
  lift = mkBuilder

instance MonadIO m => MonadIO (Builder m) where
  liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (Builder m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (Builder m) where
  catch m c = mkBuilder $ runBuilder m `catch` (runBuilder . c)

instance MonadReader r m => MonadReader r (Builder m) where
  ask = lift ask
  local f = lift . local f . runBuilder

instance MonadState s m => MonadState s (Builder m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (Builder m) where
  tell = lift . tell
  listen = mkBuilder . listen . runBuilder
  pass = mkBuilder . pass . runBuilder

instance MonadError e m => MonadError e (Builder m) where
  throwError = mkBuilder . throwError
  catchError m f = mkBuilder $ catchError (runBuilder m) (runBuilder . f)

----------------------------------------
-- IsString instance -------------------

instance (Applicative m, IsString a) => IsString (Builder m a) where
  fromString = pure . fromString

----------------------------------------
-- Has* classes ------------------------

-- | @r@ contains a password value.
class HasPassword r where
  -- | A 'Builder' which returns the password value.
  password :: MonadReader r m => Builder m String

-- | @r@ contains key-value pairs.
class HasKeyValues r where
  -- | Get the value of the given key, returning 'Nothing' if it is
  -- not found.
  valueMaybe :: MonadReader r m => String -> Builder m (Maybe String)

  -- | Get the value of the given key, throwing a 'KeyValueException'
  -- if it is not found.
  value :: (MonadReader r m, MonadThrow m) => String -> Builder m String
  value key = valueMaybe key >>= \case
    Nothing -> throwM $ KeyNotPresentError key
    Just v  -> pure v

-- | An exception for operations on a value which 'HasKeyValues'.
data KeyValueException
  -- | The given key is not present.
  = KeyNotPresentError String
  deriving anyclass (Exception)

instance Show KeyValueException where
  show (KeyNotPresentError key) = concat
    [ "Key "
    , show key
    , " not present in password file."
    ]
