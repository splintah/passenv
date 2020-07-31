{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

{-|
Module      : Passenv.Pass
Description : Functions for interacting with the password store.
Copyright   : (c) Splinter Suidman, 2020
License     : GPL-3
Portability : POSIX

Functions for interacting with the password store.
-}


module Passenv.Pass
  ( pass
  , passKV
  , passKVWith
  , passWithParser
  , passContents
  , PassException (..)
    -- Re-exports
  , HasPassword (..)
  , HasKeyValues (..)
  ) where

import           Control.Exception     (Exception,
                                        SomeException (SomeException))
import           Control.Monad.Catch   (MonadCatch (catch), MonadThrow (throwM))
import           Control.Monad.Reader  (ReaderT, runReaderT)
import           Control.Monad.Trans
import           Data.Typeable         (Typeable)
import           System.Exit           (ExitCode (..))
import           System.Process        (readProcessWithExitCode)
import           Text.Megaparsec       (Parsec, ShowErrorComponent, parse)

import           Passenv.Builder       (Builder, HasKeyValues (..),
                                        HasPassword (..), mkBuilder, runBuilder)
import           Passenv.Pass.KeyValue (KeyValueFile (..),
                                        KeyValueFileOptions (..), keyValueFile,
                                        keyValueFileWith)
import           Passenv.Pass.Password (PasswordFile (..), passwordFile)


-- | An exception that occurs when interacting with the password
-- store.
data PassException
    -- | The @pass@ process returned with an error.
  = PassProcessError
      Int -- ^ Process exit code.
      String -- ^ @stderr@ of the process.

    -- | An (unrelated) exception was thrown when interacting with a
    -- file in the password store.
  | forall e. Exception e => PassErrorWithFile
      String -- ^ The name of the file.
      e -- ^ The exception
  deriving anyclass (Exception)

instance Show PassException where
  show (PassProcessError exitCode stderr) = concat
    [ "pass process ended with unsuccessful exit code "
    , show exitCode
    , ", with stderr:\n"
    , stderr
    ]
  show (PassErrorWithFile file err) = concat
    [ show err
    , "\n"
    , "When reading file "
    , show file
    , " in the password store."
    ]

-- | 'passContents' @name@ gets the contents of the file named @name@ in the
-- password store.
--
-- The contents are retrieved by executing the @pass@ command with @name@ as its
-- argument.
passContents :: String -> IO String
passContents name = do
  (exitCode, out, err) <- readProcessWithExitCode "pass" [name] ""
  case exitCode of
    ExitFailure code -> throwM $ PassProcessError code err
    ExitSuccess      -> pure out

-- | 'passWithParser' @p@ @f@ @b@: read a file named @f@ from the
-- password store, parse it using @p@ which returns a value of type
-- @r@, run the builder @b@ that builds values of type @a@ in a
-- context with a value of type @r@, and return a 'Builder' for @a@.
passWithParser
  :: ( MonadIO m
     , MonadThrow m
     , MonadCatch m
       -- The following constraints for @e@ are necessary to use
       -- @throwM@.
     , Show e
     , ShowErrorComponent e
     , Typeable e
     )
  => Parsec e String r
  -> String
  -> Builder (ReaderT r m) a
  -> Builder m a
passWithParser parser file b = do
  contents <- liftIO $ passContents file
  r <- either throwM pure $ parse parser file contents
  mkBuilder (flip runReaderT r $ runBuilder b)
    `catch` (\(SomeException e) -> throwM $ PassErrorWithFile file e)

-- | 'pass' @f@ @b@: read a file named @f@ from the password store,
-- parse it as a 'PasswordFile', run the builder @b@ that builds
-- values of type @a@ in a context with a 'PasswordFile', and return a
-- 'Builder' for @a@.
pass :: (MonadIO m, MonadThrow m, MonadCatch m)
     => String
     -> Builder (ReaderT PasswordFile m) a
     -> Builder m a
pass = passWithParser passwordFile

-- | The same as 'passKVWith', but use the default options.
passKV :: (MonadIO m, MonadThrow m, MonadCatch m)
       => String
       -> Builder (ReaderT KeyValueFile m) a
       -> Builder m a
passKV = passWithParser keyValueFile

-- | 'passKV' @opts@ @f@ @b@: read a file named @f@ from the password
-- store, parse it as a 'KeyValueFile' with the options in @opts@, run
-- the builder @b@ that builds values of type @a@ in a context with a
-- 'KeyValueFile', and return a 'Builder' for @a@.
passKVWith :: (MonadIO m, MonadThrow m, MonadCatch m)
           => KeyValueFileOptions
           -> String
           -> Builder (ReaderT KeyValueFile m) a
           -> Builder m a
passKVWith = passWithParser . keyValueFileWith
