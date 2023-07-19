{-# LANGUAGE Rank2Types #-}

module Data.Validation.Types.Trans
  ( ValidatorT (..)
  , liftV
  )
where

import Control.Monad.Trans.Class
import qualified Data.Text as Text
import Data.Validation.Types.Pure

newtype ValidatorT input m a = ValidatorT
  { runValidatorT :: input -> m (ValidationResult a)
  }

instance Functor m => Functor (ValidatorT input m) where
  fmap f (ValidatorT ma) = ValidatorT $ fmap (fmap (fmap f)) ma

instance Applicative m => Applicative (ValidatorT input m) where
  pure a = ValidatorT (const (pure (pure a)))

  (ValidatorT mf) <*> (ValidatorT ma) = ValidatorT $ \input ->
    fmap (<*>) (mf input) <*> ma input

instance Monad m => Monad (ValidatorT input m) where
  return = pure
  (ValidatorT ma) >>= f = ValidatorT $ \input -> do
    result <- ma input
    case result of
      Valid a -> runValidatorT (f a) input
      Invalid errs -> pure (Invalid errs)

internalFail :: Applicative a => [Char] -> ValidatorT input a b
internalFail str = ValidatorT $ \_ -> pure $ Invalid (errMessage (Text.pack str))

instance Monad m => MonadFail (ValidatorT input m) where
  fail = internalFail

instance MonadTrans (ValidatorT input) where
  lift ma = ValidatorT (const (Valid <$> ma))

liftV :: Applicative m => Validator input a -> ValidatorT input m a
liftV validator = ValidatorT $ \input -> pure (run validator input)
