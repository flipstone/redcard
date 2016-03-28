{-# LANGUAGE Rank2Types #-}
module Data.Validation.Types.Trans where

import            Control.Monad.Trans.Class

import qualified  Data.Text as Text
import            Data.Validation.Types.Pure

newtype ValidatorT m a = ValidatorT {
    runValidatorT :: forall input. Validatable input
                  => input
                  -> m (ValidationResult a)
  }

instance Functor m => Functor (ValidatorT m) where
  fmap f (ValidatorT ma) = ValidatorT $ fmap (fmap (fmap f)) ma

instance Applicative m => Applicative (ValidatorT m) where
  pure a = ValidatorT (const (pure (pure a)))

  (ValidatorT mf) <*> (ValidatorT ma) = ValidatorT $ \input ->
    (fmap (<*>) (mf input)) <*> ma input

instance Monad m => Monad (ValidatorT m) where
  return a = ValidatorT (const (return (pure a)))
  (ValidatorT ma) >>= f = ValidatorT $ \input -> do
    result <- ma input
    case result of
      Valid a -> runValidatorT (f a) input
      Invalid errs -> pure (Invalid errs)

  fail str = ValidatorT $ \_ -> pure $ Invalid (errMessage (Text.pack str))

instance MonadTrans ValidatorT where
  lift ma = ValidatorT (const (Valid <$> ma))

liftV :: Applicative m => Validator a -> ValidatorT m a
liftV validator = ValidatorT $ \input -> pure (run validator input)
