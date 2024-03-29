{-# LANGUAGE Rank2Types #-}

module Data.Validation.Types.Pure
  ( CanNull (..)
  , Validator (..)
  , Validatable (..)
  , Lookup (..)
  , ValidationResult (..)
  , Errors (..)
  , errMessage
  , nestErrors
  , mapResult
  , mapErrors
  , errorsAppend
  , validationResultAppend
  ) where

import Control.Applicative
import qualified Data.Map.Strict as Map
import Data.Scientific
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Data.Vector as Vec

newtype Validator input a = Validator
  { run :: input -> ValidationResult a
  }

data CanNull
  = IsNull
  | NotNull
  | InvalidNull Text.Text
  deriving (Show, Eq)

class Typeable input => Validatable input where
  inputText :: input -> Maybe Text.Text
  inputBool :: input -> Maybe Bool
  inputNull :: input -> CanNull
  arrayItems :: input -> Maybe (Vec.Vector input)
  scientificNumber :: input -> Maybe Scientific
  lookupChild :: Text.Text -> input -> Lookup input

data Lookup input
  = LookupResult (Maybe input)
  | InvalidLookup

data ValidationResult a
  = Valid a
  | Invalid Errors
  deriving (Eq, Show)

data Errors
  = Messages (Set.Set Text.Text)
  | Group (Map.Map Text.Text Errors)
  deriving (Eq, Show)

-- Helpers for building primitive validators

errMessage :: Text.Text -> Errors
errMessage text = Messages (Set.singleton text)

nestErrors :: Text.Text -> Errors -> Errors
nestErrors attr err = Group (Map.singleton attr err)

mapResult ::
  (ValidationResult a -> ValidationResult b) ->
  Validator input a ->
  Validator input b
mapResult f v = Validator $ \value -> f (run v value)

mapErrors :: (Errors -> Errors) -> ValidationResult a -> ValidationResult a
mapErrors f (Invalid errs) = Invalid (f errs)
mapErrors _ valid = valid

-- Instances
instance Semigroup Errors where
  (<>) = errorsAppend

instance Monoid Errors where
  mempty = Messages Set.empty
  mappend = (<>)

errorsAppend ::
  Errors ->
  Errors ->
  Errors
errorsAppend (Messages m) (Messages m') = Messages (m `mappend` m')
errorsAppend (Group g) (Group g') = Group (Map.unionWith mappend g g')
errorsAppend g m@(Messages _) = g `mappend` nestErrors "" m
errorsAppend m g = nestErrors "" m `mappend` g

instance Monoid a => Semigroup (ValidationResult a) where
  (<>) = validationResultAppend

instance Monoid a => Monoid (ValidationResult a) where
  mempty = Valid mempty
  mappend = (<>)

validationResultAppend ::
  Monoid m =>
  ValidationResult m ->
  ValidationResult m ->
  ValidationResult m
validationResultAppend (Valid a) (Valid a') = Valid (a `mappend` a')
validationResultAppend (Invalid e) (Invalid e') = Invalid (e `mappend` e')
validationResultAppend (Valid _) invalid = invalid
validationResultAppend invalid (Valid _) = invalid

instance Functor ValidationResult where
  f `fmap` (Valid a) = Valid (f a)
  _ `fmap` (Invalid errors) = Invalid errors

instance Applicative ValidationResult where
  pure = Valid

  (Valid f) <*> (Valid a) = Valid (f a)
  (Invalid errs) <*> (Invalid errs') = Invalid (errs `mappend` errs')
  Invalid errs <*> _ = Invalid errs
  _ <*> Invalid errs = Invalid errs

instance Functor (Validator input) where
  f `fmap` v = mapResult (fmap f) v

instance Applicative (Validator input) where
  pure a = Validator (const (pure a))
  v <*> v' = Validator $ \value -> run v value <*> run v' value

instance Monad (Validator input) where
  v >>= f = Validator $ \input ->
    case run v input of
      Invalid errors -> Invalid errors
      Valid a -> run (f a) input

internalFail :: [Char] -> Validator input a
internalFail str = Validator $ \_ -> Invalid (errMessage (Text.pack str))

instance MonadFail (Validator input) where
  fail = internalFail

instance Functor Lookup where
  fmap _ InvalidLookup = InvalidLookup
  fmap f (LookupResult r) = LookupResult (fmap f r)

instance Applicative Lookup where
  pure = LookupResult . pure

  InvalidLookup <*> _ = InvalidLookup
  _ <*> InvalidLookup = InvalidLookup
  (LookupResult f) <*> (LookupResult a) = LookupResult (f <*> a)

instance Alternative Lookup where
  empty = LookupResult Nothing

  InvalidLookup <|> other = other
  other <|> InvalidLookup = other
  (LookupResult a) <|> (LookupResult b) = LookupResult (a <|> b)
