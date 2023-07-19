{-# LANGUAGE ScopedTypeVariables #-}

module Data.Validation.Primitives where

import Control.Monad (join)
import Data.Char
import Data.Convertible
import Data.Foldable
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Ord (Down (Down))
import Data.Scientific
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vec

import Data.Validation.Types

liftResult :: ValidationResult a -> Validator input a
liftResult result = Validator (const result)

validRead :: Validatable input => String -> ReadS a -> Validator input a
validRead err reader = do
  text <- string

  case reader (Text.unpack text) of
    [(value, "")] -> pure value
    _ -> fail err

bool :: Validatable input => Validator input Bool
bool =
  Validator $ \input ->
    case inputBool input of
      Just True -> Valid True
      Just False -> Valid False
      _ -> Invalid (errMessage "must_be_bool")

string :: Validatable input => Validator input Text.Text
string =
  Validator $ \input ->
    case inputText input of
      Just t -> Valid (Text.strip t)
      _ -> Invalid (errMessage "must_be_string")

notBlank :: Validator input Text.Text -> Validator input Text.Text
notBlank validator =
  Validator $ \value ->
    case run validator value of
      Valid t | Text.all isSpace t -> Invalid (errMessage "must_not_be_blank")
      result -> result

atMost :: Int -> Validator input Text.Text -> Validator input Text.Text
atMost len validator =
  Validator $ \value ->
    case run validator value of
      Valid t | Text.length t > len -> Invalid (errMessage $ Text.pack ("max_length_" ++ show len))
      result -> result

atLeast :: Int -> Validator input Text.Text -> Validator input Text.Text
atLeast len validator =
  Validator $ \value ->
    case run validator value of
      Valid t | Text.length t < len -> Invalid (errMessage $ Text.pack ("min_length_" ++ show len))
      result -> result

exactly :: Int -> Validator input Text.Text -> Validator input Text.Text
exactly len validator =
  Validator $ \value ->
    case run validator value of
      Valid t | Text.length t /= len -> Invalid (errMessage $ Text.pack ("must_be_length_" ++ show len))
      result -> result

numeric :: Validatable input => Validator input Scientific
numeric = Validator $ \input ->
  case scientificNumber input of
    Just n -> Valid n
    _ -> Invalid (errMessage "must_be_numeric")

integer :: (Validatable input, Integral i, Bounded i) => Validator input i
integer = do
  number <- numeric
  case toBoundedInteger number of
    Just int -> return int
    _ -> fail "must_be_integer"

double :: Validatable input => Validator input Double
double = fromRational . toRational <$> numeric

nonEmpty :: Validator input [a] -> Validator input [a]
nonEmpty validator =
  Validator $ \input ->
    case (run validator input) of
      Valid [] -> Invalid (errMessage $ Text.pack ("must_have_at_least_one_in_list"))
      result -> result

foldableOf ::
  (Validatable input, Applicative f, Monoid (f a)) =>
  Validator input a ->
  Validator input (f a)
foldableOf validator =
  Validator $ \input ->
    case arrayItems input of
      Just items ->
        let
          itemValidator = runIndexed (pure <$> validator)
        in
          fold (Vec.imap itemValidator items)
      _ -> Invalid (errMessage "must_be_array")

arrayOf :: Validatable input => Validator input a -> Validator input [a]
arrayOf = foldableOf

nonEmptyOf :: Validatable input => Validator input a -> Validator input (NonEmpty.NonEmpty a)
nonEmptyOf validator =
  NonEmpty.fromList <$> nonEmpty (arrayOf validator)

setIgnoringDuplicatesOf ::
  (Validatable input, Ord a) =>
  Validator input a ->
  Validator input (Set.Set a)
setIgnoringDuplicatesOf validator =
  Set.fromList <$> arrayOf validator

{- | Returns Invalid if the input contains the same element more than once.
 | WARNING: Does not report duplicate elements if validator does not
 |          pass on every element.
-}
setRejectingDuplicatesOf ::
  forall a input.
  (Validatable input, Ord a, Show a) =>
  Validator input a ->
  Validator input (Set.Set a)
setRejectingDuplicatesOf validator = do
  list <- arrayOf validator
  let
    withOnes = [(c :: a, 1) | c <- list]
    valueToFreq = Map.fromListWith (+) withOnes

    sortBySndDescending = map (fmap (\(Down x) -> x)) . sortOn snd . map (fmap Down)

    sortedByFreq :: [(a, Int)]
    sortedByFreq = sortBySndDescending $ Map.toList valueToFreq

    duplicates :: [(a, Int)]
    duplicates = takeWhile ((> 1) . snd) sortedByFreq

    errs :: [Text.Text]
    errs = map makeSetErr duplicates

  if length errs /= 0
    then Validator $ \_ -> Invalid $ Messages $ Set.fromList errs
    else pure $ Set.fromList list

makeSetErr :: Show a => (a, Int) -> Text.Text
makeSetErr (value, occurrances) =
  Text.pack $ "duplicate_element_in_array_validated_as_set: " ++ show value ++ " occurs " ++ show occurrances ++ " times"

ifInvalid ::
  Validator input a ->
  Validator input b ->
  Validator input (Either a b)
ifInvalid validA validB = Validator $ \input ->
  case run validA input of
    Valid a -> Valid (Left a)
    Invalid _ -> Right <$> run validB input

firstValid :: Validator input a -> Validator input a -> Validator input a
firstValid v1 v2 = either id id <$> ifInvalid v1 v2

foldUntilValid :: NE.NonEmpty (Validator input a) -> Validator input a
foldUntilValid = foldl1 firstValid

validConversion :: Convertible a b => Validator input a -> Validator input b
validConversion validator = do
  a <- validator

  case safeConvert a of
    Right b -> pure b
    Left err -> fail (convErrorMessage err)

runIndexed ::
  Validator input a ->
  Int ->
  input ->
  ValidationResult a
runIndexed validator idx = run (nestIndex idx validator)

nestIndex :: Int -> Validator input a -> Validator input a
nestIndex = nest . Text.pack . show

nest :: Text.Text -> Validator input a -> Validator input a
nest attr validator = mapErrors (nestErrors attr) `mapResult` validator

mustBeNull :: Validatable input => Validator input ()
mustBeNull = Validator $ \input ->
  case inputNull input of
    IsNull -> Valid ()
    NotNull -> Invalid (errMessage "must_be_null")
    InvalidNull text -> Invalid (errMessage text)

canBeNull :: Validatable input => Validator input ()
canBeNull = Validator $ \input ->
  case inputNull input of
    IsNull -> Valid ()
    NotNull -> Valid ()
    InvalidNull text -> Invalid (errMessage text)

nullable' :: Validatable input => Validator input a -> Validator input (Maybe a)
nullable' validator =
  either (const Nothing) Just
    <$> (mustBeNull `ifInvalid` validator)

nullable :: Validatable input => Validator input a -> Validator input (Maybe a)
nullable validator = Validator $ \input ->
  case run canBeNull input of
    Valid _ -> run (nullable' validator) input
    Invalid err -> Invalid err

required :: Validatable input => Text.Text -> Validator input a -> Validator input a
attrName `required` validator = validateAttr attrName req
 where
  req (Just subvalue) = run validator subvalue
  req Nothing = Invalid (errMessage "must_be_present")

optional :: Validatable input => Text.Text -> Validator input a -> Validator input (Maybe a)
attrName `optional` validator = validateAttr attrName opt
 where
  opt (Just value) = Just <$> run validator value
  opt Nothing = Valid Nothing

optionalAndNullable :: Validatable input => Text.Text -> Validator input a -> Validator input (Maybe a)
attrName `optionalAndNullable` validator = join <$> attrName `optional` nullable validator

infixr 5 `required`
infixr 5 `optional`

notPresent :: Validatable input => Text.Text -> Validator input ()
notPresent attr = validateAttr attr $ isNotPresent
 where
  isNotPresent (Just _) = Invalid (errMessage "must_not_be_present")
  isNotPresent Nothing = Valid ()

validateAttr ::
  Validatable input =>
  Text.Text ->
  (Maybe input -> ValidationResult a) ->
  Validator input a
validateAttr attrName f =
  Validator $ \input ->
    case lookupChild attrName input of
      LookupResult result ->
        mapErrors (nestErrors attrName) (f result)
      InvalidLookup ->
        Invalid (errMessage "must_be_object")
