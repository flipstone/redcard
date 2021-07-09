{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Validation
import Data.Aeson

rejectsAll :: ValidatorT IO Int
rejectsAll = liftV $ Validator $ const $ Invalid (errMessage "I reject everything!")

rejectsAllUsingFail :: ValidatorT IO Int
rejectsAllUsingFail = liftV $ fail "Validator fail message which shouldn't be IO's fail"

emptyJSON :: Value
emptyJSON = toJSON ("" :: String)

main :: IO ()
main = do
  -- Just verify that they do not throw
  result1 <- runValidatorT rejectsAll emptyJSON
  print result1
  result2 <- runValidatorT rejectsAllUsingFail emptyJSON
  print result2
  pure ()
