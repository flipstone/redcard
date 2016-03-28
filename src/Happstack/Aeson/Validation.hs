{-# LANGUAGE FlexibleContexts #-}
module Happstack.Aeson.Validation where

import            Data.Aeson
import            Control.Monad.IO.Class
import qualified  Happstack.Server as HST
import            Data.Validation
import            Happstack.Aeson.Message ()

{-
 - HTTP Error code 422
 -}
unprocessibleEntityCode :: Int
unprocessibleEntityCode = 422

{-
 - Generate a web response with a 422 error code
 -}
unprocessibleEntity :: (HST.FilterMonad HST.Response m) => a -> m a
unprocessibleEntity = HST.resp unprocessibleEntityCode

{-
 - Validate the request body as JSON with n pure validator and the default error handler
 -}
validateJSONBody :: (MonadIO m, HST.ServerMonad m, HST.WebMonad HST.Response m)
                 => Validator a -> m a
validateJSONBody = validateJSONBodyWith respondWithErrors

{-
 - Validate the request body as JSON with an impure validator and the default error handler
 -}
validateJSONBodyT :: (MonadIO m, HST.ServerMonad m, HST.WebMonad HST.Response m)
                  => ValidatorT m a -> m a
validateJSONBodyT = validateJSONBodyTWith respondWithErrors

{-
 - Validate the request body as JSON with a pure validator and a custom error handler
 -}
validateJSONBodyWith :: (MonadIO m, HST.ServerMonad m)
                     => (Errors -> m a) -> Validator a -> m a
validateJSONBodyWith handleErrors = validateJSONBodyTWith handleErrors . liftV

{-
 - Validate the request body as JSON with an impure validator and a custom error handler
 -}
validateJSONBodyTWith :: (MonadIO m, HST.ServerMonad m)
                      => (Errors -> m a) -> ValidatorT m a -> m a
validateJSONBodyTWith handleErrors validator = do
  req <- HST.askRq
  requestBody <- HST.takeRequestBody req
  let bodyString = maybe "" HST.unBody requestBody

  validationResult <- decodeValidJSONT validator bodyString

  case validationResult of
    Valid f -> pure f
    Invalid errs -> handleErrors errs

{-
 - Default error response generator. Returns the Errors JSON
 - as the body with response code 422
 -}
respondWithErrors :: HST.WebMonad HST.Response m => Errors -> m a
respondWithErrors errs =
  let response = HST.toResponse (toJSON errs)
  in HST.finishWith (response { HST.rsCode = unprocessibleEntityCode })

