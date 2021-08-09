{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Machinations.JWT where
import qualified Web.JWT as JWT
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Aeson
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)

newtype Token = Token { email :: T.Text }
  deriving (Generic, ToJSON, FromJSON)

encodeToken :: UTCTime -> NominalDiffTime -> T.Text -> Token -> T.Text
encodeToken now secondsValid secret token =
  case (JWT.numericDate $ utcTimeToPOSIXSeconds $ now,
        JWT.numericDate $ utcTimeToPOSIXSeconds $ addUTCTime secondsValid now) of
    (Just iat, Just exp) ->
      JWT.encodeSigned
        (JWT.hmacSecret secret)
        (JWT.JOSEHeader { JWT.typ = Nothing,
                          JWT.cty = Nothing,
                          JWT.alg = Just JWT.HS256,
                          JWT.kid = Nothing })
        (mempty { JWT.iat = Just iat, -- issued time
                  JWT.nbf = Just iat, -- don't accept before time
                  JWT.exp = Just exp, -- expiration time
                  JWT.unregisteredClaims =
                    JWT.ClaimsMap $ M.singleton "token" (toJSON token) })
    _ -> error "Invalid time!"

decodeToken :: UTCTime -> T.Text -> T.Text -> Maybe Token
decodeToken now secret txt = do
  s <- JWT.decodeAndVerifySignature 
       (JWT.hmacSecret secret)
       (T.drop 6 txt) -- txt comes with a "Token " prefix
  now' <- JWT.numericDate $ utcTimeToPOSIXSeconds $ now
  u <- M.lookup "token" $ JWT.unClaimsMap
                       $ JWT.unregisteredClaims
                       $ JWT.claims s
  ((toMaybe . (now' <)) =<<) $ JWT.exp $ JWT.claims s
  case fromJSON u of
    Success t -> pure t
    _ -> Nothing
  where toMaybe True = Just ()
        toMaybe False = Nothing
