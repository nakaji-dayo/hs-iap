-- todo: move lobrary
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Network.IAP.AppStore where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson             as A
import           Data.Scientific
import           Data.Text              as T
import           Data.Time
import           GHC.Generics
import qualified Network.HTTP.Conduit   as C
import           Text.Casing
import           Text.Read

data Env = Sandbox | Production
  deriving (Eq, Show)
getUrl Sandbox    = "https://sandbox.itunes.apple.com/verifyReceipt"
getUrl Production = "https://buy.itunes.apple.com/verifyReceipt"

data Request = Request
  { _receiptData            :: Text
  , _password               :: Maybe String
  , _excludeOldTransactions :: Maybe Bool
  }
  deriving (Eq, Show, Generic)
makeLenses ''Request

kebabOption = defaultOptions
  { A.fieldLabelModifier = kebab . Prelude.drop 1
  }

snakeOption = defaultOptions
  { A.fieldLabelModifier = quietSnake . Prelude.drop 1
  }

instance ToJSON Request where
  toEncoding = genericToEncoding kebabOption

newtype AppleTime = AppleTime { unAppleTime :: UTCTime }
  deriving (Show, Read, Eq, Generic)

-- apple returns time with strange format
-- https://forums.developer.apple.com/thread/70335
-- https://stackoverflow.com/questions/38928232/converting-the-date-given-from-apple-receipts-to-a-net-datetime-object
instance FromJSON AppleTime where
  parseJSON = withText "UTCTime" $ \t ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack $ T.take 19 t) of
      Just d -> pure $ AppleTime d
      _      -> fail "could not parse apple date"

newtype AppleScientific = AppleScientific { unAppleScientific :: Scientific }
  deriving (Show, Read, Eq, Num, Generic)

instance FromJSON AppleScientific where
  parseJSON = withText "AppleInt" $ \t ->
    case readMaybe (unpack t) of
      Just s -> pure $ AppleScientific s
      _      -> fail "could not parse number"

data InApp = InApp
  { _quantity               :: AppleScientific
  , _productId              :: String
  , _transactionId          :: String
  , _originalTransactionId  :: String
  , _purchaseDate           :: AppleTime
  , _originalPurchaseDate   :: AppleTime
  , _expiresDate            :: Maybe AppleTime
  , _expirationIntent       :: Maybe String
  -- | For an expired subscription, whether or not Apple is still attempting to automatically renew the subscription.
  -- "1" - App Store is still attempting to renew the subscription.
  -- "0" - App Store has stopped attempting to renew the subscription.
  , _isInBillingRetryPeriod :: Maybe String
  , _isTrialPeriod          :: String
  , _isInIntroOfferPeriod   :: Maybe String
  , _cancellationDate       :: Maybe String
  , _cancellationReason     :: Maybe String
  , _appItemId              :: Maybe String
  }
  deriving (Eq, Show, Generic)
makeLenses ''InApp

instance FromJSON InApp where
  parseJSON = genericParseJSON snakeOption

data Receipt = Receipt
  { _bundleId            :: String
  , _applicationVersion  :: String
  , _receiptCreationDate :: AppleTime
  , _expirationDate      :: Maybe AppleTime
  , _inApp               :: [InApp]
  }
  deriving (Eq, Show, Generic)
makeLenses ''Receipt

instance FromJSON Receipt where
  parseJSON = genericParseJSON snakeOption

data Response = Response
  { _status            :: Int
  , _receipt           :: Maybe Receipt
  , _latestReceiptInfo :: Maybe [InApp]
  }
  deriving (Eq, Show, Generic)
makeLenses ''Response

data ErrorInfo = ErrorInfo
  { _errorStatus :: Maybe Int
  , _message     :: Maybe String
  }
  deriving (Eq, Show, Generic)
makeLenses ''ErrorInfo

pattern SandboxReceiptError <- Response 21007 _ _

instance FromJSON Response where
  parseJSON = genericParseJSON snakeOption

type ResponseStatus = Int

request :: MonadIO m
  => C.Manager -> Env -> Request -> m (Either String Response)
request manager env req = liftIO $ do
  initReq <- C.parseRequest (getUrl env)
  let body = encode req
  let request = initReq { C.method = "POST", C.requestBody = C.RequestBodyLBS body }
  response <- C.httpLbs request manager
  print $ C.responseBody response
  return $ eitherDecode (C.responseBody response)
