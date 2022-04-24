module Notifier where

import Servant.Client (ClientEnv, ClientM)
import qualified Servant.Client as Servant
import Servant.API (ToHttpApiData)
import qualified Servant as Servant

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Text
import Data.IORef
import Data.Aeson
import Data.Maybe
import GHC.Generics

data AppConfig
  = AppConfig { bot :: Text
              , chatId :: Text
              }
  deriving stock Generic
  deriving anyclass FromJSON

data Env = Env { appConfig :: AppConfig
               , clientEnv :: ClientEnv
               }

data NState  =
  NState { posts :: [Article] -- ^ post names
         } deriving stock (Eq, Show, Generic)
           deriving anyclass (FromJSON, ToJSON)

-- | Represents the name of a post
data Article =
  Article { filename :: FilePath -- ^ considered to be unique
          , title :: Text
          }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq Article where
  (filename -> a) == (filename -> b) =
    a == b

-- | Would be nice to start using commands somehow
data Action = DoNothing | Notify Article

type NotifierM = ReaderT Env (StateT NState ClientM)

type f ~> g = forall a. f a -> g a

add :: Article -> NState -> NState
add post = NState . (post :) . posts

contains :: Article -> NState -> Bool
contains post = (post `elem`) . posts

runNotifierM :: Env -> NState -> NotifierM a -> IO a
runNotifierM env@Env{..} fallbackState action = do
  currentState <-
    decodeFileStrict "db.json" `catch` (\(e :: SomeException) -> pure Nothing)
  let st  = fromMaybe fallbackState currentState
  let req = runStateT (runReaderT action env) st
  res <- Servant.runClientM req clientEnv
  case res of
    Left e  -> throwM e
    Right (a, newState) ->
      encodeFile "db.json" newState *> pure a

notifierMToHandler :: Env -> NState -> NotifierM ~> Servant.Handler
notifierMToHandler env fallbackState =
  Servant.Handler . ExceptT . try . runNotifierM env fallbackState
