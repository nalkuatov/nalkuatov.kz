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

-- | TODO: Implement a persistent state
-- using 'stm'(?)
data NState  =
  NState { posts :: [Name] -- ^ post names
         } deriving stock (Eq, Show)

-- | Represents the name of a post
newtype Name = Name { unName :: Text }
  deriving stock (Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToHttpApiData)

-- | Would be nice to start using commands somehow
data Action = DoNothing | Notify Name

type NotifierM = ReaderT Env (StateT NState ClientM)

type f ~> g = forall a. f a -> g a

add :: Name -> NState -> NState
add post = NState . (post :) . posts

contains :: Name -> NState -> Bool
contains post = (post `elem`) . posts

runNotifierM :: Env -> NState -> NotifierM a -> IO a
runNotifierM env@Env{..} initState action = do
  let req = evalStateT (runReaderT action env) initState
  res <- Servant.runClientM req clientEnv
  case res of
    Left e  -> throwM e
    Right a -> pure a

notifierMToHandler :: Env -> NState -> NotifierM ~> Servant.Handler
notifierMToHandler env initState =
  Servant.Handler . ExceptT . try . runNotifierM env initState
