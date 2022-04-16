module Notifier where

import Servant.Client (ClientEnv, ClientM)
import Servant.API (ToHttpApiData)
import qualified Servant as Servant

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Text
import Data.Aeson

data Env = Env { code :: String
               , clientEnv :: ClientEnv
               }

data NState  =
  NState { posts :: [Name] -- ^ post names
         }

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

runNotifierM :: Env -> NotifierM a -> ClientM a
runNotifierM env action = do
  evalStateT (runReaderT action env) initState
  where
    initState = NState []

notifierMToHandler :: NotifierM ~> Servant.Handler
notifierMToHandler = undefined
