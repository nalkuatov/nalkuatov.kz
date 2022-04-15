module Notifier where

import Servant.Client (ClientEnv)

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Text

data Env = Env { code :: String
               , clientEnv :: ClientEnv
               }

data NState  =
  NState { posts :: [Name] -- ^ post names
         }

-- | Represents the name of a post
data Name = Name { unName :: Text }
  deriving stock (Show, Eq)

data Action = DoNothing | Notify Name

type NotifierM a = forall m. (MonadThrow m, MonadIO m) =>
  ReaderT Env (StateT NState m) a

put :: Name -> NState -> NState
put post = NState . (post :) . posts

contains :: Name -> NState -> Bool
contains post = (post `elem`) . posts

runNotifierM
  :: (MonadReader Env m
  , MonadThrow m
  , MonadIO m)
  => NotifierM a -> m a
runNotifierM action = do
  env <- ask
  evalStateT (runReaderT action env) initState
  where
    initState = NState []
