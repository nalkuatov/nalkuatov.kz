module Notifier where

import Servant.Client (ClientEnv)

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

data Env = Env { code :: String
               , clientEnv :: ClientEnv
               }

data NState  =
  NState { posts :: [Post]
         }

newtype Post = Post { file :: FilePath }
  deriving stock (Show, Eq)

data Action = DoNothing | Notify Post

type NotifierM a = forall m. (MonadThrow m, MonadIO m) =>
  ReaderT Env (StateT NState m) a

put :: Post -> NState -> NState
put post = NState . (post :) . posts

contains :: Post -> NState -> Bool
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
