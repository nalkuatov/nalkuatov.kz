module Notifier where

import Control.Monad.Reader
import Control.Monad.State

data Env = Env String

data NState  =
  NState { posts :: [Post]
         }

newtype Post = Post { file :: FilePath }
  deriving stock (Show, Eq)

data Action = DoNothing | Notify Post

type NotifierM a = forall m. MonadIO m =>
  ReaderT Env (StateT NState m) a

put :: Post -> NState -> NState
put post = NState . (post :) . posts

contains :: Post -> NState -> Bool
contains post = (post `elem`) . posts

runNotifierM :: NotifierM a -> IO a
runNotifierM action = undefined
