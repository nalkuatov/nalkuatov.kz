module Client
  ( runClientM
  , notify
  ) where


import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Text
import Servant
import Servant.Client (ClientM, ClientEnv)
import qualified Servant.Client as Servant

import Notifier

type Api = "api" :> "something" :> Get '[JSON] Text

proxy :: Proxy Api
proxy = Proxy

notify :: ClientM Text
notify = Servant.client proxy

runClientM :: ClientM a -> NotifierM a
runClientM action = do
  env <- asks clientEnv
  res <- liftIO $ Servant.runClientM action env
  case res of
    Left e  -> throwM e
    Right v -> pure v
