module Client
  ( runClientM
  , liftClientM
  , notify
  ) where


import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Text
import Data.Aeson (Value)
import Servant
import Servant.Client (ClientM, ClientEnv)
import qualified Servant.Client as Servant

import Notifier

type Api
  = Capture "bot" Text
  :> "sendMessage"
  :> QueryParam' '[Required] "chat_id" Text
  :> QueryParam' '[Required] "text"    Text
  :> Get '[JSON] Value

proxy :: Proxy Api
proxy = Proxy

notify :: Text -> Text -> Text -> ClientM Value
notify = Servant.client proxy

liftClientM :: ClientM a -> NotifierM a
liftClientM = lift . lift

runClientM :: ClientM a -> NotifierM a
runClientM action = do
  env <- asks clientEnv
  res <- liftIO $ Servant.runClientM action env
  case res of
    Left e  -> throwM e
    Right v -> pure v
