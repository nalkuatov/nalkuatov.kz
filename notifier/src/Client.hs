module Client
  ( runClientM
  , liftClientM
  , notify
  , fetch
  , mkMessage
  ) where


import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text as Text
import Data.Aeson (Value, ToJSON)
import Servant
import Servant.Client (ClientM, ClientEnv)
import qualified Servant.Client as Servant
import GHC.Generics

import Notifier

data Message =
  Message { chat_id :: Text
          , text :: Text
          , parse_mode :: Text
          }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

type ChatId = Text

mkMessage :: ChatId -> Article -> Message
mkMessage chat_id Article{..} =
  let text = Text.unlines [ "<b>На сайте новый пост!</b>"
                          , "<i>" <> title <> "</i>"
                          , "<a href=\"https://nalkuatov.kz/posts.html\">Перейти &gt&gt&gt</a>"
                          ]
  in Message { parse_mode = "HTML"
             , ..
             }

type Api
  = Capture "bot" Text
  :> "sendMessage"
  :> ReqBody '[JSON] Message
  :> Post '[JSON] Value

  :<|> Capture "bot" Text
  :> "getMessage"
  :> QueryParam' '[Required] "chat_id" Text
  :> Get '[JSON] Value

proxy :: Proxy Api
proxy = Proxy

notify :: Text -> Message -> ClientM Value
fetch :: Text -> Text -> ClientM Value
notify :<|> fetch = Servant.client proxy

liftClientM :: ClientM a -> NotifierM a
liftClientM = lift . lift

runClientM :: ClientM a -> NotifierM a
runClientM action = do
  env <- asks clientEnv
  res <- liftIO $ Servant.runClientM action env
  case res of
    Left e  -> throwM e
    Right v -> pure v
