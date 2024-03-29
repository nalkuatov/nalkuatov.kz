
import Control.Monad.IO.Class
import Control.Monad.State (get, modify)
import Control.Monad.Reader (asks)
import Control.Monad (when, forM)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Yaml
import Servant
import Servant.Server
import qualified Servant.Client as Servant
import System.Directory
import System.Environment
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import GHC.Generics

import Notifier
import Client

data Resp = Resp { ok :: Bool, message :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

success :: Text -> Resp
success = Resp True

type Api
  = "api"
  :> "posts"
  :> ReqBody '[JSON] Article
  :> Post '[JSON] Resp

proxy :: Proxy Api
proxy = Proxy

appendAndNotify :: ServerT Api NotifierM
appendAndNotify post@(filename -> raw) = do
  AppConfig{..} <- asks appConfig
  state <- get
  when (not $ contains post state) $ do
    liftClientM $ notify bot $ mkMessage chatId post
    modify $ add post
    pure ()
  pure $ success "fine"

main :: IO ()
main = do

  appConfig@AppConfig{..} <- decodeFileThrow "config.yaml"

  posts     <- map (\f -> Article f (pack f)) <$> listDirectory "posts"
  baseUrl   <- Servant.parseBaseUrl "https://api.telegram.org/"
  manager   <- newManager tlsManagerSettings

  let clientEnv = Servant.mkClientEnv manager baseUrl
  let env = Env{..}
  let initState = NState{..}
  let app = Servant.serve proxy $ hoistServer proxy (notifierMToHandler env initState) appendAndNotify

  putStrLn "Running server..."

  run 8558 app
