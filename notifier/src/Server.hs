
import Control.Monad.State (get, put)
import Control.Monad (when)
import Data.Text
import Servant
import Servant.Server

import Notifier
import Client

data Resp = Resp { ok :: Bool, message :: Text }
  deriving stock (Show, Eq)

success :: Text -> Resp
success = Resp True

type Api
  = "api"
  :> "posts"
  :> ReqBody '[JSON] Name
  :> Post '[JSON] Resp

appendAndNotify :: ServerT Api NotifierM
appendAndNotify post = do
  state <- get
  when (not $ contains post state) $
    liftClientM $ notify post *> pure ()
  put  $ add post state
  pure $ success "fine"

main :: IO ()
main = putStrLn "hello world"
