module Logging where
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           MyPrelude
import           System.IO

class Monad m => MonadLogger m where
  putLogLn :: String -> m ()

instance MonadLogger IO where
  putLogLn = hPutStrLn stderr

instance MonadLogger m => MonadLogger (ReaderT a m) where
  putLogLn = lift . putLogLn

instance MonadLogger m => MonadLogger (StateT s m) where
  putLogLn = lift . putLogLn

instance MonadLogger m => MonadLogger (ExceptT e m) where
  putLogLn = lift . putLogLn
