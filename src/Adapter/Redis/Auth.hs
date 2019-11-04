module Adapter.Redis.Auth where

import           ClassyPrelude
import           Data.Has
import qualified Database.Redis                as R
import qualified Domain.Auth                   as D
import           Text.StringRandom

-- | Type synonym for consistency
type State = R.Connection

type Redis r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

-- | URL-parser for Redis connection URL
withState :: String -> (State -> IO a) -> IO a
withState connUrl action = case R.parseConnectInfo connUrl of
  Left  _        -> throwString "Invalid Redis connection URL"
  Right connInfo -> R.checkedConnect connInfo >>= (\conn -> action conn)
    -- the Right expression could also be written in do-Notation but I want to
    -- get the binding operators into my head.

-- | Execute Redis under the Redis r m constraint (execute actions via "runRedis")
withConn :: Redis r m => R.Redis a -> m a
withConn action = asks getter >>= (\conn -> liftIO (R.runRedis conn action))

newSession :: Redis r m => D.UserId -> m D.SessionId
newSession userId = do
  sId    <- liftIO $ stringRandomIO "[A-Za-z0-9]{32}"
  result <- withConn $ R.set (encodeUtf8 sId) (fromString . show $ userId)
  case result of
    Right R.Ok -> return sId
    err        -> throwString $ "Unexpected redis error: " <> show err

findUserIdBySessionId :: Redis r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  result <- withConn (R.get (encodeUtf8 sId))
  return $ case result of
    Right (Just uIdStr) -> readMay . unpack . decodeUtf8 $ uIdStr
    err                 -> throwString $ "Unexpected redis error: " <> show err
