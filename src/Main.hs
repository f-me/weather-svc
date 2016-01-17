{-# LANGUAGE DeriveGeneric #-}

module Main where


import Control.Monad.IO.Class
import Control.Monad.Trans.Either (left)
import Control.Lens

import Data.Aeson.Lens
import Data.Text (Text)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Double.Conversion.Text as D
import Data.Time

import System.Environment (getArgs, getProgName)
import qualified Data.Configurator as Config
import Network.Wreq hiding (Proxy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp (run)
import Servant


type API
  = "weather"
    :> QueryParam "lon" Double
    :> QueryParam "lat" Double
    :> Get '[PlainText] Text


openWeatherAPI :: String
openWeatherAPI = "http://api.openweathermap.org/data/2.5/weather"


getWeather :: Text -> Text -> Text -> IO (Maybe Double)
getWeather apiKey lonT latT = do
  let opts = defaults
        & (param "units" .~ ["metric"])
        & (param "lon"   .~ [lonT])
        & (param "lat"   .~ [latT])
        & (param "APPID" .~ [apiKey])
  r <- getWith opts openWeatherAPI
  return $ r ^? responseBody . key "main" . key "temp" . _Double


main :: IO ()
main = do
  progName <- getProgName
  getArgs >>= \case
    [configFile] -> do
      conf <- Config.load [Config.Required configFile]
      apiKey      <- Config.require conf "api.key"
      serverPort  <- Config.require conf "server.port"
      cachePeriod <- (*60) <$> Config.require conf "cache.minutes"
      -- statsdPort <- Config.require conf "statsd.port"

      cache <- newCache

      run serverPort
        $ logStdoutDev
        $ serve (Proxy :: Proxy API)
        $ curry $ \case
          (Just lon, Just lat) -> do
            let lonT = D.toShortest lon
            let latT = D.toShortest lat

            res <- cacheLookup cache (fromInteger cachePeriod) (lonT, latT)
              (\_ -> do
                Just temp <- getWeather apiKey lonT latT
                return $ D.toShortest temp
              )
            case res of
              Left temp -> return temp -- report cache miss
              Right temp -> return temp

          _ -> left $ err401 {errBody = "Invalid lon/lat"}

    _ -> error $ "Usage: " ++ progName ++ " <config>"



type Cache key val = IORef (Map key (UTCTime, val))

newCache :: MonadIO m => m (Cache k v)
newCache = liftIO $ newIORef Map.empty

cacheLookup
  :: (MonadIO m, Ord key)
  => Cache key val -> NominalDiffTime
  -> key -> (key -> IO val)
  -> m (Either val val)
cacheLookup cache ttl cacheKey getVal = liftIO $ do
  curTime <- getCurrentTime
  cacheMap <- readIORef cache
  case Map.lookup cacheKey cacheMap of
    Just (cacheTime, val)
      | diffUTCTime curTime cacheTime < ttl -> return $ Right val
    _ -> do
      val <- getVal cacheKey
      atomicModifyIORef' cache
        $ (,()) . Map.insert cacheKey (curTime, val)
      return $ Left val
