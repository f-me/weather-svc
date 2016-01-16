{-# LANGUAGE DeriveGeneric #-}

module Main where


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (left)
import Control.Lens

import Data.Aeson.Lens
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Double.Conversion.Text as D
import Data.Time (getCurrentTime, diffUTCTime)

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
      cachePeriod <- (*60) <$> Config.require conf "cache.minutes" :: IO Int
      -- statsdPort <- Config.require conf "statsd.port"

      cache <- newIORef Map.empty

      run serverPort
        $ logStdoutDev
        $ serve (Proxy :: Proxy API)
        $ curry $ \case
          (Just lon, Just lat) -> do
            let lonT = D.toShortest lon
            let latT = D.toShortest lat
            let cacheKey = lonT <> ":" <> latT

            curTime <- liftIO getCurrentTime

            cacheMap <- liftIO (readIORef cache)
            case Map.lookup cacheKey cacheMap of
              Just (time, temp)
                | round (diffUTCTime curTime time) < cachePeriod
                  -> return temp

              _ -> liftIO $ do
                Just temp <- getWeather apiKey lonT latT
                let tempT = D.toShortest temp
                atomicModifyIORef' cache
                  $ (,()) . Map.insert cacheKey (curTime, tempT)
                return tempT

          _ -> left $ err401 {errBody = "Invalid lon/lat"}

    _ -> error $ "Usage: " ++ progName ++ " <config>"
