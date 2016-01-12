{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Text (Text)
import qualified Data.Double.Conversion.Text as D
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (left)

import Control.Lens
import Data.Aeson.Lens

import System.Environment (getArgs, getProgName)
import qualified Data.Configurator as Config
import Network.Wreq hiding (Proxy)
import Network.Wai.Handler.Warp (run)
import Servant


type API
  = "weather"
    :> QueryParam "lon" Double
    :> QueryParam "lat" Double
    :> Get '[PlainText] Text


openWeatherAPI :: String
openWeatherAPI = "http://api.openweathermap.org/data/2.5/weather"


main :: IO ()
main = do
  progName <- getProgName
  getArgs >>= \case
    [configFile] -> do
      conf <- Config.load [Config.Required configFile]
      apiKey     <- Config.require conf "api.key"
      serverPort <- Config.require conf "server.port"
      -- statsdPort <- Config.require conf "statsd.port"

      run serverPort
        $ serve (Proxy :: Proxy API)
        $ curry $ \case
          (Just lon, Just lat) -> do
            let opts = defaults
                  & (param "units" .~ ["metric"])
                  & (param "lon"   .~ [D.toShortest lon])
                  & (param "lat"   .~ [D.toShortest lat])
                  & (param "APPID" .~ [apiKey])
            r <- liftIO $ getWith opts openWeatherAPI
            let Just temp = r ^? responseBody . key "main" . key "temp" . _Double
            return $ D.toShortest temp

          _ -> left $ err401 {errBody = "Invalid lon/lat"}

    _ -> error $ "Usage: " ++ progName ++ " <config>"

-- getWeather apiKey lon lat
