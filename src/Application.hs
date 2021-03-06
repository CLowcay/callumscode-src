{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( getApplicationDev,
    appMain,
    develMain,
    makeFoundation,
    makeLogWare,
  )
where

import Control.Lens
import Control.Monad.Logger (defaultLoc, liftLoc, runLoggingT)
import Database.Persist.Sqlite (createSqlitePool, runSqlPool, sqlDatabase, sqlPoolSize)
import Handler.Blog
import Handler.Common
import Handler.Contact
import Handler.File
import Handler.Home
import Handler.Projects
import Import
import Language.Haskell.TH.Syntax (qLocation)
import qualified Network.AWS as AWS
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, defaultShouldDisplayException, runSettings, setHost, setOnException, setPort)
import qualified Network.Wai.Middleware.Gzip as Gzip
import Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource (..), OutputFormat (..), destination, mkRequestLogger, outputFormat)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  -- Some basic initializations: HTTP connection manager, logger, and static
  -- subsite.
  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
      (appStaticDir appSettings)

  awsEnv <- AWS.newEnv AWS.Discover

  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let mkFoundation appConnPool = App {..}
      -- The App {..} syntax is an example of record wild cards. For more
      -- information, see:
      -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger
      awsLogger logLevel builder =
        logFunc
          defaultLoc
          "AWS"
          (awsLogLevelToYesodLogLevel logLevel)
          (toLogStr builder)
      appAWS =
        awsEnv & AWS.envManager .~ appHttpManager
          & AWS.envLogger .~ awsLogger

  -- Create the database connection pool
  pool <-
    flip runLoggingT logFunc $
      createSqlitePool
        ( pack $
            (unpack . appDatabaseRoot $ appSettings)
              </> (unpack . sqlDatabase . appDatabaseConf $ appSettings)
        )
        (sqlPoolSize $ appDatabaseConf appSettings)

  -- Perform database migration using our application's logging settings.
  runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

  -- Return the foundation
  pure $ mkFoundation pool
  where
    awsLogLevelToYesodLogLevel awsLogLevel = case awsLogLevel of
      AWS.Info -> LevelInfo
      AWS.Error -> LevelError
      AWS.Debug -> LevelDebug
      AWS.Trace -> LevelDebug

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  -- Create the WAI application and apply middlewares
  let gzip = Gzip.gzip Gzip.def {Gzip.gzipFiles = Gzip.GzipCompress}
  appPlain <- toWaiAppPlain foundation
  pure . gzip . logWare . defaultMiddlewaresNoLogging $ appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
      { outputFormat =
          if appDetailedRequestLogging $ appSettings foundation
            then Detailed True
            else
              Apache
                ( if appIpFromHeader $ appSettings foundation
                    then FromFallback
                    else FromSocket
                ),
        destination = Logger $ loggerSet $ appLogger foundation
      }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation) $
    setHost (appHost $ appSettings foundation) $
      setOnException
        ( \_req e ->
            when (defaultShouldDisplayException e) $
              messageLoggerSource
                foundation
                (appLogger foundation)
                $(qLocation >>= liftLoc)
                "yesod"
                LevelError
                (toLogStr $ "Exception from Warp: " ++ show e)
        )
        defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  pure (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  -- Get the settings from all relevant sources
  settings <-
    loadYamlSettingsArgs
      -- fall back to compile-time values, set to [] to require values at runtime
      [configSettingsYmlValue]
      -- allow environment variables to override
      useEnv

  -- Generate the foundation from the settings
  foundation <- makeFoundation settings

  -- Generate a WAI Application from the foundation
  app <- makeApplication foundation

  -- Run the application with Warp
  runSettings (warpSettings foundation) app
