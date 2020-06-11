{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import           Control.Lens
import           Database.Persist.Sql           ( ConnectionPool
                                                , runSqlPool
                                                )
import           Import.NoFoundation
import           Text.Hamlet                    ( hamletFile )
import           Text.Jasmine                   ( minifym )
import           Yesod.Auth.Dummy
import           Yesod.Auth.OAuth2.Google
import           Yesod.ReCaptcha2
import           Yesod.Core.Types               ( Logger )
import           Yesod.Default.Util             ( addStaticContentExternal )
import qualified Network.AWS                   as AWS
import qualified Yesod.Core.Unsafe             as Unsafe

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appAWS         :: AWS.Env
    }

screenshotDir :: App -> FilePath
screenshotDir master =
  (unpack . uploadDir . appSettings $ master) </> "screenshots"

screenshotFile :: App -> String -> FilePath
screenshotFile master name = screenshotDir master </> name

binDir :: App -> FilePath
binDir master = (unpack . uploadDir . appSettings $ master) </> "bin"

binFile :: App -> String -> FilePath
binFile master name = binDir master </> name

srcDir :: App -> FilePath
srcDir master = (unpack . uploadDir . appSettings $ master) </> "src"

srcFile :: App -> String -> FilePath
srcFile master name = srcDir master </> name

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

instance AWS.HasEnv App where
  environment = lens appAWS (\x a -> x { appAWS = a })

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot = ApprootRequest $ \app req -> fromMaybe
    (getApprootText guessApproot app req)
    (appRoot $ appSettings app)

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend _ = laxSameSiteSessions $ Just <$> envClientSessionBackend
    120    -- timeout in minutes
    "CLIENT_SESSION_KEY"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware = defaultYesodMiddleware

  defaultLayout widget = do
    --mmsg <- getMessage

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    mauthId <- maybeAuthId
    pc      <- widgetToPageContent $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  errorHandler errorResponse = fmap toTypedContent . defaultLayout $ do
    setTitle $ case errorResponse of
      NotFound           -> "Not found"
      InternalError _    -> "Internal error"
      InvalidArgs   _    -> "Invalid arguments"
      NotAuthenticated   -> "Not authenticated"
      PermissionDenied _ -> "Permission denied"
      BadMethod        _ -> "Bad method"
    $logError (pack (show errorResponse))
    $(widgetFile "error")
    where formatArgs = intercalate "/"

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal minifym
                             genFileName
                             staticDir
                             (StaticR . flip StaticRoute [])
                             ext
                             mime
                             content
    where
      -- Generate a unique filename based on the content itself
          genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO app _source level =
    pure
      $  appShouldLogAll (appSettings app)
      || (level == LevelWarn)
      || (level == LevelError)

  makeLogger = pure . appLogger

  authRoute _ = Just $ AuthR LoginR

  isAuthorized FaviconR          False = pure Authorized
  isAuthorized RobotsR           False = pure Authorized
  isAuthorized HomeR             False = pure Authorized
  isAuthorized FeedR             False = pure Authorized
  isAuthorized (BlogOldR _)      False = pure Authorized
  isAuthorized BlogHomeR         False = pure Authorized
  isAuthorized BlogR{}           False = pure Authorized
  isAuthorized ProjectsR         False = pure Authorized
  isAuthorized OldProjectsR      False = pure Authorized
  isAuthorized ContactR          _     = pure Authorized
  isAuthorized AboutR            False = pure Authorized
  isAuthorized PrivacyR          False = pure Authorized
  isAuthorized HealthCheckR      False = pure Authorized
  isAuthorized (UploadFileR _ _) False = pure Authorized
  isAuthorized (StaticR _      ) False = pure Authorized
  isAuthorized (AuthR   _      ) _     = pure Authorized
  isAuthorized _                 _     = isAdmin

isAdmin :: Handler AuthResult
isAdmin = do
  mu   <- maybeAuthId
  site <- getYesod
  let admin = adminGoogleId $ appSettings site

  pure $ case mu of
    Nothing     -> AuthenticationRequired
    Just authId -> if authId == admin
      then Authorized
      else Unauthorized "You do not have admin rights on this site"

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodReCaptcha App where
  reCaptchaSiteKey   = recaptchaSiteKey . appSettings <$> getYesod
  reCaptchaSecretKey = recaptchaSecretKey . appSettings <$> getYesod
  reCaptchaLanguage  = pure Nothing

instance YesodAuth App where
  type AuthId App = Text
  getAuthId = pure . Just . credsIdent
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins master =
    [oauth2GoogleScoped ["email", "profile"] clientId clientSecret] <> dummy
   where
    clientId     = googleClientId $ appSettings master
    clientSecret = googleClientSecret $ appSettings master
    dummy        = [ authDummy | appAuthDummyLogin $ appSettings master ]

  authHttpManager = appHttpManager <$> getYesod
  maybeAuthId     = lookupSession "_ID"
  loginHandler    = do
    ma <- maybeAuthId
    when (isJust ma) . redirect $ HomeR
    plugins <- authPlugins <$> getYesod
    rtp     <- getRouteToParent

    authLayout $ do
      setTitle "Administrator login"
      $(widgetFile "auth")

  authenticate creds = do
    let authId = credsIdent creds
    setSession "_ID" authId
    pure $ Authenticated authId

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
