{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import qualified Yesod.Auth.Message as YAM
import qualified Yesod.Core.Unsafe as Unsafe
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Dummy
import Yesod.Auth.GoogleEmail2
import Yesod.Core.Types     (Logger)
import Yesod.Default.Util   (addStaticContentExternal)

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
    }

screenshotFile :: App -> String -> FilePath
screenshotFile master name = (unpack.uploadScreenshot.appSettings$ master) </> name

binFile :: App -> String -> FilePath
binFile master name = (unpack.uploadBin.appSettings$ master) </> name

srcFile :: App -> String -> FilePath
srcFile master name = (unpack.uploadSrc.appSettings$ master) </> name

screenshotDir :: App -> FilePath
screenshotDir master = unpack.uploadScreenshot.appSettings$ master

binDir :: App -> FilePath
binDir master = unpack.uploadBin.appSettings$ master

srcDir :: App -> FilePath
srcDir master = unpack.uploadSrc.appSettings$ master


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
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    120    -- timeout in minutes
    "config/client_session_key.aes"

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
    pc <- widgetToPageContent $ do
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  errorHandler errorResponse = (fmap toTypedContent).defaultLayout$ do
    setTitle.toHtml.show$ errorResponse
    $(widgetFile "error")
    where formatArgs = concat.intersperse "/"

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
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
  shouldLog app _source level =
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger = return . appLogger

  authRoute _ = Just $ AuthR LoginR

  isAuthorized (FaviconR) False = return Authorized
  isAuthorized (RobotsR) False = return Authorized
  isAuthorized (HomeR) False = return Authorized
  isAuthorized (FeedR) False = return Authorized
  isAuthorized (BlogOldR _) False = return Authorized
  isAuthorized (BlogR _ _ _) False = return Authorized
  isAuthorized (ProjectsR) False = return Authorized
  isAuthorized (OldProjectsR) False = return Authorized
  isAuthorized (ContactR) _ = return Authorized
  isAuthorized (AboutR) False = return Authorized
  isAuthorized (PrivacyR) False = return Authorized
  isAuthorized (UploadFileR _ _) False = return Authorized
  isAuthorized (StaticR _) False = return Authorized
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized _ _ = isAdmin

isAdmin :: Handler AuthResult
isAdmin = do
  mu <- maybeAuthId
  site <- getYesod
  let admin = adminEmail$ appSettings site

  return$ case mu of
    Nothing -> AuthenticationRequired
    Just authId ->
      if authId == admin then Authorized
      else Unauthorized "You do not have admin rights on the site"

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = Text
  getAuthId = return . Just . credsIdent
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins master = [authGoogleEmail clientId clientSecret] <> dummy
    where clientId = googleClientId$ appSettings master
          clientSecret = googleClientSecret$ appSettings master
          dummy =
            if appAuthDummyLogin$ appSettings master
            then [authDummy] else []

  authHttpManager = appHttpManager
  maybeAuthId = lookupSession "_ID"
  loginHandler = do
    ma <- lift maybeAuthId
    when (isJust ma).lift.redirect$ HomeR
    plugins <- authPlugins <$> (lift getYesod)
    rtp <- getRouteToParent

    lift$ defaultLayout$ do
      setTitle "Administrator login"
      $(widgetFile "auth")

  authenticate creds = do
    site <- getYesod
    let admin = adminEmail$ appSettings site
    let authId = credsIdent creds
    if authId == admin then do
      setSession "_ID" authId 
      return$ Authenticated authId
    else do
      return$ UserError YAM.UserName

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

