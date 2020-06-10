{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Projects
  ( getProjectEditR
  , postProjectEditR
  , postProjectLivenessR
  , getProjectNewR
  , postProjectNewR
  , getProjectsR
  , getOldProjectsR
  )
where

import           Common
import           Import
import           System.Directory

data Categorized a = Categorized {
  catHaskell :: [a],
  catXBlite ::[a],
  catLibertyBasic ::[a]
} deriving (Show)

emptyCats :: Categorized a
emptyCats = Categorized [] [] []

categorize :: [Entity Software] -> Categorized Software
categorize = foldr
  (\(Entity _ v@Software { softwareCat = cat }) cats -> case cat of
    Haskell      -> cats { catHaskell = v : catHaskell cats }
    XBlite       -> cats { catXBlite = v : catXBlite cats }
    LibertyBasic -> cats { catLibertyBasic = v : catLibertyBasic cats }
  )
  emptyCats

newURL :: Text
newURL = "new"

dirField :: MonadIO m => FilePath -> m (Field Handler [Element String])
dirField = fmap (selectFieldList . fmap option) . liftIO . listDirectory
  where option x = (pack x :: Text, x)

-- The editing form
softwareForm :: Maybe Software -> Form Software
softwareForm mSoft html = do
  app             <- getYesod
  linkField       <- lift . dirField $ binDir app
  linkSrcField    <- lift . dirField $ srcDir app
  screenshotField <- lift . dirField $ screenshotDir app

  let
    form =
      renderDivs
        $   Software
        <$> pure (maybe newURL softwareUrl mSoft)
        <*> areq checkBoxField "Live" (softwareLive <$> mSoft)
        <*> areq textField "Name" (softwareName <$> mSoft)
        <*> areq textField "Version" (softwareVersion <$> mSoft)
        <*> areq (selectFieldList categories) "Category" (softwareCat <$> mSoft)
        <*> lift (liftIO getCurrentTime)
        <*> areq linkField linkFieldSettings (softwareLink <$> mSoft)
        <*> aopt linkSrcField linkSrcFieldSettings (softwareLinkSrc <$> mSoft)
        <*> aopt screenshotField
                 screenshotFieldSettings
                 (softwareScreenshot <$> mSoft)
        <*> (Markdown <$> areq textField
                               "Description"
                               (unMarkdown . softwareDescription <$> mSoft)
            )

  form html
 where
  linkFieldSettings =
    FieldSettings "Link" Nothing (Just "bin-field") Nothing []
  linkSrcFieldSettings =
    FieldSettings "Source link" Nothing (Just "src-field") Nothing []
  screenshotFieldSettings =
    FieldSettings "Screenshot" Nothing (Just "screenshot-field") Nothing []

  categories =
    [ ("Haskell" :: Text, Haskell)
    , ("XBlite"         , XBlite)
    , ("Liberty Basic"  , LibertyBasic)
    ]

projectEditWidget :: Widget -> Enctype -> Maybe Software -> Route App -> Widget
projectEditWidget widget enctype item action = do
  app             <- getYesod
  binFiles        <- liftIO . listDirectory $ binDir app
  srcFiles        <- liftIO . listDirectory $ srcDir app
  screenshotFiles <- liftIO . listDirectory $ screenshotDir app

  setTitle
    ("Callum's Code - " ++ maybe "New project" (toHtml . softwareName) item)
  $(widgetFile "projectEdit")

-- Editor pages
getProjectEditR :: Text -> Handler Html
getProjectEditR url = do
  Entity _ item     <- runDB $ getBy404 (UniqueSoftware url)
  (widget, enctype) <- generateFormPost (softwareForm (Just item))
  defaultLayout
    (projectEditWidget widget enctype (Just item) (ProjectEditR url))

postProjectEditR :: Text -> Handler Html
postProjectEditR url = do
  Entity itemId item          <- runDB $ getBy404 (UniqueSoftware url)
  ((result, widget), enctype) <- runFormPost (softwareForm Nothing)
  case result of
    FormSuccess r -> do
      now <- liftIO getCurrentTime
      runDB $ update
        itemId
        [ SoftwareLive =. softwareLive r
        , SoftwareName =. softwareName r
        , SoftwareVersion =. softwareVersion r
        , SoftwareCat =. softwareCat r
        , SoftwareLink =. softwareLink r
        , SoftwareLinkSrc =. softwareLinkSrc r
        , SoftwareScreenshot =. softwareScreenshot r
        , SoftwareDescription =. softwareDescription r
        , SoftwareLastUpdate =. now
        ]
      redirect (ProjectsR :#: url)

    _ -> defaultLayout
      (projectEditWidget widget enctype (Just item) (ProjectEditR url))

postProjectLivenessR :: Text -> Handler Value
postProjectLivenessR url = do
  live            <- runInputPost $ ireq checkBoxField "live"
  Entity itemId _ <- runDB $ getBy404 (UniqueSoftware url)
  runDB $ update itemId [SoftwareLive =. live]
  pure (object ["live" .= live])

-- New page
getProjectNewR :: Handler Html
getProjectNewR = do
  (widget, enctype) <- generateFormPost (softwareForm Nothing)
  defaultLayout (projectEditWidget widget enctype Nothing ProjectNewR)

postProjectNewR :: Handler Html
postProjectNewR = do
  ((result, widget), enctype) <- runFormPost (softwareForm Nothing)
  case result of
    FormSuccess r -> do
      let url0 = mkURL (softwareName r)
      mr <- runDB $ selectFirst [SoftwareUrl ==. url0] [Desc SoftwareUrl]
      let url = case mr of
            Nothing              -> url0
            Just (Entity _ item) -> adjustURL (softwareUrl item) url0

      _ <- runDB $ insert (r { softwareUrl = url, softwareLive = False })
      redirect (ProjectsR :#: url)

    _ -> defaultLayout (projectEditWidget widget enctype Nothing ProjectNewR)

-- Regular project pages
getProjectsR :: Handler Html
getProjectsR = do
  auth     <- isAdmin <&> (== Authorized)
  projects <- categorize
    <$> runDB (selectList (filterProjects auth) [Desc SoftwareLastUpdate])

  defaultLayout $ do
    when auth $ addScript (StaticR js_liveness_js)

    setTitle "Callum's Code - projects"
    $(widgetFile "projects")
 where
  filterProjects True  = []
  filterProjects False = [SoftwareLive ==. True]
  haskellDescription = "These are some of the apps I've made in Haskell."
  xbliteDescription
    = "XBlite is a structured and compiled dialect of BASIC which works quite well for Win32 development.  I haven't used it in many years, and I'm not actively maintaining any of these programs."
  lbDescription
    = "Liberty BASIC is an interpreted BASIC which had a strong community some years ago.  I used it to get into Windows programming (previously I'd been using QBasic under DOS).  These programs are very old now and I'm not even sure they'd still run under the latest versions of Liberty BASIC."
  projectSummary auth item = $(widgetFile "projectSummary")

  categoryProjects :: Bool -> Text -> Text -> [Software] -> Widget
  categoryProjects auth catName catDescription catData =
    $(widgetFile "projectCat")

getOldProjectsR :: Handler Html
getOldProjectsR = redirect ProjectsR
