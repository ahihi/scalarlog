{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}
module Lib
    ( appMain
    ) where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Foldable
import Database.Persist.Sql
import Database.Persist.Sqlite
import Data.Text (Text, pack, unpack)
import Data.Time.Clock
import Data.Time.Format
import System.Environment
import Text.Hamlet
import Yesod hiding (parseTime)
import Yesod.Static

import Lib.Persist
import Lib.Template

data ScalarLog = ScalarLog
  { getPool :: ConnectionPool
  , getStatic :: Static
  , getApiKey :: Text
  }

staticFiles "static"

mkYesod "ScalarLog" [parseRoutes|
  /         HomeR GET
  /#Text    TagR  GET POST
  !/_static StaticR Static getStatic
|]

instance Yesod ScalarLog where
  jsLoader _ = BottomOfHeadBlocking 

instance RenderMessage ScalarLog FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist ScalarLog where
  type YesodPersistBackend ScalarLog = SqlBackend
  
  runDB action = runSqlPool action . getPool =<< getYesod

layout :: Widget -> Handler Html
layout w = defaultLayout $ do
  addStylesheet $ StaticR reset_css
  toWidget $(cassiusTemplate "style")
  w

getHomeR :: Handler Html
getHomeR = do
  tags <- runDB $ selectList [] [Asc TagName]
  layout $ do
    toWidget $(hamletTemplate "home")

getTagR :: Text -> Handler Html
getTagR name = do
  Entity tagId tag <- runDB $ getBy404 $ TagNameU name
  scalars <- runDB $ selectList [ScalarTagId ==. tagId] [Asc ScalarTime]
  let scalarsJs = map (\(Entity _ s) -> (scalarTime s, scalarValue s)) scalars
  layout $ do
    addScript $ StaticR jquery_js
    addScript $ StaticR chart_core_js
    addScript $ StaticR chart_scatter_js
    toWidget $(juliusTemplate "graph") 
    toWidget $(hamletTemplate "tag")

postTagR :: Text -> Handler ()
postTagR name = do
  correctApiKey <- getApiKey <$> getYesod
  Entity tagId tag <- runDB $ getBy404 $ TagNameU name
  (apiKey, timeStr, value) <- runInputPost $
    (,,)
      <$> ireq textField "apiKey"
      <*> ireq textField "time"
      <*> ireq doubleField "value"
      
  if apiKey /= correctApiKey
    then invalidArgs ["invalid api key"]
    else case parseUTCTime timeStr of
      Nothing -> invalidArgs ["invalid time value"]
      Just time -> do
        runDB $ insert $ Scalar tagId time value
        return ()
  where
    parseUTCTime :: Text -> Maybe UTCTime
    parseUTCTime = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" . unpack

appMain :: IO ()
appMain = do
  let openConnectionCount = 10
  
  dbFile <- pack <$> getEnv "SCALARLOG_DB_FILE"
  apiKey <- pack <$> getEnv "SCALARLOG_API_KEY"
  
  runStderrLoggingT $
    withSqlitePool dbFile openConnectionCount $
      \pool -> liftIO $ do
        getStatic <- static "static"
        let site = ScalarLog {
          getPool = pool,
          getStatic = getStatic,
          getApiKey = apiKey
        }
        
        runResourceT $ flip runSqlPool pool $ do
          runMigration migrateAll
          
        warp 3000 site
