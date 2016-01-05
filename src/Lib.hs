{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}
module Lib
    ( appMain
    ) where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Foldable
import Database.Persist.Sql
import Database.Persist.Sqlite
import Data.Text (Text)
import Data.Time.Clock
import Text.Hamlet
import Yesod
import Yesod.Static

import Lib.Persist
import Lib.Template

data ScalarLog = ScalarLog
  { getPool :: ConnectionPool
  , getStatic :: Static
  }

staticFiles "static"

mkYesod "ScalarLog" [parseRoutes|
/         HomeR GET
/#Text    TagR  GET
!/_static StaticR Static getStatic
|]

instance Yesod ScalarLog where
  jsLoader _ = BottomOfHeadBlocking 

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
  -- toWidget [lucius| .#{contactClass} { color: #F0F; } |]

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

appMain :: IO ()
appMain = do
  let openConnectionCount = 10
  runStderrLoggingT $
    withSqlitePool "test.sqlite" openConnectionCount $
      \pool -> liftIO $ do
        getStatic <- static "static"
        let site = ScalarLog { getPool = pool
          , getStatic = getStatic
          }
        
        runResourceT $ flip runSqlPool pool $ do
          runMigration migrateAll
          insertExampleData
          
        warp 3000 site
  where    
    insertExampleData = do
      -- insert $ Tag "temperature" "°C"
      -- insert $ Tag "monnos" "°3°"
    
      {-
      let increment (time, x) = (fromRational 3600 `addUTCTime` time, x + 1)
      let temperatureData = take 20 $ iterate increment (read "2016-01-01 00:00:00", 0)
      mapM_ (insertScalar "temperature") temperatureData
      -}
      return ()
      where
        insertScalar tagName (time, x) = do
          let scalarWithEntity e = Scalar (entityKey e)
          entityMay <- getBy $ TagNameU tagName
          mapM_ (\e -> insert $ scalarWithEntity e time x) entityMay