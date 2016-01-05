{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Lib
    ( appMain
    ) where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Text.Hamlet
import Yesod

import Lib.Persist
import Lib.Template

data ScalarLog = ScalarLog ConnectionPool

mkYesod "ScalarLog" [parseRoutes|
/ HomeR GET
|]

instance Yesod ScalarLog

instance YesodPersist ScalarLog where
  type YesodPersistBackend ScalarLog = SqlBackend
  
  runDB action = do
    ScalarLog pool <- getYesod
    runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
  tags <- runDB $ selectList [] [Asc TagName]
  defaultLayout $ do
    toWidget $(hamletTemplate "home")
  -- toWidget [lucius| .#{contactClass} { color: #F0F; } |]

appMain :: IO ()
appMain = runStderrLoggingT $ withSqlitePool "test.sqlite" openConnectionCount $ \pool -> liftIO $ do
  runResourceT $ flip runSqlPool pool $ do
    runMigration migrateAll
    insert $ Tag "temperature" "°C"
  warp 3000 $ ScalarLog pool
  where
    openConnectionCount = 10