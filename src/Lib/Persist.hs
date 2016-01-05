{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Lib.Persist where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Tag
  name Text
  unit Text
  TagNameU name
  deriving Show
Scalar
  tagId TagId
  time UTCTime
  value Double
  deriving Show
|]
