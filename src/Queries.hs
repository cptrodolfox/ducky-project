{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Queries where

import           Control.Applicative
import           Control.Monad.Reader  (ask)
import           Control.Monad.State   (modify)
import           Data.Acid
import qualified Data.ByteString.Char8 as C
import           Data.Function
import qualified Data.IntMap           as IM
import           Data.Map              as M
import           Data.SafeCopy
import           Data.SafeCopy
import           Data.Time
import           Data.Time.Calendar
import           Data.Typeable
import           Types

-- Adds a student to the student's database, the primary key is the
-- students dni (identification document number).
addStudent :: Student -> Update StudentDb ()
addStudent student@Student{stdDni = dni} = modify go
  where
    go (StudentDb db) = Student $
      if M.null db
      then Map.singleton dni student
      else Map.insert dni student db

-- Adds a employee to the employee's database, the primary key is the
-- employee dni (identification document number).
addEmployee :: Employee -> Update EmployeeDb ()
addEmployee emp@Employee{empDni = dni} = modify go
  where
    go (EmployeeDb db) = Employee $
      if M.null db
      then Map.singleton dni emp
      else Map.insert dni emp db




