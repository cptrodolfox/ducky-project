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
import qualified Data.Map              as M
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
    go (StudentDb db) = StudentDb $
      if M.null db
      then M.singleton dni student
      else M.insert dni student db

-- Adds a employee to the employee's database, the primary key is the
-- employee dni (identification document number).
addEmployee :: Employee -> Update EmployeeDb ()
addEmployee emp@Employee{empDni = dni} = modify go
  where
    go (EmployeeDb db) = EmployeeDb $
      if M.null db
      then M.singleton dni emp
      else M.insert dni emp db

-- Adds a department to the department's database, the primaty key is the
-- department DeptId.
addDepartment :: Department -> Update DepartmentDb ()
addDepartment dep@Department{deptId = dpid} = modify go
  where
    go (DepartmentDb db) = DepartmentDb $
      if M.null db
      then M.singleton dpid dep
      else M.insert dpid dep db

-- Adds a schools to the schools database, the primary key is the
-- schools SchooId.
addSchool :: School -> Update SchoolDb ()
addSchool sch@School{schoolId = id} = modify go
  where
    go (SchoolDb db) = SchoolDb $
      if M.null db
      then M.singleton id sch
      else M.insert id sch db

-- Adds a result to the results database, increments the index.
addResult :: Result -> Update ResultDb ()
addResult res = modify go
  where
    go (ResultDb db) = ResultDb $
      if IM.null db
      then IM.singleton 1 res
      else let (index, _) = IM.findMax db in
           IM.insert (index + 1) res db

-- Adds a company to the companies database, increments the index.
addCompany :: Company -> Update CompanyDb ()
addCompany comp = modify go
  where
    go (CompanyDb db) = CompanyDb $
      if IM.null db
      then IM.singleton 1 comp
      else let index = fst $ IM.findMax db in
        IM.insert (index + 1) comp db

-- Adds a Grant to the grants database, increments the index.
addGrant :: Grant -> Update GrantDb ()
addGrant grant = modify go
  where
    go (GrantDb db) = GrantDb $
      if IM.null db
      then IM.singleton 1 grant
      else let index = fst $ IM.findMax db in
        IM.insert (index + 1) grant db

-- Adds a Project to the project database, increments the index.
addProject :: Project -> Update ProjectDb ()
addProject pro = modify go
  where
    go (ProjectDb db) = ProjectDb $
      if IM.null db
      then IM.singleton 1 pro
      else let index = fst $ IM.findMax db in
        IM.insert (index + 1) pro db

-- Adds a NGO to the ngos database, increments the index.
addNGO :: NGO -> Update NGODb ()
addNGO ngo = modify go
  where
    go (NGODb db) = NGODb $
      if IM.null db
      then IM.singleton 1 ngo
      else let index = fst $ IM.findMax db in
        IM.insert (index + 1) ngo db

-- Adds a GOD to the gods database, increments the index.
addGO :: GovernmentalOrganization -> Update GODb ()
addGO gov = modify go
  where
    go (GODb db) = GODb $
      if IM.null db
      then IM.singleton 1 gov
      else let index = fst $ IM.findMax db in
        IM.insert (index + 1) gov db

-- Search for a student by the DNI.
searchStudentDNI :: DNI -> Query StudentDb (Maybe Student)
searchStudentDNI dni = (M.lookup dni) . (\(StudentDb db) -> db) <$> ask

-- Search for a employee by the DNI.
searchEmployeeDNI :: DNI -> Query EmployeeDb (Maybe Employee)
searchEmployeeDNI dni = (M.lookup dni) . (\(EmployeeDb db) -> db) <$> ask

-- Search for a publication by Authors
searchPubAuthor :: C.ByteString -> Query ResultDb [Result]
searchPubAuthor author = (map snd) . IM.toList
  . (IM.filter (\(Publication{pubAuthors = authors})
                -> any ((==) author) authors))
  . (\(ResultDb db) -> db) <$> ask

-- Search for a publication by Keyword
searchPubKeyword :: C.ByteString -> Query ResultDb [Result]
searchPubKeyword word = (map snd) . IM.toList
  . (IM.filter (\(Publication{pubKeywords = words})
                -> any ((==) word) words))
  . (\(ResultDb db) -> db) <$> ask



--   getMail :: T.Username -> Query MailDb T.MailBox
-- getMail user = (\(Just mailbox) -> mailbox)
--   . (Map.lookup user)
--   . allmails <$> ask
