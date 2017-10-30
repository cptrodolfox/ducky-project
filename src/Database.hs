-------------------------------------------------------------------
-- Module : Database
-- Authors : William R. Arellano, Paul Silva.
-- Description : This module contains all the functions that handle
-- the transactions with acid-state database.
-- Date : 27 - 10 - 2017
-- Version: 0.1
-------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Database where

import           Control.Applicative
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (modify)
import           Data.Acid
import           Data.ByteString      (ByteString)
import qualified Data.List            as L
import           Data.Map             (Map, insert, null, singleton, (!))
import           Prelude              hiding (id, null)
import           Types

-------------------------------------------------------------------
----------------------Helper functions-----------------------------
-------------------------------------------------------------------
-- | Updates the students database inside the university, the update is given by
-- | a function.
updateStudents :: University -> (Students -> Students) -> University
updateStudents uni@University{students = sts} f = uni {students = f sts}

-- | Updates the employees database inside the university, the update is given
-- | by a function.
updateEmployees :: University -> (Employees -> Employees) -> University
updateEmployees uni@University{employees = emps} f = uni {employees = f emps}

-- | Updates the schools database inside the university, the update is given by
-- | a function.
updateSchools :: University -> (Schools -> Schools) -> University
updateSchools uni@University{schools = schcs} f = uni {schools = f schcs}

-- | Updates the projects database inside the university, the update is given by
-- | a function
updateProjects :: University -> (Projects -> Projects) -> University
updateProjects uni@University{projects = pros} f = uni {projects = f pros}

-- | Updates the assignations database inside the university, the update is
-- | given by a function.
updateAssignations :: University -> (Assignations -> Assignations) -> University
updateAssignations uni@University{assignations = assigns} f =
  uni {assignations = f assigns}

-- | Updates the funders database of the university, the update is given by a
-- | function.
updateFunders :: University -> (Funders -> Funders) -> University
updateFunders uni@University{funders = fs} f = uni {funders = f fs}

-- | Updates the departments database inside a school.
updateDepts' :: School -> (Departments -> Departments) -> School
updateDepts' sch@School{departments = dps} f = sch {departments = f dps}

-- | Updates the departments database inside a school inside the university, the
-- | update is given by a function. The school is assumed to exist inside the
-- | university.
updateDepts :: SchoolId -> University -> (Departments -> Departments)
  -> University
updateDepts sId uni@University{schools = schs} f = uni {schools = g schs}
  where
    g = (\s -> insert sId (updateDepts' (s ! sId) f) s)

-------------------------------------------------------------------
----------------------Database additions---------------------------
-------------------------------------------------------------------

-- | Adds a student to the university, the primary key is the id number.
addStudent :: Student -> Update University ()
addStudent std@(Student person) = modify go
  where
    go uni = updateStudents uni (\stds ->
                                   if null stds
                                   then singleton (id person) std
                                   else insert (id person) std stds)

-- | Adds an employee to the university, the primary key is the id number.
addEmployee :: Employee -> Update University ()
addEmployee emp@(Employee person _) = modify go
  where
    go uni = updateEmployees uni (\emps ->
                                   if null emps
                                   then singleton (id person) emp
                                   else insert (id person)  emp emps)

-- | Adds a school to the university, the primary key is the schoolId.
addSchool :: SchoolId -> School -> Update University ()
addSchool schId sch = modify go
  where
    go uni = updateSchools uni (\schs ->
                                  if null schs
                                  then singleton schId sch
                                  else insert schId sch schs)

-- | Adds a project to the university, the primary key the projectId.
addProject :: ProjectId -> Project -> Update University ()
addProject proId pro = modify go
  where
    go uni = updateProjects uni (\pros ->
                                   if null pros
                                   then singleton proId pro
                                   else insert proId pro pros)

-- | Adds an assignation to the university.
addAssignation :: Assignation -> Update University ()
addAssignation assign = modify go
  where
    go uni = updateAssignations uni (\assigns ->
                                       if L.null assigns
                                       then [assign]
                                       else assign : assigns)

-- | Adds a funding institution to the university.
addFunder :: FunderId -> Funder -> Update University ()
addFunder fundId fund = modify go
  where
    go uni = updateFunders uni (\funds ->
                                  if null funds
                                  then singleton fundId fund
                                  else insert fundId fund funds)

-- | Adds a department to a school inside the university, a check for
-- | the consistency of the parameters should be done before hand.
addDepartment :: DeptId -> Department -> SchoolId -> Update University ()
addDepartment deptId dept schId = modify go
  where
    go uni = updateDepts schId uni (\dps -> insert deptId dept dps)
-------------------------------------------------------------------
----------------------Database queries  ---------------------------
-------------------------------------------------------------------
-- | Retrieves the students map from the university.
getStudents :: Query University Students
getStudents = students <$> ask

-- | Retrieves the employees map from the university.
getEmployees :: Query University Employees
getEmployees = employees <$> ask

-- | Retrieves the schools map from the university.
getSchools :: Query University Schools
getSchools = schools <$> ask

-- | Retrieves the projects map from the university.
getProjects :: Query University Projects
getProjects = projects <$> ask

-- | Retrieves the assignations from the university.
getAssignations :: Query University Assignations
getAssignations = assignations <$> ask

-- | Retrieves the funding organizations from the university.
getFunders :: Query University Funders
getFunders = funders <$> ask

-------------------------------------------------------------------
------------------- Making the transactions acidic-----------------
-------------------------------------------------------------------
makeAcidic ''University ['addStudent
                        , 'addEmployee
                        , 'addSchool
                        , 'addProject
                        , 'addAssignation
                        , 'addFunder
                        , 'getStudents
                        , 'getEmployees
                        , 'getSchools
                        , 'getProjects
                        , 'getAssignations
                        , 'getFunders
                        ]
