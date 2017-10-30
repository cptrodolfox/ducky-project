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
import           Control.Monad.State  (modify, state)
import           Data.Acid
import           Data.ByteString      (ByteString)
import qualified Data.List            as L
import           Data.Map             (Map, insert, lookup, null, singleton,
                                       (!))
import           Prelude              hiding (id, lookup, null, pi)
import           Types

-------------------------------------------------------------------
----------------------Helper functions-----------------------------
-------------------------------------------------------------------
-- | Updates the students database inside the university, the update is given by
-- a function.
updateStudents :: University -> (Students -> Students) -> University
updateStudents uni@University{students = sts} f = uni {students = f sts}

-- | Updates the employees database inside the university, the update is given
-- by a function.
updateEmployees :: University -> (Employees -> Employees) -> University
updateEmployees uni@University{employees = emps} f = uni {employees = f emps}

-- | Updates the schools database inside the university, the update is given by
-- a function.
updateSchools :: University -> (Schools -> Schools) -> University
updateSchools uni@University{schools = schcs} f = uni {schools = f schcs}

-- | Updates the projects database inside the university, the update is given by
-- a function
updateProjects :: University -> (Projects -> Projects) -> University
updateProjects uni@University{projects = pros} f = uni {projects = f pros}

-- | Updates the assignations database inside the university, the update is
-- given by a function.
updateAssignations :: University -> (Assignations -> Assignations) -> University
updateAssignations uni@University{assignations = assigns} f =
  uni {assignations = f assigns}

-- | Updates the funders database of the university, the update is given by a
-- function.
updateFunders :: University -> (Funders -> Funders) -> University
updateFunders uni@University{funders = fs} f = uni {funders = f fs}

-- | Updates the departments database inside a school.
updateDepts' :: School -> (Departments -> Departments) -> School
updateDepts' sch@School{departments = dps} f = sch {departments = f dps}

-- | Updates the departments database inside a school inside the university, the
-- update is given by a function. The school is assumed to exist inside the
-- university.
updateDepts :: SchoolId -> University -> (Departments -> Departments)
  -> University
updateDepts sId uni@University{schools = schs} f = uni {schools = g schs}
  where
    g = (\s -> insert sId (updateDepts' (s ! sId) f) s)

-- | Updates the person's pi value.
updatePI :: Bool -> Person -> Person
updatePI v person@Person{pi = p} = person {pi = v}

-- | Find Student inside University.
findStudent :: Id -> University -> Maybe Student
findStudent idn University{students = sts} = lookup idn sts

-- | Find Employee inside University.
findEmployee :: Id -> University -> Maybe Employee
findEmployee idn University{employees = emps} = lookup idn emps
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
-- the consistency of the parameters should be done before hand.
addDepartment :: DeptId -> Department -> SchoolId -> Update University ()
addDepartment deptId dept schId = modify go
  where
    go uni = updateDepts schId uni (\dps -> insert deptId dept dps)

-- | Adds a pledge to a grant inside the university, returns the updated Grant
-- if the Grant does not exist returns Nothing
addPledge :: GrantId -> Pledge -> Update University (Maybe Grant)
addPledge gId pleg = state go
  where
    go uni = (\x -> (x, uni)) . fmap (\g@Grant{grant_pledges = ps}
                           -> g {grant_pledges = pleg : ps})
             . lookup gId $ grants uni

-- | Adds a result to a project, returns the updated Project if the project does
-- not exist returns Nothing.
addResult :: ResultId -> ProjectId -> Update University (Maybe Project)
addResult rId pId = state go
  where
    go uni = (\x -> (x, uni))
      . fmap (\p@Project{project_results = res}
              -> p {project_results = rId : res})
      . lookup pId $ projects uni

-- | Adds a participant to a project, returns the updated Project. If the
-- project does not exist return Nothing.
addParticipant :: ProjectId -> Participant -> Update University (Maybe Project)
addParticipant pId par = state go
  where
    go uni = (\x -> (x, uni)) . fmap (\p@Project{project_participants = pars}
              -> p {project_participants = par : pars})
      . lookup pId $ projects uni


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

-- | Retrieves the PI status of a student given the id number.
getStudentPI :: Id -> Query University Bool
getStudentPI idn = (\(Student person) -> pi person)
  . flip (!) idn . students <$> ask

-- | Retrieves a student by his/her id number, if none is found returns Nothing.
getStudent :: Id -> Query University (Maybe Student)
getStudent idn = findStudent idn <$> ask

-- | Retrieves a employee by his/her id number, if none is found returns
-- Nothing.
getEmployee :: Id -> Query University (Maybe Employee)
getEmployee idn = findEmployee idn <$> ask

-- | Retrieves a project, If the project does not exist returns Nothing.
getProject :: ProjectId -> Query University (Maybe Project)
getProject pId = lookup pId . projects <$> ask

-- | Retrieves a grant, If the grant does not exist returns Nothing.
getGrant :: GrantId -> Query University (Maybe Grant)
getGrant gId = lookup gId . grants <$> ask

-- | Retrieves the project's participants, If the project does not exist returns
-- Nothing.
getParticipants :: ProjectId -> Query University (Maybe Participants)
getParticipants pId = fmap project_participants . lookup pId . projects <$> ask

-- | Retrieves the project's results, If the project does not exist returns
-- Nothing.
getResults :: ProjectId -> Query University (Maybe [ResultId])
getResults pId = fmap project_results . lookup pId . projects <$> ask

-- | Retrieves the grants and amount of money associated to a project, returns
-- an empty list if no grants are associated or if the project does not exist.
getGrantsOfProject :: ProjectId -> Query University [(GrantId, Money)]
getGrantsOfProject proId = foldr (\(Assignation grant pro m) l ->
                                    if pro == proId
                                    then (grant, m) : l
                                    else l) [] . assignations <$> ask

-- | Retrieves the projects associated to a grant, returns an empty list if no
-- projects are associated or if the grant does not exist.
getProjectsOfGrant :: GrantId -> Query University [ProjectId]
getProjectsOfGrant gId = foldr (\(Assignation g pro _) l ->
                                  if g == gId
                                  then pro : l
                                  else l) [] . assignations <$> ask

-- | Retrieves the funders of a grant, returns Nothing if the grant does not
-- exist.
getFundersGrant :: GrantId -> Query University (Maybe [FunderId])
getFundersGrant gId = fmap (map (\(Pledge funder _) -> funder))
  . fmap grant_pledges . lookup gId . grants <$> ask

-------------------------------------------------------------------
------------------- Changing values in database -------------------
-------------------------------------------------------------------

-- | Changes the PI status of a student given the id number.
setStudentPI :: Bool -> Id -> Update University ()
setStudentPI v idn = modify go
  where
    go uni = updateStudents uni $ (\sts ->
                                     let f = \(Student person@Person{pi = p}) np
                                           -> Student (person  {pi = np})
                                     in insert idn (f (sts ! idn) v) sts)
-- |

-------------------------------------------------------------------
------------------- Making the transactions acidic-----------------
-------------------------------------------------------------------
makeAcidic ''University ['addStudent
                        , 'addEmployee
                        , 'addSchool
                        , 'addProject
                        , 'addAssignation
                        , 'addFunder
                        , 'addDepartment
                        , 'addPledge
                        , 'getStudents
                        , 'getEmployees
                        , 'getSchools
                        , 'getProjects
                        , 'getAssignations
                        , 'getFunders
                        , 'getStudentPI
                        , 'getStudent
                        , 'getEmployee
                        , 'getGrantsOfProject
                        , 'getProjectsOfGrant
                        , 'getParticipants
                        , 'setStudentPI
                        ]
