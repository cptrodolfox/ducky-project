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
      if M.null db
      then M.singleton (fromIntegral 1) res
      else let (index, _) = M.findMax db in
           M.insert (index + 1) res db

-- Adds a company to the companies database, increments the index.
addCompany :: Company -> Update CompanyDb ()
addCompany comp = modify go
  where
    go (CompanyDb db) = CompanyDb $
      if M.null db
      then M.singleton (fromIntegral 1) comp
      else let index = fst $ M.findMax db in
        M.insert (index + 1) comp db

-- Adds a Grant to the grants database, increments the index.
addGrant :: Grant -> Update GrantDb ()
addGrant grant = modify go
  where
    go (GrantDb db) = GrantDb $
      if M.null db
      then M.singleton (fromIntegral 1) grant
      else let index = fst $ M.findMax db in
        M.insert (index + 1) grant db

-- Adds a Project to the project database, increments the index.
addProject :: Project -> Update ProjectDb ()
addProject pro = modify go
  where
    go (ProjectDb db) = ProjectDb $
      if M.null db
      then M.singleton (fromIntegral 1) pro
      else let index = fst $ M.findMax db in
        M.insert (index + 1) pro db

-- Adds a NGO to the ngos database, increments the index.
addNGO :: NGO -> Update NGODb ()
addNGO ngo = modify go
  where
    go (NGODb db) = NGODb $
      if M.null db
      then M.singleton (fromIntegral 1) ngo
      else let index = fst $ M.findMax db in
        M.insert (index + 1) ngo db

-- Adds a GOD to the gods database, increments the index.
addGO :: GovernmentalOrganization -> Update GODb ()
addGO gov = modify go
  where
    go (GODb db) = GODb $
      if M.null db
      then M.singleton (fromIntegral 1) gov
      else let index = fst $ M.findMax db in
        M.insert (index + 1) gov db

-- Search for a student by the DNI.
searchStudentDNI :: DNI -> Query StudentDb (Maybe Student)
searchStudentDNI dni = (M.lookup dni) . (\(StudentDb db) -> db) <$> ask

-- Search for a employee by the DNI.
searchEmployeeDNI :: DNI -> Query EmployeeDb (Maybe Employee)
searchEmployeeDNI dni = (M.lookup dni) . (\(EmployeeDb db) -> db) <$> ask

-- Search for a publication by Authors
searchPubAuthor :: C.ByteString -> Query ResultDb [Result]
searchPubAuthor author = (map snd) . M.toList
  . (M.filter (\(Publication{pubAuthors = authors})
                -> any ((==) author) authors))
  . (\(ResultDb db) -> db) <$> ask

-- Search for a publication by Keyword
searchPubKeyword :: C.ByteString -> Query ResultDb [Result]
searchPubKeyword word = (map snd) . M.toList
  . (M.filter (\(Publication{pubKeywords = words})
                -> any ((==) word) words))
  . (\(ResultDb db) -> db) <$> ask

-- Gets all the departments.
getDeptAll :: Query DepartmentDb [(DeptId, Department)]
getDeptAll = M.toList . (\(DepartmentDb db) -> db) <$> ask

-- Gets all the schools.
getSchoolAll :: Query SchoolDb [(SchoolId, School)]
getSchoolAll = M.toList . (\(SchoolDb db) -> db) <$> ask

-- Gets all the students.
getStudentAll :: Query StudentDb [(DNI, Student)]
getStudentAll = M.toList . (\(StudentDb db) -> db) <$> ask

-- Gets all the Employees.
getEmployeeAll :: Query EmployeeDb [(DNI, Employee)]
getEmployeeAll = M.toList . (\(EmployeeDb db) -> db) <$> ask

-- Gets all Results.
getResultAll :: Query ResultDb [Result]
getResultAll = (map snd) . M.toList . (\(ResultDb db) -> db) <$> ask

-- Gets all the Companies.
getCompAll :: Query CompanyDb [Company]
getCompAll = (map snd) . M.toList . (\(CompanyDb db) -> db) <$> ask

-- Gets all the NGOs.
getNGOAll :: Query NGODb [NGO]
getNGOAll = (map snd) . M.toList . (\(NGODb db) -> db) <$> ask

-- Gets all the GOs.
getGOAll :: Query GODb [GovernmentalOrganization]
getGOAll = (map snd) . M.toList . (\(GODb db) -> db) <$> ask

-- Gets all the Grants.
getGrantAll :: Query GrantDb [Grant]
getGrantAll = (map snd) . M.toList . (\(GrantDb db) -> db) <$> ask

-- Get all the Projects.
getProjectAll :: Query ProjectDb [Project]
getProjectAll = (map snd) . M.toList . (\(ProjectDb db) -> db) <$> ask

-- Adds the relationship between a Student and a Department.
addStudentToDept :: DNI -> DeptId -> Update HasRelDb ()
addStudentToDept dni dept = modify go
  where
    go (HasRelDb db) = HasRelDb $
      let rel = DeptHasStudent dept dni in
      if null db
      then [rel]
      else rel : db

-- Adds the relationship between a School and a Department.
addDeptToSchool :: DeptId -> SchoolId -> Update HasRelDb ()
addDeptToSchool dept sch = modify go
  where
    go (HasRelDb db) = HasRelDb $
      let rel = SchoolHasDept sch dept in
      if null db
      then [rel]
      else rel : db

-- Adds the relationship between a University and a School.
addSchoolToUni :: SchoolId -> UniId -> Update HasRelDb ()
addSchoolToUni sch uni = modify go
  where
    go (HasRelDb db) = HasRelDb $
      let rel = UniversityHasSchool uni sch in
      if null db
      then [rel]
      else rel : db

 -- Adds the relationship between a University and a Project.
addProjectToUni :: ProjectId -> UniId -> Update HasRelDb ()
addProjectToUni pro uni = modify go
  where
    go (HasRelDb db) = HasRelDb $
      let rel = UniversityHasProject uni pro in
      if null db
      then [rel]
      else rel : db

-- Adds the relationship between a University and a Grant.
addGrantToUni :: GrantId -> UniId -> Update HasRelDb ()
addGrantToUni grant uni =  modify go
  where
    go (HasRelDb db) = HasRelDb $
      let rel = UniversityHasGrant uni grant in
      if null db
      then [rel]
      else rel : db

-- Adds the relationship between a Project and a Grant.
addGrantToProject :: GrantId -> ProjectId -> Update HasRelDb ()
addGrantToProject grant pro = modify go
  where
    go (HasRelDb db) = HasRelDb $
      let rel = ProjectHasGrant pro grant in
      if null db
      then [rel]
      else rel : db

-- Adds the relationship between a Student and a Project
addStudentToProject :: DNI -> ProjectId -> Update WorksOnDb ()
addStudentToProject stu pro = modify go
  where
    go (WorksOnDb db) = WorksOnDb $
      let rel = StudentWorksOn stu pro in
      if null db
      then [rel]
      else rel : db

-- Adds the relationship between a Employee and a Project
addEmployeeToProject :: DNI -> ProjectId -> Update WorksOnDb ()
addEmployeeToProject emp pro = modify go
  where
    go (WorksOnDb db) = WorksOnDb $
      let rel = EmployeeWorksOn emp pro in
      if null db
      then [rel]
      else rel : db

-- Adds the relationship between a Company and a Grant.
addCompanyToGrant :: CompanyId -> GrantId -> Update FundsDb ()
addCompanyToGrant comp grant = modify go
  where
    go (FundsDb db) = FundsDb $
      let rel = CompanyFunds comp grant in
      if null db
      then [rel]
      else rel : db

-- Adds the relationship between a NGO and A Grant.
addNGOToGrant :: NGOId -> GrantId -> Update FundsDb ()
addNGOToGrant ngo grant = modify go
  where
    go (FundsDb db) = FundsDb $
      let rel = NGOFunds ngo grant in
      if null db
      then [rel]
      else rel : db

-- Adds an employee to a Department.
addEmployeeToDept :: DNI -> DeptId -> Update EmploysDb ()
addEmployeeToDept emp dept = modify go
  where
    go (EmploysDb db) = EmploysDb $
      let rel = EmployedByDept dept emp in
      if null db
      then [rel]
      else rel : db

-- Adds an employee to a School.
addEmployeeToSchool :: DNI -> SchoolId -> Update EmploysDb ()
addEmployeeToSchool emp sch = modify go
  where
    go (EmploysDb db) = EmploysDb $
      let rel = EmployedBySchool sch emp in
      if null db
      then [rel]
      else rel : db

-- Adds an employee to the University.
addEmployeeToUni :: DNI -> UniId -> Update EmploysDb ()
addEmployeeToUni emp uni = modify go
  where
    go (EmploysDb db) = EmploysDb $
      let rel = EmployedByUniversity uni emp in
      if null db
      then [rel]
      else rel : db

-- Looks for the students (DNI) that belongs to a given Department.
studentsOnDept :: DeptId -> Query HasRelDb [DNI]
studentsOnDept dept = (foldr (\(DeptHasStudent d stu) l ->
                              if d == dept
                              then stu : l
                              else l) [])
                      . allHasRels <$> ask

-- Looks for the departments that belongs to a given School.
deptsOnSchool :: SchoolId -> Query HasRelDb [DeptId]
deptsOnSchool sch = (foldr (\(SchoolHasDept s stu) l ->
                                 if s == sch
                                 then stu : l
                                 else l) [])
                       . allHasRels <$> ask

-- Looks for the schools inside a University.
schoolsOnUniversity :: UniId -> Query HasRelDb [SchoolId]
schoolsOnUniversity uni = (foldr (\(UniversityHasSchool u sch) l ->
                                    if u == uni
                                    then sch : l
                                    else l) [])
                          . allHasRels <$> ask

-- Looks for the Projects inside the University.
projectsOnUniversity :: UniId -> Query HasRelDb [ProjectId]
projectsOnUniversity uni = (foldr (\(UniversityHasProject u pro) l ->
                                     if u == uni
                                     then pro : l
                                     else l) [])
                           . allHasRels <$> ask

-- Looks for the grants inside a University.
grantsOnUniversity :: UniId -> Query HasRelDb [GrantId]
grantsOnUniversity uni = (foldr (\(UniversityHasGrant u grant) l ->
                                   if u == uni
                                   then grant : l
                                   else l) [])
                         . allHasRels <$> ask

-- Looks for the grants associated to a Project.
grantsOnProject :: ProjectId -> Query HasRelDb [GrantId]
grantsOnProject pro = (foldr (\(ProjectHasGrant p grant) l ->
                                if p == pro
                                then grant : l
                                else l) [])
                      . allHasRels <$> ask
--studentsOnDept dept = (map (\(DeptHasStudent _ stu) -> stu))
--  . (filter (\(DeptHasStudent d _) -> d == dept ))
--  . allHasRels <$> ask

--students
