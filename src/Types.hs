{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Types where

import           Control.Applicative
import           Control.Monad.Reader  (ask)
import           Control.Monad.State   (modify)
import           Data.Acid
import qualified Data.ByteString.Char8 as C
import           Data.Function
import           Data.IntMap           (IntMap)
import           Data.Map              (Map)
import           Data.SafeCopy
import           Data.Time
import           Data.Time.Calendar
import           Data.Typeable

newtype SchoolId = SchoolId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''SchoolId

newtype DeptId = DeptId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''DeptId

newtype UniId = UniId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''UniId

newtype NGOId = NGOId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''NGOId

newtype GOId = GOId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''GOId

newtype GrantId = GrantId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''GrantId

newtype ProjectId =  ProjectId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''ProjectId

newtype CompanyId = CompanyId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''CompanyId

newtype ResultId = ResultId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''ResultId

newtype PatentId = PatentId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''PatentId

newtype PubId = PubId Integer
 deriving (Eq, Show, Typeable, Ord)
deriveSafeCopy 0 'base ''PubId

newtype DNI = DNI Integer
  deriving (Eq, Show, Ord, Typeable)
deriveSafeCopy 0 'base ''DNI

data Degree = Bachelor
               | Master
               | PhD
               | PostPhD
               deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''Degree

data UniType = Public
             | Private
             deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''UniType

data NType = National
             | International
              deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''NType


data Student = Student { stdFname    :: !C.ByteString
                       , stdLname    :: !C.ByteString
                       , stdDni      :: !DNI
                       , stdPi       :: !Bool
                       , stdBirthday :: !Day
                       }
                deriving (Eq, Show, Typeable)
deriveSafeCopy 1 'base ''Student

data Employee = Employee { empFname    :: !C.ByteString
                         , empLname    :: !C.ByteString
                         , empDni      :: !DNI
                         , empPi       :: !Bool
                         , empBirthday :: !Day
                         , empStudies  :: !Degree
                         }
                 deriving (Eq, Show, Typeable)
deriveSafeCopy 1 'base ''Employee

data Department = Department { deptId   :: !DeptId
                             , deptName :: !C.ByteString
                             }
                   deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''Department

data School = School { schoolId    :: !SchoolId
                     , schoolName  :: !C.ByteString
                     , students    :: ![DNI]
                     , employees   :: ![DNI]
                     , departments :: ![DeptId]
                     }
               deriving (Eq, Show, Typeable)
deriveSafeCopy 1 'base ''School

data NGO = NGO { ngoName :: !C.ByteString
               , ngoType :: !NType
               }
            deriving (Eq, Show, Typeable)
deriveSafeCopy 1 'base ''NGO

data GovernmentalOrganization = GO { goId  :: !GOId
                                  , goName :: !C.ByteString
                                  }
                                deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''GovernmentalOrganization

data Grant = Grant { grantId          :: !GrantId
                   , grantName        :: !C.ByteString
                   , grantLegislation :: !C.ByteString
                   , grantTotalAmount :: !Double
                   }
              deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''Grant

data Project = Project { projectId     :: !ProjectId
                       , projectName   :: !C.ByteString
                       , projectBudget :: !Double
                       , projectGrants :: !GrantId
                       , projectDesp   :: !C.ByteString
                       }
                deriving (Eq, Show, Typeable)
deriveSafeCopy 1 'base ''Project

data Result = Publication { pubId       :: !PubId
                          , pubTitle    :: !C.ByteString
                          , pubDate     :: !Day
                          , pubAbstract :: !C.ByteString
                          , pubJournal  :: !C.ByteString
                          , pubKeywords :: ![C.ByteString]
                          , pubAuthors  :: ![C.ByteString]
                          }
            | Patent { patentId      :: !PatentId
                     , patentName    :: !C.ByteString
                     , patentType    :: !C.ByteString
                     , patentNumber  :: !C.ByteString
                     , patentTerm    :: !Integer
                     , patentDate    :: !Day
                     , patentAuthors :: ![C.ByteString]
                     }
            | BiologicalOrganism --TODO
            | Molecule --TODO
            | Software --TODO
            | Hardware --TODO
             deriving (Eq, Show, Typeable)
deriveSafeCopy 2 'base ''Result

data Company = Company { companyIdimport
                       , companyName   :: !C.ByteString
                       , companyType   :: !NType
                       , companyProfit :: !Double
                       }
                deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''Company


-- DATABASES----

data StudentDb = StudentDb (Map DNI Student)
deriveSafeCopy 0 'base ''StudentDb

data EmployeeDb = EmployeeDb (Map DNI Employee)
deriveSafeCopy 0 'base ''EmployeeDb

data DepartmentDb = DepartmentDb (Map DeptId Department)
deriveSafeCopy 0 'base ''DepartmentDb

data SchoolDb = SchoolDb (Map SchoolId School)
deriveSafeCopy 0 'base ''SchoolDb

data ResultDb = ResultDb (IntMap Result)
deriveSafeCopy 0 'base ''ResultDb

data CompanyDb = CompanyDb (IntMap Company)
deriveSafeCopy 0 'base ''CompanyDb

data NGODb = NGODb (IntMap NGO)
deriveSafeCopy 0 'base ''NGODb

data GODb = GODb (IntMap GovernmentalOrganization)
deriveSafeCopy 0 'base ''GODb

data GrantDb = GrantDb (IntMap Grant)
deriveSafeCopy 0 'base ''GrantDb

data ProjectDb = ProjectDb (IntMap Project)
deriveSafeCopy 0 'base ''ProjectDb

data University = University { studentDb        :: StudentDb
                                 , employeeDb   :: EmployeeDb
                                 , departmentDb :: DepartmentDb
                                 , schoolDb     :: SchoolDb
                                 , resultDb     :: ResultDb
                                 , projectDb    :: ProjectDb
                                 , grantDb      :: GrantDb
                                 , name         :: !C.ByteString
                                 , uniType      :: !UniType
                                 }
deriveSafeCopy 2 'base ''University

-- Relationships

-- Defines the employer and employee relationship between a university,
-- department and school, and a employee. (Employer, Employee)
data Employs = EmployedByDept (DeptId, DNI)
             | EmployedBySchool (SchoolId, DNI)
             | EmployedByUniversity DNI
             deriving (Show, Eq)

deriveSafeCopy 1 'base ''Employs

-- Defines the has relationship between entities.
data HasRel = DeptHasStudent (DeptId, DNI)
            | SchoolHasDept (SchoolId, DeptId)
            | UniversityHasSchool SchoolId
            | UniversityHasProject Int
            | UniversityHasGrant Int
            | ProjectHasGrant (Int, Int)
            deriving (Show, Eq)

deriveSafeCopy 0 'base ''HasRel

-- Defines the relation works on between employee and student, and project.
data WorksOn = StudentWorksOn (DNI, Int)
             | EmployeeWorksOn (DNI, Int)

deriveSafeCopy 0 'base ''WorksOn

-- Defines the funds relationship between company, go and ngo, and grants.
-- (go / ngo / company, grant).
data Funds = CompanyFunds (Int, Int)
           | NGOFunds (Int, Int)
           | GOFunds (Int, Int)


