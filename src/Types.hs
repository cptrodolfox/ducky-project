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

type ProjectId  = Integer
type GrantId  = Integer
type ResultId = Integer
type GOId = Integer
type NGOId = Integer
type CompanyId = Integer
type UniId = Integer

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


-- |  DATABASES----

data StudentDb = StudentDb (Map DNI Student)
deriveSafeCopy 0 'base ''StudentDb

data EmployeeDb = EmployeeDb (Map DNI Employee)
deriveSafeCopy 0 'base ''EmployeeDb

data DepartmentDb = DepartmentDb (Map DeptId Department)
deriveSafeCopy 0 'base ''DepartmentDb

data SchoolDb = SchoolDb (Map SchoolId School)
deriveSafeCopy 0 'base ''SchoolDb

data ResultDb = ResultDb (Map ResultId Result)
deriveSafeCopy 1 'base ''ResultDb

data CompanyDb = CompanyDb (Map CompanyId Company)
deriveSafeCopy 1 'base ''CompanyDb

data NGODb = NGODb (Map NGOId NGO)
deriveSafeCopy 0 'base ''NGODb

data GODb = GODb (Map GOId GovernmentalOrganization)
deriveSafeCopy 0 'base ''GODb

data GrantDb = GrantDb (Map GrantId Grant)
deriveSafeCopy 0 'base ''GrantDb

data ProjectDb = ProjectDb (Map ProjectId Project)
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

-- | Relationships

-- | Defines the employer and employee relationship between a university,
-- | department and school, and a employee. (Employer, Employee)
data Employs = EmployedByDept DeptId DNI
             | EmployedBySchool SchoolId DNI
             | EmployedByUniversity UniId DNI
             deriving (Show, Eq)

deriveSafeCopy 2 'base ''Employs

-- | Defines the has relationship between entities.
data HasRel = DeptHasStudent DeptId DNI
            | SchoolHasDept SchoolId DeptId
            | UniversityHasSchool UniId SchoolId
            | UniversityHasProject UniId ProjectId
            | UniversityHasGrant UniId GrantId
            | ProjectHasGrant ProjectId GrantId
            deriving (Show, Eq)

deriveSafeCopy 3 'base ''HasRel

-- | Defines the relation works on between employee and student, and project.
data WorksOn = StudentWorksOn DNI ProjectId
             | EmployeeWorksOn DNI ProjectId

deriveSafeCopy 1 'base ''WorksOn

-- | Defines the funds relationship between company, go and ngo, and grants.
-- | (go / ngo / company, grant).
data Funds = CompanyFunds CompanyId GrantId
           | NGOFunds NGOId GrantId
           | GOFunds GOId GrantId
deriveSafeCopy 1 'base ''Funds

-- | The database for the hasRel relationships.
data HasRelDb = HasRelDb {allHasRels :: [HasRel]}
deriveSafeCopy 0 'base ''HasRelDb

-- | The database for the worksOn relationships.
data WorksOnDb = WorksOnDb {allWorksOn :: [WorksOn]}
deriveSafeCopy 0 'base ''WorksOnDb

-- | The database for the Funds relationships.
data FundsDb = FundsDb {allFunds :: [Funds]}
deriveSafeCopy 0 'base ''FundsDb

-- | The database for the employs relationships.
data EmploysDb = EmploysDb {allEmploys :: [Employs]}
deriveSafeCopy 0 'base ''EmploysDb

