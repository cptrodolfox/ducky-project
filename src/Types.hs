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
import qualified Data.IntMap           as IM
import           Data.Map              as M
import           Data.SafeCopy
import           Data.SafeCopy
import           Data.Time
import           Data.Time.Calendar
import           Data.Typeable

newtype SchoolId = SchoolId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''SchoolId

newtype DeptId = DeptId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''DeptId

newtype UniId = UniId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''UniId

newtype NGOId = NGOId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''NGOId

newtype GOId = GOId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''GOId

newtype GrantId = GrantId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''GrantId

newtype ProjectId =  ProjectId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''ProjectId

newtype CompanyId = CompanyId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''CompanyId

newtype ResultId = ResultId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''ResultId

newtype PatentId = PatentId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''PatentId

newtype PubId = PubId Integer
 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''PubId

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

data NGOType = National
             | International
              deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''NGOType


data Student = Student { stdFname    :: !C.ByteString
                       , stdLname    :: !C.ByteString
                       , stdDni      :: !Integer
                       , stdPi       :: !Bool
                       , stdBirthday :: !Day
                       }
                deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''Student

data Employee = Employee { empFname    :: !C.ByteString
                         , empLname    :: !C.ByteString
                         , empDni      :: !Integer
                         , empPi       :: !Bool
                         , empBirthday :: !Day
                         , empStudies  :: !Degree
                         }
                 deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''Employee

data Department = Department { deptId   :: !DeptId
                             , deptName :: !C.ByteString
                             }
                   deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''Department

data School = School { schoolId    :: !SchoolId
                     , schoolName  :: !C.ByteString
                     , students    :: ![Student]
                     , employees   :: ![Employee]
                     , departments :: ![Department]
                     }
               deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''School

data University = University { uniId   :: !UniId
                             , uniName :: !C.ByteString
                             , schools :: ![School]
                             }
                   deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''University

data NGO = NGO { ngoName :: !C.ByteString
               , ngoType :: !NGOType
               }
            deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''NGO

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
                       , projectTitle  :: !C.ByteString
                       , projectBudget :: !Double
                       , projectGrants :: !GrantId
                       , projectDesp   :: !C.ByteString
                       }
                deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''Project

data Result = Publication { pubId       :: !PubId
                          , pubTitle    :: !C.ByteString
                          , pubDate     :: !Day
                          , pubAbstract :: !C.ByteString
                          , pubMagazine :: !C.ByteString
                          , pubKeywords :: ![C.ByteString]
                          }
            | Patent { patentId     :: !PatentId
                     , patentName   :: !C.ByteString
                     , patentType   :: !C.ByteString
                     , patentNumber :: !C.ByteString
                     , patentTerm   :: !Integer
                     , patentDate   :: !Day
                     }
            | BiologicalOrganism --TODO
            | Molecule --TODO
            | Software --TODO
            | Hardware --TODO
             deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''Result

data Company = Company { companyId     :: !CompanyId
                       , companyName   :: !C.ByteString
                       , companyType   :: !C.ByteString
                       , companyProfit :: !Double
                       }
                deriving (Eq, Show, Typeable)
deriveSafeCopy 0 'base ''Company



