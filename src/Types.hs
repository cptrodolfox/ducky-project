-------------------------------------------------------------------
--
-- Module : Types
-- Authors : William R. Arellano, Paul Silva.
-- Description : This module contains the types and ADTs used
-- in the ducky-project program.
-- Date : 27 - 10 - 2017
-- Version: 0.1
-------------------------------------------------------------------
--{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE TypeFamilies       #-}

module Types where

import           Data.Acid
import           Data.ByteString.Char8 (ByteString)
import           Data.Map              (Map)
import           Data.SafeCopy
import           Data.Text             (Text)
import           Data.Time             (Day)
-------------------------------------------------------------------
----------------- Types Synonyms ----------------------------------
-------------------------------------------------------------------

-- | Number for person identification, in spain is known as the DNI.
type Id = Integer

-- | A ByteString for naming purposes.
type Name = ByteString

-- | A Double that represents of money.
type Money = Double

-- | An Integer that identifies a School inside the university.
type SchoolId = Integer

-- | An Integer that identifies a Department inside the university.
type DeptId = Integer

-- | An Integer that identifies a Funder.
type FunderId = Integer

-- | An Integer that identifies a Grant.
type GrantId = Integer

-- | An Integer that identifies a Project.
type ProjectId = Integer

-- | A ByteString that represents a legislation, set of laws governing an
-- activity, resource, etc.
type Legislation = ByteString

-- | An Int representing a number of years.
type Years = Int

-- | An Integer representing a patent's registration number.
type RegistrationNumber = Integer

-- | An Integer that identifies a result.
type ResultId = Integer

-------------------------------------------------------------------
--------------------------- ADTs ----------------------------------
-------------------------------------------------------------------

-- | The genres a person identifies as.
data Genre = Male
           | Female
           | Other
           deriving (Show)
deriveSafeCopy 0 'base ''Genre

-- | A person inside the university identified by Id.
data Person = Person { fname    :: !Name -- ^The person's first name.
                     , lname    :: !Name -- ^The person's last name.
                     , birthday :: !Day -- ^The person's birthday.
                     , pi       :: !Bool -- ^Whether or not the person is
                                         -- responsible for a project.
                     , genre    :: !Genre -- ^The Genre identifies as.
                     , id       :: !Id -- ^The person's national identification
                                       -- number.
                     } deriving (Show)
deriveSafeCopy 0 'base ''Person

-- | The different degrees a person can have.
data Degree = Bachelor
            | Master
            | PhD
            | PostPhd
            deriving (Show)
deriveSafeCopy 0 'base ''Degree

-- | An employee inside the university. He/She is a person with studies.
data Employee = Employee Person Degree
              deriving (Show)
deriveSafeCopy 0 'base ''Employee

-- | A student inside the university.
data Student = Student { person :: !Person -- ^A student has the type person.
                       } deriving (Show)
deriveSafeCopy 1 'base ''Student

-- | The third level of organization inside a university.
data Department = Department { dept_name     :: !Name -- ^The department's name.
                             , dept_students :: ![Id] -- ^The student's that
                                                      -- belongs to the
                                                      -- department.
                             , dept_head     :: !Id -- ^The department's head.
                             }deriving (Show)
deriveSafeCopy 0 'base ''Department

-- | A Map between a DeptId and a Department.
type Departments = Map DeptId Department

-- | The second level of organization inside a university.
-- Example: School of Natural Sciences.
data School = School { school_name :: !Name -- ^The School's name.
                     , departments :: !Departments -- ^The departments that
                                                   -- belong to the school.
                     , dean        :: !Id -- ^The School's dean identifies by
                                          -- the Id
                     }deriving (Show)
deriveSafeCopy 0 'base ''School

-- | The Nationality of Companies and NGOs.
data Nationality = National
                 | International
                 deriving (Show)
deriveSafeCopy 0 'base ''Nationality

-- | An organization that can fund a grant.
data Funder = NGO Name Nationality -- ^No governmental organizations.
            | GO Name -- ^Govermental Organtizations.
            | Company Name Nationality Money -- ^The money are the last year
                                             -- profits.
            deriving (Show)
deriveSafeCopy 0 'base ''Funder

-- | A promise from a funder to give and amount of money to a grant.
data Pledge = Pledge FunderId Money
            deriving (Show, Eq)
deriveSafeCopy 0 'base ''Pledge

-- | A List of pledges of a Grant.
type Pledges = [Pledge]

-- | A grant that funds projects and is given by a funder.
data Grant = Grant { grant_name        :: !Name -- ^The grant's name.
                   , grant_legislation :: !Legislation -- ^The grant's
                                                       -- legislation.
                   , grant_amount      :: !Money -- ^The grant's amount.
                   , grant_pledges     :: !Pledges -- ^Funding institutions.
                   } deriving (Show)
deriveSafeCopy 1 'base ''Grant

-- | An assignation of a grant to a project by a given amount.
data Assignation = Assignation GrantId ProjectId Money
                 deriving (Show)
deriveSafeCopy 0 'base ''Assignation

-- | The types of patents recognized by the USPTO.
data PatentType  = Utility -- ^Type that covers processes, compositions of
                           -- matter, machines, and manufactures that are new
                           -- and useful.
                 | Design -- ^Type that covers the surface ornamentation of an
                          -- object, the design must be intrisic to the object.
                 | Plant -- ^Type that covers new and distinctive plants, that
                         -- may not be found in nature.
                 deriving (Show)
deriveSafeCopy 0 'base ''PatentType

-- | The result of a project done by the university.
data Result = Publication { pub_title    :: !Name -- ^The publication's title.
                          , pub_date     :: !Day -- ^The publication date.
                          , pub_abstract :: !ByteString -- ^The publication's
                                                        -- abstract.
                          , pub_keywords :: ![Name] -- ^The publication's
                                                    -- keywords.
                          , pub_magazine :: !Name -- ^The publication magazine.
                          }
            | Patent { patent_name   :: !Name -- ^The patent's name.
                     , patent_type   :: !PatentType -- ^The patent's Type.
                     , patent_number :: !RegistrationNumber -- ^The patent's
                                                            -- registration
                                                            -- number.
                     , patent_date   :: !Day -- ^The day when the patent was
                                           -- awarded.
                     , patent_term   :: !Years -- ^The amount of years a patent
                                               -- is valid.
                     }
            | BiologicalOrganism
            | Molecule
            | Software
            | Hardware
            deriving (Show)
deriveSafeCopy 0 'base ''Result

-- | A Student or Employee that works on a project.
data Participant = PStudent { student_id :: !Id } -- ^The participant is a student.
                 | PEmployee { employee_id :: !Id } -- ^The participant is an employee.
                 deriving (Show, Eq, Ord)
deriveSafeCopy 1 'base ''Participant

-- | A list of participants of a project.
type Participants = [Participant]

-- | A project inside the university.
data Project = Project { project_title        :: !Name -- ^The project's name.
                       , project_description  :: !ByteString -- ^The project's
                                                            -- description.
                       , project_budget       :: !Money -- ^The project's
                                                       -- assigned budget.
                       , project_results      :: ![ResultId] -- ^The projects
                                                            -- results.
                       , project_participants :: !Participants -- ^The project's
                                                               -- participants.
                       } deriving (Show)
deriveSafeCopy 1 'base ''Project

--------------------------------------------------------------------------------
---------------------- Types Synonyms and ADT for Database ---------------------
--------------------------------------------------------------------------------

-- | A database for the schools.
type Schools = Map SchoolId School

-- | A database for the funders.
type Funders = Map FunderId Funder

-- | A database for the persons.
type Persons = Map Id Person

-- | A database of the students inside the university.
type Students = Map Id Student

-- | A database of the employees inside the university.
type Employees = Map Id Employee

-- | A database for the grants.
type Grants = Map GrantId Grant

-- | A database for the projects.
type Projects = Map ProjectId Project

-- | A database of the assignations.
type Assignations = [Assignation]

-- | A database of the results inside the university.
type Results = Map ResultId Result

-- | A university, stores all the relevant databases for the university.
data University = University { name         ::  !Name
                             , students     :: !Students -- ^The students that
                                                         -- belongs to the
                                                         -- university.
                             , employees    :: !Employees -- ^The employees that
                                                          -- belongs to the
                                                          -- university.
                             , schools      :: !Schools -- ^The schools that
                                                        -- belongs to the
                                                        -- university.
                             , projects     :: !Projects -- ^The projects inside
                                                         -- the univeristy.
                             , assignations :: !Assignations -- ^The
                                                             -- assignations of
                                                             -- the projects
                                                             -- inside the
                                                             -- university.
                             , funders      :: !Funders -- ^The Funder parties
                                                        -- related to the
                                                        -- university.
                             , grants       :: !Grants -- ^The grants given to
                                                       -- projects inside the
                                                       -- university.
                             , results      :: !Results -- ^The results of
                                                        -- projects done inside
                                                        -- the university.
                             } deriving (Show)
deriveSafeCopy 1 'base ''University
