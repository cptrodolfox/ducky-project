-------------------------------------------------------------------
-- Module : Main
-- Authors : William R. Arellano, Paul Silva.
-- Description : This module contains the main function of the program.
-- Date : 27 - 10 - 2017
-- Version: 0.1
-------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import           Data.Acid
import           Data.ByteString.Char8 hiding (empty, putStrLn)
import           Data.Map              (empty)
import           Data.Time             (Day, fromGregorian, showGregorian)
import           Database
import           Prelude               hiding (getLine)
import           Types
--------------------------------------------------------------------
------------------- Initial Values for databases--------------------
--------------------------------------------------------------------
emptyStudents = empty :: Students
emptyEmployees = empty :: Employees
emptySchools = empty :: Schools
emptyAssignations = [] :: Assignations
emptyFunders = empty :: Funders
emptyProjects = empty :: Projects
emptyGrants = empty :: Grants
emptyResults = empty :: Results

-- | Creates a University from a name, and with empty databases.
createUniversity :: Name -> University
createUniversity n = University { name = n
                                , students = emptyStudents
                                , employees = emptyEmployees
                                , schools = emptySchools
                                , projects = emptyProjects
                                , assignations = emptyAssignations
                                , funders = emptyFunders
                                , grants = emptyGrants
                                , results = emptyResults
                                }


main :: IO ()
main = do
  putStrLn "The name of the university is test"
  state <- openLocalState $ createUniversity $ pack "test"
  allStuds <- query state GetStudents
  mapM_ print allStuds
  putStrLn "Please write the id number of a new student: "
  idS <- getLine
  let idN = read (unpack idS) :: Integer
  putStrLn "Please write the student's first name: "
  fname <- getLine
  putStrLn "Please write the student's last name: "
  lname <- getLine
  putStrLn "Please write the student's birthday first year, second month and third day"
  yS <- getLine
  mS <- getLine
  dS <- getLine
  let year = read (unpack yS) :: Integer
      month = read (unpack mS) :: Int
      day = read (unpack dS) :: Int
      birthday = fromGregorian year month day
  putStrLn "The student's birthday is: "
  putStrLn $ showGregorian birthday
  putStrLn "For testing the student's genre will be other."
  let std_person = Person fname lname birthday False Other idN
      std = Student std_person
  putStrLn $ show std
  update state (AddStudent $ std)
  putStrLn "Ending session..."
  return ()
