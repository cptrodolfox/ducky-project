-------------------------------------------------------------------
-- Module : Main
-- Authors : William R. Arellano, Paul Silva.
-- Description : This module contains the main function of the 
-- program. This includes the GUI part. (threepenny-gui)
-- Date : 27 - 10 - 2017
-- Version: 0.1
-------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import           Data.Acid
import           Control.Monad                                 (void)  --Check
import           qualified Data.ByteString.Char8 as ByteString hiding (empty, putStrLn)
import           qualified Data.Map as Map                     (empty, elems, keys)
import           Data.Time                                     (Day, fromGregorian, showGregorian)
import           Database
import           Prelude                                       hiding (id)
import           Types
import           Graphics.UI.Threepenny.Core
import           qualified Graphics.UI.Threepenny as UI
import           System.IO.Unsafe  --Check

--------------------------------------------------------------------
------------------- Initial Values for databases--------------------
--------------------------------------------------------------------
emptyStudents = Map.empty :: Students
emptyEmployees = Map.empty :: Employees
emptySchools = Map.empty :: Schools
emptyAssignations = [] :: Assignations
emptyFunders = Map.empty :: Funders
emptyProjects = Map.empty :: Projects
emptyGrants = Map.empty :: Grants
emptyResults = Map.empty :: Results
emptyResultsId = [] :: [ResultId]
emptyParticipants = [] :: Participants

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
  state <- openLocalState $ createUniversity $ ByteString.pack "test"
  startGUI defaultConfig $ setup state

-- | setup controls de UI implementation
setup :: AcidState University -> Window -> UI ()
setup state window = void $ do

  return window # set UI.title "Project Management Utility"
  UI.addStyleSheet window "foundation-5.css"

  header <- UI.h2 # set UI.text "Welcome to the Project Management Utility of Yachay Tech" 
  buttonAdd <- UI.button # set UI.text "Add Project"
  -- buttonRemove <- UI.button # set UI.text "Remove Project"
  buttonShStudent <- UI.button # set UI.text "Show Students"
  -- buttonAddStudent <- UI.button # set UI.text "Add Student"
  -- buttonRmStudent <- UI.button # set UI.text "Remove Student"
  
  -- | Titles for the Project Table
  title <- UI.h4 # set UI.text "Title" # set style [("border","solid black 1px")]
  description <- UI.h4 # set UI.text "Description" # set style [("border","solid black 1px")]
  budget <- UI.h4 # set UI.text "Budget" # set style [("border","solid black 1px")]
  participants <- UI.h4 # set UI.text "Participants" # set style [("border","solid black 1px")]
  let proj = unsafePerformIO $ query state GetProjects
      prTitle = map string $ map ByteString.unpack $ map project_title $ Map.elems proj 
      prDescription = map string $ map ByteString.unpack $ map project_description $ Map.elems proj 
      prBudget = map string $ map show $ map project_budget $ Map.elems proj
      prStudent = map string $ map show $ map project_participants $ Map.elems proj 
  
  canvas <- UI.div #+ [element header, 
                       column [ grid [ map element [title, description, budget, participants], 
                                       map column [prTitle , prDescription, prBudget, prStudent]] # set style [("border","solid black 1px")]],
                       grid [map element [buttonAdd, buttonShStudent]]]
  
  getBody window 
    #+ [ element canvas ]
    # set style [("background-color", "#eeeeee"),("vertical-align","middle"),("horizontal-align","middle")]
    
  on UI.click buttonShStudent $ \_ -> void $ do
    -- | Students Section
    fName <- UI.h4 # set UI.text "First Name" # set style [("border","solid black 1px")]
    lName <- UI.h4 # set UI.text "Last Name" # set style [("border","solid black 1px")]
    idS <- UI.h4 # set UI.text "Id" # set style [("border","solid black 1px")]
    let stud = unsafePerformIO $ query state GetStudents
        fromPerson = map person $ Map.elems stud
        stFName = map string $ map ByteString.unpack $ map fname $ fromPerson
        stLName = map string $ map ByteString.unpack $ map lname $ fromPerson
        stId = map string $ map show $ map id $ fromPerson
    
    header <- UI.h2 #+ [string $ "Students"] 
    
    studCanvas <- UI.div #. "small-8 row" 
      #+ [ element header, 
           column [ grid [ map element [fName, lName, idS], 
                           map column [stFName , stLName, stId]]]]
    
    getBody window # set children [ studCanvas ]
        
    return ()

  on UI.click buttonAdd $ \_ -> void $ do 
    -- | UI input elements for the project 
    projTitleInput <- UI.input # set (attr "placeholder") "title of the project"
    projDescriptionInput <- UI.input # set (attr "placeholder") "short description of the project"
    projBudgetInput <- UI.input # set (attr "placeholder") "amount of money for the project"
    {-projResults <- column [
                     grid [ [string "Publication:", UI.input] ]
                 ] -}
    projParticipantsInput <- UI.input # set (attr "placeholder") "Id of participant"
    
    add <- UI.button # set UI.text "Add"
    
    on UI.click add $ \_ -> void $ do
      -- | Values of the new Project
      projTitle <- get value projTitleInput
      projDescription <- get value projDescriptionInput
      projBudget <- get value projBudgetInput
      projParticipants <- get value projParticipantsInput
      let projBudN = read projBudget :: Double
          projPartId = read projParticipants :: Id
          projPart = PStudent projPartId
          nextKey = let listId = Map.keys proj 
                  in if length listId > 1 then maximum listId + 1 else 1
          project = Project (ByteString.pack projTitle) (ByteString.pack projDescription) projBudN emptyResultsId [projPart] 
      liftIO $ update state $ AddProject nextKey project
    
    header <- UI.h2 #+ [string $ "Add a new Project"] 
    projCanvas <- UI.div #. "small-4 row" 
      #+ [ element header, 
           row [ grid [ [string "Title:", element projTitleInput]
          , [string "Description:", element projDescriptionInput]
          , [string "Budget:", element projBudgetInput]
          , [string "Participants:", element projParticipantsInput]]]] 
      # set style [("align","center")]
    
    getBody window # set children [ projCanvas , add ]
  
  --on UI.click buttonRemove $ \_ -> do
  --on UI.click buttonAddStudent $ \_ -> do    
  
  {- putStrLn "The name of the university is test"
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
  return ()-}
