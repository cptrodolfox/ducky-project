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
  buttonShStudent <- UI.button # set UI.text "Show Students"
  buttonAddStudent <- UI.button # set UI.text "Add Student"
  
  -- | Titles for the Project Table
  title <- UI.h4 # set UI.text "Title" # set style [("border","solid black 1px")]
  description <- UI.h4 # set UI.text "Description" # set style [("border","solid black 1px")]
  budget <- UI.h4 # set UI.text "Budget" # set style [("border","solid black 1px")]
  participants <- UI.h4 # set UI.text "Participants" # set style [("border","solid black 1px")]
  let proj = unsafePerformIO $ query state GetProjects
      elemProj = Map.elems proj
      prTitle = map string $ map ByteString.unpack $ map project_title $ Map.elems proj 
      prDescription = map string $ map ByteString.unpack $ map project_description $ Map.elems proj 
      prBudget = map string $ map show $ map project_budget $ Map.elems proj
      idParticipant = [ student_id x | x <- concat $ map project_participants elemProj]
      prStudent = map string $ map show idParticipant
  
  canvas <- UI.div #+ [ 
                       column [ element header, 
                                grid [ map element [title, description, budget, participants], 
                                       map column [prTitle , prDescription, prBudget, prStudent]],
                                grid [ map element [buttonAdd, buttonShStudent, buttonAddStudent]] # set style [("position","absolute"),("left","50%"),("transform","translateX(-50%)")]]
                       ]
  
  getBody window 
    #+ [ element canvas # set style [("position","absolute"),("background-color", "#eeeeee"),("margin","auto"),("left","50%"),("transform","translateX(-50%)")]] # set style [("position","relative")]
    
  on UI.click buttonShStudent $ \_ -> void $ do
    -- | Students Section
    fName <- UI.h4 # set UI.text "First Name" # set style [("border","solid black 1px")]
    lName <- UI.h4 # set UI.text "Last Name" # set style [("border","solid black 1px")]
    idS <- UI.h4 # set UI.text "Id" # set style [("border","solid black 1px")]
    let stud = unsafePerformIO $ query state GetStudents
        people = map person $ Map.elems stud
        stFName = map string $ map ByteString.unpack $ map fname $ people
        stLName = map string $ map ByteString.unpack $ map lname $ people
        stId = map string $ map show $ map id $ people
    
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
                  in if length listId > 0 then maximum listId + 1 else 1
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
    
  on UI.click buttonAddStudent $ \_ -> do
    -- | UI input elements for student 
    studFNameInput <- UI.input # set (attr "placeholder") "Student's first name"
    studLNameInput <- UI.input # set (attr "placeholder") "Student's last name"
    studBDInput <- UI.input # set (attr "placeholder") "day"
    studBMInput <- UI.input # set (attr "placeholder") "month"
    studBYInput <- UI.input # set (attr "placeholder") "year"
    studGenderInput <- UI.input # set (attr "placeholder") "Male | Female | Other"
    studPIInput <- UI.input # set (attr "placeholder") "True | False"
    studIdInput <- UI.input # set (attr "placeholder") "Student's Id"
    
    add <- UI.button # set UI.text "Add"
    
    on UI.click add $ \_ -> void $ do
      -- | Values of the new Project
      studFName <- get value studFNameInput
      studLName <- get value studLNameInput
      studBD <- get value studBDInput
      studBM <- get value studBMInput
      studBY <- get value studBYInput
      studGender <- get value studGenderInput
      studPI <- get value studPIInput
      studId <- get value studIdInput
      let year = read studBY :: Integer
          month = read studBM :: Int
          day = read studBD :: Int
          birthday = fromGregorian year month day
          studIdN = read studId :: Integer          
          std_person = Person (ByteString.pack studFName) (ByteString.pack studLName) birthday (toBool studPI) (toGender studGender) studIdN
          std = Student std_person
      liftIO $ update state $ AddStudent std
    
    header <- UI.h2 #+ [string $ "Add a new Student"] 
    projCanvas <- UI.div #. "small-4 row" 
      #+ [ element header, 
           row [ grid [[string "First name:", element studFNameInput]
          , [string "Last name:", element studLNameInput]
          , [string "Birthday:", grid [map element [studBYInput, studBMInput, studBDInput]]]
          , [string "Gender:", element studGenderInput]
          , [string "PI:", element studPIInput]
          , [string "Id:", element studIdInput]]]] 
      # set style [("align","center")]
    
    getBody window # set children [ projCanvas , add ]
  
toBool :: String -> Bool
toBool "True" = True
toBool "False" = False

toGender :: String -> Genre
toGender "Male" = Male
toGender "Female" = Female
toGender "Other" = Other 
