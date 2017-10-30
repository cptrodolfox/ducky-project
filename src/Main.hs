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
import           Data.ByteString.Char8 hiding (empty)
import           Data.Map              (empty)
import           Database
import           Prelude               hiding (getLine, putStrLn)
import           Types

-- | Creates a University from a name
createUniversity :: Name -> University
createUniversity n = University n (empty)


main :: IO ()
main = putStrLn "Hello, Haskell!"
