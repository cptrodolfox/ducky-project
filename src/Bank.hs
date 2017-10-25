{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Bank where

import           Control.Applicative
import           Control.Monad.Reader  (ask)
import           Control.Monad.State   (modify)
import           Data.Acid
import qualified Data.ByteString.Char8 as BS
import           Data.Map              (Map, lookup)
import           Data.SafeCopy
import           Data.Time
type AccountNumber = Integer
type Id = Integer
data AccountType = Savings
                 | Current
                 deriving (Eq, Show, Ord)
deriveSafeCopy 0 'base ''AccountType

data Person = Person { id   :: !Id
                     , name :: !BS.ByteString
                     } deriving (Eq, Show, Ord)
deriveSafeCopy 0 'base ''Person

data Account = Account { creationDate :: !UTCTime
                       , balance      :: !Double
                       , accType      :: !AccountType
                       , person       :: !Person
                       } deriving (Eq, Show, Ord)
deriveSafeCopy 0 'base ''Account


data AccDatabase = AccDatabase {accDatabase :: Map AccountNumber Account
                               } deriving (Eq, Show, Ord)
deriveSafeCopy 0 'base ''AccDatabase

data Bank = Bank { ruc      :: !Id
                 , bankName :: !BS.ByteString
                 , ceo      :: !Person
                 , accounts :: !AccDatabase
                 } deriving (Eq, Show, Ord)
deriveSafeCopy 0 'base ''Bank

data Transaction = Deposit {idDep      :: !Id
                           , dateDep   :: !UTCTime
                           , personDep :: !Person
                           , valueDep  :: !Double
                           , accDep    :: !AccountNumber
                           }
                 | Withdraw {idW      :: !Id
                            , dateW   :: !UTCTime
                            , personW :: !Person
                            , valueW  :: !Double
                            , accW    :: !AccountNumber}
                 | Transference {idT        :: !Id
                                , dateT     :: !UTCTime
                                , personT   :: !Person
                                , valueT    :: !Double
                                , originAcc :: !AccountNumber
                                , destAcc   :: !AccountNumber
                                } deriving (Eq, Show, Ord)
deriveSafeCopy 0 'base ''Transaction

data PersonDatabase = PersonDatabase {allPersons :: [Person]
                                     } deriving (Eq, Show, Ord)
deriveSafeCopy 0 'base ''PersonDatabase


data TrDatabase = TrDatabase {allTransactions :: [Transaction]
                             } deriving (Eq, Show, Ord)
deriveSafeCopy 0 'base ''TrDatabase

data Banks = Banks {allBanks :: [Bank]
                   } deriving (Eq, Show, Ord)
deriveSafeCopy 0 'base ''Banks

-- Adds a Bank
addBank :: Bank -> Update Banks ()
addBank bank = modify go
  where
    go (Banks db) = Banks $
      if null db
      then [bank]
      else bank : db
-- Adds a Account
addAccount :: Bank -> Account -> Maybe (Update Banks ())
addAccount (ruc name ceo accs) acc = modify go
  where
    go (Banks db) = Banks $
      if 
