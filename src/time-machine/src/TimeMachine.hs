{-# LANGUAGE RecordWildCards #-}

module TimeMachine where

-- Data types for the Time Machine Store
data TTDirection = Past | Future | Both deriving Show

data TimeMachine = TimeMachine
   {  tmCompany :: String
    , tmId :: Int
    , tmName :: String
    , tmDir :: TTDirection
    , tmPrice :: Float
    } deriving Show

data TravelGuide = TravelGuide
    { tgTitle :: String
    , tgAuthor :: String
    , tgPrice :: Float
    }

data Tool = Tool
    { tlName :: String
    , tlPrice :: Float
}

-- Class for handling items with prices
class Priceable p where
    price :: p -> Double

instance Priceable TimeMachine where
    price = realToFrac . tmPrice

instance Priceable TravelGuide where
    price = realToFrac . tgPrice

instance Priceable Tool where
    price = realToFrac . tlPrice


totalPrice :: Priceable a => [a] -> Double
totalPrice = foldr ((+) . price) 0



-- Some Data
tardis = TimeMachine {  tmCompany = "Time Lord, Inc"
                      , tmId = 1
                      , tmName = "TARDIS"
                      , tmDir = Both
                      , tmPrice = 1000000.00 
                      }

theTimemMchine = TimeMachine {  tmCompany = "The Innovation Company"
                              , tmId = 2
                              , tmName = "The Time Machine"
                              , tmDir = Both
                              , tmPrice = 250000.00
                              }
epsteinDrive = TimeMachine {  tmCompany = "Epstein Drives, Inc."
                            , tmId = 3
                            , tmName = "Eptein Drive"
                            , tmDir = Future
                            , tmPrice = 75000.00
                            }

hitchhikersGuide = TravelGuide {  tgAuthor = "Douglas Adams"
                                , tgTitle = "A Hitch Hiker's Guide to the Galaxy"
                                , tgPrice = 25.00
                                }

whovianChronicles = TravelGuide {  tgAuthor = "The Doctor"
                                 , tgTitle = "The Whovian Chronicles"
                                 , tgPrice = 19.99
                                 }

sonicScrewdriver = Tool {  tlName = "Sonic Screwdriver"
                         , tlPrice = 49.99
                        }  

