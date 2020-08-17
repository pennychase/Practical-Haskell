{-# LANGUAGE RecordWildCards #-}

module TimeMachine where

-- Data types for the Time Machine Store
data TTDirection = Past | Future | Both deriving (Show, Eq, Ord)

data TimeMachine = TimeMachine
   {  tmCompany :: String
    , tmId :: Int
    , tmName :: String
    , tmDir :: TTDirection
    , tmPrice :: Float
    }
    deriving (Show, Eq, Ord)

data TravelGuide = TravelGuide
    { tgTitle :: String
    , tgAuthors :: String
    , tgPrice :: Float
    } 
    deriving (Show, Eq, Ord)

newtype TGByPrice = TGByPrice TravelGuide deriving Eq

instance Ord TGByPrice where
    (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) =
        p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

data Tool = Tool
    { tlName :: String
    , tlPrice :: Float
    }
    deriving (Show, Eq, Ord)



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

hitchhikersGuide = TravelGuide {  tgAuthors = "Douglas Adams"
                                , tgTitle = "A Hitch Hiker's Guide to the Galaxy"
                                , tgPrice = 25.00
                                }

restaurant = TravelGuide {  tgAuthors = "Douglas Adams"
                          , tgTitle = "Restaurant at the End of the Universe"
                          , tgPrice = 25.00
                          }

whovianChronicles = TravelGuide {  tgAuthors = "The Doctor"
                                 , tgTitle = "The Whovian Chronicles"
                                 , tgPrice = 29.99
                                 }

companionMemoir = TravelGuide {   tgAuthors = "Sarah Jane"
                                , tgTitle = "Memoir of a Time Lord's Companion"
                                , tgPrice = 19.99
                                }

timeAndAgain  = TravelGuide {  tgAuthors = "Jack Finney"
                             , tgTitle = "Time and Again"
                             , tgPrice = 19.99
                            }

sonicScrewdriver = Tool {  tlName = "Sonic Screwdriver"
                         , tlPrice = 49.99
                        }  

