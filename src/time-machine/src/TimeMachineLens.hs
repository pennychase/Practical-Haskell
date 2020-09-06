{-# LANGUAGE TemplateHaskell #-}

module TimeMachineLens where

import Lens.Micro.Platform

-- Data types for the Time Machine Store
data TTDirection = Past | Future | Both deriving (Show, Eq, Ord)

data TimeMachine = TimeMachine
   {  _tmCompany :: String
    , _tmId :: Int
    , _tmName :: String
    , _tmDir :: TTDirection
    , _tmPrice :: Float
    }
    deriving (Show, Eq, Ord)

data TravelGuide = TravelGuide
    { _tgTitle :: String
    , _tgAuthors :: String
    , _tgPrice :: Float
    } 
    deriving (Show, Eq, Ord)

data Tool = Tool
    { _tlName :: String
    , _tlPrice :: Float
    }
    deriving (Show, Eq, Ord)

makeLenses ''TimeMachine
makeLenses ''TravelGuide
makeLenses ''Tool

newtype TGByPrice = TGByPrice TravelGuide deriving Eq

instance Ord TGByPrice where
    (TGByPrice tg1) <= (TGByPrice tg2) =
        tg1^.tgPrice < tg2^.tgPrice || 
        (tg1^.tgPrice == tg2^.tgPrice &&
            (tg1^.tgTitle < tg2^.tgTitle || tg1^.tgTitle == tg2^.tgTitle && tg1^.tgAuthors <= tg2^.tgAuthors))
    

-- Class for handling items with prices
class Priceable p where
    price :: p -> Double
    upDatePrice :: p -> Double -> p

instance Priceable TimeMachine where
    price tm = realToFrac $ tm^.tmPrice
    upDatePrice tm amt = tm & tmPrice +~ (realToFrac amt)

instance Priceable TravelGuide where
    price tg = realToFrac $ (tg^.tgPrice)
    upDatePrice tg amt = tg & tgPrice +~ (realToFrac amt)

instance Priceable Tool where
    price tl = realToFrac $ (tl^.tlPrice)
    upDatePrice tl amt = tl & tlPrice +~ (realToFrac amt)


totalPrice :: Priceable a => [a] -> Double
totalPrice = foldr ((+) . price) 0

-- Price Change
priceChange :: Priceable a => Double -> a -> a
priceChange amt item = upDatePrice item amt 

percentChange :: Priceable a => Double -> a -> a
percentChange percent item  =
    let change = (price item) * percent
    in priceChange change item

-- Some Data
tardis = (TimeMachine "Time Lord, Inc" 1 "TARDIS" Both 10000000.00)

theTimeMachine = (TimeMachine "The Innovation Company" 2 "The Time Machine" Both 250000.00)

epsteinDrive = (TimeMachine "Epstein Drives, Inc." 3 "Rocinate" Future 75000.00)

hitchhikersGuide = (TravelGuide "Douglas Adams" "A Hitch Hiker's Guide to the Galaxy" 25.00)
 
restaurant = (TravelGuide "Douglas Adams" "Restaurant at the End of the Universe" 25.00)

whovianChronicles = (TravelGuide "The Doctor" "The Whovian Chronicles" 29.99)

companionMemoir = (TravelGuide "Sarah Jane" "Memoir of a Time Lord's Companion" 19.99)

timeAndAgain  = (TravelGuide "Jack Finney" "Time and Again" 19.99)

sonicScrewdriver = (Tool "Sonic Screwdriver" 49.99)


