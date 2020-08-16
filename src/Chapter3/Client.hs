{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TransformListComp #-}

module Chapter3.Client where

import Data.List
import Data.Function
import GHC.Exts

data Client i = GovOrg { clientId :: i
                       , clientName :: String
                       }
               | Company { clientId :: i
                         , clientName :: String
                         , person :: Person
                         , duty :: String
                         }
                | Individual { clientId :: i
                             , person :: Person
                             }
                -- deriving (Show, Eq, Ord)     -- for Chapter 3
                deriving (Show)                 -- for Chapter 4

data Person = Person { firstName :: String
                     , lastName :: String
                     }
                    -- deriving (Show, Eq, Ord)     -- for Chapter 3
                    deriving (Show)                 -- for Chapter 4


filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs = filter (\case (GovOrg { .. }) -> True ;
                              _               -> False
                        )

-- compareClient orders clients so that GovOrgs and Companies appear before Individuals, (ordered
-- by clientName) and Individuals are ordered by lastName then firstName
compareClient :: Client a -> Client a -> Ordering
compareClient (Individual { person = p1 }) (Individual { person = p2 }) =
    case compareLastNames of
        EQ  -> compare (firstName p1) (firstName p2)
        _   -> compareLastNames
    where
        compareLastNames = compare (lastName p1) (lastName p2)
compareClient (Individual { }) _    = GT
compareClient _ (Individual { } )   = LT
compareClient c1 c2 = compare (clientName c1) (clientName c2)

-- companyDuty Analytics
-- Analytic that produces a list of duties found in the different company records  sorted by number
-- of duties
companyDutyAnalytics :: [Client a] -> [String]
companyDutyAnalytics lis = map (duty . head) . sortBy (flip (compare `on` length)) .
                           groupBy ((==) `on` duty) . sortBy (compare `on` duty) $ filter isCompany lis    

isCompany :: Client a-> Bool
isCompany (Company {  })  = True
isCompany _               = False

-- Another version that pulls out duty first and returns a list of tuples of the duties and
-- number of individuals with the duties
companyDutyAnalytics' :: [Client a] -> [(String, Int)]
companyDutyAnalytics' lis = 
    zip (map head duties) (map length duties)
    where
        duties = sortBy (flip (compare `on` length)) .
                        groupBy (==) . sort . map duty  $ filter isCompany lis


companyAnalytics :: [Client a] -> [(String, [(Person, String)])]
companyAnalytics clients = [ (the clientName, zip person duty)
                             | client@(Company { .. }) <- clients
                             , then sortWith by duty
                             , then group by clientName using groupWith
                             , then sortWith by length client
                             ]

--
-- Sample data
--
listOfClients =
    [ Individual 2 (Person "H. G." "Wells")
    , GovOrg 3 "NTTF"  -- National Time Travel Foundation
    , Company 4 "Wormhole Inc." (Person "Karl" "Schwartzchild") "Physicist"
    , Individual 5 (Person "The" "Doctor")
    , Individual 6 (Person "Melody" "Pond")
    , Company 7 "Gallafrey Devices Inc." (Person "River" "Song") "Time Lord"
    , Company 8 "Surrey Machines Inc" (Person "Time" "Traveler") "Scientist"
    , Company 9 "UConn Time Travel" (Person "Ronald" "Mallet") "Scientist"
    , Company 4 "Wormhole Inc." (Person "Robert" "Epstein") "Engineer"
    , Company 4 "Wormhole Inc." (Person "Fred" "Rogers") "Physicist"
    ]
