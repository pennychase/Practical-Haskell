{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Chapter3.Client where

import Data.List
import Data.Function

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
                deriving (Show, Eq, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     }
            deriving (Show, Eq, Ord)


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

companyDutyAnalytics :: [Client a] -> [String]
companyDutyAnalytics lis = map head . sortBy (flip (compare `on` length)) .
                           groupBy (==) $ sort . (map duty) $ filter isCompany lis

isCompany :: Client a-> Bool
isCompany (Company {  })  = True
isCompany _               = False


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
    ]
