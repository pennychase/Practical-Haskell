{-# LANGUAGE RecordWildCards #-}

module Chapter2.TypeExamples where

import Data.Char

--
-- Clients
--

-- Type defintions for Clients
data Client = GovOrg  String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

data Person
  = Person String String Gender
  deriving Show

data Gender = Male | Female | Unknown deriving Show

-- Accessors
clientName :: Client -> String
clientName client =
  case client of
    GovOrg name -> name
    Company name _ _ _ -> name
    Individual (Person fn ln _) _ -> fn ++ " " ++ ln

companyName :: Client -> Maybe String
companyName client =
  case client of
    Company name _ _ _ -> Just name
    _ -> Nothing

-- Gender Count
data GenderCount = GenderCount Gender Int deriving Show

genderCount :: [Client] -> (GenderCount, GenderCount, GenderCount)
genderCount clients = genderCount' clients (GenderCount Male 0, GenderCount Female 0, GenderCount Unknown 0)

genderCount' :: [Client] -> (GenderCount, GenderCount, GenderCount) -> (GenderCount, GenderCount, GenderCount)
genderCount' [] counts = counts
genderCount' (client:clients) (males, females, unknowns) =
  case client of
    GovOrg _ -> genderCount' clients (males, females, unknowns)
    Company _ _ (Person _ _ gender) _ -> genderCount' clients
                                                      (updateCounts gender (males, females, unknowns))
    Individual (Person _ _ gender) _ -> genderCount' clients
                                                     (updateCounts gender (males, females, unknowns))
  where
    updateCounts gender (GenderCount Male m, GenderCount Female f, GenderCount Unknown u) =
      case gender of
        Male -> (GenderCount Male (m+1), GenderCount Female f, GenderCount Unknown u)
        Female -> (GenderCount Male m, GenderCount Female (f+1), GenderCount Unknown u)
        Unknown ->(GenderCount Male m, GenderCount Female f, GenderCount Unknown (u+1))

-- some clients
nasa = GovOrg "NASA"
mitre = Company "MITRE" 1001 (Person "Jason" "Providakes" Male) "CEO"
engenuity = Company "Engenuity" 1002 (Person "Laurie" "G" Female) "CEO"
john = Individual (Person "John" "Doe" Male) True
jane = Individual (Person "Jane" "Roe" Female) True
alex = Individual (Person "Alex" "King" Unknown) False

--
-- Record Version of CLients
--

data ClientR = GovOrgR { clientRName :: String }
            | CompanyR  { clientRName ::String
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String
                        }
            | IndividualR { person :: PersonR }
            deriving Show

data PersonR = PersonR  { firstName :: String
                        , lastName :: String
                        } deriving Show

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
  let newName = (toUpper initial):rest
  in p { firstName = newName }
nameInCapitals p@(PersonR { firstName = " "}) = p

-- Example Clients
nasaR = GovOrgR { clientRName = "NASA" }

mitreR = CompanyR { clientRName = "MITRE"
                  , companyId = 1001
                  , person = PersonR { firstName = "Jason"
                                    , lastName = "Providakes"
                                    }
                  , duty = "CEO"
                  }

johnjones = IndividualR { person = PersonR { firstName = "john"
                                          , lastName = "Jones"
                                           }
                  }

--
-- Time Machines
--

data TimeMachine
  = TimeMachine String Int String TTDirection Float
  deriving Show

data TTDirection = Past | Future | Both deriving Show

tmDiscount :: TimeMachine -> Float -> TimeMachine
tmDiscount (TimeMachine company id name dir price) discount =
  let newPrice = price - price * discount in (TimeMachine company id name dir newPrice)

tmsDiscount :: [TimeMachine] -> Float -> [TimeMachine]
tmsDiscount tms discount = map (\x -> tmDiscount x discount) tms


-- Time Machines Using Records

data TimeMachineR = TimeMachineR
   {  tmCompany :: String
    , tmId :: Int
    , tmName :: String
    , tmDir :: TTDirection
    , tmPrice :: Float
    } deriving Show

tmrDiscount :: TimeMachineR -> Float -> TimeMachineR
tmrDiscount t@(TimeMachineR { .. }) discount =
  let newprice = tmPrice - (tmPrice * discount)
  in t { tmPrice = newprice }

tmrDiscountMap discount = map (flip tmrDiscount discount)

tardisR = TimeMachineR { tmCompany = "Time Lord, Inc"
                      , tmId = 1
                      , tmName = "TARDIS"
                      , tmDir = Both
                      , tmPrice = 10000.00 
                      }