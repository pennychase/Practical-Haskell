module Chapter7.AssociationRules where 

import Data.Set (Set)
import qualified Data.Set as S
import Data.List(unfoldr)
import Control.Monad
import System.Random

-- Clients
data Client = GovOrg { clientName :: String }
            | Company { clientName :: String
                      , person :: Person
                      , duty :: String 
                      }
            | Individual  { person :: Person }
            deriving (Show, Eq, Ord)

data ClientKind = KindGovOrg | KindCompany | KindIndividual
                deriving (Show, Eq, Ord)

data Person = Person { firstname :: String
                     , lastName :: String
                     , gender :: Gender
                     }
                     deriving (Show, Eq, Ord)

data Gender = Male | Female | UnknownGender
            deriving (Show, Eq, Ord)

-- Products
data Product = Product { productID :: Integer
                       , productType :: ProductType
                       }
                       deriving (Show, Eq, Ord)

data ProductType = TimeMachine | TravelGuide | Tool | Trip
                deriving (Show, Eq, Ord)

-- Purchases
data Purchase = Purchase { client :: Client
                         , products :: [Product]
                         }
                         deriving (Show, Eq, Ord)

data PurchaseInfo =   InfoClientKind ClientKind
                    | InfoClientDuty String
                    | InfoClientGender Gender
                    | InfoPurchasedProduct Integer
                    | InfoPurchasedProductType ProductType
                    deriving (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo)
                    deriving (Show, Eq, Ord)

-- Association Rules

newtype FrequentSet = FrequentSet (Set PurchaseInfo)
                deriving (Show, Eq, Ord)

data AssocRule = AssocRule (Set PurchaseInfo) (Set PurchaseInfo)
                deriving (Eq, Ord)
instance Show AssocRule where
    show (AssocRule a b) = show a ++ " => " ++ show b


productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo =
    foldr (\(Product i t) pinfos -> S.insert (InfoPurchasedProduct i) $
                                        S.insert (InfoPurchasedProductType t) pinfos) 
           S.empty

clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo client =
    case client of
        (GovOrg {}) -> S.singleton (InfoClientKind KindGovOrg)
        (Company {}) -> S.insert (InfoClientKind KindCompany) $
                            S.insert (InfoClientDuty (duty client))  S.empty
        (Individual p@(Person {}))->  S.insert (InfoClientKind KindIndividual) $
                                        S.insert (InfoClientGender (gender p)) S.empty

purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c p) = 
    Transaction $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo p


-- Apriori Algorithm

-- Compute the support for a frequent set: the number of transactions containing items in the set 
-- divided by the total number of transactions
setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) =
    let total = length trans
        f (Transaction tElts) = sElts `S.isSubsetOf` tElts
        supp = length (filter f trans)
    in fromIntegral supp / fromIntegral total

-- Compute the confidence of an association rule: the support of the antecdent and consequent
-- divided by the support of the antecedent
ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence trans (AssocRule a b) =
    setSupport trans (FrequentSet $ a `S.union` b) / setSupport trans (FrequentSet a)

-- Generate initial frequent sets of one element
generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions = noDups $ do
    Transaction t <- transactions
    e <- S.toList t
    let fs = FrequentSet $ S.singleton e
    guard $ setSupport transactions fs > minSupport
    return fs

-- noDups remove duplicates in a list by converting to a set and back again (faster than nub)
noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

-- Generate the frequent sets at level K
-- Use list monad to generate all comnbinations of pairs of frequent sets and find the ones
-- that have k-1 elements in common. If so, create new frequent set of the union of the
-- sets as long as the support is > minSupport.
generateNextLk :: Double -> [Transaction] -> (Int, [FrequentSet]) 
                                          -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk _ _ (_, []) = Nothing
generateNextLk minSupport transactions (k, lk) =
    let lk1 = noDups $ do
        FrequentSet a <- lk
        FrequentSet b <- lk
        guard $ S.size (a `S.intersection` b) == k-1
        let fs = FrequentSet $ a `S.union` b
        guard $ setSupport transactions fs > minSupport
        return fs
    in Just (lk1, (k+1, lk1))

-- Generate association rules by looking at all subsets of each frequent set
generateAssocRules :: Double -> [Transaction] -> [FrequentSet] -> [AssocRule]
generateAssocRules minConfidence transactions sets = do
    FrequentSet fs <- sets
    subset@(_:_) <- powerset $ S.toList fs
    let ssubset = S.fromList subset
        rule = AssocRule ssubset (fs `S.difference` ssubset)
    guard $ ruleConfidence transactions rule > minConfidence
    return rule

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

-- Apriori Algorithm
apriori :: Double -> Double -> [Transaction] -> [AssocRule]
apriori minSupport minConfidence transactions =
    generateAssocRules minConfidence transactions
        $ concat $ unfoldr (generateNextLk minSupport transactions)
                            (1, generateL1 minSupport transactions)


-- Some test data
-- Products
tms = Product <$> [1..5] <*> [TimeMachine]
tgs = Product <$> [10..20] <*> [TravelGuide]
tools = Product <$> [30..40] <*> [Tool]
trips = Product <$> [100..103] <*> [Trip]

--Clients
persons = (replicate 5 $ Person "Jane" "Doe" Female) ++ (replicate 5 $ Person "Jon" "Snow" Male)
govOrgs = GovOrg <$> ["NASA", "USAF", "CIA"]
companies = Company <$> ["Epstein Drive, Inc", "WormHole, Ltd", "Time Lords Company", "Quantum Leap, Inc"]
                    <*> [Person "Morgan" "Smith" Female] 
                    <*> ["Engineer", "Pilot", "Travel Writer"]
individuals = Individual <$> persons

-- makePurchase makes a single purchase
-- Takes a list of clients (could be mixed or a list of one client type) and a list
-- of homogeneous product type lists
-- Randomly selects one client. For the products, randomly number of product types and then
-- randomly selects the product types. For now just picks one of each prodoct type.
-- Example use: makePurchase govOrgs [tms, tgs, tools, trips]
-- Since we're using randomRIO, the purchase is actually IO Purchase, so need to use
-- fmap to lift pure functions into the monad, e.g., purchaseToTransaction <$> makePurchase govOrgs [tms, tgs, tools, trips]
makePurchase :: [Client] -> [[Product]] -> IO Purchase
makePurchase clients products = do
    let numProductTypes = length products
    clientIndex <- randomRIO (0, (length clients)-1)
    howManyProductTypes <- randomRIO (1, numProductTypes)
    productTypeIndex <- replicateM howManyProductTypes $ randomRIO (0, numProductTypes - 1)
    let prods = map head $ map (products !!) $ productTypeIndex
    return $ Purchase (clients !! clientIndex) prods

-- To create a list of transactions to test apriori in GHCI:
-- trans <- mapM (purchaseToTransaction <$>) purchases



