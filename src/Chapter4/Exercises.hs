{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter4.Exercises where
    
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Random

import BinaryTree

-- For timing
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

import Chapter3.Client

-- Exercise 4.2: Write Map library functions in terms of M.alter

insert' :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert' k newVal m = M.alter (\case _ -> (Just newVal)) k  m

delete' :: Ord k => k -> M.Map k a -> M.Map k a
delete' k m = M.alter (\case _ -> Nothing) k m

adjust' :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust' f k m = M.alter (\case (Just x) -> (Just (f x)); Nothing -> Nothing) k m


-- Exercise 4.3: Classifying Clients

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving (Show, Enum, Eq, Ord)

-- Implementation 1: traverse list of clients and classify each element
classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients clients = 
    foldr classifyClient M.empty clients
    where
        classifyClient :: Client Integer -> M.Map ClientKind (S.Set (Client Integer)) -> M.Map ClientKind (S.Set (Client Integer))
        classifyClient c m =
            case c of
                GovOrg {} -> M.insertWith S.union GovOrgKind (S.singleton c) m
                Company {} -> M.insertWith S.union CompanyKind (S.singleton c) m
                Individual {} -> M.insertWith S.union IndividualKind (S.singleton c) m

-- Implementation 2: Divide list into three lists of the three client kinds. Convert lists to sets. 
-- Create map by inserting ClientKind/set
classifyClients' :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients' clients =
    M.insert GovOrgKind govs . M.insert CompanyKind comps $ M.singleton IndividualKind indivs
    where
        splitByClient = foldr splitter [[], [], []]
        splitter client [gs, cs, is] =
            case client of
                GovOrg {} -> [client:gs, cs, is]
                Company {} -> [gs, client:cs, is]
                Individual {} -> [gs, cs, client:is]
        [govs, comps, indivs] = map S.fromList (splitByClient clients)

numberOfClientKind :: ClientKind -> M.Map ClientKind (S.Set (Client Integer)) -> Maybe Int
numberOfClientKind ck m = liftM (length . S.toList) $ M.lookup ck m


-- For testing performance of the two implementations, we want to generate large number of random clients.
-- genClients will produce a list IO [ClientKind] according to weights (0.3 for each ClientKind will 
-- approximate a uniform distribution).
-- We can then use "liftM (take N)" to get N random clientKind, which in turn is used to generate actual
-- clients:
--    cks = genCLients 0.3 0.3 0.3
--    liftM genClientList $ liftM (take 20) cks
-- The liftM lifts values from the monad context, so we can apply pure functions

genClients :: Rational -> Rational -> Rational -> IO [ClientKind]
genClients  govW compW indivW  = do
    someClientTypes <- evalRandIO . sequence . repeat . fromList $ [  (GovOrgKind, govW)
                                                                    , (CompanyKind, compW)
                                                                    , (IndividualKind, indivW)]
    return someClientTypes

genClientList :: [ClientKind] -> [Client Integer]
genClientList clients =
    map genClient (zip clients [ 1 .. ])
        where
            genClient (clientKind, i) = 
                case clientKind of
                    GovOrgKind -> GovOrg { clientId = i, clientName = "Govt Agency" }
                    CompanyKind -> Company {  clientId = i
                                            , clientName = "ACME Inc."
                                            , person = Person { firstName = "Jordan", lastName = "Jones" }
                                            , duty = "Staff"
                                           }
                    IndividualKind -> Individual { clientId = i
                                                 , person = Person { firstName ="Alex", lastName = "Roe"}
                                                 }  

-- timing
main :: IO ()
main = do
    let clientsU = genClients 0.3 0.3 0.3
    let sampleU = liftM genClientList $ liftM (take 100000) clientsU
    let clientsN = genClients 0.1 0.8 0.4
    let sampleN = liftM genClientList $ liftM (take 100000) clientsN
    print "Timing for classifyClients: 100000 clients uniform"
    start <- getTime Monotonic
    evaluate (liftM classifyClients sampleU) 
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end
    print "Timing for classifyClients' 100000 clients uniform"
    start <- getTime Monotonic
    evaluate (liftM classifyClients' sampleU) 
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end
    print "Timing for classifyClients: 100000 clients non-uniform"
    start <- getTime Monotonic
    evaluate (liftM classifyClients sampleN) 
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end
    print "Timing for classifyClients' 100000 clients non-uniform"
    start <- getTime Monotonic
    evaluate (liftM classifyClients' sampleN) 
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end   

-- Exercise 4.4 is in time-machine package



-- Exercise 4.5

-- Define our own Eq Person and Eq (Client i) instances
instance Eq Person where
   p1 == p2 = 
       firstName p1 == firstName p2 &&
       lastName p1 == lastName p2

instance Eq i => Eq (Client i) where
    g1@(GovOrg { }) == g2@(GovOrg { }) =
        clientId g1 == clientId g2 &&
        clientName g1 == clientName g2
    c1@(Company { }) == c2@(Company { }) =
        clientId c1 == clientId c2 &&
        clientName c1 == clientName c2 &&
        person c1 == person c2 &&
        duty c1 == duty c2
    i1@(Individual { }) == i2@(Individual { }) =
        clientId i1 == clientId i2 &&
        person i1 == person i2
    _ == _ = False

getPersonName :: Person -> String
getPersonName p = lastName p ++ ", " ++ firstName p

getClientName :: Client i -> String
getClientName client =
    case client of
        g@(GovOrg {}) -> clientName g
        c@(Company {}) -> clientName c
        i@(Individual {}) -> getPersonName (person i)

-- Write our own implementation of Ord for Client i
-- First compare on clientName
-- If equal, order by individual, company, government
-- For companies, further order by duty and person
instance (Eq i, Ord i) => Ord (Client i) where
    compare client1 client2 =
        case compare (getClientName client1) (getClientName client2) of
            LT -> LT
            GT -> GT
            EQ -> case (client1, client2) of
                (Individual {}, _) -> LT
                (_, Individual {}) -> GT
                (GovOrg {}, _) -> GT
                (_, GovOrg {}) -> LT
                (c1@(Company {}), c2@(Company {})) ->
                    case compare (duty c1) (duty c2) of
                        LT -> LT
                        GT -> GT
                        EQ -> compare (getPersonName (person c1)) (getPersonName (person c2))

-- Exercise 4.8

data Maybe' a = Just' a | Nothing' deriving Show

instance Functor Maybe' where
    fmap f (Just' a)    = Just' (f a)
    fmap _ Nothing'     = Nothing'

instance Functor BinaryTree2 where
    fmap f Leaf2 = Leaf2
    fmap f (Node2 v l r) = Node2 (f v) (fmap f l) (fmap f r)

-- Exercise 4.9

instance Foldable BinaryTree2 where
    foldr f i Leaf2 = i
    foldr f i (Node2 v l r) = foldr f (f v (foldr f i r)) l

    foldl f i Leaf2 = i
    foldl f i (Node2 v l r) = foldl f (f (foldl f i l) v) r

-- Use to define traversals:

preorderBT2 :: BinaryTree2 a -> [a]
preorderBT2 t = foldr (:) [] t

    