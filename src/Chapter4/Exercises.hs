{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter4.Exercises where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Random

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