{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Chapter3.Exercises where

import Chapter2.TypeExamples

-- Exercise 3.2

filterOnes :: [Integer] -> [Integer]
filterOnes = filter (== 1)

filterANumber :: (Num a, Eq a) =>  a -> [a] -> [a]
filterANumber n = filter (== n)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = filter $ not . p

filterGovOrgRs :: [ClientR] -> [ClientR]
filterGovOrgRs = filter isGovOrgR
    where
        isGovOrgR (GovOrgR { .. }) = True
        isGovOrgR _                = False

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter (\client -> case client of
                                        (GovOrg _) -> True
                                        _       -> False
                        )

filterGovOrgs' :: [ClientR] -> [ClientR]
filterGovOrgs' = filter (\case (GovOrgR { .. }) -> True ;
                                _               -> False
                        )
