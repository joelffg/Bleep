{-# LANGUAGE OverloadedStrings #-}
module Bleep where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.List as L
import Natural

type Signature = Set.Set Connective

-- (Nome, Aridade)
type Connective = (String, Natural)

neg' :: Connective
neg' = ("¬", 1)

and' :: Connective
and' = ("^", 2)

or' :: Connective
or' = ("v", 2)

signature :: Signature
signature = Set.insert neg' $ Set.insert or' $ Set.singleton and'

type Variable = Char

type Language = (Set.Set Connective, Set.Set Variable)

data Formula = Atomic Variable
             | Compound Connective [Formula]
    deriving (Eq)

instance Show Formula where
    show (Atomic v) = [v]
    show (Compound (nome, n) vs)
        | n == 1    = nome ++ show (L.head vs)
        | n == 2    = let (x:y:_) = vs in "(" ++ show x ++ " " ++ nome ++ " " ++ show y ++ ")"
        | otherwise = nome ++ T.unpack (T.replace "]" ")" (T.replace "[" "(" (T.pack (show vs))))

instance Ord Formula where
    (Atomic x) <= (Atomic y) = x <= y
    (Atomic _) <= _          = True
    (Compound (nome,_) _) <= (Compound (nome',_) _) = nome <= nome'
    (Compound _ _) <= _ = False

validFormula :: Language -> Formula -> Bool
validFormula lang (Atomic c) = Set.member c (snd lang)
validFormula lang (Compound c@(_, aridade) fs) = if Set.member c (fst lang) && aridade == L.genericLength fs then and (map (validFormula lang) fs) else False

var :: Formula -> Set.Set Variable
var (Atomic v) = Set.singleton v
var (Compound _ fs) = Set.unions (map var fs)

sub :: Formula -> Set.Set Formula
sub f@(Atomic _) = Set.singleton f
sub f@(Compound _ fs) = Set.union (Set.singleton f) (Set.unions (map sub fs))

head :: Formula -> Connective
head (Atomic _) = error $ "Atomic formula don't have connecives"
head (Compound c _) = c

substitution :: Variable -> Formula -> Formula -> Formula
substitution v f f'@(Atomic v') = if v == v' then f else f'
substitution v f (Compound c fs) = Compound c (map (substitution v f) fs)

--translation :: 
