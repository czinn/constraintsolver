module Constraint where

import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Debug.Trace (trace)

data Constraint v t = Constraint { vars :: [v]
                                 , predicate :: [t] -> Bool
                                 }

constraintSatisfiable :: (Ord v) => Map v [t] -> Constraint v t -> Bool
constraintSatisfiable doms Constraint{vars=v, predicate=p} =
  let assignments = sequence . (map ((!) doms)) $ v in
  not (null assignments) && (foldr (||) False . map p $ assignments)

arc :: (Ord v, Eq t) => Map v [t] -> [Constraint v t] -> (Map v [t], [Constraint v t])
arc doms constraints =
  let (doms', constraints') =
        foldl (\(d, cs) c ->
          let Constraint {vars=vars, predicate=p} = c in
            let d' = foldl (\d' v ->
                            Map.adjust (filter (\x->constraintSatisfiable (Map.insert v [x] d') c)) v d'
                          ) d vars in
            (d', c:cs)
          ) (doms, []) constraints
    in
  if doms == doms' then (doms', constraints') else arc doms' constraints'

solve :: (Ord v, Eq t, Show v, Show t) => [v] -> (Map v [t]) -> [Constraint v t] -> [t]
solve vars doms constraints =
  case aux vars doms constraints of
    Just x -> x
    Nothing -> error "no solution found"
  where
    aux vars doms constraints =
      let (doms', constraints') = arc doms constraints
          domSizes = map length $ Map.elems doms' in
      if foldr ((&&) . ((==) 1)) True domSizes
        then Just (map (head . (!) doms') vars)
        else if foldr ((||) . ((==) 0)) False domSizes
          then Nothing
          else
            let (v, ts) = head . Map.assocs . Map.filter ((<) 1 . length) $ doms' in
              case aux vars (Map.insert v [head ts] doms') constraints of
                Just x -> Just x
                Nothing -> aux vars (Map.insert v (tail ts) doms') constraints

binaryConstraint :: [v] -> (t -> t -> Bool) -> Constraint v t
binaryConstraint vars f = Constraint { vars=vars, predicate=(\x->f (head x) (head . tail $ x)) }

notEqual :: (Eq t) => [v] -> [Constraint v t]
notEqual vars = aux vars
  where aux (hd:tl) = (do val <- tl
                          return $ binaryConstraint [hd,val] (/=))
                     ++ aux tl
        aux [] = []

equalDoms :: (Ord v) => [v] -> [t] -> Map v [t]
equalDoms vars dom = Map.fromSet (const dom) . Set.fromList $ vars
