{-# LANGUAGE FlexibleContexts #-}
module Simpath.Frontier where

import Control.Applicative
import Control.Monad
import Control.Monad.State (State, state, get, put, runState)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Simpath.Common
import Simpath.Edge (Edge, Node)
import qualified Simpath.Edge as E
import Simpath.Border (Border)
import qualified Simpath.Border as B

type Used  = Set Node
type Edges = Set Edge
data Frontier = Frontier { edges :: Edges, used :: Used } deriving (Show, Eq, Ord)

initial :: Frontier
initial = Frontier (Set.singleton E.start) (Set.empty)

modify :: (Edges -> Edges) -> (Used -> Used) -> Frontier -> Frontier
modify f g = Frontier <$> f . edges <*> g . used

add :: Edge -> Frontier -> Maybe Frontier
add e@(E.Edge l r) fr@(Frontier _ used) =
    justIf (notUsed e && E.isOpen e') $ modify (Set.insert e') id fr'
  where (e', fr')     = runState (connect l r >>= connect r . E.opposite r) fr
        notUsed       = not . E.either isUsed where isUsed = flip Set.member used
        connect n1 n2 = state (mapOrElse <$> step <*> (,)(E.edge n1 n2) <*> find)
          where find      = E.find n1 . edges
                step fr e = (E.connect n2 n1 e, update fr)
                  where update = modify (Set.delete e) (Set.insert n1)

proceed :: Border -> Frontier -> (Maybe Frontier, Maybe Frontier)
proceed (B.Border edge done) = (,) <$> proceedHi <*> proceedLo
  where proceedHi = add edge >=> proceedLo
        proceedLo = mapOrElse (\d -> justIf <$> not . contains d <*> removeUsed d) Just $ done
          where removeUsed = modify id . Set.delete
                contains n = Foldable.any (E.contains n) . edges

