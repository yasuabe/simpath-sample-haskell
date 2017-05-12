module Simpath.Border where

import Control.Monad.State (State, get, put, evalState)
import Simpath.Common
import Simpath.Edge
import Data.Set (Set)
import qualified Data.Set as Set

data Border = Border { edge :: Edge, done :: Maybe Node } deriving (Show)

borders :: [Edge] -> [Border]
borders es = reverse $ evalState bordersState Set.empty
  where
    bordersState = sequence $ map fromEdge $ reverse es
    fromEdge :: Edge -> State (Set Node) Border
    fromEdge e@(Edge l _) = do used <- get
                               put $ Set.insert l used
                               return $ Border e $ justIf (Set.notMember l used) l

