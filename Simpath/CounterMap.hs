module Simpath.CounterMap where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import Data.Function
import qualified Simpath.Frontier as F
import Simpath.Frontier (Frontier)
import Simpath.Border (Border)

type CounterMap = Map Frontier Integer

merge :: Integer -> CounterMap -> Frontier -> CounterMap
merge cnt cm fr = Map.insertWith (+) fr cnt cm

proceedAll :: Border -> CounterMap -> CounterMap
proceedAll b = Foldable.foldl f Map.empty . Map.toList
  where f cm (fr, cnt) = on (.) recount lo hi cm
          where (hi, lo) = F.proceed b fr
                recount  = flip $ Foldable.foldl $ merge cnt

countPaths :: [Border] -> Integer
countPaths = headCount . foldl (flip proceedAll) initialMap
  where initialMap = Map.singleton F.initial 1
        headCount  = head . Map.elems

