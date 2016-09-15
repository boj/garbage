{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell, RankNTypes, TypeFamilies, MultiParamTypeClasses, RecordWildCards #-}

-- module Repa where
module Main where

import NaiveGame3 hiding (World(..), Chunk(..), mkChunk, processEntities, nloops, loadWorld, loop)
    
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Identity
import Control.Monad.State.Strict
import Criterion
import Criterion.Main
import Criterion.Types
import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Repr.Vector (V, fromVector, computeVectorP)
import Data.Bits
import Data.Int
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import Linear
    
    
-- chunks hold blockids, not actual blocks
data Chunk = Chunk { cBlocks   :: !(V.Vector BlockId)
                   , cEntities :: !(V.Vector Entity)
                   , cLocation :: !(V3 Float)
                   } deriving (Eq, Show)

    
instance NFData Chunk where rnf Chunk{} = ()
           
mkChunk :: Int -> Chunk
mkChunk p = Chunk (-- R.fromUnboxed (Z :. numBlocks) $ 
                   V.generate numBlocks fromIntegral)
                  (V.generate numEntities newEntity)
                  (V3 p' 0 0)
  where
    p' = fromIntegral p
    newEntity n =
      let i = fromIntegral n
      in  case n .&. 3 of
              0 -> mkEntity (V3 i i i) Chicken
              1 -> mkEntity (V3 i i i) Zombie
              2 -> mkEntity (V3 i i i) Exploder
              _ -> mkEntity (V3 i i i) TallCreepyThing

processEntities :: V.Vector Entity -> V.Vector Entity
processEntities = V.map updateEntityPosition
    where
        updateEntityPosition e = e { eLocation = eLocation e ^+^ entitySpeed (eType e) }

loadWorld :: World
loadWorld = World (VB.generate 256 mkBlock)
                  (fromVector (Z :. numChunks) $ VB.generate numChunks mkChunk)
                  (V3 0 0 0)
                  numChunks

data World = World { wblocks  :: !(VB.Vector Block)
                   , wchunks  :: !(Array V DIM1 Chunk)
                   , wploc    :: !(V3 Float)
                   , wcounter :: !Int
                   }

instance NFData World where
  rnf World{..} =
      rnf wblocks `seq`
      wchunks `seq`
      ()

loop :: World -> World
loop (World blocks chunks oldpp counter) = World blocks chunks' playerPosition counter'
    where
        playerPosition = oldpp ^+^ V3 0.1 0.0 0.0

        -- ASSUMPTION: only one chunk will fall out at once in this highly artificial
        -- benchmark.  The artificial use of a sequential chunk counter limits the
        -- parallelizability of this benchmark.  But if we make this assumption, which
        -- holds of the original benchmark, the limitation does not chafe.
        chunks' :: Array V DIM1 Chunk
        chunks' = runIdentity $ computeVectorP $ R.map snd pairs

        -- FIXME: this is horribly innefficient.  Need a one pass fold/map operation in Repa:
        incr  = (runIdentity $ R.sumP $ R.map fst pairs) R.! Z
        counter' = counter + incr
                
        pairs :: Array D DIM1 (Int,Chunk)
        pairs = R.map updateChunk chunks

        updateChunk :: Chunk -> (Int,Chunk)
        updateChunk c = let !d = distance (cLocation c) playerPosition
                        in  if d > distCap
                            then (1, mkChunk (counter+1))
                            else (0, c { cEntities = processEntities (cEntities c) })

nloops :: World -> Int64 -> World
nloops !w 0 = w
nloops !w n = nloops (loop w) (n - 1)
              
main :: IO ()
main = do
  putStrLn "Repa: Running Repa version with unboxed Entity Vector, parallel update."
  putStrLn "Loading World..."
  tm1 <- timeitMS $ evaluate $ rnf loadWorld 
  putStrLn $ "Finished loading in "++tm1             
  tm2 <- timeitMS $ evaluate $ rnf loadWorld 
  putStrLn $ "A second RNF traversal took: "++tm2

  defaultMain [ bench "main loop" $ 
                     Benchmarkable (void . evaluate . nloops loadWorld) ]


