{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module NaiveGame2 (run, Block(..), Entity(..), entityName, entitySpeed, Chunk(..), World(..)) where

import Control.DeepSeq
import Control.Exception (evaluate)
import GHC.Generics (Generic)
import Linear
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Control.Monad.State.Strict
import Data.Bits
import Data.Word
import Data.Int
import Criterion
import Criterion.Types
import Criterion.Main
import System.Clock

data Block = Block { bLocation   :: !(V3 Float)
                   , bName       :: !String
                   , bDurability :: !Int
                   , bTextureId  :: !Int
                   , bBreakable  :: !Bool
                   , bVisible    :: !Bool
                   , bType       :: !Int
                   } deriving (Eq, Generic, Show)

instance NFData Block

mkBlock :: Int -> Block
mkBlock n = Block (V3 p p p) ("Block: " ++ show n) 100 1 True True 1
    where
        p = fromIntegral n

data EntityType = Zombie
                | Chicken
                | Exploder
                | TallCreepyThing
                deriving (Eq, Show, Generic)

data Entity = Entity { eLocation :: !(V3 Float)
                     , eHealth   :: !Int
                     , eType     :: !EntityType
                     } deriving (Eq, Generic, Show)

instance NFData EntityType
instance NFData Entity

entityName :: EntityType -> String
entityName e = case e of
                   TallCreepyThing -> "Tall Creepy Thing"
                   _ -> show e

entitySpeed :: EntityType -> V3 Float
entitySpeed e = case e of
                    Zombie          -> V3 0.5 0.0 0.5
                    Chicken         -> V3 0.75 0.5 0.75
                    Exploder        -> V3 0.75 0.0 0.75
                    TallCreepyThing -> V3 1.0 1.0 1.0

entityBaseHealth :: EntityType -> Int
entityBaseHealth e = case e of
                         Zombie          -> 50
                         Chicken         -> 25
                         Exploder        -> 75
                         TallCreepyThing -> 500

mkEntity :: V3 Float -> EntityType -> Entity
mkEntity loc typ = Entity { eLocation = loc
                          , eHealth = entityBaseHealth typ
                          , eType = typ
                          }

numBlocks :: Int
numBlocks = 65536

numEntities :: Int
numEntities = 1000

numChunks :: Int
numChunks = 100
              
type BlockId = Word8

-- chunks hold blockids, not actual blocks
data Chunk = Chunk { cBlocks   :: !(VP.Vector BlockId)
                   , cEntities :: !(V.Vector Entity)
                   , cLocation :: !(V3 Float)
                   } deriving (Eq, Generic, Show)

instance NFData Chunk

mkChunk :: Int -> Chunk
mkChunk p = Chunk (VP.generate numBlocks fromIntegral)
                  (V.generate numEntities newEntity)
                  (V3 p' p' p')
  where
    p' = fromIntegral p
    newEntity n =
      let i = fromIntegral n
      in  case n .&. 3 of
              0 -> mkEntity (V3 i i i) Chicken
              1 -> mkEntity (V3 i i i) Zombie
              2 -> mkEntity (V3 i i i) Exploder
              _ -> mkEntity (V3 i i i) TallCreepyThing

processEntities :: Functor f => f Entity -> f Entity
processEntities = fmap updateEntityPosition
    where
        updateEntityPosition e = e { eLocation = eLocation e ^+^ entitySpeed (eType e) }

loadWorld :: World
loadWorld = World (V.generate 256 mkBlock)
                  (V.generate 100 mkChunk)
                  (V3 0 0 0)
                  numChunks

data World = World { wblocks  :: !(V.Vector Block)
                   , wchunks  :: !(V.Vector Chunk)
                   , wploc    :: !(V3 Float)
                   , wcounter :: !Int
                   } deriving (Generic)

instance NFData World

loop :: World -> World
loop (World blocks chunks oldpp counter) = World blocks chunks' playerPosition counter'
    where
        playerPosition = oldpp ^+^ V3 0.1 0.0 0.0
        (chunks', counter') = runState (traverse updateChunk chunks) counter
        newId :: State Int Int
        newId = do
            x <- get
            let !x' = x + 1
            put x'
            return x'
        updateChunk :: Chunk -> State Int Chunk
        updateChunk c = let !d = distance (cLocation c) playerPosition
                        in  if d > 100
                                then fmap mkChunk newId
                                else return (c { cEntities = processEntities (cEntities c) })

nloops :: World -> Int64 -> World
nloops !w 0 = w
nloops !w n = nloops (loop w) (n - 1)

run :: IO ()
run = do
  let 
  putStrLn "Loading World..."
  start <- getTime MonotonicRaw
  evaluate $ rnf loadWorld 
  end <- getTime MonotonicRaw
  putStrLn $ "FINISHED loading in "++
             show (fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1000.0)++" ms"
  defaultMain [ bench "main loop" $ 
                     Benchmarkable (void . evaluate . nloops loadWorld) ]
