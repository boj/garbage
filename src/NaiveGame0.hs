{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module NaiveGame0 where

import Control.Concurrent (threadDelay)
import Control.DeepSeq
import Control.Monad (when)
import Data.List ((\\), foldl', mapAccumR)
import GHC.Generics (Generic)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

data Vector = Vector !Float !Float !Float deriving (Eq, Generic)

instance NFData Vector

idVector :: Vector
idVector = Vector 1.0 1.0 1.0

vMul :: Vector -> Vector -> Vector
vMul (Vector ax ay az) (Vector bx by bz) = Vector (ax * bx) (ay * by) (az * bz)

vAdd :: Vector -> Vector -> Vector
vAdd (Vector ax ay az) (Vector bx by bz) = Vector (ax + bx) (ay + by) (az + bz)

vSub :: Vector -> Vector -> Vector
vSub (Vector ax ay az) (Vector bx by bz) = Vector (ax - bx) (ay - by) (az - bz)

getDistance :: Vector -> Vector -> Float
getDistance a b = do
  let (Vector sx sy sz) = a `vSub` b
  sqrt (sx * sx + sy * sy + sz * sz)

data Block = Block { bLocation   :: !Vector
                   , bName       :: !String
                   , bDurability :: !Integer
                   , bTextureId  :: !Integer
                   , bBreakable  :: !Bool
                   , bVisible    :: !Bool
                   , bType       :: !Integer }
             deriving (Eq, Generic)

instance NFData Block

mkBlock :: Vector -> String -> Integer -> Integer -> Bool -> Bool -> Integer -> Block
mkBlock loc nam dur tid brk vis typ =
  Block { bLocation   = loc
        , bName       = nam
        , bDurability = dur
        , bTextureId  = tid
        , bBreakable  = brk
        , bVisible    = vis
        , bType       = typ }

data EntityType = Zombie | Chicken | Exploder | TallCreepyThing deriving (Eq)

data Entity = Entity { eLocation :: !Vector
                     , eName     :: !String
                     , eHealth   :: !Integer
                     , eSpeed    :: !Vector }
              deriving (Eq, Generic)

instance NFData Entity

mkEntity :: Vector -> EntityType -> Entity
mkEntity loc typ =
  case typ of
    Zombie ->
      Entity { eLocation = loc
             , eName     = "Zombie"
             , eHealth   = 50
             , eSpeed    = Vector 0.5 0.0 0.5 }
    Chicken ->
      Entity { eLocation = loc
             , eName     = "Chicken"
             , eHealth   = 25
             , eSpeed    = Vector 0.75 0.5 0.75 }
    Exploder ->
      Entity { eLocation = loc
             , eName     = "Exploder"
             , eHealth   = 75
             , eSpeed    = Vector 0.75 0.0 0.75 }
    TallCreepyThing ->
      Entity { eLocation = loc
             , eName     = "Tall Creepy Thing"
             , eHealth   = 500
             , eSpeed    = Vector 1.0 1.0 1.0 }

updateEntityPosition :: Entity -> Entity
updateEntityPosition entity =
  entity { eSpeed = idVector `vMul` eSpeed entity }

numBlocks :: Integer
numBlocks = 65536

numEntities :: Integer
numEntities = 1000

data Chunk = Chunk { cBlocks   :: ![Block]
                   , cEntities :: ![Entity]
                   , cLocation :: !Vector }
             deriving (Eq, Generic)

instance NFData Chunk

mkChunk :: Vector -> Chunk
mkChunk loc =
  Chunk { cBlocks   = foldl' newBlock [] [0..numBlocks]
        , cEntities = foldl' newEntity [] [0..(numEntities `div` 4)]
        , cLocation = loc }
  where
    newBlock bs n = do
      let i = fromInteger n
      mkBlock (Vector i i i) ("Block: " ++ show n) 100 1 True True 1 : bs
    newEntity es n = do
      let i = fromInteger n
      mkEntity (Vector i i i) Chicken :
        mkEntity (Vector (i+2) i i) Zombie :
        mkEntity (Vector (i+3) i i) Exploder :
        mkEntity (Vector (i+4) i i) TallCreepyThing : es

processEntities :: [Entity] -> [Entity]
processEntities = fmap updateEntityPosition

loadWorld :: Integer -> [Chunk]
loadWorld chunkCount =
  foldl' newChunk [] [0..chunkCount]
  where
    newChunk cs n = mkChunk (Vector (fromInteger n) 0.0 0.0) : cs

updateChunks :: Vector -> Integer -> [Chunk] -> ([Chunk], Integer)
updateChunks playerLocation chunkCount chunks = do
  let (rcs, cs) = mapAccumR runChunk [] chunks
      rcl       = fromIntegral (length rcs)
      ncs       = if rcl > 0
                  then foldl' (\ncs' n -> mkChunk (Vector (fromInteger n) 0.0 0.0) : ncs') [] [0..rcl]
                  else []
  (ncs ++ (cs \\ rcs), chunkCount + fromIntegral rcl)
  where
    runChunk rcs chunk = do
      let c = chunk { cEntities = processEntities (cEntities chunk) }
      if getDistance (cLocation c) playerLocation > fromInteger chunkCount
      then (c : rcs, c)
      else (rcs, c)

run :: IO ()
run = do
  let chunkCount0 = 100
  putStrLn "Loading World..."
  start <- getCPUTime
  let !world0 = force $ loadWorld chunkCount0
  end <- getCPUTime
  putStrLn "FINISHED"
  putStrLn $ "Load Time: " ++ show ((end - start) `div` 1000000000) ++ " milliseconds"
  loop world0 (Vector 0.0 0.0 0.0) chunkCount0
  where
    loop world1 playerLocation chunkCount = do
      start <- getCPUTime
      let playerMovement         = Vector 0.1 0.0 0.0
          playerLocation'        = playerLocation `vAdd` playerMovement
          !(world', chunkCount') = force $ updateChunks playerLocation' chunkCount world1
      end <- getCPUTime

      printf "%.4f\n" ((fromInteger (end - start) :: Float) / 1000000000.0) -- milliseconds

      let !delay = (end - start) `div` 1000000 -- microseconds
      when (delay < 16000) $
        threadDelay $ 16000 - fromIntegral delay

      loop world' playerLocation' chunkCount'
