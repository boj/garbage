{-# LANGUAGE BangPatterns #-}

module NaiveGame1 where

import Control.Concurrent (threadDelay)
import Control.DeepSeq
import Control.Monad (when)
import qualified Data.Sequence as DS
import Linear
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

idVector :: V3 Float
idVector = V3 1.0 1.0 1.0

data Block = Block { bLocation   :: !(V3 Float)
                   , bName       :: !String
                   , bDurability :: !Integer
                   , bTextureId  :: !Integer
                   , bBreakable  :: !Bool
                   , bVisible    :: !Bool
                   , bType       :: !Integer }
             deriving (Eq, Show)

instance NFData Block where
  rnf (Block bl bn bd bt bb bv bty) =
    rnf bl `seq` rnf bn `seq` rnf bd `seq` rnf bt `seq` rnf bb `seq` rnf bv `seq` rnf bty

mkBlock :: V3 Float -> String -> Integer -> Integer -> Bool -> Bool -> Integer -> Block
mkBlock loc nam dur tid brk vis typ =
  Block { bLocation   = loc
        , bName       = nam
        , bDurability = dur
        , bTextureId  = tid
        , bBreakable  = brk
        , bVisible    = vis
        , bType       = typ }

data EntityType = Zombie | Chicken | Exploder | TallCreepyThing deriving (Eq)

data Entity = Entity { eLocation :: !(V3 Float)
                     , eName     :: !String
                     , eHealth   :: !Integer
                     , eSpeed    :: !(V3 Float) }
              deriving (Eq, Show)

instance NFData Entity where
  rnf (Entity elo en eh es) =
    rnf elo `seq` rnf en `seq` rnf eh `seq` rnf es

mkEntity :: V3 Float -> EntityType -> Entity
mkEntity loc typ =
  case typ of
    Zombie ->
      Entity { eLocation = loc
             , eName     = "Zombie"
             , eHealth   = 50
             , eSpeed    = V3 0.5 0.0 0.5 }
    Chicken ->
      Entity { eLocation = loc
             , eName     = "Chicken"
             , eHealth   = 25
             , eSpeed    = V3 0.75 0.5 0.75 }
    Exploder ->
      Entity { eLocation = loc
             , eName     = "Exploder"
             , eHealth   = 75
             , eSpeed    = V3 0.75 0.0 0.75 }
    TallCreepyThing ->
      Entity { eLocation = loc
             , eName     = "Tall Creepy Thing"
             , eHealth   = 500
             , eSpeed    = V3 1.0 1.0 1.0 }

updateEntityPosition :: Entity -> Entity
updateEntityPosition entity =
  entity { eSpeed = idVector * eSpeed entity }

numBlocks :: Integer
numBlocks = 65536

numEntities :: Integer
numEntities = 1000

data Chunk = Chunk { cBlocks   :: !(DS.Seq Block)
                   , cEntities :: !(DS.Seq Entity)
                   , cLocation :: !(V3 Float) }
             deriving (Eq, Show)

instance NFData Chunk where
  rnf (Chunk cbs ces cl) =
    rnf cbs `seq` rnf ces `seq` rnf cl

mkChunk :: V3 Float -> Chunk
mkChunk loc =
  Chunk { cBlocks   = DS.fromList $ foldr newBlock [] [0..numBlocks]
        , cEntities = DS.fromList $ foldr newEntity [] [0..(numEntities `div` 4)]
        , cLocation = loc }
  where
    newBlock n bs = do
      let i = fromInteger n
      mkBlock (V3 i i i) ("Block: " ++ show n) 100 1 True True 1 : bs
    newEntity n es = do
      let i = fromInteger n
      mkEntity (V3 i i i) Chicken :
        mkEntity (V3 (i+2) i i) Zombie :
        mkEntity (V3 (i+3) i i) Exploder :
        mkEntity (V3 (i+4) i i) TallCreepyThing : es

processEntities :: DS.Seq Entity -> DS.Seq Entity
processEntities = fmap updateEntityPosition

loadWorld :: Integer -> DS.Seq Chunk
loadWorld chunkCount =
  DS.fromList $ foldr newChunk [] [0..chunkCount]
  where
    newChunk n cs = mkChunk (V3 (fromInteger n) 0.0 0.0) : cs

updateChunks :: V3 Float -> Integer -> DS.Seq Chunk -> (DS.Seq Chunk, Integer)
updateChunks playerLocation chunkCount chunks = do
  let ucs = fmap runChunk (DS.dropWhileL checkChunk chunks)
      dif = fromIntegral (DS.length chunks - DS.length ucs)
      ncs = foldr (\n ucs' -> ucs' DS.|> mkChunk (V3 (fromInteger n) 0.0 0.0)) ucs [chunkCount..chunkCount + dif]
  (ncs, chunkCount + dif)
  where
    checkChunk chunk = distance (cLocation chunk) playerLocation > fromInteger chunkCount
    runChunk chunk = chunk { cEntities = processEntities (cEntities chunk) }

run :: IO ()
run = do
  let chunkCount0 = 100
  putStrLn "Loading World..."
  start <- getCPUTime
  let !world0 = force $ loadWorld chunkCount0
  end <- getCPUTime
  putStrLn "FINISHED"
  putStrLn $ "Load Time: " ++ show ((end - start) `div` 1000000000) ++ " milliseconds"
  loop world0 (V3 0.0 0.0 0.0) chunkCount0
  where
    loop world1 playerLocation chunkCount = do
      start <- getCPUTime
      let playerMovement         = V3 0.1 0.0 0.0
          playerLocation'        = playerLocation ^+^ playerMovement
          !(world', chunkCount') = force $ updateChunks playerLocation' chunkCount world1
      end <- getCPUTime

      printf "%.4f\n" ((fromInteger (end - start) :: Float) / 1000000000.0) -- milliseconds

      let !delay = (end - start) `div` 1000000 -- microseconds
      when (delay < 16000) $
        threadDelay $ 16000 - fromIntegral delay

      loop world' playerLocation' chunkCount'
