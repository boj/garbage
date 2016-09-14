{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module NaiveGame0Criterion where

import Control.DeepSeq
import Criterion
import Criterion.Main
import Data.List ((\\), foldl', mapAccumR)
import Data.Text (Text, pack, append)
import GHC.Generics (Generic)

data Vector = Vector !Float !Float !Float deriving (Eq, Generic)

instance NFData Vector

mkVector :: Float -> Float -> Float -> Vector
mkVector = Vector

idVector :: Vector
idVector = mkVector 1.0 1.0 1.0

vMul :: Vector -> Vector -> Vector
vMul (Vector ax ay az) (Vector bx by bz) =
  Vector (ax * bx) (ay * by) (az * bz)

vAdd :: Vector -> Vector -> Vector
vAdd (Vector ax ay az) (Vector bx by bz) =
  Vector (ax + bx) (ay + by) (az + bz)

vSub :: Vector -> Vector -> Vector
vSub (Vector ax ay az) (Vector bx by bz) =
  Vector (ax - bx) (ay - by) (az - bz)

getDistance :: Vector -> Vector -> Float
getDistance a b =
  sqrt (sx * sx + sy * sy + sz * sz)
  where
    (Vector sx sy sz) = a `vSub` b

data Block = Block { bLocation   :: !Vector
                   , bName       :: !Text
                   , bDurability :: !Int
                   , bTextureId  :: !Int
                   , bBreakable  :: !Bool
                   , bVisible    :: !Bool
                   , bType       :: !Int }
             deriving (Eq, Generic)

instance NFData Block

mkBlock :: Vector -> Text -> Int -> Int -> Bool -> Bool -> Int -> Block
mkBlock loc nam dur tid brk vis typ =
  Block { bLocation   = loc
        , bName       = nam
        , bDurability = dur
        , bTextureId  = tid
        , bBreakable  = brk
        , bVisible    = vis
        , bType       = typ }

data EntityType = Zombie | Chicken | Exploder | TallCreepyThing deriving (Eq, Generic)

instance NFData EntityType

data Entity = Entity { eLocation :: !Vector
                     , eName     :: !Text
                     , eHealth   :: !Int
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
             , eSpeed    = mkVector 0.5 0.0 0.5 }
    Chicken ->
      Entity { eLocation = loc
             , eName     = "Chicken"
             , eHealth   = 25
             , eSpeed    = mkVector 0.75 0.5 0.75 }
    Exploder ->
      Entity { eLocation = loc
             , eName     = "Exploder"
             , eHealth   = 75
             , eSpeed    = mkVector 0.75 0.0 0.75 }
    TallCreepyThing ->
      Entity { eLocation = loc
             , eName     = "Tall Creepy Thing"
             , eHealth   = 500
             , eSpeed    = mkVector 1.0 1.0 1.0 }

numBlocks :: Int
numBlocks = 65536

numEntities :: Int
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
    newBlock bs n =
      mkBlock (mkVector i i i) ("Block: " `append` pack (show n)) 100 1 True True 1 : bs
      where
        i = fromIntegral n :: Float
    newEntity es n =
      mkEntity (mkVector i i i) Chicken :
        mkEntity (mkVector (i+2) i i) Zombie :
        mkEntity (mkVector (i+3) i i) Exploder :
        mkEntity (mkVector (i+4) i i) TallCreepyThing : es
      where
        i = fromIntegral n :: Float

processEntities :: [Entity] -> [Entity]
processEntities = fmap updateEntityPosition
  where
    updateEntityPosition e =
      e { eLocation = (idVector `vMul` eSpeed e) `vAdd` eLocation e }

loadWorld :: Int -> World
loadWorld chunkCount =
  force $ World (foldl' newChunk [] [0..chunkCount]) (mkVector 0.0 0.0 0.0) chunkCount
  where
    newChunk cs n =
      mkChunk (mkVector (fromIntegral n) 0.0 0.0) : cs

updateChunks :: Vector -> Int -> [Chunk] -> ([Chunk], Int)
updateChunks playerLocation chunkCount chunks =
  (ncs ++ (cs \\ rcs), chunkCount + fromIntegral rcl)
  where
    (rcs, cs) = mapAccumR runChunk [] chunks
    rcl       = fromIntegral (length rcs)
    ncs       = if rcl > 0
                then foldl' (\ncs' n -> mkChunk (mkVector (fromInteger n) 0.0 0.0) : ncs') [] [0..rcl]
                else []
    runChunk rcs' chunk =
      if getDistance (cLocation c) playerLocation > fromIntegral chunkCount
      then (c : rcs', c)
      else (rcs', c)
      where
        c = chunk { cEntities = processEntities (cEntities chunk) }

data World = World { wChunks    :: ![Chunk]
                   , wPlayerLoc :: !Vector
                   , wCounter   :: !Int }
             deriving (Generic)

instance NFData World

loop :: World -> World
loop (World world1 playerLocation chunkCount) =
  World world' playerLocation' chunkCount'
  where
    playerMovement  = mkVector 0.1 0.0 0.0
    playerLocation' = playerLocation `vAdd` playerMovement
    (world', chunkCount') = force $ updateChunks playerLocation' chunkCount world1

nloops :: Int -> World -> World
nloops 0 = id
nloops n = nloops (n - 1) . loop

run :: IO ()
run =
  defaultMain [ bench "run world" (nf (nloops 10) world) ]
  where
    world = loadWorld 100
