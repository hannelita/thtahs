{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
  -- ( ComponentData(..)
  -- , ComponentType(..)
  -- , decodeItems
  -- , decodeItemsFromFile
  -- , filterCoomponents
  -- )
  -- where

-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Vector as V
-- -- from cassava
-- import Data.Csv


-- data ComponentType = Resistor | Capacitor | Inductor | Source deriving (Eq, Show)
-- data Magnitude = Zero | Infinite | Float deriving (Eq, Show)
-- type Param = Float

-- type ComponentData = (BL.ByteString, Int, Int, Float, Param, Param, Int)

-- main :: IO ()
-- main = do
--   csvData <- BL.readFile "data/components.csv"
--   let v = decode NoHeader csvData :: Either String (V.Vector ComponentData)
--   let summed = fmap (V.foldr cp 0) v
--   putStrLn $ "Total was: " ++ (show summed)
--   where cp (component_type, node_k, node_m, magnitude, param_1, param_2, plot) n = n + 1


import qualified Data.Foldable as Foldable

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- Matrix
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- Exception
import Control.Exception

-- base
import qualified Control.Monad as Monad
import qualified System.Exit as Exit



data ComponentData = 
  ComponentData { 
    componentType :: ComponentType,
    nodeK :: Int,
    nodeM :: Int,
    magnitude :: Float,
    param1 :: Float,
    param2 :: Float,
    plot :: Int
     }
  deriving (Eq, Show)


data SimulationData = 
  SimulationData { 
    nodes :: Int,
    voltageSources :: Int,
    stepSize :: Float,
    tmax :: Float
     }
  deriving (Eq, Show)

data ComponentType = Resistor | Capacitor | Inductor | EAC | EDC | Other Text deriving (Eq, Show)
-- data Source = EAC | EDC deriving (Eq, Show)
-- data Magnitude = Infinite | Float deriving (Eq, Show)
-- type Param = Float

-- type Component = (ComponentType, Magnitude)
-- type Node = [Component]
-- type Circuit = [Node]

instance FromNamedRecord SimulationData where
  parseNamedRecord m =
    SimulationData
      <$> m .: "Number of Nodes"
      <*> m .: "Number of Voltages Sources"
      <*> m .: "Step Size"
      <*> m .: "Maximum time for simulation"


instance FromNamedRecord ComponentData where
  parseNamedRecord m =
    ComponentData
      <$> m .: "Element Type"
      <*> m .: "Node K"
      <*> m .: "Node M"
      <*> m .: "Value"
      <*> m .: "Source param 1"
      <*> m .: "Source param 2"
      <*> m .: "Plot"

instance FromField ComponentType where
  parseField "R" =
    pure Resistor

  parseField "L" =
    pure Inductor

  parseField "C" =
    pure Capacitor

  parseField "EDC" =
    pure EDC

  parseField "EAC" =
    pure EAC

  parseField otherType =
    Other <$> parseField otherType


decodeItems :: ByteString -> Either String (Vector ComponentData)
decodeItems =
  fmap snd . Cassava.decodeByName

decodeItemsFromFile :: FilePath -> IO (Either String (Vector ComponentData))
decodeItemsFromFile filePath =
  catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeItems

decodeSimulation :: ByteString -> Either String (Vector SimulationData)
decodeSimulation =
  fmap snd . Cassava.decodeByName

decodeSimulationFromFile :: FilePath -> IO (Either String (Vector SimulationData))
decodeSimulationFromFile filePath =
  catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeSimulation

getSingleSimulationLine :: Vector SimulationData -> SimulationData
getSingleSimulationLine = 
    Vector.head 

filterResistor :: Vector ComponentData -> Vector ComponentData
filterResistor =
  Vector.filter isResistor

isResistor :: ComponentData -> Bool
isResistor =
  (==) Resistor . componentType

filterInductor :: Vector ComponentData -> Vector ComponentData
filterInductor =
  Vector.filter isInductor

isInductor :: ComponentData -> Bool
isInductor =
  (==) Inductor . componentType

filterCapacitor :: Vector ComponentData -> Vector ComponentData
filterCapacitor =
  Vector.filter isCapacitor

isCapacitor :: ComponentData -> Bool
isCapacitor =
  (==) Capacitor . componentType


filterEnergyStorageComponent :: Vector ComponentData -> Vector ComponentData
filterEnergyStorageComponent =
  Vector.filter (\r -> (componentType r == Capacitor) || (componentType r == Inductor))
    
-- filter ((== Zaal) . reviewLocation) reviews  
  

filterDCSource :: Vector ComponentData -> Vector ComponentData
filterDCSource =
  Vector.filter isEDC

isEDC :: ComponentData -> Bool
isEDC =
  (==) EDC . componentType


filterACSource :: Vector ComponentData -> Vector ComponentData
filterACSource =
  Vector.filter isEAC

isEAC :: ComponentData -> Bool
isEAC =
  (==) EAC . componentType

condutance :: ComponentData -> Float -> Float
condutance component dt =
  case componentType component of
    Resistor -> 1.0 / (magnitude component)
    Capacitor -> (magnitude component) * 0.000001 * 2 / dt
    Inductor -> dt / (2 * 0.001 * (magnitude component))
    otherwise -> 0.0




gkm :: Vector ComponentData -> Float -> Vector Float
gkm components dt =
  Vector.map (\c -> condutance c dt) components
  -- Matrix.colVector (Vector.map (\c -> condutance c dt) components)
  -- Matrix.fromList (length components) 1 (Vector.toList (Vector.map (\c -> condutance c dt) components))

diagonalUpdate :: Int -> Matrix Float -> Float -> Matrix Float
diagonalUpdate d buffer gkmHead =
  let updated = (Matrix.getElem d d buffer) + gkmHead
  in Matrix.setElem updated (d, d) buffer

gMatrixUpdate :: [((Int, Int), Float)] -> Matrix Float -> Matrix Float
gMatrixUpdate [] gmatrix = gmatrix
gMatrixUpdate (((k, m), toUpdate):xs) gmatrix =
  gMatrixUpdate xs (Matrix.setElem toUpdate (k, m) gmatrix)


gMatrix :: (Int, Int) -> Matrix Float -> Float -> Matrix Float
gMatrix (k, 0) buffer gkmHead = diagonalUpdate k buffer gkmHead
gMatrix (0, m) buffer gkmHead = diagonalUpdate m buffer gkmHead
gMatrix (k, m) buffer gkmHead = 
  let updateK = ((k, k), (Matrix.getElem k k buffer) + gkmHead)
      updateM = ((m, m), (Matrix.getElem m m buffer) + gkmHead)
      updated1 = ((k, m), (Matrix.getElem k m buffer) - gkmHead)
      updated2 = ((m, k), (Matrix.getElem m k buffer) - gkmHead)
  in
    gMatrixUpdate [updateK, updateM, updated1, updated2] buffer
  -- Matrix.zero nodes nodes

buildGMatrixFromVector :: SimulationData -> Vector ComponentData -> Matrix Float
buildGMatrixFromVector simulation components =
  buildGMatrixFromList simulation (Matrix.zero (nodes simulation) (nodes simulation)) (Vector.toList components)

buildGMatrixFromList :: SimulationData -> Matrix Float -> [ComponentData] ->  Matrix Float
buildGMatrixFromList _ buffer [] = buffer
buildGMatrixFromList simulation buffer (c:cs) =
  let gkm = condutance c $ stepSize simulation 
      cmatrix = gMatrix (nodeK c, nodeM c) buffer gkm  
      in buildGMatrixFromList simulation cmatrix cs

iMatrix :: Int -> Matrix Int
iMatrix nodes =
  Matrix.matrix nodes 1 $ \(i,j) -> 0  

iaMatrix :: Int -> Matrix Int
iaMatrix unknownSources =
  Matrix.matrix unknownSources 1 $ \(i,j) -> 0

vMatrix :: Int -> Int -> Matrix Int
vMatrix nodes npoints =
  Matrix.matrix nodes npoints $ \(i,j) -> 0

vaMatrix :: Int -> Matrix Int
vaMatrix unknownSources =
  Matrix.matrix unknownSources 1 $ \(i,j) -> 0

vbMatrix :: Int -> Matrix Int
vbMatrix voltageSources =
  Matrix.matrix voltageSources 1 $ \(i,j) -> 0

tMatrix :: Int -> Matrix Int
tMatrix npoints =
  Matrix.matrix npoints 1 $ \(i,j) -> 0

npoints :: SimulationData -> Int
npoints sim = 
  (round . stepSize $ sim) `div` (round . tmax $ sim) + 1

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException =
      return . Left . show

main :: IO ()
main = do

  eitherSimulation <-
      fmap getSingleSimulationLine
        <$> decodeSimulationFromFile "data/simulation.csv"

  case eitherSimulation of
    Left reason ->
      Exit.die reason

    Right simulation -> do
      putStr "D: "
      print (nodes simulation - voltageSources simulation)

      components_list <- decodeItemsFromFile "data/components.csv"
      case components_list of
        Left reason -> Exit.die reason
        Right components -> do
          putStr "Number of components: "
          print (length components)
          let resistors = filterResistor components
          putStr "Number of resistors: "
          print (length resistors)
          let lc = filterEnergyStorageComponent components
          putStr "Number of Storage Components LC: "
          print (length lc)
          let dt = stepSize simulation
          putStr "setpSize dt: "
          print (dt)
          let values = Vector.map (\c -> condutance c dt) components
          putStr "Component Values: "
          print (values)
          
          putStr "Gkm Values: \n"
          print (gkm components dt)

          let gmatr = buildGMatrixFromVector simulation components
          putStr "G Matrix Values: \n"
          print (gmatr)



  


  --     eitherLC <-
  --       fmap filterEnergyStorageComponent
  --         <$> decodeItemsFromFile "data/components.csv"

  --     case eitherLC of
  --       Left reason ->
  --         Exit.die reason

  --       Right lc -> do
  --         putStr "Number of Storage Components LC: "
  --         print (length lc)







