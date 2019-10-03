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
    stepSize :: Int,
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

  


  --     eitherLC <-
  --       fmap filterEnergyStorageComponent
  --         <$> decodeItemsFromFile "data/components.csv"

  --     case eitherLC of
  --       Left reason ->
  --         Exit.die reason

  --       Right lc -> do
  --         putStr "Number of Storage Components LC: "
  --         print (length lc)
