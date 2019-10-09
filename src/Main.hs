{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import qualified Data.Foldable as Foldable

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- Matrix
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

-- HMatrix
import qualified Numeric.LinearAlgebra as HMatrix

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
    magnitude :: Double,
    param1 :: Double,
    param2 :: Double,
    plot :: Int
     }
  deriving (Eq, Show)


data SimulationData = 
  SimulationData { 
    nodes :: Int,
    voltageSources :: Int,
    stepSize :: Double,
    tmax :: Double
     }
  deriving (Eq, Show)

data ComponentType = Resistor | Capacitor | Inductor | EAC | EDC | Other Text deriving (Eq, Show)
-- data Source = EAC | EDC deriving (Eq, Show)
-- data Magnitude = Infinite | Double deriving (Eq, Show)
-- type Param = Double

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

nh :: Vector ComponentData -> Int
nh components =
  length $ filterEnergyStorageComponent components
    
-- filter ((== Zaal) . reviewLocation) reviews  

filterSources :: Vector ComponentData -> Vector ComponentData
filterSources =
  Vector.filter (\r -> (componentType r == EAC) || (componentType r == EAC))

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

condutance :: ComponentData -> Double -> Double
condutance component dt =
  case componentType component of
    Resistor -> 1.0 / (magnitude component)
    Capacitor -> (magnitude component) * 0.000001 * 2 / dt
    Inductor -> dt / (2 * 0.001 * (magnitude component))
    otherwise -> 0.0

gkm :: Vector ComponentData -> Double -> Vector Double
gkm components dt =
  Vector.map (\c -> condutance c dt) components
  -- Matrix.colVector (Vector.map (\c -> condutance c dt) components)
  -- Matrix.fromList (length components) 1 (Vector.toList (Vector.map (\c -> condutance c dt) components))

diagonalUpdate :: Int -> Matrix Double -> Double -> Matrix Double
diagonalUpdate d buffer gkmHead =
  let updated = (Matrix.getElem d d buffer) + gkmHead
  in Matrix.setElem updated (d, d) buffer

gMatrixUpdate :: [((Int, Int), Double)] -> Matrix Double -> Matrix Double
gMatrixUpdate [] gmatrix = gmatrix
gMatrixUpdate (((k, m), toUpdate):xs) gmatrix =
  gMatrixUpdate xs (Matrix.setElem toUpdate (k, m) gmatrix)


gMatrix :: (Int, Int) -> Matrix Double -> Double -> Matrix Double
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

buildGMatrixFromVector :: SimulationData -> Vector ComponentData -> Matrix Double
buildGMatrixFromVector simulation components =
  buildGMatrixFromList simulation (Matrix.zero (nodes simulation) (nodes simulation)) (Vector.toList components)

buildGMatrixFromList :: SimulationData -> Matrix Double -> [ComponentData] ->  Matrix Double
buildGMatrixFromList _ buffer [] = buffer
buildGMatrixFromList simulation buffer (c:cs) =
  let gkm = condutance c $ stepSize simulation 
      cmatrix = gMatrix (nodeK c, nodeM c) buffer gkm  
      in buildGMatrixFromList simulation cmatrix cs

ihMatrix :: Int -> Matrix Int
ihMatrix nodes =
  Matrix.matrix nodes 1 $ \(i,j) -> 0

buildIhVector :: [ComponentData] -> [Double] -> Int -> Vector Double -> Vector Double -> Matrix Double -> Vector Double
buildIhVector [] _ _ _ ihnew _ = ihnew
buildIhVector (component:cs) (gkml:gkms) n ihold ihnew vMatrix =
  case (componentType component, nodeK component, nodeM component) of (Inductor, 0, m) -> buildIhVector cs gkms n ihold (ihnew Vector.// [(0, (2*gkml*(Matrix.getElem m n vMatrix) + ihold Vector.! 0))]) vMatrix
                                                                      (Inductor, k, 0) -> buildIhVector cs gkms n ihold (ihnew Vector.// [(0, (-2*gkml*(Matrix.getElem k n vMatrix) + ihold Vector.! 0))]) vMatrix
                                                                      (Inductor, k, m) -> buildIhVector cs gkms n ihold (ihnew Vector.// [(0, (-2*gkml*((Matrix.getElem k n vMatrix) - (Matrix.getElem m n vMatrix)) + ihold Vector.! 0))]) vMatrix
                                                                      (Capacitor, 0, m) -> buildIhVector cs gkms n ihold (ihnew Vector.// [(0, (-2*gkml*(Matrix.getElem m n vMatrix) - ihold Vector.! 0))]) vMatrix
                                                                      (Capacitor, k, 0) -> buildIhVector cs gkms n ihold (ihnew Vector.// [(0, (2*gkml*(Matrix.getElem k n vMatrix) - ihold Vector.! 0))]) vMatrix
                                                                      (Capacitor, k, m) -> buildIhVector cs gkms n ihold (ihnew Vector.// [(0, (2*gkml*((Matrix.getElem k n vMatrix) - (Matrix.getElem m n vMatrix)) - ihold Vector.! 0))]) vMatrix
                                                                      (_, _, _) -> buildIhVector cs gkms n ihold ihnew vMatrix



buildVBVector :: [ComponentData] -> Vector Double
buildVBVector components =
  Vector.map (\s -> magnitude s) (filterSources (Vector.fromList components))


buildIVector :: [ComponentData] -> Vector Double -> Vector Double
buildIVector [] buffer = buffer
buildIVector (component:cs) iVec =
  case (nodeK component, nodeM component) of (k, 0) -> buildIVector cs (iVec Vector.// [(k, ((magnitude component) + iVec Vector.! k))])
                                             (0, m) -> buildIVector cs (iVec Vector.// [(m, ((magnitude component) - iVec Vector.! m))])
                                             (k, m) -> buildIVector cs (iVec Vector.// [(k, ((magnitude component) + iVec Vector.! k)), (m, ((magnitude component) - iVec Vector.! m))])


thtaControl :: Int -> Vector Double -> Vector Double -> (Int, Vector Double)
thtaControl thtactl ihnew ih
  | thtactl <= 0 = (thtactl, ihnew)
  | thtactl < 3 = (thtactl + 1, (Vector.map (\i -> i/2) $ Vector.zipWith (+) ih ihnew))
  | otherwise = (0, ihnew)

toHMatrixTransformer :: Matrix Double -> HMatrix.Matrix Double
toHMatrixTransformer matrix =
  HMatrix.fromLists $ Matrix.toLists matrix

toHMatrixVectorTransformer :: Vector Double -> HMatrix.Vector Double
toHMatrixVectorTransformer vec =
  HMatrix.fromList $ Vector.toList vec

solver :: HMatrix.Vector Double -> HMatrix.Matrix Double -> HMatrix.Vector Double -> SimulationData -> (HMatrix.Vector Double, HMatrix.Matrix Double)
solver iVector condutances vb simulation =
  let ia = HMatrix.subVector 0 ((nodes simulation) - (voltageSources simulation)) iVector
      [[gaa], [gab], [gba], [gbb]] = HMatrix.toBlocksEvery ((nodes simulation) - (voltageSources simulation)) ((nodes simulation) - (voltageSources simulation)) condutances
      rhsa = ia - (gab HMatrix.#> vb)
      va = gaa HMatrix.<\> rhsa
      ib = (gba HMatrix.#> va) - (gbb HMatrix.#> vb)
      iVec = HMatrix.vjoin [ia, ib]
      vVec = va HMatrix.— vb
  in
    solver(iVec condutances )
      

thtaSimulationStep :: [ComponentData] -> [Double] -> SimulationData -> Int -> Int -> Double  -> Vector Double -> Vector Double -> Vector Double -> Matrix Double -> Vector Double -> Vector Double -> (Vector Double, Vector Double, Vector Double)
thtaSimulationStep components gkms simulation thtactl n time ihold ihnew ih vMatrix vbVector iVector =
  let ihBuffer = buildIhVector components gkms n ihold ihnew vMatrix
      vbVector = buildVBVector components
      iVector = buildIVector components iVector
      (thta, ihThta) = (thtaControl thtactl ihBuffer ih)
  in
      (snd (thtaControl thtactl ihBuffer ih), snd (thtaControl thtactl ihBuffer ih), snd (thtaControl thtactl ihBuffer ih))


-- thtaSimulation :: Vector Double -> [ComponentData] -> SimulationData -> Matrix Double -> Vector Double -> Double Vector
-- thtaSimulation [] components simulation condutanceMtr voltageVec = voltageVec
-- thtaSimulation (t:ts) condutanceMtr simulation voltageVec = 
--   thtaSimulationStep components (Vector.toList gkm components (stepSize simulation)) simulation 1 1 t (ihMatrix . length .  nh components) (ihMatrix . length .  nh components) (Matrix.zero (nodes simulation) (npoints simulation)) (Vector.replicate (voltageSources simulation) 0)




npoints :: SimulationData -> Int
npoints sim = 
  round ((stepSize sim)/(tmax sim)) + 1

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
          let lc = nh components
          putStr "Number of Storage Components LC: "
          print (lc)
          let dt = stepSize simulation
          putStr "setpSize dt: "
          print (dt)
          let values = Vector.map (\c -> condutance c dt) components
          putStr "Component Values: "
          print (values)
          
          let npts = npoints simulation
          putStr "npoints: "
          print (npts)

          putStr "Gkm Values: \n"
          print (gkm components dt)

          let gmatr = buildGMatrixFromVector simulation components
          putStr "G Matrix Values: \n"
          print (gmatr)



