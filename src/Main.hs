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
import qualified Numeric.LinearAlgebra.Data as HMatrix
import qualified Numeric.LinearAlgebra.HMatrix as HMatrix

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
import qualified Debug.Trace as Trace



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
type SimulationResults = (Vector Double, Matrix Double)

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


nhComponents :: [ComponentData] -> [ComponentData]
nhComponents =
  filter (\r -> (componentType r == Capacitor) || (componentType r == Inductor))

filterEnergyStorageComponent :: Vector ComponentData -> Vector ComponentData
filterEnergyStorageComponent =
  Vector.filter (\r -> (componentType r == Capacitor) || (componentType r == Inductor))

nh :: Vector ComponentData -> Int
nh components =
  length $ filterEnergyStorageComponent components
    
-- filter ((== Zaal) . reviewLocation) reviews  

filterSources :: Vector ComponentData -> Vector ComponentData
filterSources =
  Vector.filter (\r -> (componentType r == EDC) || (componentType r == EAC))

condutance :: ComponentData -> Double -> Double
condutance component dt =
  case componentType component of
    Resistor -> 1.0 / (magnitude component)
    Capacitor -> (magnitude component) * 0.000001 * 2 / dt
    Inductor -> dt / (2 * 0.001 * (magnitude component))
    _ -> 0.0

gkm :: Vector ComponentData -> Double -> Vector Double
gkm components dt =
  Vector.map (\c -> condutance c dt) components
  -- Matrix.colVector (Vector.map (\c -> condutance c dt) components)
  -- Matrix.fromList (length components) 1 (Vector.toList (Vector.map (\c -> condutance c dt) components))

buildCompactGMatrix :: Double -> [ComponentData] -> Matrix Double -> Matrix Double
buildCompactGMatrix dt [] buffer = buffer
buildCompactGMatrix dt (component:cs) buffer =
  case (nodeK component, nodeM component) of (0, m) -> buildCompactGMatrix dt cs (Matrix.setElem (Matrix.getElem m m buffer + condutance component dt) (m, m) buffer)
                                             (k, 0) -> buildCompactGMatrix dt cs (Matrix.setElem (Matrix.getElem k k buffer + condutance component dt) (k, k) buffer)
                                             (k, m) -> buildCompactGMatrix dt cs (Matrix.setElem (Matrix.getElem k k buffer + condutance component dt) (k, k) (Matrix.setElem (Matrix.getElem m m buffer + condutance component dt) (m, m) (Matrix.setElem (Matrix.getElem k m buffer - condutance component dt) (k, m) (Matrix.setElem (Matrix.getElem m k buffer - condutance component dt) (m, k) buffer))))
                                             (_, _) -> buildCompactGMatrix dt cs buffer


buildGMatrixFromVector :: SimulationData -> Vector ComponentData -> Matrix Double
buildGMatrixFromVector simulation components =
  buildCompactGMatrix (stepSize simulation) (Vector.toList components) (Matrix.zero (nodes simulation) (nodes simulation))


buildIhVector :: [ComponentData] -> Double -> Int -> [Double] -> [Double] -> Matrix Double -> Vector Double
buildIhVector [] _ _ _ ihnew _ = Vector.fromList ihnew
buildIhVector (component:cs) dt n (hold:ihold) ihnew vMatrix =
  case (componentType component, nodeK component, nodeM component) of (Inductor, 0, m) -> buildIhVector cs dt n ihold (ihnew ++ [(2*(condutance component dt)*(Matrix.getElem m n vMatrix) + hold)]) vMatrix
                                                                      (Inductor, k, 0) -> buildIhVector cs dt n ihold (ihnew ++ [(-2*(condutance component dt)*(Matrix.getElem k n vMatrix) + hold)]) vMatrix
                                                                      (Inductor, k, m) -> buildIhVector cs dt n ihold (ihnew ++ [(-2*(condutance component dt)*((Matrix.getElem k n vMatrix) - (Matrix.getElem m n vMatrix)) + hold)]) vMatrix
                                                                      (Capacitor, 0, m) -> buildIhVector cs dt n ihold (ihnew ++ [(-2*(condutance component dt)*(Matrix.getElem m n vMatrix) - hold)]) vMatrix
                                                                      (Capacitor, k, 0) -> buildIhVector cs dt n ihold (ihnew ++ [(2*(condutance component dt)*(Matrix.getElem k n vMatrix) - hold)]) vMatrix
                                                                      (Capacitor, k, m) -> buildIhVector cs dt n ihold (ihnew ++ [(2*(condutance component dt)*((Matrix.getElem k n vMatrix) - (Matrix.getElem m n vMatrix)) - hold)]) vMatrix
                                                                      (_, _, _) -> buildIhVector cs dt n ihold ihnew vMatrix



buildVBVector :: [ComponentData] -> Double -> [Double] -> Vector Double
buildVBVector [] _ buffer = Vector.fromList buffer
buildVBVector (c:components) time buffer =
  case (componentType c) of EDC -> buildVBVector components time ((magnitude c) : buffer)
                            EAC -> buildVBVector components time (((magnitude c * cos (2 * pi * param2 c * time + (param1 c * (pi/180))))) : buffer)
                            _   -> buildVBVector components time buffer


buildIVector :: [ComponentData] -> [Double] -> Vector Double -> Vector Double
buildIVector [] _ iVector = iVector
buildIVector (component:cs) (ihEl:ih) iVector =
  case (componentType component, nodeK component, nodeM component) of (Inductor, k, 0) -> buildIVector cs ih (iVector Vector.// [((k - 1), ((iVector Vector.! (k-1)) + ihEl))])
                                                                      (Inductor, 0, m) -> buildIVector cs ih (iVector Vector.// [((m - 1), ((iVector Vector.! (m-1)) - ihEl))])
                                                                      (Inductor, k, m) -> buildIVector cs ih (iVector Vector.// [((m - 1), ((iVector Vector.! (m-1)) - ihEl)), ((k-1), ((iVector Vector.! (k-1)) + ihEl))])
                                                                      (Capacitor, k, 0) -> buildIVector cs ih (iVector Vector.// [((k - 1), ((iVector Vector.! (k-1)) + ihEl))])
                                                                      (Capacitor, 0, m) -> buildIVector cs ih (iVector Vector.// [((m - 1), ((iVector Vector.! (m-1)) - ihEl))])
                                                                      (Capacitor, k, m) -> buildIVector cs ih (iVector Vector.// [((m - 1), ((iVector Vector.! (m-1)) - ihEl)), ((k-1), ((iVector Vector.! (k-1)) + ihEl))])
                                                                      (_, _, _) -> buildIVector cs ih iVector


thtaControl :: Int -> Double -> Vector Double -> Vector Double -> SimulationData -> (Int, Vector Double, Double)
thtaControl thtactl time ihnew ih simulation
  | thtactl <= 0 = (thtactl, ihnew, (stepSize simulation + time))
  | thtactl < 3 = (thtactl + 1, (Vector.map (\i -> i/2) $ Vector.zipWith (+) ih ihnew), (time + (stepSize simulation/2)))
  | otherwise = (0, ihnew, (stepSize simulation + time))

fromHMatrixTransformer :: HMatrix.Matrix Double -> Matrix Double
fromHMatrixTransformer matrix =
  Matrix.fromLists $ HMatrix.toLists matrix

toHMatrixTransformer :: Matrix Double -> HMatrix.Matrix Double
toHMatrixTransformer matrix =
  HMatrix.fromLists $ Matrix.toLists matrix

fromHMatrixVectorTransformer :: HMatrix.Vector Double -> Vector Double
fromHMatrixVectorTransformer vec =
  Vector.fromList $ HMatrix.toList vec

toHMatrixVectorTransformer :: Vector Double -> HMatrix.Vector Double
toHMatrixVectorTransformer vec =
  HMatrix.fromList $ Vector.toList vec

solver :: HMatrix.Vector Double -> HMatrix.Matrix Double -> HMatrix.Matrix Double -> HMatrix.Matrix Double -> HMatrix.Matrix Double -> HMatrix.Vector Double -> SimulationData -> (Vector Double, Vector Double)
solver iVector gaa gab gba gbb vb simulation =
  let ia = HMatrix.subVector 0 ((nodes simulation) - (voltageSources simulation)) iVector
      rhsa = ia - (gab HMatrix.#> vb)
      va = gaa HMatrix.<\> rhsa
      ib = (gba HMatrix.#> va) + (gbb HMatrix.#> vb)
      iVec = HMatrix.vjoin [ia, ib]      
      vVec = HMatrix.vjoin [va, vb]
  in
    ((fromHMatrixVectorTransformer iVec), (fromHMatrixVectorTransformer vVec))
      

thtaSimulationStep :: [ComponentData] -> Matrix Double -> SimulationData -> Int -> Int -> Double  -> Vector Double -> Matrix Double -> Vector Double -> Vector Double -> SimulationResults
thtaSimulationStep _ _ _ _ 1 _ _ vMatrix _ iVector = (iVector, vMatrix)
thtaSimulationStep components condutances simulation thtactl n time ih vMatrix vbVector iVector =
  let (gaa, gab, gba, gbb) = Matrix.splitBlocks (nodes simulation - voltageSources simulation) (nodes simulation - voltageSources simulation) condutances
      ihBuffer = buildIhVector (nhComponents components) (stepSize simulation) n (Vector.toList ih) [] vMatrix
      (thta, ihThta, timeThta) = thtaControl thtactl time ihBuffer ih simulation
      vbVec = buildVBVector components timeThta []
      iVec = buildIVector (nhComponents components) (Vector.toList ihThta) (Vector.replicate (nodes simulation) 0)
      -- (iVecCalc, vVec) = Trace.trace ("Solver = \n" ++ show (solver (toHMatrixVectorTransformer iVec) (toHMatrixTransformer gaa) (toHMatrixTransformer gab) (toHMatrixTransformer gba) (toHMatrixTransformer gbb) (toHMatrixVectorTransformer vbVec) simulation)) solver (toHMatrixVectorTransformer iVec) (toHMatrixTransformer gaa) (toHMatrixTransformer gab) (toHMatrixTransformer gba) (toHMatrixTransformer gbb) (toHMatrixVectorTransformer vbVec) simulation
      (iVecCalc, vVec) = solver (toHMatrixVectorTransformer iVec) (toHMatrixTransformer gaa) (toHMatrixTransformer gab) (toHMatrixTransformer gba) (toHMatrixTransformer gbb) (toHMatrixVectorTransformer vbVec) simulation
      vMatr = Matrix.mapCol (\r _ -> vVec Vector.! (r - 1)) (n-1) vMatrix
  in
      thtaSimulationStep components condutances simulation thta (n-1) timeThta ihThta vMatr vbVec iVecCalc


thtaSimulation :: Vector ComponentData -> SimulationData -> SimulationResults
thtaSimulation components simulation = 
  thtaSimulationStep (Vector.toList components) (buildGMatrixFromVector simulation components) simulation 1 (npoints simulation) 0.0 (Vector.replicate (nh components) 0) (Matrix.zero (nodes simulation) (npoints simulation)) (Vector.replicate (voltageSources simulation) 0) (Vector.replicate (nodes simulation) 0)


npoints :: SimulationData -> Int
npoints sim = 
  round ((tmax sim)/(stepSize sim)) + 1

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
      components_list <- decodeItemsFromFile "data/components.csv"
      case components_list of
        Left reason -> Exit.die reason
        Right components -> do
          let gmt = buildGMatrixFromVector simulation components
          putStr "GMatrix: \n"
          print (gmt)
          let results = thtaSimulation components simulation
          putStr "Simulation: \n"
          print (results)



