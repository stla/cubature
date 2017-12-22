{-# LANGUAGE ForeignFunctionInterface #-}
module Cubature
  where
import           Foreign.C.Types       (CUInt(..))
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Ptr           (FunPtr, Ptr, freeHaskellFunPtr)
import           Foreign.Storable      (poke, peek, sizeOf)

type Integrand = CUInt -> Ptr Double -> Ptr () -> CUInt -> Ptr Double -> IO Int

foreign import ccall safe "wrapper" integrandPtr
    :: Integrand -> IO (FunPtr Integrand)

foreign import ccall safe "mintegration" c_cubature
    :: Char
    -> FunPtr Integrand
    -> Int
    -> Ptr Double
    -> Ptr Double
    -> Double
    -> Ptr Double
    -> IO Double

fun2integrand :: ([Double] -> Double) -> Int -> Integrand
fun2integrand f n _ x _ _ fval = do
  list <- peekArray n x
  poke fval (f list)
  return 0

cubature :: Char                 -- cubature version, 'h' or 'p'
         -> ([Double] -> Double) -- integrand
         -> Int                  -- dimension
         -> [Double]             -- lower limit of integration
         -> [Double]             -- upper limit of integration
         -> Double               -- desired relative error
         -> IO (Double, Double)  -- output: integral value and error estimate
cubature version f n xmin xmax relError = do
  fPtr <- integrandPtr (fun2integrand f n)
  xminPtr <- mallocBytes (n * sizeOf (0.0 :: Double))
  pokeArray xminPtr xmin
  xmaxPtr <- mallocBytes (n * sizeOf (0.0 :: Double))
  pokeArray xmaxPtr xmax
  errorPtr <- mallocBytes (sizeOf (0.0 :: Double))
  result <- c_cubature version fPtr n xminPtr xmaxPtr relError errorPtr
  errorEstimate <- peek errorPtr
  free errorPtr
  free xmaxPtr
  free xminPtr
  freeHaskellFunPtr fPtr
  return (result, errorEstimate)

fExample :: [Double] -> Double
fExample list = exp (-0.5 * (sum $ zipWith (*) list list))

example :: IO (Double, Double)
example = cubature 'h' fExample 3 [-2,-2,-2] [2,2,2] 1e-10
