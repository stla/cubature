module Main
  where
import           Cubature

main :: IO ()
main = do
  hcubature <- cubature 'h' fExample 3 [-2,-2,-2] [2,2,2] 1e-4
  pcubature <- cubature 'p' fExample 3 [-2,-2,-2] [2,2,2] 1e-4
  putStrLn $ "h-cubature: " ++ (show hcubature)
             ++ "\np-cubature: " ++ (show pcubature)
