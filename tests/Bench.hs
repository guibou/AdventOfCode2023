{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use <$>" #-}
import Test.Syd
import Test.Syd.OptParse (Settings (..), defaultSettings)
import qualified Data.Map.Strict as Map
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Data.Text as Text
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BS

import Discover

main = do
  let sets = defaultSettings {
       settingFilters = ["works"]
    }
  res <- sydTestResult sets spec

  let flattened = Map.fromListWith (+) $ map (\(name, v) -> (head name, (fromIntegral ( timedTime $ testDefVal v) / (10^(6 :: Int) :: Double)) :: Double)) $ flattenSpecForest $ timedValue res
  
  -- Write bar plot
  toFile def "bench.svg" $ do
    layout_title .= "Advent of code 2023"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map (take 5) $ map Text.unpack $ Map.keys flattened)
    plot (fmap plotBars $ bars ["Time elapsed (ms)"] (addIndexes (map (\x -> [x]) $ Map.elems flattened)))

  -- Encode to json, and add the sum of all runs
  let all = sum $ Map.elems flattened
  let flattened' = Map.insert "_all" all flattened

  BS.writeFile "bench.json" (encodePretty flattened')
