{
  description = "AoC 2023";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskellPackages;

        deps = [
          "ansi-terminal"
          "base"
          "base16-bytestring"
          "besout"
          "bytestring"
          "containers"
          "cryptohash"
          "directory"
          "Earley"
          "file-embed"
          "generic-deriving"
          "generic-lens"
          "hashable"
          "here"
          "hex"
          "sydtest"
          "JuicyPixels"
          "lens"
          "linear"
          "matrix"
          "megaparsec"
          "memoize"
          "monad-memo"
          "monoidal-containers"
          "mtl"
          "optics-core"
          "parallel"
          "parser-combinators"
          "pqueue"
          "pretty-simple"
          "process"
          "PyF"
          "QuickCheck"
          "random"
          "relude"
          "split"
          "template-haskell"
          "text"
          "time"
          #"union-find"
          "unordered-containers"
          "vector"
          "weigh"
          "array"
          "Chart"
          "Chart-diagrams"
        ];

        extensions = [
          "BangPatterns"
          "BinaryLiterals"
          "DeriveAnyClass"
          "DeriveGeneric"
          "DerivingStrategies"
          "FlexibleContexts"
          "GeneralizedNewtypeDeriving"
          "LambdaCase"
          "MultiWayIf"
          "NamedFieldPuns"
          "OverloadedRecordDot"
          "OverloadedStrings"
          "PartialTypeSignatures"
          "PatternSynonyms"
          "QuasiQuotes"
          "ScopedTypeVariables"
          "StandaloneDeriving"
          "TemplateHaskell"
          "TupleSections"
          "TypeApplications"
          "ViewPatterns"
          "GHC2021"
        ];

        flags = (builtins.map (e: "-X${e}") extensions)
          ++
          [
            "-isrc"
            "-Wall"
            "-ilib"
            "lib/Utils.hs"
            "lib/Path.hs"
            "lib/Direction.hs"
          ];

        haskellPackages = pkgs.haskellPackages.extend (
          hfinal: hprev:
            with pkgs.haskell.lib;
            {

              besout = unmarkBroken (doJailbreak hprev.besout);

            }
        );

        myGHC =
          (haskellPackages.ghcWithPackages (p:
            (builtins.map (i: p.${i}) deps)
          ));

        ghci = pkgs.writeShellScriptBin "ghci"
          ''
            ${myGHC}/bin/ghci ${toString flags} -interactive-print=Text.Pretty.Simple.pPrintLightBg
          '';

        allDays = builtins.map (x: pkgs.lib.strings.removeSuffix ".hs" x) (builtins.attrNames (builtins.readDir ./src));

        sydtest = pkgs.writeTextDir "Main.hs"
          ''
            import Test.Syd
            ${builtins.concatStringsSep "\n"
                  (builtins.map (x: "import ${x}") allDays)
                }

            main = sydTest $ do
              ${builtins.concatStringsSep "\n  "
                  (builtins.map (x: "describe \"${x}\" $ ${x}.test") allDays)
                }
          '';

        sydbench = pkgs.writeTextDir "Main.hs"
          ''
            import Test.Syd
            import Test.Syd.OptParse (Settings (..), defaultSettings)
            import qualified Data.Map.Strict as Map
            import Data.Aeson (encodeFile)
            import Graphics.Rendering.Chart.Easy
            import Graphics.Rendering.Chart.Backend.Diagrams
            import qualified Data.Text as Text

            ${builtins.concatStringsSep "\n"
                  (builtins.map (x: "import ${x}") allDays)
                }

            tests = do
              ${builtins.concatStringsSep "\n  "
                  (builtins.map (x: "describe \"${x}\" $ ${x}.test") allDays)
                }

            main = do
              let sets = defaultSettings {
                   settingFilters = ["works"]
                }
              res <- sydTestResult sets tests
            
              let flattened = Map.fromListWith (+) $ map (\(name, v) -> (head name, ((fromIntegral $ timedTime $ testDefVal v) / (10^(6 :: Int) :: Double)) :: Double)) $ flattenSpecForest $ timedValue res

              print flattened
              encodeFile "bench.json" flattened
              toFile def "bench.svg" $ do
                layout_title .= "Advent of code 2023"
                layout_title_style . font_size .= 10
                layout_x_axis . laxis_generate .= autoIndexAxis (map Text.unpack $ Map.keys flattened)
                plot (fmap plotBars $ bars ["Time elapsed (ms)"] (addIndexes (map (\x -> [x]) $ Map.elems flattened)))
          '';

      in
      {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [
              ghci
              myGHC
              hPkgs.haskell-language-server
            ];
          };
        };

        packages = {
          all = pkgs.runCommand "all"
            {
              buildInputs = [ myGHC ];

            }
            ''
              set -x
              mkdir src lib content
              cp -r ${./src}/* src
              cp -r ${./lib}/* lib
              cp -r ${./content}/* content
              cp ${sydtest}/Main.hs Main.hs

              mkdir -p $out/bin
              ghc -O2 -threaded -rtsopts -with-rtsopts=-N ${toString flags} Main.hs -o $out/bin/all
            '';

          bench = pkgs.runCommand "bench"
            {
              buildInputs = [ myGHC ];

            }
            ''
              set -x
              mkdir src lib content
              cp -r ${./src}/* src
              cp -r ${./lib}/* lib
              cp -r ${./content}/* content
              cp ${sydbench}/Main.hs Main.hs

              mkdir -p $out/bin
              ghc -O2 -threaded -rtsopts -with-rtsopts=-N ${toString flags} Main.hs -o $out/bin/bench
            '';

          haskell-hie-bios = pkgs.writeShellScriptBin "haskell-hie-bios"
            ''
              for i in ${toString flags}
              do
                echo $i >> $HIE_BIOS_OUTPUT
              done
            '';
        };
      });
}
