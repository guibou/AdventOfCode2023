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
          "hspec"
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

        hspec = pkgs.writeTextDir "Main.hs"
          ''
            import Test.Hspec
            ${builtins.concatStringsSep "\n"
                  (builtins.map (x: "import ${x}") allDays)
                }

            main = hspec $ do
              ${builtins.concatStringsSep "\n  "
                  (builtins.map (x: "describe \"${x}\" $ ${x}.test") allDays)
                }
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
              cp ${hspec}/Main.hs Main.hs
              find

              mkdir -p $out/bin
              ghc -O2 ${toString flags} Main.hs -o $out/bin/all
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
