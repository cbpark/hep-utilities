{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, lens, linear
      , mwc-random, pipes, pipes-attoparsec, pipes-bytestring, stdenv
      , transformers, vector
      }:
      mkDerivation {
        pname = "hep-utilities";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = false;
        libraryHaskellDepends = [
          attoparsec base bytestring lens linear mwc-random pipes
          pipes-attoparsec pipes-bytestring transformers vector
        ];
        homepage = "https://github.com/cbpark/hep-utilities";
        description = "Utilities for analyzing high energy physics data";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
