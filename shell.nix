with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, linear, stdenv, vector }:
             mkDerivation {
               pname = "hep-utilities";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base linear vector ];
               homepage = "https://github.com/cbpark/hep-utilities";
               description = "Utilities for analyzing high energy physics data";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
