with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, linear, stdenv }:
             mkDerivation {
               pname = "hep-kinematics";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base linear ];
               homepage = "http://github.com/cbpark/hep-kinematics";
               description = "Tools for Relativistic kinematics in high energy physics";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
