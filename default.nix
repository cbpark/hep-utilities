{ mkDerivation, attoparsec, base, bytestring, containers, hpack
, lens, linear, mwc-random, pipes, pipes-attoparsec
, pipes-bytestring, stdenv, transformers, vector
}:
mkDerivation {
  pname = "hep-utilities";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    attoparsec base bytestring containers lens linear mwc-random pipes
    pipes-attoparsec pipes-bytestring transformers vector
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/cbpark/hep-utilities#readme";
  description = "Utilities for analyzing high energy physics data";
  license = stdenv.lib.licenses.bsd3;
}
