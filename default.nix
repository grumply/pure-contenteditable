{ mkDerivation, stdenv, ghc, base, pure-elm
}:
mkDerivation {
  pname = "pure-contenteditable";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-elm ];
  homepage = "github.com/grumply/pure-contenteditable";
  license = stdenv.lib.licenses.bsd3;
}
