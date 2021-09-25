{ mkDerivation, stdenv, ghc, base, pure-core, pure-elm, pure-marker, pure-prop
}:
mkDerivation {
  pname = "pure-contenteditable";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-elm pure-marker pure-prop ];
  homepage = "github.com/grumply/pure-contenteditable";
  license = stdenv.lib.licenses.bsd3;
}
