with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "myEnv";
  buildInputs = [
    cmdstan
    cairo
    pkgconfig
    zlib
    glibcLocales
    haskell.packages.lts-3_13.ghc
  ];
  libraryPkgconfigDepends = [ zlib ];
  STACK_IN_NIX_EXTRA_ARGS
      = " --extra-lib-dirs=${cairo}/lib"
      + " --extra-include-dirs=${cairo}/include"
      + " --extra-lib-dirs=${zlib}/lib"
      + " --extra-include-dirs=${zlib}/include"
  ;
}
