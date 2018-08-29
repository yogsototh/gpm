let
  rev = "16d475334409f7fa632929b2838421b4ffe34927";
  _nixpkgs = import <nixpkgs> { };
  pkgs = import ( fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz" ) { };
  ghc = pkgs.haskellPackages;
  ghcPackages = ghc.ghcWithPackages (ps: [ ps.protolude ps.turtle ]);
in with pkgs;

stdenv.mkDerivation {
  name = "gpm";
  buildInputs = [ ghcPackages ];
  buildDepends = [ libiconv ];
}