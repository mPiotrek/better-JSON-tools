with import <nixpkgs> {};
let
  a = 3;
  hp = haskellPackages.ghcWithPackages (p:
    [ p.aeson p.servant ]
  );
in
  mkShell {
    nativeBuildInputs = [
      hp
    ];
  }
