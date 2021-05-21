with import <nixpkgs> {};
let
  a = 3;
  hp = haskellPackages.ghcWithPackages (p:
    [ p.aeson p.aeson-pretty
      p.servant p.servant-server
    ]
  );
in
  mkShell {
    nativeBuildInputs = [ hp ];
  }
