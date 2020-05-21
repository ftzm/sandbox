let pkgs = import <nixpkgs> {};
in
rec {
  boggler-dev = pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
        ])
      // {buildInputs = [pkgs.zlib];
          isExecutable = true;};
  };
  boggler = pkgs.haskell.lib.justStaticExecutables (pkgs.haskellPackages.callCabal2nix "boggler" ./. { });
}
