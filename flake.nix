{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };

      haskellPackages = pkgs.haskellPackages;

    in
    {
      devShell = pkgs.mkShell {
        buildInputs = [
          pkgs.nodejs_20
          # All Haskell tools from the same package set
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hlint
          haskellPackages.stylish-haskell
          haskellPackages.fourmolu  # If you use formatters
          haskellPackages.hoogle    # Documentation

          pkgs.elmPackages.elm
          pkgs.elmPackages.elm-format
          pkgs.zlib
          pkgs.zstd
          pkgs.postgresql
          pkgs.pkg-config
          pkgs.playwright-driver.browsers
          pkgs.sqitchPg
        ];

        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
          pkgs.zlib
          pkgs.zstd
          pkgs.postgresql
        ];

        shellHook = ''
          export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
          export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
          export PATH="${pkgs.pkg-config}/bin:$PATH"
          export PKG_CONFIG_PATH=${pkgs.lib.makeSearchPathOutput "dev" "lib/pkgconfig" [
            pkgs.zlib
            pkgs.zstd
            pkgs.postgresql
          ]}
        '';
      };
    }
  );
}
