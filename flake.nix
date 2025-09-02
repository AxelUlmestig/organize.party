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
    in
    {
      devShell = pkgs.mkShell {
        buildInputs = [
          pkgs.nodejs_20
          pkgs.ghc
          pkgs.cabal-install
          pkgs.elmPackages.elm
          pkgs.elmPackages.elm-format
          pkgs.zlib
          pkgs.zstd
          pkgs.postgresql
          pkgs.playwright-driver.browsers
          pkgs.hlint
          pkgs.stylish-haskell
        ];

        # Add the C libraries to the library path so Cabal can find them
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
          pkgs.zlib
          pkgs.zstd
        ];

        shellHook = ''
          export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
          export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
          # Also set pkg-config path to help Cabal find the C libraries
          export PKG_CONFIG_PATH=${pkgs.lib.makeSearchPathOutput "dev" "lib/pkgconfig" [
            pkgs.zlib
            pkgs.zstd
          ]}
        '';
      };
    }
  );
}
