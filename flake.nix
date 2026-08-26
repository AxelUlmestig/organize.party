{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # not in nixpkgs, the CLI repo ships it as a flake. pinned to the release
    # tag rather than a branch: the platform, CLI and OpenTofu provider release
    # on one version number, so this is the same release infra/main.tf pins the
    # provider to, and moving either is a deliberate edit to both
    fpcloud.url = "github:fogpipe/cloud-cli/v0.144.1";
    fpcloud.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, flake-utils, fpcloud, ... }: flake-utils.lib.eachDefaultSystem (system:
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

          # Deploying to Fogpipe Cloud via infra/. docker is the client only,
          # `make images` still needs a daemon a dev shell cannot provide
          pkgs.opentofu
          fpcloud.packages.${system}.fpcloud
          pkgs.docker
          pkgs.jq
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
