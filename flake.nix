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
          pkgs.elmPackages.elm
          pkgs.elmPackages.elm-format
          pkgs.zlib
          pkgs.postgresql
          pkgs.playwright-driver.browsers
          pkgs.hlint
          pkgs.stylish-haskell
        ];

        shellHook = ''
          export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
          export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
        '';
      };
    }
  );
}
