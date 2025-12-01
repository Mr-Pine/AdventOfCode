{
  description = "Development environment with Granule tools";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    granule. url = "github:granule-project/granule";
  };

  outputs = { nixpkgs, granule, ... }:
    let
      system = "x86_64-linux"; # or "aarch64-darwin" for M1/M2 Macs
      pkgs = nixpkgs. legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        LANG="C.UTF-8";
        buildInputs = with granule.packages.${system}; [
          granule-interpreter
          granule-repl-with-stdlib
          granule-compiler
          # granule-language-server
        ];
      };
    };
}
