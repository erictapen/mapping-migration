{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: let
    config = { system = "x86_64-linux"; };
    pkgs = import nixpkgs config;
  in {

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.website;

    packages.x86_64-linux.website = import ./default.nix {
      inherit nixpkgs config;
    };

    devShell.x86_64-linux = pkgs.mkShell {
      buildInputs = with pkgs; with elmPackages; [
        elm
        elm-format
        elm-test
        elm2nix
      ];
    };

  };
}
