{
  description = "A very basic flake";

  outputs = { self, nixpkgs }:
    let
      config = { system = "x86_64-linux"; };
      pkgs = import nixpkgs config;
    in
    {

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.webapp;

      packages.x86_64-linux.webapp = import ./default.nix {
        inherit nixpkgs config;
      };

      nixosModule = import ./module.nix self.packages.x86_64-linux.webapp;

      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = with pkgs; with elmPackages; [
          elm
          elm-format
          elm-test
          elm-json
          elm2nix
          (python3.withPackages (ps: with ps; [
            pandas
          ]))
          (inkscape-with-extensions.override {
            inkscapeExtensions = with inkscape-extensions; [
              applytransforms
            ];
          })
        ];
      };

    };
}
