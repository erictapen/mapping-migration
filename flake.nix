{
  description = "Mapping asylum for Mapping Cities – Making Cities";

  inputs.nixpkgs.url = "github:NixOS/Nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      config = { system = "x86_64-linux"; };
      pkgs = import nixpkgs config;
    in
    {

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.webapp;

      packages.x86_64-linux.webapp = let
        webapp = import ./default.nix {
            inherit nixpkgs config;
            revision = self.rev or "dirty";
          };
       in pkgs.runCommand "mappingmigration" { } ''
         mkdir -p $out/mapping
         ln -s ${webapp} $out/mapping/asylum
       '';

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
