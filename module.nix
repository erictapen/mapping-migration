webapp:
{ config, lib, pkgs, ... }:
let
  cfg = config.services.mappingmigration;
in
{

  options.services.mappingmigration = {
    enable = lib.mkEnableOption "Mapping migration Nginx config";
    domain = lib.mkOption {
      description = "The domain serving the webapp.";
      example = "mappingmigration.example.org";
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable {

    services.nginx = {
      enable = true;
      virtualHosts."${cfg.domain}" = {
        locations =
          {
            "= /mapping/asylum/favicon.ico" = {
              root = webapp;
              tryFiles = "$uri =404";
              priority = 999;
            };
            "/mapping/asylum/assets/" = {
              root = webapp;
              tryFiles = "$uri =404";
              priority = 999;
            };
            # catch all for everything else
            "/mapping/asylum/" = {
              root = webapp;
              tryFiles = "/mapping/asylum/index.html =404";
              priority = 1000;
            };
            "/".return = "301 /mapping/asylum/";
          };
        enableACME = true;
        forceSSL = true;
      };

    };
  };

}
