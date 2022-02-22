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
    userAgent = lib.mkOption {
      description = ''
        User-Agent string that is sent to upstream, so they have
        some way to reach out to the API user. Setting it to an email address
        might be fine.
      '';
      example = "contact@example.org";
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable {

    services.nginx = {
      enable = true;
      appendHttpConfig = ''
        proxy_cache_path /var/cache/nginx/api.unhcr.org levels=1:2 keys_zone=unhcr:2m;
      '';
      virtualHosts."${cfg.domain}" = {
        locations =
          let
            extraConfig = ''
              proxy_cache unhcr;
              proxy_cache_valid 200 720h;
              proxy_ignore_headers X-Accel-Expires Expires Cache-Control;
              proxy_set_header User-Agent "${cfg.userAgent}";
            '';
          in
          {
            "= /Main.min.js" = {
              root = webapp;
              tryFiles = "/Main.min.js =404";
              priority = 999;
            };
            "= /Main.js" = {
              root = webapp;
              tryFiles = "/Main.js =404";
              priority = 999;
            };
            "= /style.css" = {
              root = webapp;
              tryFiles = "/style.css =404";
              priority = 999;
            };
            "= /favicon.ico" = {
              root = webapp;
              tryFiles = "$uri =404";
              priority = 999;
            };
            "= /unhcr-api/population/v1/countries/" = {
              proxyPass = "https://api.unhcr.org/population/v1/countries/";
              inherit extraConfig;
              priority = 999;
            };
            "/unhcr-api/population/v1/asylum-decisions/" = {
              proxyPass = "https://api.unhcr.org/population/v1/asylum-decisions/";
              inherit extraConfig;
              priority = 999;
            };
            # catch all for everything else
            "/" = {
              root = webapp;
              tryFiles = "/index.html =404";
              priority = 1000;
            };
          };
        enableACME = true;
        forceSSL = true;
      };

    };
  };

}
