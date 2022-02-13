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
            "/" = {
              root = webapp;
              tryFiles = "$uri/Main.html =404";
            };
            "= /unhcr-api/population/v1/countries/" = {
              proxyPass = "https://api.unhcr.org/population/v1/countries/";
              inherit extraConfig;
            };
            "/unhcr-api/population/v1/asylum-decisions/" = {
              proxyPass = "https://api.unhcr.org/population/v1/asylum-decisions/";
              inherit extraConfig;
            };
          };
        enableACME = true;
        forceSSL = true;
      };

    };
  };

}
