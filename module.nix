webapp:
{ config, lib, pkgs, ... }: {

  services.nginx = {
    enable = true;
    virtualHosts."${cfg.domain}" = {
         locations = {
           "/".root = webapp;
           "/".tryFiles = "$uri/Main.html =404";
         };
         enableACME = true;
         forceSSL = true;
       };

  };

}
