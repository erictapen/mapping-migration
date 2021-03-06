{ nixpkgs ? <nixpkgs>
, config ? {}
, revision
}:

with (import nixpkgs config);

let
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? []
    , registryDat ? ./registry.dat
    , outputJavaScript ? false
    }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm ]
        ++ lib.optional outputJavaScript nodePackages.uglify-js;

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion = "0.19.1";
        inherit registryDat;
      };

      installPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
        extension = if outputJavaScript then "js" else "html";
        revision_url = "https://github.com/erictapen/mapping-migration/" + (if revision == "dirty" then "" else "commit/${revision}");
      in ''
        sed -i 's|GIT_REV|${revision}|g' src/Introduction.elm
        sed -i 's|GITHUB_URL|${revision_url}|g' src/Introduction.elm

        mkdir -p $out/share/doc $out/assets
        ${lib.concatStrings (map (module: ''
          echo "compiling ${elmfile module}"
          elm make ${elmfile module} --optimize --output $out/assets/${module}.${extension} --docs $out/share/doc/${module}.json
          ${lib.optionalString outputJavaScript ''
            echo "minifying ${elmfile module}"
            uglifyjs $out/assets/${module}.${extension} --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
                | uglifyjs --mangle --output $out/assets/${module}.min.${extension}
          ''}
        '') targets)}
        cp ${./assets}/index.html $out/
        cp ${./assets}/style.css $out/assets/
        cp ${./assets}/explore-data-button.svg $out/assets/
        cp ${./assets}/*.png $out/assets/
        cp ${./README.md} $out/README.md
        cp ${./.htaccess} $out/.htaccess
        cp ${./assets}/.htaccess $out/assets/.htaccess
        ${imagemagick}/bin/magick convert assets/favicon-32.png assets/favicon-16.png $out/assets/favicon.ico
        cp ${google-fonts}/share/fonts/truetype/Karla-* $out/assets/
        ${haskellPackages.webify}/bin/webify --no-svg $out/assets/Karla-*.ttf
      '';
    };
in mkDerivation {
  name = "elm-app-0.1.0";
  srcs = ./elm-srcs.nix;
  src = ./.;
  targets = ["Main"];
  srcdir = "./src";
  outputJavaScript = true;
}

