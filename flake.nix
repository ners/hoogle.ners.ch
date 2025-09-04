{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    imgui = {
      url = "github:ocornut/imgui/v1.90.9";
      flake = false;
    };
    rhine = {
      url = "github:turion/rhine";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    imgui-hs = {
      url = "github:haskell-game/dear-imgui.hs";
      flake = false;
    };
    fclabels = {
      url = "github:GuillaumedeVolpiano/fclabels";
      flake = false;
    };
    fir = {
      url = "git+https://gitlab.com/sheaf/fir.git";
      flake = false;
    };
    fraxl = {
      url = "github:russellmcc/fraxl/bump-dependency-versions";
      flake = false;
    };
    kubernetes-client = {
      url = "github:kubernetes-client/haskell";
      flake = false;
    };
    stroll = {
      url = "github:snowleopard/stroll";
      flake = false;
    };
    opus = {
      url = "github:yutotakano/opus";
      flake = false;
    };
    nixcon-vouchers = {
      url = "github:nixcon/nixcon-vouchers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    postgres-effectful = {
      url = "github:sekunho/postgres-effectful";
      flake = false;
    };
    syntax = {
      url = "github:ners/syntax/bytes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dosh = {
      url = "github:ners/dosh/rhine";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rhine-chat = {
      url = "github:ners/rhine-chat";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        rhine.follows = "rhine";
        vodozemac.follows = "vodozemac";
      };
    };
    rhine-linux = {
      url = "github:ners/rhine-linux";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        rhine.follows = "rhine";
      };
    };
    rhine-sdl2 = {
      url = "github:ners/rhine-sdl2/rhine-kobl";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        rhine.follows = "rhine";
      };
    };
    co-log-effectful = {
      url = "github:eldritch-cookie/co-log-effectful";
      flake = false;
    };
    vodozemac = {
      url = "github:ners/vodozemac-haskell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      unbreak-all = prev: hfinal: with lib; with prev.haskell.lib.compose; mapAttrs (name: v: pipe v (concatLists [
        (optionals (isDerivation v && v.override.__functionArgs == { }) (concatLists [
          (optionals v.meta.broken [
            (trace "unbreaking ${v.name}")
            doJailbreak
            dontCheck
            unmarkBroken
          ])
        ]))
      ]));
      overlay = final: prev:
        let
          inherit (prev) lib;
          resolveLinks = input: prev.runCommandNoCC "source" { } /*bash*/ ''
            cd ${input}
            find . -not -type d | while read f; do
              mkdir -p $out/$(dirname $f)
              ln -s $(realpath $f) $out/$f
            done
          '';
        in
        lib.composeManyExtensions [
          inputs.dosh.overlays.default
          inputs.rhine-chat.overlays.default
          inputs.rhine-linux.overlays.default
          inputs.rhine-sdl2.overlays.default
          inputs.syntax.overlays.default
          inputs.vodozemac.overlays.default
          inputs.nixcon-vouchers.overlays.default
          (final: prev: with prev.haskell.lib.compose; {
            haskell = prev.haskell // {
              packageOverrides = lib.composeManyExtensions [
                prev.haskell.packageOverrides
                (hfinal: hprev: {
                  dhall = doJailbreak hprev.dhall;
                  dhall-json = doJailbreak hprev.dhall-json;
                  dhall-yaml = doJailbreak hprev.dhall-yaml;
                  discord-haskell-voice = addBuildDepend hfinal.opus hprev.discord-haskell-voice;
                  ekg-wai = doJailbreak hprev.ekg-wai;
                  fclabels = hfinal.callCabal2nix "fclabels" inputs.fclabels { };
                  fir = dontCheck (doJailbreak (hfinal.callCabal2nix "fir" inputs.fir { }));
                  fraxl = doJailbreak (hfinal.callCabal2nix "fraxl" inputs.fraxl { });
                  g2 = dontCheck (doJailbreak hprev.g2);
                  g2q = dontCheck (doJailbreak hprev.g2q);
                  greskell = doJailbreak hprev.greskell;
                  greskell-websocket = doJailbreak hprev.greskell-websocket;
                  gtk = appendPatch
                    (prev.fetchpatch {
                      url = "https://github.com/gtk2hs/gtk2hs/commit/ca7f98bd3e9462deac3661244dc76004a36fc8c3.patch";
                      hash = "sha256-FaIohq7pEA4OnX/b6hBwsF5wcRO3uBtE2IsabJDGKT4=";
                    })
                    hprev.gtk;
                  heftia = hprev.callHackage "heftia" "0.7.0.0" { };
                  heftia-effects = hprev.callHackage "heftia-effects" "0.7.0.0" { };
                  kubernetes-client = hfinal.callCabal2nix "kubernetes-client" "${resolveLinks inputs.kubernetes-client}/kubernetes-client" { };
                  kubernetes-client-core = hfinal.callCabal2nix "kubernetes-client-core" "${resolveLinks inputs.kubernetes-client}/kubernetes-1.30" { };

                  OTP = lib.pipe hprev.OTP [
                    unmarkBroken
                    (drv: drv.overrideAttrs (attrs: {
                      patches = (attrs.patches or []) ++ [
                        ./patches/OTP-remove-flag.patch
                      ];
                    }))
                    (overrideCabal {
                      libraryHaskellDepends = [ hprev.SHA ];
                    })
                  ];

                  lens-process = addSetupDepend hprev.cabal-doctest hprev.lens-process;
                  langchain-hs = dontCheck (hfinal.callHackageDirect
                    {
                      pkg = "langchain-hs";
                      ver = "0.0.2.0";
                      sha256 = "sha256-DuLaD7+NdrVW55fvle4/xnPjHCnv2qQgh3wRSi4W8Jg=";
                    }
                    {
                      ollama-haskell = dontCheck (doJailbreak (unmarkBroken hprev.ollama-haskell));
                    });
                  ollama-haskell = dontCheck (hfinal.callHackageDirect
                    {
                      pkg = "ollama-haskell";
                      ver = "0.2.0.0";
                      sha256 = "sha256-ZnISgJgNzag64LjkeY8zwBgokbwVQ3fwTQ5lMDh8j6I=";
                    }
                    { });
                  tmp-postgres = dontCheck (hfinal.callCabal2nix "tmp-postgres"
                    (prev.fetchFromGitHub {
                      owner = "bitnomial";
                      repo = "tmp-postgres";
                      rev = "64f65752a14ffde5f8c68ce15d0ed5eedc7aeb1d";
                      hash = "sha256-OYmKDwrkrhcXkLxGN1NKCWcZ1VC/V+JSnjD5mEGb0AU=";
                    })
                    { });
                  notifications-tray-icon = hprev.notifications-tray-icon.overrideAttrs (attrs: {
                    postPatch = ''
                      sed -i 's,GLib.mainContextInvokeFull context,GLib.mainContextInvokeFull (Just context),' src/StatusNotifier/Item/Notifications/OverlayIcon.hs
                    '';
                  });
                  opus = addPkgconfigDepend prev.libopus (hfinal.callCabal2nix "opus" inputs.opus { });
                  parsable = hfinal.callHackageDirect
                    {
                      pkg = "parsable";
                      ver = "0.1.0.0";
                      sha256 = "sha256-G+TmJS0AeJ6ZZSGgi1S7zDxlk0rtGCmgNM26vFr248w=";
                    }
                    { };
                  targeted-quickcheck = doJailbreak (hfinal.callHackageDirect
                    {
                      pkg = "targeted-quickcheck";
                      ver = "0.1.0.2";
                      sha256 = "sha256-sasgcmmj/TFPc5Rw/oW7KAPP0An7xc6uyOfQICsGFMg=";
                    }
                    { });
                  perf = dontCheck (doJailbreak hprev.perf_0_14_0_2);
                  postgres-effectful = dontCheck (doJailbreak (hfinal.callCabal2nix "postgres-effectful" inputs.postgres-effectful { }));
                  prettychart = hprev.prettychart_0_3_0_1;
                  chart-svg = hprev.chart-svg_0_8_1_0;
                  changeset = dontCheck hprev.changeset;
                  numhask-space = hprev.numhask-space_0_13_0_0;
                  rel8 = doJailbreak hprev.rel8;
                  hasql-mover = hfinal.callCabal2nix "hasql-mover"
                    (prev.fetchFromGitHub {
                      owner = "mikeplus64";
                      repo = "hasql-mover";
                      rev = "d852112c2bc5b7824a8fa82ecb1a3eab05293c90";
                      hash = "sha256-X/IvIPDOTuU47YBaui25yGIuGNtWk7tSuekpMA9e9F0=";
                    })
                    { };
                  hasql-queue = hprev.hasql-queue.overrideAttrs (attrs: {
                    patchPhase = ''
                      ${attrs.patchPhase or ""}
                      sed -i 's/QueryError/SessionError/g' src/Hasql/Queue/Internal.hs
                    '';
                  });
                  co-log-effectful = hfinal.callCabal2nix "co-log-effectful" inputs.co-log-effectful { };
                  qrcode-juicypixels = doJailbreak hprev.qrcode-juicypixels;
                  servant-serialization = dontCheck (doJailbreak hprev.servant-serialization);
                  servant-rate-limit = dontCheck hprev.servant-rate-limit;
                  stroll = doJailbreak (hfinal.callCabal2nix "stroll" inputs.stroll { });
                  sdl2-image = lib.pipe hprev.sdl2-image [
                    (drv: drv.overrideAttrs (attrs: {
                      dontWrapQtApps = true;
                    }))
                  ];
                  sdl2-mixer = lib.pipe hprev.sdl2-mixer [
                    (drv: drv.overrideAttrs (attrs: {
                      dontWrapQtApps = true;
                    }))
                  ];
                  dear-imgui = (hfinal.callCabal2nix "dear-imgui" inputs.imgui-hs { }).overrideAttrs (attrs: {
                    postPatch = ''
                      ${attrs.postPatch or ""}
                      rmdir ./imgui
                      ln -s ${inputs.imgui} ./imgui
                    '';
                    buildInputs = with prev; [
                      gcc
                      glew
                      SDL2
                    ]
                    ++ attrs.nativeBuildInputs or [ ];
                  });
                })
                (unbreak-all prev)
              ];
            };
          })
        ]
          final
          prev;
      packagesFor = pkgs: with pkgs.haskellPackages;
        let
          fixPackage = drv: lib.pipe drv [
            pkgs.haskell.lib.compose.doHaddock
            (drv: drv.overrideAttrs (_: { meta.platforms = lib.platforms.all; }))
          ];
          inherit (pkgs.haskell.packages.ghc98) json-spec-openapi;
        in
        map fixPackage [
          #discord-haskell-voice
          #g2q
          #hasql-effectful
          #heftia-effects
          #quickcheck-lockstep
          #sdl2-image
          #sdl2-mixer
          #tasty-flaky
          #websockets-json
          #websockets-rpc
          AesonBson
          Blammo-wai
          GLFW-b
          JuicyCairo
          JuicyPixels-extra
          OTP
          QuickCheck
          Rasterific
          aeson
          ansi-terminal
          attoparsec
          blaze-htmx
          brick
          brick-skylighting
          clash-ghc
          clash-shake
          co-log
          co-log-effectful
          co-log-json
          crem
          criterion
          data-default
          data-fix
          dhall
          dhall-csv
          dhall-recursive-adt
          dhall-toml
          dhall-yaml
          diagrams
          diagrams-cairo
          diagrams-pandoc
          diagrams-svg
          dimensional
          dimensional-codata
          discord-haskell
          dosh
          effectful
          either
          ekg
          ekg-prometheus-adapter
          ekg-statsd
          ekg-wai
          esqueleto
          extra
          file-embed
          fir
          fraxl
          generic-arbitrary
          generic-lens
          generic-lens-lite
          generic-optics
          generic-optics-lite
          gerrit
          ghc-syntax-highlighter
          github
          github-rest
          greskell
          greskell-websocket
          hashable
          haskell-modbus
          hasql-migration
          hasql-mover
          hasql-queue
          hasql-th
          hinotify
          hnix
          hoauth2
          hspec
          hspec-webdriver
          htmx-servant
          http-media
          http-types
          i3ipc
          inline-c
          inline-c-cpp
          io-classes
          io-sim
          jose
          jose-jwt
          json-rpc
          json-spec-openapi
          ki-effectful
          kubernetes-client
          langchain-hs
          lens
          lens-family-th
          lens-process
          lens-regex
          lens-time
          libmodbus
          lifted-base
          log-effectful
          lrucaching-haxl
          lsp-client
          lsp-test
          matrix-client
          megaparsec
          microlens
          microlens-th
          mighttpd2
          miso
          monad-control
          monad-logger
          monad-logger-aeson
          monad-logger-extras
          monad-metrics
          monad-time-effectful
          monomer
          monomer-hagrid
          morpheus-graphql
          morpheus-graphql-client
          morpheus-graphql-server
          morpheus-graphql-subscriptions
          myers-diff
          named
          net-mqtt
          net-mqtt-lens
          net-mqtt-rpc
          notifications-tray-icon
          numhask
          numhask-space
          opaleye
          openapi3
          optics
          optparse-applicative
          optparse-generic
          optparse-simple
          optparse-th
          os-release
          pango
          parsable
          parser-combinators
          path
          path-io
          path-text-utf8
          perf
          plot
          plots
          postgres-effectful
          pretty-simple
          prettyprinter
          process-extras
          qrcode-juicypixels
          quickcheck-dynamic
          quickcheck-state-machine
          quickcheck-webdriver
          random
          random-fu
          rasterific-svg
          rattle
          rediscaching-haxl
          regex-applicative-text
          rel8
          replace-attoparsec
          retry-effectful
          rhine-dbus
          rhine-i3
          rhine-inotify
          rhine-sdl2
          rhine-terminal
          rhine-udev
          rhine-v4l2
          rio
          science-constants-dimensional
          sdl2-ttf
          servant-blaze
          servant-client
          servant-effectful
          servant-jsonrpc
          servant-multipart-client
          servant-oauth2
          servant-openapi3
          servant-quickcheck
          servant-rate-limit
          servant-serialization
          servant-server
          servant-websockets
          shake
          shake-bench
          shake-c
          shake-cabal
          shake-dhall
          shake-ext
          shake-language-c
          shake-literate
          shake-path
          shake-persist
          shake-plus
          shakespeare
          shelly
          simple-cairo
          skylighting
          slick
          stm-containers
          string-random
          stroll
          svgcairo
          syntax
          syntax-attoparsec
          syntax-printer
          systemd
          targeted-quickcheck
          tasty-bench
          tasty-discover
          tasty-hspec
          tasty-inspection-testing
          tasty-program
          tasty-wai
          telegram-bot-simple
          text-rope-zipper
          these
          turtle
          typechain
          typed-process-effectful
          units
          unliftio
          unordered-containers
          uuid
          vec
          vec-lens
          vector
          vodozemac
          vty
          vulkan
          vulkan-utils
          wai
          wai-cli
          wai-cors
          wai-extra
          wai-logger
          wai-middleware-auth
          wai-transformers
          warp
          warp-systemd
          waterfall-cad
          websockets-simple
          wreq-effectful
          zip
        ];
      nixosModule = { pkgs, ... }:
        let pkgs' = pkgs.extend overlay; in
        {
          services.hoogle = {
            inherit (pkgs') haskellPackages;
            packages = _: packagesFor pkgs';
          };
        };
    in
    {
      overlays.default = overlay;
      nixosModules.default = nixosModule;
    }
    //
    foreach inputs.nixpkgs.legacyPackages (system: pkgs:
      let pkgs' = pkgs.extend overlay; in
      {
        formatter.${system} = pkgs.nixpkgs-fmt;
        legacyPackages.${system} = pkgs.extend overlay;
        packages.${system}.default = pkgs.symlinkJoin {
          name = "hoogle-packages";
          paths = map (lib.getOutput "doc") (packagesFor pkgs');
        };
      }
    );
}
