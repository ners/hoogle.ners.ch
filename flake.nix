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
    syntax = {
      url = "github:ners/syntax/bytes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dosh = {
      url = "github:ners/dosh/rhine";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        rhine.follows = "rhine";
      };
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
    servant-effectful = {
      url = "github:Diamondy4/servant-effectful";
      flake = false;
    };
    wai-middleware-auth = {
      url = "github:NoRedInk/wai-middleware-auth";
      flake = false;
    };
    wreq-effectful = {
      url = "github:The1Penguin/wreq-effectful";
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
          (final: prev: with prev.haskell.lib.compose; {
            haskell = prev.haskell // {
              packageOverrides = lib.composeManyExtensions [
                prev.haskell.packageOverrides
                (hfinal: hprev: {
                  dependent-sum-template = hprev.dependent-sum-template_0_2_0_1;
                  dhall = doJailbreak hprev.dhall;
                  dhall-json = doJailbreak hprev.dhall-json;
                  dhall-yaml = doJailbreak hprev.dhall-yaml;
                  discord-haskell-voice = addBuildDepend hfinal.opus hprev.discord-haskell-voice;
                  ekg-wai = doJailbreak hprev.ekg-wai;
                  fir = dontCheck (doJailbreak (hfinal.callCabal2nix "fir" inputs.fir { }));
                  fraxl = doJailbreak (hfinal.callCabal2nix "fraxl" inputs.fraxl { });
                  greskell = doJailbreak hprev.greskell;
                  greskell-websocket = doJailbreak hprev.greskell-websocket;
                  gtk = appendPatch
                    (prev.fetchpatch {
                      url = "https://github.com/gtk2hs/gtk2hs/commit/ca7f98bd3e9462deac3661244dc76004a36fc8c3.patch";
                      hash = "sha256-FaIohq7pEA4OnX/b6hBwsF5wcRO3uBtE2IsabJDGKT4=";
                    })
                    hprev.gtk;
                  heftia = hprev.callHackage "heftia" "0.5.0.0" { };
                  heftia-effects = hprev.callHackage "heftia-effects" "0.5.0.0" { };
                  hyperbole =
                    let
                      hp = prev.haskellPackages.override {
                        overrides = lib.composeManyExtensions [
                          (hfinal: hprev: {
                            data-default = hprev.data-default_0_8_0_0;
                            effectful = hprev.effectful_2_5_1_0;
                            effectful-core = hprev.effectful-core_2_5_1_0;
                            http-api-data = doJailbreak hprev.http-api-data_0_6_1;
                            hyperbole = hfinal.callHackageDirect
                              {
                                pkg = "hyperbole";
                                ver = "0.4.3";
                                sha256 = "sha256-dnftn/JYjuYIjn2DwcghUk4cyK4jl3jWCXggYW6tLk0=";
                              }
                              { };
                            web-view = dontCheck (doJailbreak (hprev.callHackageDirect
                              {
                                pkg = "web-view";
                                ver = "0.7.0";
                                sha256 = "sha256-50bJvoffv/ZPR98qNgqO+aFfziBAZW4mh66IoCsFBgo=";
                              }
                              { }));
                          })
                          (unbreak-all prev)
                        ];
                      };
                    in
                    dontCheck hp.hyperbole;
                  json-rpc = hprev.json-rpc_1_1_1;
                  kubernetes-client = hfinal.callCabal2nix "kubernetes-client" "${resolveLinks inputs.kubernetes-client}/kubernetes-client" { };
                  kubernetes-client-core = hfinal.callCabal2nix "kubernetes-client-core" "${resolveLinks inputs.kubernetes-client}/kubernetes-1.30" { };
                  mighttpd2 =
                    let
                      hp = prev.haskellPackages.override {
                        overrides = _: hprev: {
                          auto-update = hprev.auto-update_0_2_6;
                          crypton-connection = hprev.crypton-connection_0_4_3;
                          http-semantics = hprev.http-semantics_0_3_0;
                          http2 = hprev.http2_5_3_9;
                          http3 = dontCheck hprev.http3;
                          network = hprev.network_3_2_7_0;
                          network-control = hprev.network-control_0_1_3;
                          network-run = hprev.network-run_0_4_0;
                          quic = dontCheck (unmarkBroken hprev.quic);
                          time-manager = hprev.time-manager_0_2_2;
                          tls = hprev.tls_2_1_5;
                          tls-session-manager = hprev.tls-session-manager_0_0_7;
                          wai-app-file-cgi = dontCheck (unmarkBroken hprev.wai-app-file-cgi);
                          warp = dontCheck (hprev.callHackage "warp" "3.4.7" { });
                        };
                      };
                    in
                    lib.pipe hp.mighttpd2 [
                      unmarkBroken
                      doJailbreak
                      dontCheck
                      dontHaddock
                      (enableCabalFlag "tls")
                      (enableCabalFlag "dhall")
                      (enableCabalFlag "quic")
                      (addBuildDepends (with hp; [
                        dhall
                        quic
                        warp-quic
                        warp-tls
                        prev.libcap.dev
                      ]))
                    ];
                  morpheus-graphql-app = hprev.morpheus-graphql-app_0_28_1;
                  morpheus-graphql-client = hprev.morpheus-graphql-client_0_28_1;
                  morpheus-graphql-code-gen-utils = hprev.morpheus-graphql-code-gen-utils_0_28_1;
                  morpheus-graphql-core = hprev.morpheus-graphql-core_0_28_1;
                  morpheus-graphql-server = hprev.morpheus-graphql-server_0_28_1;
                  morpheus-graphql-subscriptions = hprev.morpheus-graphql-subscriptions_0_28_1;
                  morpheus-graphql-tests = hprev.morpheus-graphql-tests_0_28_1;
                  notifications-tray-icon = hprev.notifications-tray-icon.overrideAttrs (attrs: {
                    postPatch = ''
                      sed -i 's,GLib.mainContextInvokeFull context,GLib.mainContextInvokeFull (Just context),' src/StatusNotifier/Item/Notifications/OverlayIcon.hs
                    '';
                  });
                  opus = addPkgconfigDepend prev.libopus (hfinal.callCabal2nix "opus" inputs.opus { });
                  perf = dontCheck (doJailbreak hprev.perf_0_14_0_1);
                  chart-svg = hprev.chart-svg_0_8_0_0;
                  numhask = hprev.numhask_0_12_1_0;
                  numhask-space = hprev.numhask-space_0_12_0_0;
                  co-log-effectful = hfinal.callCabal2nix "co-log-effectful" inputs.co-log-effectful { };
                  servant-effectful = dontCheck (hfinal.callCabal2nix "servant-effectful" inputs.servant-effectful { });
                  wai-middleware-auth = dontCheck (hfinal.callCabal2nix "wai-middleware-auth" inputs.wai-middleware-auth { });
                  hoauth2 = doJailbreak (hprev.callHackage "hoauth2" "2.1.0" {});
                  servant-oauth2 = hprev.servant-oauth2.overrideAttrs (attrs: {
                    postPatch = ''
                      ${attrs.postPatch or ""}
                      sed -i 's/, OA2.oauth2RedirectUri = callbackURI/, OA2.oauth2RedirectUri = Just callbackURI/' src/Servant/OAuth2/Hacks.hs
                    '';
                  });
                  wreq-effectful = hfinal.callCabal2nix "wreq-effectful" inputs.wreq-effectful { };
                  servant-serialization = dontCheck (doJailbreak hprev.servant-serialization);
                  servant-rate-limit = dontCheck hprev.servant-rate-limit;
                  stroll = doJailbreak (hfinal.callCabal2nix "stroll" inputs.stroll { });
                  sdl2-mixer = lib.pipe hprev.sdl2-mixer [
                    (drv: drv.overrideAttrs (attrs: {
                      strictDeps = true;
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
        in
        map fixPackage [
          #discord-haskell-voice
          #heftia-effects
          #tasty-flaky
          #websockets-rpc
          GLFW-b
          JuicyCairo
          JuicyPixels-extra
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
          hinotify
          hnix
          hoauth2
          hspec
          hspec-webdriver
          htmx-servant
          http-media
          http-types
          hyperbole
          i3ipc
          inline-c
          inline-c-cpp
          io-classes
          io-sim
          jose
          jose-jwt
          json-rpc
          ki-effectful
          kubernetes-client
          lens
          lens-family-th
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
          parser-combinators
          path
          path-io
          path-text-utf8
          perf
          plot
          plots
          pretty-simple
          prettyprinter
          process-extras
          qrcode-juicypixels
          quickcheck-webdriver
          random
          random-fu
          rasterific-svg
          rattle
          rediscaching-haxl
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
          sdl2-image
          sdl2-mixer
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
          stroll
          svgcairo
          syntax
          syntax-attoparsec
          syntax-printer
          systemd
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
          wai-extra
          wai-logger
          wai-middleware-auth
          wai-transformers
          warp
          warp-systemd
          waterfall-cad
          websockets-json
          websockets-simple
          wreq-effectful
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
