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
    ekg-core = {
      url = "github:tibbe/ekg-core";
      flake = false;
    };
    ekg-json = {
      url = "github:vshabanov/ekg-json/aeson-2.0";
      flake = false;
    };
    fir = {
      url = "git+https://gitlab.com/sheaf/fir.git";
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
      url = "github:ners/syntax";
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
      overlay = final: prev:
        let inherit (prev) lib; in
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
                  ekg = doJailbreak hprev.ekg;
                  ekg-core = doJailbreak (hfinal.callCabal2nix "ekg-core" inputs.ekg-core { });
                  ekg-json = doJailbreak (hfinal.callCabal2nix "ekg-json" inputs.ekg-json { });
                  ekg-wai = doJailbreak hprev.ekg-wai;
                  fir = dontCheck (doJailbreak (hfinal.callCabal2nix "fir" inputs.fir { }));
                  greskell = doJailbreak hprev.greskell;
                  greskell-websocket = doJailbreak hprev.greskell-websocket;
                  json-rpc = hprev.json-rpc_1_1_1;
                  mighttpd2 =
                    let
                      hp = prev.haskellPackages.override {
                        overrides = _: hprev: {
                          auto-update = hprev.auto-update_0_2_1;
                          crypton-connection = hprev.crypton-connection_0_4_1;
                          http2 = hprev.http2_5_3_4;
                          http3 = dontCheck hprev.http3;
                          network-control = hprev.network-control_0_1_3;
                          network-run = hprev.network-run_0_4_0;
                          quic = unmarkBroken hprev.quic;
                          time-manager = hprev.time-manager_0_1_0;
                          tls = hprev.tls_2_1_1;
                          tls-session-manager = hprev.tls-session-manager_0_0_6;
                          wai-app-file-cgi = dontCheck (unmarkBroken hprev.wai-app-file-cgi);
                          warp = dontCheck (hprev.callHackageDirect
                            {
                              pkg = "warp";
                              ver = "3.4.3";
                              sha256 = "sha256-XkDd5X2zjOwrQ0Mjv/frk49NC//g4IsTUMrJVkSY9qE=";
                            }
                            { });
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
                  servant-serialization = dontCheck (doJailbreak hprev.servant-serialization);
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
                (hfinal: with lib; mapAttrs (name: v: pipe v (concatLists [
                  (optionals (isDerivation v && v.override.__functionArgs == { }) (concatLists [
                    (optionals v.meta.broken [
                      (trace "unbreaking ${v.name}")
                      doJailbreak
                      dontCheck
                      unmarkBroken
                    ])
                  ]))
                ])))
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
          #shakebook
          GLFW-b
          JuicyCairo
          JuicyPixels-extra
          QuickCheck
          Rasterific
          aeson
          ansi-terminal
          attoparsec
          brick
          brick-skylighting
          clash-ghc
          clash-shake
          crem
          data-default
          data-fix
          dhall
          dhall-csv
          dhall-recursive-adt
          dhall-toml
          dhall-yaml
          diagrams
          diagrams
          diagrams-cairo
          diagrams-gtk
          diagrams-pandoc
          discord-haskell
          dosh
          either
          ekg
          ekg-prometheus-adapter
          ekg-statsd
          ekg-wai
          esqueleto
          extra
          file-embed
          fir
          freetype2
          generic-arbitrary
          generic-lens
          generic-lens-lite
          generic-optics
          generic-optics-lite
          ghc-syntax-highlighter
          github
          github-rest
          greskell
          greskell-websocket
          hashable
          hinotify
          hnix
          hspec
          http-media
          http-types
          i3ipc
          inline-c
          inline-c-cpp
          jose
          jose-jwt
          json-rpc
          lens
          lens-family-th
          lens-regex
          lens-time
          lifted-base
          lrucaching-haxl
          lsp-client
          lsp-test
          matrix-client
          megaparsec
          microlens
          microlens-th
          mighttpd2
          monad-control
          monad-logger
          monad-logger-aeson
          monad-logger-extras
          monad-metrics
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
          random
          random-fu
          rasterific-svg
          rattle
          rediscaching-haxl
          replace-attoparsec
          rhine-dbus
          rhine-i3
          rhine-inotify
          rhine-sdl2
          rhine-terminal
          rhine-udev
          rhine-v4l2
          rio
          sdl2-image
          sdl2-mixer
          sdl2-ttf
          servant-blaze
          servant-client
          servant-multipart-client
          servant-openapi3
          servant-quickcheck
          servant-serialization
          servant-server
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
          telegram-bot-simple
          text-rope-zipper
          these
          turtle
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
          wai-websockets
          warp
          warp-systemd
          websockets
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
