{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:ners/nixpkgs/nixos-unstable";
    co-log-effectful = {
      url = "github:eldritch-cookie/co-log-effectful";
      flake = false;
    };
    dashi = {
      url = "github:ners/dashi";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dosh = {
      url = "github:ners/dosh/rhine";
      inputs.nixpkgs.follows = "nixpkgs";
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
    hasql-migration = {
      url = "github:Skyfold/hasql-migration";
      flake = false;
    };
    imgui = {
      url = "github:ocornut/imgui/v1.90.9";
      flake = false;
    };
    imgui-hs = {
      url = "github:haskell-game/dear-imgui.hs";
      flake = false;
    };
    kubernetes-client = {
      url = "github:kubernetes-client/haskell";
      flake = false;
    };
    langchain-hs = {
      url = "github:tusharad/langchain-hs";
      flake = false;
    };
    miso = {
      url = "github:dmjio/miso";
      flake = false;
    };
    nixcon-vouchers = {
      url = "github:nixcon/nixcon-vouchers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ollama-haskell = {
      url = "github:tusharad/ollama-haskell";
      flake = false;
    };
    opus = {
      url = "github:yutotakano/opus";
      flake = false;
    };
    postgres-effectful = {
      url = "github:sekunho/postgres-effectful";
      flake = false;
    };
    pup = {
      url = "github:tweag/pup";
      flake = false;
    };
    rhine = {
      url = "github:ners/rhine/update";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rhine-chat = {
      url = "github:ners/rhine-chat";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rhine-linux = {
      url = "github:ners/rhine-linux";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rhine-sdl2 = {
      url = "github:ners/rhine-sdl2";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stacked = {
      url = "github:tweag/stacked";
      flake = false;
    };
    stroll = {
      url = "github:snowleopard/stroll";
      flake = false;
    };
    syntax = {
      url = "github:ners/syntax/bytes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    vodozemac = {
      url = "github:ners/vodozemac-haskell";
      inputs.nixpkgs.follows = "nixpkgs";
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
        (optionals (isDerivation v) (concatLists [
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
          inputs.rhine.overlays.default
          inputs.rhine-chat.overlays.default
          inputs.rhine-linux.overlays.default
          inputs.rhine-sdl2.overlays.default
          inputs.syntax.overlays.default
          inputs.vodozemac.overlays.default
          inputs.nixcon-vouchers.overlays.default
          inputs.dashi.overlays.default
          (final: prev: with prev.haskell.lib.compose; {
            haskell = prev.haskell // {
              packageOverrides = lib.composeManyExtensions [
                prev.haskell.packageOverrides
                inputs.dosh.overlays.haskell
                (hfinal: hprev: {
                  bluefin-random = hfinal.callHackageDirect
                    {
                      pkg = "bluefin-random";
                      ver = "0.0.16.1";
                      sha256 = "sha256-3hrZkcl3cYzFxVt/sLiuuH7vtQN+P2CaDbWW2/Tb5TA=";
                    }
                    {
                      random = hprev.random_1_3_1;
                    };
                  clash-prelude = dontCheck (hfinal.callHackageDirect
                    {
                      pkg = "clash-prelude";
                      ver = "1.8.3";
                      sha256 = "sha256-icmvs3m749A7vgF3C643kfgsMPmvhIh34gSmjWUni6I=";
                    }
                    { });
                  clash-ghc = dontCheck (hfinal.callHackageDirect
                    {
                      pkg = "clash-ghc";
                      ver = "1.8.3";
                      sha256 = "sha256-udCNmEWrQ+uWezgcLccuhzDDgZPNvCTHccAgvz9K2PQ=";
                    }
                    { });
                  clash-lib = dontCheck (hfinal.callHackageDirect
                    {
                      pkg = "clash-lib";
                      ver = "1.8.3";
                      sha256 = "sha256-ZSMR2uBS4BWSYgVGz47FDrAxDfgjCvvfma5quGf2kzo=";
                    }
                    { });
                  dhall = doJailbreak hprev.dhall;
                  dhall-json = doJailbreak hprev.dhall-json;
                  dhall-yaml = doJailbreak hprev.dhall-yaml;
                  diagrams-pandoc = dontCheck hprev.diagrams-pandoc;
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
                  shake-path = doJailbreak hprev.shake-path;
                  heftia = hprev.callHackage "heftia" "0.7.0.0" { };
                  heftia-effects = hprev.callHackage "heftia-effects" "0.7.0.0" { };
                  kubernetes-client =
                    lib.pipe inputs.kubernetes-client [
                      resolveLinks
                      (dir: hfinal.callCabal2nix "kubernetes-client" "${dir}/kubernetes-client" { })
                      (drv: drv.overrideAttrs (attrs: {
                        postPatch = ''
                          ${attrs.postPatch or ""}
                          sed -i 's/ show / Prelude.show /' src/Kubernetes/Data/*.hs src/Kubernetes/Client/Auth/OIDC.hs
                        '';
                      }))
                    ];
                  kubernetes-client-core = hfinal.callCabal2nix "kubernetes-client-core" "${resolveLinks inputs.kubernetes-client}/kubernetes-1.30" { };

                  OTP = lib.pipe hprev.OTP [
                    unmarkBroken
                    (drv: drv.overrideAttrs (attrs: {
                      patches = (attrs.patches or [ ]) ++ [
                        ./patches/OTP-remove-flag.patch
                      ];
                    }))
                    (overrideCabal {
                      libraryHaskellDepends = [ hprev.SHA ];
                    })
                  ];

                  lens-process = doJailbreak (addSetupDepend hprev.cabal-doctest hprev.lens-process);
                  ollama-haskell = hfinal.callCabal2nix "ollama-haskell" inputs.ollama-haskell { };
                  langchain-hs = dontCheck (hfinal.callCabal2nix "langchain-hs" inputs.langchain-hs { });
                  miso = hfinal.callCabal2nix "miso" inputs.miso { };
                  monomer-hagrid = doJailbreak hprev.monomer-hagrid;
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
                      ${attrs.postPatch or ""}
                      sed -i 's,GLib.mainContextInvokeFull context,GLib.mainContextInvokeFull (Just context),' src/StatusNotifier/Item/Notifications/OverlayIcon.hs
                      sed -i 's,subjectURL \$ notificationSubject notification,fromJust $ &,' src/StatusNotifier/Item/Notifications/GitHub.hs
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
                  postgres-effectful = dontCheck (doJailbreak (hfinal.callCabal2nix "postgres-effectful" inputs.postgres-effectful { }));
                  changeset = dontCheck hprev.changeset;
                  ki-effectful = doJailbreak hprev.ki-effectful;
                  retry-effectful = doJailbreak hprev.retry-effectful;
                  typed-process-effectful = dontCheck (doJailbreak hprev.typed-process-effectful);
                  plots = doJailbreak hprev.plots;
                  text-rope-zipper = doJailbreak hprev.text-rope-zipper;
                  typechain = doJailbreak hprev.typechain;
                  dhall-csv = dontCheck (doJailbreak hprev.dhall-csv);
                  dhall-recursive-adt = doJailbreak hprev.dhall-recursive-adt;
                  shake-dhall = dontCheck hprev.shake-dhall;
                  path-text-utf8 = doJailbreak hprev.path-text-utf8;
                  shake-persist = doJailbreak hprev.shake-persist;
                  lens-time = doJailbreak hprev.lens-time;
                  quickcheck-webdriver = doJailbreak hprev.quickcheck-webdriver;
                  rel8 = doJailbreak hprev.rel8;
                  stacked = hprev.callCabal2nix "stacked" inputs.stacked { };
                  pup = hprev.callCabal2nix "pup" inputs.pup { };
                  hasql-effectful = setBuildTarget "hasql-effectful" (dontCheck (doJailbreak hprev.hasql-effectful));
                  hasql-migration = dontCheck (hprev.callCabal2nix "hasql-migration" inputs.hasql-migration { });
                  hasql-mover = doJailbreak (hfinal.callCabal2nix "hasql-mover"
                    (prev.fetchFromGitHub {
                      owner = "mikeplus64";
                      repo = "hasql-mover";
                      rev = "70de5450b170902ab4d1afb4d055c88e97051124";
                      hash = "sha256-+z3VDkoqdFXiUNA/GErF2QOPbcySUhZEPbFZWcj+T/E=";
                    })
                    { });
                  hasql-queue = hprev.hasql-queue.overrideAttrs (attrs: {
                    patchPhase = ''
                      ${attrs.patchPhase or ""}
                      sed -i 's/QueryError/SessionError/g' src/Hasql/Queue/Internal.hs
                      echo "module Main where" | cat - hasql-queue-tmp-db/Main.hs | tee hasql-queue-tmp-db/Main.hs >/dev/null
                      sed -i 's/module Main where/&\nimport Data.Text.Encoding (decodeUtf8)\nimport Hasql.Connection.Setting (connection)\nimport Hasql.Connection.Setting.Connection (string)/' benchmarks/Main.hs hasql-queue-tmp-db/Main.hs
                      sed -i 's/acquire connStr/acquire [connection . string . decodeUtf8 $ connStr]/' benchmarks/Main.hs hasql-queue-tmp-db/Main.hs
                      sed -i 's/acquire (toConnectionString db)/acquire [connection . string . decodeUtf8 . toConnectionString $ db]/' benchmarks/Main.hs
                    '';
                  });
                  co-log-effectful = doJailbreak (hfinal.callCabal2nix "co-log-effectful" inputs.co-log-effectful { });
                  co-log-json = doJailbreak hprev.co-log-json;
                  qrcode-juicypixels = doJailbreak hprev.qrcode-juicypixels;
                  servant-serialization = dontCheck (doJailbreak hprev.servant-serialization);
                  servant-rate-limit = dontCheck hprev.servant-rate-limit;
                  stroll = lib.pipe { } [
                    (hfinal.callCabal2nix "stroll" inputs.stroll)
                    doJailbreak
                    (drv: drv.overrideAttrs (attrs: {
                      postPatch = ''
                        ${attrs.postPatch or ""}
                        sed -i 's/= show /= Prelude.show /' lib/Development/Stroll/Hash.hs
                      '';
                    }))
                  ];
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
        in
        map fixPackage [
          #crem
          #discord-haskell-voice
          #g2q
          #rattle
          #sdl2-image
          #sdl2-mixer
          #servant-oauth2
          #wai-middleware-auth
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
          bluefin
          bluefin-random
          brick
          brick-skylighting
          clash-ghc
          clash-shake
          co-log-effectful
          co-log-json
          co-log-simple
          criterion
          dashi
          data-default
          data-fix
          data-textual
          deepseq-generics
          dhall
          dhall-csv
          dhall-recursive-adt
          dhall-toml
          dhall-yaml
          diagrams
          diagrams-cairo
          diagrams-canvas
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
          ghc-source-gen
          ghc-syntax-highlighter
          github
          github-rest
          greskell
          greskell-websocket
          hashable
          haskell-modbus
          hasql
          hasql-effectful
          hasql-migration
          hasql-mover
          hasql-queue
          hasql-th
          heftia-effects
          hinotify
          hnix
          hoauth2
          hs-opentelemetry-instrumentation-hspec
          hs-opentelemetry-instrumentation-postgresql-simple
          hs-opentelemetry-instrumentation-tasty
          hs-opentelemetry-instrumentation-wai
          hs-opentelemetry-utils-exceptions
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
          isomorphism-class
          jose
          jose-jwt
          json-rpc
          json-spec-openapi
          ki-effectful
          kubernetes-client
          langchain-hs
          lawful-conversions
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
          network-ip
          nonempty-containers
          notifications-tray-icon
          numhask
          numhask-space
          opaleye
          openapi3
          opentelemetry
          optics
          optparse-applicative
          optparse-generic
          optparse-simple
          optparse-th
          os-release
          os-string_2_0_8
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
          pup
          qrcode-juicypixels
          quickcheck-dynamic
          quickcheck-lockstep
          quickcheck-state-machine
          quickcheck-webdriver
          random
          random-fu
          rasterific-svg
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
          sqlite-simple
          squeal-postgresql
          stm-containers
          string-random
          stroll
          svgcairo
          syntax
          syntax-attoparsec
          syntax-printer
          syslog
          systemd
          targeted-quickcheck
          tasty-bench
          tasty-discover
          tasty-flaky
          tasty-golden
          tasty-hspec
          tasty-inspection-testing
          tasty-program
          tasty-wai
          telegram-bot-simple
          text-rope-zipper
          these
          tree-diff
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
          wai-transformers
          warp
          warp-systemd
          waterfall-cad
          websockets-simple
          wreq-effectful
          zip
        ];
      nixosModule = { pkgs, ... }:
        let
          pkgs' = inputs.nixpkgs.legacyPackages.${pkgs.system}.extend overlay;
        in
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
