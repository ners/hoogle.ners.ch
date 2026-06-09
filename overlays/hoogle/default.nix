pkgs: hfinal: hprev: {
  hackage-revdeps = hprev.hackage-revdeps_0_3;
  hoogle = pkgs.haskell.lib.compose.doJailbreak (hprev.hoogle_5_0_19_0.overrideAttrs (attrs: {
    patches = [ ./color-scheme.patch ] ++ attrs.patches or [ ];
    postPatch = ''
      ${attrs.postPatch or ""}
      cp -f "${./.}"/hoogle.png html/hoogle.png
    '';
  }));
}
