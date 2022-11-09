{ sources ? import ./nix/sources.nix
, deploy ? false
}:
let
  nixpkgs = import sources.nixpkgs { };

  inherit (nixpkgs) lib mkShell;
in
mkShell {
  name = "elm-infinite-gallery-env";
  buildInputs = with nixpkgs;
    let
      devInputs = [
        elmPackages.elm
        elmPackages.elm-format
        elmPackages.elm-test
        niv
        readline
      ] ++ lib.optional stdenv.hostPlatform.isDarwin libiconv;
    in
    if deploy then deployInputs else devInputs;
}
