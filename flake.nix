{
  description = "The Bigfile server and App Developer Toolkit.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, ... }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      bigfile = pkgs.callPackage ./nix/bigfile.nix { inherit pkgs; };
    in
    {
      packages = utils.lib.flattenTree {
        inherit bigfile;
      };

      nixosModules.bigfile = {
        imports = [ ./nix/module.nix ];
        nixpkgs.overlays = [ (prev: final: { inherit bigfile; }) ];
      };

      defaultPackage = self.packages."${system}".bigfile;

      devShells = {
        # for bigfile development, made to work with rebar3 builds (not nix)
        default = with pkgs; mkShellNoCC {
          name = "bigfile-dev";
          buildInputs = [
            bashInteractive
            cmake
            elvis-erlang
            erlang
            erlang-ls
            gmp
            openssl
            pkg-config
            rebar3
            rsync
          ];

          PKG_CONFIG_PATH = "${openssl.dev}/lib/pkgconfig";

          shellHook = ''
            ${pkgs.fish}/bin/fish --interactive -C \
              '${pkgs.any-nix-shell}/bin/any-nix-shell fish --info-right | source'
            exit $?
          '';

        };
      };

    });

}
