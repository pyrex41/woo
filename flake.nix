{
  description = "Woo native compatibility test environment";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/ef34387ddd751e1ab8857adf4676492d32eb24ec";
  outputs = { self, nixpkgs }:
    let systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
    in { devShells = nixpkgs.lib.genAttrs systems (system:
      let pkgs = import nixpkgs { inherit system; }; in {
        default = pkgs.mkShell {
          packages = with pkgs; [ sbcl libev openssl sqlite zstd redis python3 go gcc git curl lsof ];
          WOO_HEGEL_FOREIGN_LIB_DIRS = nixpkgs.lib.makeLibraryPath
            [ pkgs.libev pkgs.openssl pkgs.sqlite pkgs.zstd ];
        };
      }); };
}
