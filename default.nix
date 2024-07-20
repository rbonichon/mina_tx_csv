{ pkgs ? import <nixpkgs> { }
}:

let
  camlpkgs = pkgs.ocamlPackages;
  ocamlDevelPackages = with pkgs;
    [
      dune_3
      ocamlformat
      camlpkgs.merlin
      camlpkgs.findlib
      camlpkgs.calendar
      camlpkgs.xml-light
      camlpkgs.cohttp
      camlpkgs.cohttp-lwt
      camlpkgs.cohttp-lwt-unix
      camlpkgs.ezjsonm
      camlpkgs.tezos-base58
      camlpkgs.utop
    ];
in
rec {
  devshell = pkgs.mkShell {
    buildInputs = with pkgs; [

    ] ++ ocamlDevelPackages;
  };
}

