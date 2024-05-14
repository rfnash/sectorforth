{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
    nasm
    # qemu_full
    qemu_kvm
    # qemu
    # qemu_test
    bashInteractive
  ];
}
