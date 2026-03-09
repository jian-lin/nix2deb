#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

# overview:
#   - build nix2deb itself and a test nix package testExe
#   - generate test-exe.deb for testExe using nix2deb
#   - install and then run test-exe.deb in an Ubuntu container using systemd-nspawn
# get an interactive shell of the container: machinectl shell $machine_name

# change suite, arch and rootfs_url in lockstep
suite="noble"
arch="Amd64"
rootfs_url="https://cloud-images.ubuntu.com/minimal/releases/noble/release-20260304/ubuntu-24.04-minimal-cloudimg-amd64-root.tar.xz"
# rootfs_url="https://cloud-images.ubuntu.com/releases/noble/release-20260225/ubuntu-24.04-server-cloudimg-amd64-root.tar.xz"

image_name="ubuntu-$suite-nix2deb"
machine_name="$image_name-temp"

# CI may have an old systemd
if importctl --version; then
    importctl_cmd="importctl"
    importctl_flags="--verify=checksum --class=machine --keep-download=no"
else
    importctl_cmd="machinectl"
    importctl_flags="--verify=checksum"
fi

build_nix2deb() {
    nix build .# --print-out-paths --no-link
}

download_rootfs() {
    sudo "$importctl_cmd" \
        "$importctl_flags" \
        pull-tar "$rootfs_url" "$image_name"
}

# alternative of --ephemeral: --template, --volatile
# --ephemeral can be slow on ext4
start_container() {
    sudo systemd-nspawn \
        --settings=no \
        --boot --notify-ready=yes \
        --directory /var/lib/machines/"$image_name" --ephemeral --suppress-sync=yes \
        --machine "$machine_name" \
        -U \
        --resolv-conf=bind-host \
        -j
}

wait_for_container_starts() {
    sleep 10
}

stop_container() {
    sudo machinectl poweroff "$machine_name"
}

generate_deb() {
    local outpath
    outpath=$(nix build .#testExe --print-out-paths --no-link)
    nix run .# -- --nix-installable .#testExe --suite "$suite" --arch "$arch" "$outpath"
}

copy_deb_into_container() {
    sudo machinectl copy-to "$machine_name" ./test-exe.deb /root/test-exe.deb
}

install_deb_in_container() {
    sudo systemd-run --wait --machine "$machine_name" apt update
    sudo systemd-run --wait --machine "$machine_name" apt --yes install /root/test-exe.deb
}

run_installed_executable_in_container() {
    sudo systemd-run --wait --machine "$machine_name" test-exe
}

build_nix2deb
download_rootfs
start_container &
wait_for_container_starts
generate_deb
copy_deb_into_container
install_deb_in_container
run_installed_executable_in_container
stop_container

wait
