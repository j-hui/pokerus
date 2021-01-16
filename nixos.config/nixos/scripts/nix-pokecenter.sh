#!/usr/bin/env bash

# Unmount all disks mounted during nix-infect.sh to start over.
# See that for more info.

# TODO: merge this into that for a single script.

DISK=/dev/nvme0n1
MOUNT=/mnt
ENC=enc

umount $MOUNT/boot $MOUNT/home $MOUNT/nix $MOUNT/persist $MOUNT/var/log
umount $MOUNT
cryptsetup close "$ENC"
swapoff "$DISK"p3
