# Setup Helpers for DreamSDK

This repository contains all scripts and programs used for producing a
valid **DreamSDK** installation package.

All these scripts/programs are used for helping the generation of the
**DreamSDK Setup** full package.

## Embedded

The `cbhelper` binary is used for helping the **DreamSDK Setup** detecting the
**Code::Blocks** installation on the user's computer.

The `pecheck` binary returns a string (i.e., `32-bit` or `64-bit`) representing
the platform architecture for the target binary passed in parameter.

All these 2 binaries are embedded in the **DreamSDK Setup** package but they
aren't used in **DreamSDK** itself.

## Offline

This script is preparing everything for embedding **KallistiOS**, **KOS Ports**,
**Dreamcast Tool** and **Ruby** libraries directly the **DreamSDK Setup**
package.

## Prepare

This script is generating the `.sources` directory that will be embedded in the
**DreamSDK Setup** package.

