# Welcome to DreamSDK!

This repository contains everything you need to produce **DreamSDK** packages.
Using this repository you'll be able to generate 3 installation packages:

* The ISO image - `DreamSDK-R<number>-Setup.iso`
* The CDI image containing `dcload-ip` - `DreamSDK-R<number>-dcload-ip-<dcload_version>.cdi`
* The CDI image containing `dcload-serial` - `DreamSDK-R<number>-dcload-serial-<dcload_version>.cdi`

To do so you will need to execute the 4 scripts described below.

## Prerequisites

First of all, you will need to install:

* DreamSDK - Yes, you will need a previous version of DreamSDK to produce a new
  one.
* Lazarus
* Python
* Git
* 7-Zip
* UPX
* ...

## Building DreamSDK

For building DreamSDK, you need to configure then execute the 4 scripts
described below. You need to execute them in that order as they are dependencies
check between all of them.

### Step 1: Offline

The first script to execute is called `offline`.

Starting with **DreamSDK R3**, you have the possibility to use DreamSDK in an
offline mode only, that's why we need to embedded the components directly in the
Setup package. Of course the online mode is better and we encourage the user to
do so but sometime, you don't have the choice to be offline. The user can choose
the offline package directly when installing DreamSDK or later on through
DreamSDK Manager.

This script is preparing everything for embedding **KallistiOS**, **KOS Ports**,
**Dreamcast Tool** and **Ruby** libraries directly the DreamSDK Setup package.

### Step 2: Prepare

The `prepare` script is very important as it gather all the components used to
produces DreamSDK. This script is generating the `.sources` directory that will
be embedded in the **DreamSDK Setup** package.

...

### Step 3: MkSetup

The `mksetup` script will generate the **DreamSDK Setup** package itself.
Starting from that point, DreamSDK is ready to be used but not really
distributed officially, this is the purpose of the next step.

### Step 4: MkImage

This is the last script to use: `mkimage`. This will produce the official images
containing DreamSDK for distribution.

## About the Code::Blocks IDE integration

For building Code::Blocks you will need some more tools

...



