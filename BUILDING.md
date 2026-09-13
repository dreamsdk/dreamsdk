# Building DreamSDK

> [!IMPORTANT]  
> This document is currently in work-in-progress and needs to be updated/reworked.
> Please note that you don't need to know how to build DreamSDK to make Sega
> Dreamcast programs! This document is only for producing DreamSDK environment
> packages.

Using this repository you'll be able to generate the 3 official installation
packages:

* The ISO image - `DreamSDK-R<release_number>-Setup.iso`;
* The Padus DiscJuggler image (CDI) containing 
[dcload-ip](https://gitlab.com/kallistios/dcload-ip): 
`DreamSDK-R<release_number>-dcload-ip-<dcload_version>.cdi`;
* The Padus DiscJuggler image (CDI) containing 
[dcload-serial](https://gitlab.com/kallistios/dcload-serial): 
`DreamSDK-R<release_number>-dcload-serial-<dcload_version>.cdi`.

The `release_number` is the version of the DreamSDK you want to generate
(this is detailed in the `setup-generator` repository). ISO image hold the
full installer of DreamSDK for the computer while
[Padus DiscJuggler images](https://en.wikipedia.org/wiki/DiscJuggler) (CDI) are
[bootable discs](https://en.wikipedia.org/wiki/MIL-CD) for your Sega Dreamcast,
containing DreamSDK and the **dcload** program used for running your programs
and remote debugging (you have to choose your flavor, `dcload-serial` for your
[Coders Cable](https://dreamcast.wiki/Coder%27s_cable) or `dcload-ip` if you
have the luck to have a [Broadband Adapter](https://segaretro.org/Broadband_Adapter)).

To produce these 3 installation packages, you will need to install the
prerequisites and execute the 4 scripts described below, in the specified order.

## Prerequisites

First of all, you will need to install:

* [DreamSDK](https://dreamsdk.org) - yes, you will need a previous version of 
DreamSDK to produce a new one - [Eating your own dog food](https://en.wikipedia.org/wiki/Eating_your_own_dog_food) 
principle!
* [Lazarus IDE](https://www.lazarus-ide.org/) - DreamSDK components are written 
in [Free Pascal](https://www.freepascal.org/). You have to install the 32-bit 
extension as well.
* [Python 3](https://www.python.org/) - some scripts used in the generation 
processes are written in Python.
* [Git](https://git-scm.com/) - used for retriving some components from Git 
repositories.
* [UPX](https://upx.github.io/) - DreamSDK binaries are packed with UPX.
* [7-Zip](https://www.7-zip.org/) - some scripts uses 7-Zip for compression.
* [Inno Setup 5](https://files.jrsoftware.org/is/5/innosetup-5.6.1-unicode.exe) - 
the tool used for creating the installer. Note that you should use the v5.x 
branch to keep Windows XP support.

## Installing the prerequisites

### Cloning all repositories on your computer

You will need to **clone** the main DreamSDK repository (the one that contains
this [read-me](README.md) file). It could be whatever directory you want but
it's better to avoid spaces in the path, e.g., using **Git CMD**:

    :: In that example, we will clone in "D:\dreamsdk"
    cd /D D:\ && git clone https://github.com/dreamsdk/dreamsdk.git dreamsdk

After cloning this repository, you need to execute the following scripts:

    cd /D "D:\dreamsdk"
    .\init.cmd
    .\update.cmd

All Git submodules should be downloaded and updated.

### Installing the previous DreamSDK package

You can install the previous version of DreamSDK in the default directory.
Please note that only one version of DreamSDK can be installed on the computer
at this time (mainly, due to the usage of the `DREAMSDK_HOME` environment
variable), so if you want to test your generated package, it will uninstall the
previous one. Then you can use your installed package for regenerating another
one, as only a few utilities embedded in DreamSDK are used for producing the
package (e.g., `patch`...).

### Installing Lazarus IDE

Lazarus is a key component as every DreamSDK binaries are written in Free
Pascal. You may install Lazarus in 64-bit with the 32-bit extension, as
DreamSDK will run on 64-bit for modern Windows and 32-bit for Windows XP.

To install Lazarus IDE, you should install (assuming you're on 64-bit Windows):

1. Install `lazarus-{x.y.z}-fpc-{x.y.z}-win64.exe`
2. Install `lazarus-{x.y.z}-fpc-{x.y.z}-cross-i386-win32-win64.exe`
3. Start Lazarus IDE and create a new project (just to access the main menu)
4. In the main menu, click on `Package` > `Online Package Manager...`.
5. Search for the following packages: `Synapse` and `UniqueInstance`. Once
   they appear, check the box next to each package to mark it for installation.
6. Click the `Install` button to install both selected packages.
7. After installation, you can recreate the `Normal Profiles` if asked.
   Lazarus will closes itself, you don't need to relaunch it now.

### Installing Python 3

You can install Python 3 using default settings. You may of course install
the 64-bit version.

Note that if you wish to test Python extensions for GDB 32-bit, you will need
to install Python 32-bit, so it means that you have to install both 32-bit and
64-bit versions of Python. You can of course make the 64-bit version primary.
To do so, install the 32-bit version first, then the 64-bit version.

### Installing Git and 7-Zip

For all these components, you can use the standard installation process.
If you are using Windows 11, `7z` could be already installed.

### Installing UPX

There is no installer for UPX, all you need to do is to extract it in a fixed
location.

### Installing Microsoft HTML Help Workshop

You need to install **Microsoft HTML Help Workshop** to create `CHM` files.
You can find it in the cloned directory, in that location:

    modules\doc\utils\Microsoft HTML Help Workshop\htmlhelp.exe

By the way, as this tool is **REALLY** old (but there is no alternative), you
may install **KEL CHM Creator** which is provided as well in the same location.
This utility can help you to update `CHM` file structure easilly.

### Installing Inno Setup 5

You need to install Inno Setup 5 (not the latest version) to keep Windows XP
support. You can find it on the
[official website](https://files.jrsoftware.org/is/5/innosetup-5.6.1-unicode.exe)
or [cached here](https://github.com/dreamsdk/setup-generator/blob/master/tools/inno-setup/inno-setup.zip).

It could be useful to install
[Inno Script Studio](https://www.kymoto.org/products/inno-script-studio) as well
as this tool simplify the development of Inno Setup scripts.

## Building DreamSDK

Starting from this point all prerequisites should be installed.

For building DreamSDK, you need to configure then execute the 4 scripts
described below. You need to execute them in that order as they are
dependencies check between all of them.

All of those scripts are located in the `workflow` directory. For all
of them, you will have a INI file. If you want to customize it without
having a difference spotted by Git, you may copy the file and remove the
`.default.` keyword, e.g., `offline.default.ini` will become `offline.ini`.
In that copied file, you may remove all the INI keys you won't change, to
keep only the different ones.

### Step 1: Offline

The first script to execute is called `offline`.

Starting with **DreamSDK R3**, you have the possibility to use DreamSDK in an
offline mode only, that's why we need to embed the components directly in the
Setup package. Of course the online mode is better and we encourage the user to
do so but sometime, you don't have the choice to be offline. The user can choose
the offline package directly when installing DreamSDK or later on through
DreamSDK Manager.

This script is preparing everything for embedding **KallistiOS**, **KOS Ports**
and **Dreamcast Tool** libraries directly the DreamSDK Setup package.

### Step 2: Prepare

The `prepare` script is very important as it gather all the components used to
produces DreamSDK. This script is generating the `.sources` directory that will
be embedded in the **DreamSDK Setup** package.

In this script, you will have to set the packages version you want to retrieve.

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

(To be continued)


## About the submodules

(work in progress)

Repositories to clone are:

* [codeblocks-patcher](https://github.com/dreamsdk/codeblocks-patcher) - 
patcher for adding DreamSDK support to [Code::Blocks IDE](http://codeblocks.org/)
* [common](https://github.com/dreamsdk/common) - 
common library/classes
* [documentation](https://github.com/dreamsdk/documentation) - 
DreamSDK documentation in [CHM format](https://en.wikipedia.org/wiki/Microsoft_Compiled_HTML_Help)
* [dreamsdk](https://github.com/dreamsdk/dreamsdk) - this repository
* [helpers](https://github.com/dreamsdk/helpers) - 
some utilities used by DreamSDK itself
* [manager](https://github.com/dreamsdk/manager) - 
DreamSDK Manager, the central utility of the DreamSDK package
* [setup-generator](https://github.com/dreamsdk/setup-generator) - 
the [Inno Setup](https://jrsoftware.org/isinfo.php) script used for production 
the setup files
* [setup-helpers](https://github.com/dreamsdk/setup-helpers) - 
some utilities only used by DreamSDK Setup
* [setup-packages](https://github.com/dreamsdk/setup-packages) - 
the packages that make DreamSDK itself
* [shell-launcher](https://github.com/dreamsdk/shell-launcher) - 
the main application of DreamSDK, the DreamSDK Shell
* [shell-runner](https://github.com/dreamsdk/shell-runner) - 
this utility is used for executing DreamSDK commands directly in Windows 
Shell.
* [system-objects](https://github.com/dreamsdk/system-objects) - some files used 
for converting a [standard MinGW installation](https://mingw.osdn.io/) into 
DreamSDK.