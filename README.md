# Welcome to DreamSDK!

![DreamSDK Manager](rsrc/readme/dreamsdk.png)

**DreamSDK is a modern, plug-and-play development environment for the Sega
Dreamcast, designed for the Microsoft Windows platform.**

It's a comprehensive, self-contained package that includes all the necessary
software to develop freely for the Sega Dreamcast system on Microsoft Windows.
It was designed primarily for beginners, but can be used by anyone interested
in Sega Dreamcast development.

DreamSDK is used for the production of Sega Dreamcast games/homebrew (i.e., Dreamcast programs),
even commercial ones, such as [The Textorcist](https://gamefairy.io/product/textorcist-dreamcast-limited-to-666/)
or [Shadow Gangs](https://www.kickstarter.com/projects/jkmcorp/shadow-gangs-dreamcast-version).
Moreover, since 2024, DreamSDK has been officially used by The Gang, the team
behind [DCA3](https://dca3.net/), the unofficial ports of Grand Theft Auto III
and Grand Theft Auto: Vice City.

By installing DreamSDK, you will have:

- A UNIX®-like shell, based on MinGW/MSYS or MinGW-w64/MSYS2.
- All necessary toolchains for the Sega Dreamcast CPU (Hitachi SuperH), the sound chip (Yamaha AICA), and of course the Win32 toolchain (for building tools for your computer).
- All required Sega Dreamcast development libraries, which include [KallistiOS](http://gamedev.allusion.net/softprj/kos/), KallistiOS Ports (kos-ports), and Dreamcast Tool (dc-tool/dcload).
- Additional useful tools (makedisc, etc.).

Everything is thoroughly explained in the DreamSDK documentation, which you will
find included in the DreamSDK package. This documentation will be exposed online
later, as the whole project homepage will be reworked in the future.

📥 [**Download official releases here**](https://github.com/dreamsdk/dreamsdk/releases)

🗣️ [**Discuss here on the official Discord channel**](https://discord.gg/K2uyFtjAZ2) (hosted by [Simulant Engine](https://simulant.dev/))

🪐 [**Visit project homepage**](https://www.dreamsdk.org/)

## How to use DreamSDK after installing it?

After installing the package on your computer, you have 2 options:

* Use the **DreamSDK Shell** and then use the package directly from the
  command-line;
* Use an IDE: currently, only [Code::Blocks](http://codeblocks.org/) is 
  officially supported.

![DreamSDK Manager](rsrc/readme/shell.png)

## About GNU/Linux, macOS and WSL

If you're using **GNU/Linux** or **macOS**, DreamSDK is completely unnecessary.
Since all the components required for Sega Dreamcast are based on GNU/Linux
components, setting up a working environment for developing on Sega Dreamcast
is relatively simple. Just follow the official steps outlined in the 
[KallistiOS documentation](https://kos-docs.dreamcast.wiki/).
Therefore, DreamSDK doesn't support GNU/Linux or macOS, as it's not necessary.

If you're using the **Windows Subsystem for Linux** (i.e., WSL) on Windows 10+,
you can avoid using DreamSDK, as you'll have a native Unix-like shell directly
on your computer. However, using DreamSDK makes setting up the environment
easier, as all the required toolchains and components are already compiled and
ready to use. Compiling the required toolchains can take several hours (mainly
with the `dc-chain` utility provided in the `utils/dc-chain` directory of the
KallistiOS repository).

## Purpose of this repository

[This repository stores all DreamSDK releases](https://github.com/dreamsdk/dreamsdk/releases).
Additionally, it contains everything needed to build DreamSDK installation
packages. To learn more, read the [BUILDING.md](BUILDING.md) document.

## What should I know about DreamSDK?

As you may already know, DreamSDK is a combination of multiple software
components. The main library used for Sega Dreamcast development is
[KallistiOS](https://github.com/Kallistios/) (`kos`). This library allows you to
harness the power of the Sega Dreamcast hardware. DreamSDK is not affiliated
with KallistiOS in any way, but this environment is 100% compatible with it,
which means that anything you read about KallistiOS on the Internet applies to
DreamSDK.

The KallistiOS library should have been compiled during DreamSDK installation,
but [KallistiOS Ports](https://github.com/Kallistios/) (`kos-ports`) are not
compiled/installed by default. If you plan to use additional libraries such as
PNG, Ogg, or Vorbis support in your program, you will need to use DreamSDK
Manager to install these additional libraries. Doing so through DreamSDK Manager
will expose those libraries to Code::Blocks integration as well. If for some
reason you have to refresh the cache (e.g., you installed a port directly from
the command-line), you can run `kosports refresh` from the DreamSDK Shell to
force the update.

**You need to learn the C/C++ programming language and the KallistiOS (KOS) API**,
as DreamSDK is not a Unity-like framework. You can find the KallistiOS
documentation link in your Start Menu or in various location, like the excellent
[Dreamcast Wiki](https://dreamcast.wiki) website.

To debug Sega Dreamcast programs, you can use GNU Debugger (GDB) directly from
the command line (`sh-elf-gdb`) or, even better, use the Code::Blocks IDE
integration. You will need either a Coder's Cable (Serial/RS-232) or a LAN
Adapter/Broadband Adapter (HIT-300/HIT-400). Debugging through a Sega Dreamcast
emulator is not supported. [Read more on this topic here](https://dreamcast.wiki/Getting_Started_with_Dreamcast_development).

## Where should I start?

The best option is to read the DreamSDK Help documentation that is installed
with DreamSDK! Additionally, if you are new to Dreamcast development, another
great option is to browse [Dreamcast Wiki](https://dreamcast.wiki).

If you don't want to read the full DreamSDK documentation right now, at least
pay attention to the Sega Dreamcast examples that come with KallistiOS.

The examples are located in the `/opt/toolchains/dc/kos/examples/dreamcast`
directory. Access this directory by typing `cd /opt/toolchains/dc/kos/examples/dreamcast && browse`
in DreamSDK Shell. The corresponding directory will then open in Windows
Explorer.

You can build these examples by typing the `make` command; however, many
examples use KallistiOS Ports, so make sure to install the required ones before
trying to build everything.

It is recommended to use DreamSDK with the Code::Blocks IDE as this simplifies
almost everything, although it isn't mandatory.

## Help! I'm stuck!

If you need help with Sega Dreamcast programming, you can:

- Join the [Discord channel](https://discord.gg/K2uyFtjAZ2) for the Simulant
  Engine (the `#dreamsdk` subchannel is dedicated to DreamSDK)
- Visit the [Dreamcast Wiki](https://dreamcast.wiki) (useful resources)
- Visit the [DCEmulation Programming forum](https://dcemulation.org/phpBB/viewforum.php?f=29) (official KallistiOS forum)

> [!NOTE]
> **About Simulant Engine**: This is a completely separate project that is based
> on KallistiOS. This project is not currently supported in DreamSDK, but many
> talented people are available on the Discord channel, so don't hesitate to
> join!

## Acknowledgments

Thanks to everyone involved in the Dreamcast scene, there are too many people
to list here!