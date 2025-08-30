# Welcome to DreamSDK!

![DreamSDK Manager](rsrc/readme/dreamsdk.png)

**DreamSDK is a modern, plug-and-play development environment for the Sega
Dreamcast, designed for the Microsoft Windows platform.**

By installing DreamSDK, you can start developing for your Sega Dreamcast today,
on Microsoft Windows, without worrying about toolchains, libraries, or
configuration. DreamSDK is used for the production of Sega Dreamcast
games/homebrew (i.e., Dreamcast programs), even commercial ones, such as
[The Textorcist](https://gamefairy.io/product/textorcist-dreamcast-limited-to-666/)
or [Shadow Gangs](https://www.kickstarter.com/projects/jkmcorp/shadow-gangs-dreamcast-version).
Moreover, since 2024, DreamSDK has been officially used by The Gang, the team
behind the unofficial ports of [Grand Theft Auto III and Grand Theft Auto: Vice City](https://dca3.net/).

DreamSDK is a complete package consisting of numerous free/open-source software
programs, primarily [KallistiOS](http://gamedev.allusion.net/softprj/kos/) (the
library used to interact with the Sega Dreamcast hardware), the associated and
required [GNU toolchain](https://en.wikipedia.org/wiki/GNU_toolchain), and a few
specific programs designed for this package (e.g., [DreamSDK Manager](https://github.com/dreamsdk/manager)).
DreamSDK is compatible with Windows XP and later (for example, it has been well
tested on Windows 11 x64).

📥 [**Download official releases here**](https://github.com/dreamsdk/dreamsdk/releases)

🗣️ [**Discuss here on the official Discord channel (hosted by Simulant)**](https://discord.gg/K2uyFtjAZ2)

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

If you're using the Windows Subsystem for Linux (i.e., WSL) on Windows 10+, you
can avoid using DreamSDK, as you'll have a native Unix-like shell directly on
your computer. However, using DreamSDK makes setting up the environment easier,
as all the required toolchains and components are already compiled and ready to
use. Compiling the required toolchains can take several hours (mainly with the
`dc-chain` utility provided in the `utils/dc-chain` directory of the KallistiOS
repository).

## Purpose of this repository

[This repository stores all DreamSDK releases](https://github.com/dreamsdk/dreamsdk/releases).
Additionally, it contains everything needed to build DreamSDK installation
packages. To learn more, read the document [BUILDING.md](BUILDING.md).




