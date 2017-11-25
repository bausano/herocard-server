# herocard-server
Erlang server for my HeroCard game.

> Version: 1.0.0

> Contact: bausanomichal@gmail.com

## Installation

1. Install Erlang.

[Ubuntu](https://hostpresto.com/community/tutorials/how-to-install-erlang-on-ubuntu-16-04/)

[OS X](http://erlang.org/doc/installation_guide/INSTALL.html#Advanced-configuration-and-build-of-ErlangOTP_Building_OS-X-Darwin)

2. Give executable permissions to the .sh files.

`chmod +x build.sh`

`chmod +x run.sh`

`chmod +x test.sh`

3. Run `./run.sh` in your terminal. Files are going to be compiled in `bin` directory.

## Commands

### Build
`./build.sh`

Compiles all files in all subdirectories of `/src` into `/bin`.
Output directory can't be changed. However you can specify input directory.

`./build.sh directory`

This command compiles .erl files, but it doesn't run them.

### Run
`./run.sh`

Compiles `src` folder and runs `start/0` function in `main` module.

You can change the boot module with `-b` flag.

`./run.sh -b module`

There's also an option to suppress the compilation with `-s` flag.

`./run.sh -s`

### Test
`./test.sh`

Compiles `src` and `test` directory and runs all `_.spec.erl` files.
