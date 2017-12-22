# AGATE
Playing with the idea of a tasking library/RTOS in Ada

## Setup

You will need an installation of GNAT for ARM Cortex-M (AKA arm-elf, AKA
arm-eabi):

If you don't have access to the Pro edition, there's a community version
available on the AdaCore website:
[www.adacore.com/download](https://www.adacore.com/download).

## Build

To build the project, simply run `gprbuild`:
```
$ gprbuild -P agate.gpr
```

## Run
You can try the project in GNATemulator (QEMU) provided with GNAT:
```
$ arm-eabi-gnatemu -P agate.gpr obj/main
```
