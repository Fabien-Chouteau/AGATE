# AGATE
Playing with the idea of a tasking/RTOS library in Ada

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
---> Dynamic T2 Clock: 921
---> Static T2 Clock: 1640
---> Static T1 Clock: 1796
---> Dynamic T2 Clock: 1008551
---> Dynamic T2 Clock: 2016354
---> Dynamic T2 Clock: 3024332
---> Dynamic T2 Clock: 4032392
---> Dynamic T2 Clock: 5040271
---> Static T1 Clock: 5040332
---> Dynamic T2 Clock: 6048447
---> Dynamic T2 Clock: 7056388
---> Dynamic T2 Clock: 8064407
---> Dynamic T2 Clock: 9072455
---> Dynamic T2 Clock: 10080463
---> Static T1 Clock: 10080594
---> Static T2 Clock: 10080726
---> Dynamic T2 Clock: 11088401
[...]
```
