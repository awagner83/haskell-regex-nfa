#!/bin/bash

ghc --make -Wall -O2 Main.hs -outputdir=build
time ./Main
