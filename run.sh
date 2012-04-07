#!/bin/bash

ghc --make -O2 Main.hs -outputdir=build
time ./Main
