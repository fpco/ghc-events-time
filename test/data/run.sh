#!/bin/sh

ghc -O --make example-eventlog.hs -eventlog && ./example-eventlog +RTS -l
