#!/bin/bash

find src app -name "*.hs" -execdir brittany --write-mode=inplace {} \;
