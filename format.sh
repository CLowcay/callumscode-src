#!/bin/bash

find src app test -name "*.hs" -execdir brittany --write-mode=inplace {} \;

