#!/bin/bash

if [ ! -d "data" ]; then
    swipl -s boot.pl 2>/dev/null -g boot -t halt.
fi

swipl -s main.pl 2>/dev/null -g main -t halt.
