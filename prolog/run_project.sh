#!/bin/bash

swipl -s boot.pl 2>/dev/null -g boot -t halt.
swipl -s main.pl 2>/dev/null -g main -t halt.
