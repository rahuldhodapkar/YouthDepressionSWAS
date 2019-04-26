#!/usr/bin/env bash
#
# Run all analysis scripts in pipeline
#

set -e
set -x

if [ -z ./calc ]
then
    rm ./calc/*
fi
mkdir -p calc

cd src
./Main.R

