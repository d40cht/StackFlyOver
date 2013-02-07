#!/bin/bash
set -e

PACKAGE_NAME=StackNative.tar.bz2

../../play-2.0.2/play stage
tar cjf $PACKAGE_NAME runserver.sh rundevserver.sh target/staged/*
echo "Site successfully built to $PACKAGE_NAME"
