#!/bin/bash
set -e
../../play-2.0.2/play stage
tar cjf StackNative.tar.bz2 runserver.sh target/staged/*
