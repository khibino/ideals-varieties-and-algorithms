#!/bin/sh
set -x

ghci -XOverloadedStrings -XDataKinds -isrc \
	"$@"
