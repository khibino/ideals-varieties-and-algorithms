#!/bin/sh
set -x

ghci -XOverloadedStrings -XDataKinds -isrc \
	src/Math/Polynomial/Examples.hs
