#!/bin/sh
cd `dirname "$0"`
pwd
mkdir ../bases
mkdir ../bases/etc
cp -R ./base-test.gw ../bases
cp -R ./images ../bases
cp -R ./lang ../bases
cp -R ./src ../bases
cp -R ./tex ../bases/etc

../../geneweb/distribution/gw/gwc -f -o ../bases/base-test base-test.gw
