#!/bin/sh
cd `dirname "$0"`
cp -R ./gw2l_dist bases
cd bases
dune exec -- mkBook -base base-test -family test

