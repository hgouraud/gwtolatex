#!/bin/sh
cd `dirname "$0"`
cp -R ./gw2l_dist bases
cd bases
rm ../livres/test.pdf
dune exec -- mkBook -base base-test -family test -once
