#!/bin/sh
cd `dirname "$0"`
cp -R ./gw2l_dist bases
cd bases
rm ../livres/test.pdf
dune exec -- mkBook -bases /Users/Henri/GitHub/hgouraud/gwtolatex/bases -base base-test -family test $1


