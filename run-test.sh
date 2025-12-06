#!/bin/sh
cd `dirname "$0"`

cd bases
rm ../livres/test.pdf
dune exec -- mkBook -bases /Users/Henri/GitHub/hgouraud/gwtolatex/bases -base base-test -family test $1


