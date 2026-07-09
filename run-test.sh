#!/bin/sh
cd `dirname "$0"`

killall gwd
/Users/Henri/github/hgouraud/geneweb/distribution/gw/gwd -bd bases   > ./gwd.log 2>&1 &

cd bases
rm ../livres/test.pdf
dune exec -- mkBook -bases /Users/Henri/GitHub/hgouraud/gwtolatex/bases -base base-test -family test $1 -v


