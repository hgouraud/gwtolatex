#!/bin/sh
cd `dirname "$0"`
cd bases
dune exec -- mkBook -base base-test -family test -dev -v

