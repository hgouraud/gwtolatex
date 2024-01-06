#!/bin/sh
cd `dirname "$0"`
mkdir ./bases
mkdir ./bases/etc
cp -R ./gw2l_env/base-test.gw ./bases
cp -R ./gw2l_env/images ./bases
cp -R ./gw2l_env/lang ./bases
cp -R ./gw2l_env/src ./bases
cp -R ./gw2l_env/tex ./bases/etc
# execution of gw2l components must be in bases
mv ./gw2l_env ./bases

./../geneweb/distribution/gw/gwc -f -o ./bases/base-test ./bases/base-test.gw
if [ $? -eq 0 ]; then
    echo "bases folder initialized and base-test created"
else
    echo "GeneWeb must be installed and launched. See README"
fi