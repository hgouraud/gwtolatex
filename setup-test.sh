#!/bin/sh

cd `dirname "$0"`
GW_BIN_DIR="./../geneweb/distribution/gw"
BASES_DIR="./bases"

if [ ! -d ./bases ]; then
  mkdir ./bases
fi
if [ ! -d ./bases/etc ]; then
  mkdir ./bases/etc
fi

rm -f -R ./bases/images
rm -f -R ./bases/src
cp -f -R ./gw2l_env/base-test.gw ./bases
cp -f -R ./gw2l_env/images ./bases
cp -f -R ./gw2l_env/lang ./bases
cp -f -R ./gw2l_env/src ./bases
cp -f -R ./gw2l_env/tex ./bases/etc
# execution of gw2l components must be in bases

if [ -d ./gw2l_dist ]; then
  cp -f -R ./gw2l_dist ./bases
else
  echo "gw2l_dist must be present.\nRun make distrib."
fi

$GW_BIN_DIR/gwc -f -o $BASES_DIR/base-test $BASES_DIR/base-test.gw
if [ $? -eq 0 ]; then
    echo "bases folder initialized and base-test created"
else
    echo "GeneWeb must be installed in ./../geneweb/distribution. See README"
fi

# Shutdown daemon allready running
killall ocamlrun 2> /dev/null
killall gwd 2> /dev/null
killall gwsetup 2> /dev/null
killall gwstp 2> /dev/null

OCAMLRUNPARAM=b "$GW_BIN_DIR"/gwd \
  -bd "$BASES_DIR" \
  -hd "$GW_BIN_DIR" \
  -log "<stderr>" \
  > "$GW_BIN_DIR"/gwd.log 2>&1 &

if [ $? -eq 0 ]; then
  read -p "Do you whish to proceed with ./run-test.sh: [Y/n]" response
  response=${response:-y}
  response=$(echo "$response" | tr '[:upper:]' '[:lower:]')
  if [ "$response" == "y" ]; then
      ./run-test.sh
  elif [ "$response" == "n" ]; then
      echo "Ready for test."
      # Add your logic here for the "no" case
  else
      echo "Invalid response. Please enter 'y' or 'n'."
  fi
else
    echo "Launch of gwd failed"
fi

