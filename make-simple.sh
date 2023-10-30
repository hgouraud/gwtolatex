#!/bin/sh

# This script must execute in the BASES folder.
# gw2l_dist should be copier in the BASES folder
cd `dirname "$0"`
cd ..
# we are now in GeneWeb-Bases

check_errs()
{
  # Function. Parameter 1 is the return code
  # Para. 2 is text to display on failure.
  if [ "${1}" -ne "0" ]; then
    echo "ERROR # ${1} : ${2}"
    # as a bonus, make our script exit with the right error code.
    exit ${1}
  fi
}

VERBPDF="-interaction=batchmode"
POSITIONAL_ARGS=()
DEBUG="0"

while [[ $# -gt 0 ]]; do
  case $1 in
    -b|--base)
      BASE="$2"
      shift # past argument
      shift # past value
      ;;
    -f|--family)
      FAMILY="$2"
      shift # past argument
      shift # past value
      ;;
    -l|--livres)
      LIVRES="$2"
      shift # past argument
      shift # past value
      ;;
    -g|--gw)
      GW="$2"
      shift # past argument
      shift # past value
      ;;
    -d|--debug)
      DEBUG="$2"
      shift # past argument
      shift # past value
      ;;
    -v)
      VERBPDF=""
      shift # past argument
      ;;
    -*|--*)
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      POSITIONAL_ARGS+=("$1") # save positional arg
      shift # past argument
      ;;
  esac
done

# global parameters
GW2L="./gw2l_dist"
BASES="."
# This script must execute in the BASES folder.

TMP="$GW2L/tmp"
if [ ! -d $TMP ]
then
  mkdir "$TMP"
fi

cp -R $GW2L/tex ./etc

echo "Running make-simple for $FAMILY on $BASE"
$GW/gwu ./$BASE -o ./$BASE.gw
echo "gwc warnings available in $TMP/gwc.log"
rm $TMP/$FAMILY.aux
$GW2L/mkTex -family $FAMILY -base $BASE -o $TMP/$FAMILY.tex -livres $LIVRES -debug $DEBUG
pdflatex $VERBPDF -output-directory="$TMP" $TMP/$FAMILY
check_errs $? "pdflatex"
makeindex $TMP/$FAMILY
mv $TMP/$FAMILY.pdf $LIVRES/$FAMILY.pdf

