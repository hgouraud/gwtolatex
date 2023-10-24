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

GW="/Users/Henri/GitHub/hgouraud/geneweb/distribution/gw"
GW2L="./gw2l_dist"
FAMILY="chausey"
BASE="chausey"
BASES="."
# This script must execute in the BASES folder.
LIVRES="/Users/Henri/Desktop/Livres-2"
VERBPDF="-interaction=batchmode"
VERBMK=""
TMP="./$GW2L/tmp"
mkdir "$TMP"
cp -R $GW2L/tex ./etc

$GW/gwu ./$BASE -o ./$BASE.gw
$GW2L/mkImgDict -family $FAMILY -base $BASE $VERBMK
$GW2L/mkNewGw -family $FAMILY -base $BASE -o ./$BASE-new.gw
$GW/gwc -f ./$BASE-new.gw -o ./$BASE-new 2&> $TMP/gwc.log
echo "gwc warnings available in $TMP/gwc.log"
rm $TMP/$FAMILY.aux
$GW2L/mkTex -family $FAMILY -base $BASE-new -o $TMP/$FAMILY.tex -livres $LIVRES $VERBMK
pdflatex $VERBPDF -output-directory="$TMP" $TMP/$FAMILY
check_errs $? "pdflatex"
makeindex $TMP/$FAMILY
pdflatex $VERBPDF -output-directory="$TMP" $TMP/$FAMILY
$GW2L/mkTweekIndSort -family $FAMILY -o $TMP/temp
mv $TMP/$FAMILY.idx $TMP/$FAMILY.idx.sav
mv $TMP/temp $TMP/$FAMILY.idx
$GW2L/mkTweekIndMerge -family $FAMILY -o $TMP/temp
mv $TMP/$FAMILY.ind $TMP/$FAMILY.ind.sav
mv $TMP/temp $TMP/$FAMILY.ind
pdflatex $VERBPDF -output-directory="$TMP" $TMP/$FAMILY
mv $TMP/$FAMILY.pdf $LIVRES/$FAMILY.pdf

