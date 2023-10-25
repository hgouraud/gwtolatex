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

echo "Running make-generic for $FAMILY on $BASE"
$GW/gwu ./$BASE -o ./$BASE.gw
$GW2L/mkImgDict -family $FAMILY -base $BASE
$GW2L/mkNewGw -family $FAMILY -base $BASE -o ./$BASE-new.gw
$GW/gwc -f ./$BASE-new.gw -o ./$BASE-new 2&> $TMP/gwc.log
echo "gwc warnings available in $TMP/gwc.log"
rm $TMP/$FAMILY.aux
$GW2L/mkTex -family $FAMILY -base $BASE-new -o $TMP/$FAMILY.tex -livres $LIVRES
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

echo "--------------------------------------------------------------------------------"
echo "\nNombre de pages ->"
touch pages.tmp
grep "Output written on $FAMILY.pdf" $TMP/$FAMILY.log | \
sed s/"Output written on $FAMILY.pdf ("// | \
sed s/" pages".*//> pages.tmp

if test $ODDEVEN -eq 1
then echo "Number of pages is : $PAGES (odd)"
./cpdf $LIVRES/$FAMILY.pdf  $LIVRES/Blank.pdf ./$FAMILY-inputs/annexes.pdf -o $LIVRES/$FAMILY-full.pdf  2>/dev/null
fi

if test $ODDEVEN -eq 0
then echo "Number of pages is : $PAGES (even)"
./cpdf $LIVRES/$FAMILY.pdf  $LIVRES/$FAMILY-inputs/annexes.pdf -o $LIVRES/$FAMILY-full.pdf  2>/dev/null
fi

echo "--------------------------------------------------------------------------------"
