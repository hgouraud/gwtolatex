#!/bin/sh
FAMILY=$1
LIVRES=$2

# This script assumes that you have some tool to append several .pdf files
# I use cpdf which I cannot re-distribute
# Adjust the script to your liking

if [ -f cpdf ]; then
  {
    echo "appending $FAMILY-inputs/Annexes.pdf"
    cd tmp
    LINE=`grep "Output written on " $FAMILY.log`
    export PAGES=`echo $LINE | sed 's/[^0-9]*\([0-9]\+\).*/\1/'`
    export ODDEVEN=`expr $PAGES % 2`
    if test $ODDEVEN -eq 1
    then echo "Number of pages for $FAMILY is : $PAGES (odd)"
    # assumes LIVRES is relative to bases!! 
    ../cpdf  ./$FAMILY.pdf  ../gw2l_dist/Blank.pdf ../$LIVRES/$FAMILY-inputs/Annexes.pdf -o ./$FAMILY-full.pdf
    fi
    if test $ODDEVEN -eq 0
    then echo "Number of pages for $FAMILY is : $PAGES (even)"
    ../cpdf  ./$FAMILY.pdf  ../$LIVRES/$FAMILY-inputs/Annexes.pdf             -o ./$FAMILY-full.pdf
    fi
    mv ./$FAMILY-full.pdf ./$FAMILY.pdf
  }
else
  echo "cpdf tool not available"
fi
