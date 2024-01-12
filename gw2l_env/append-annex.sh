#!/bin/sh
FAMILY=$1

# This script assumes that you have some tool to append several .pdf files
# I use cpdf which I cannot re-distribute
# Adjust the script to your liking

if [ -f cpdf ]; then
  {
    cd tmp
    LINE=`grep "Output written on ./tmp/$FAMILY.pdf" $FAMILY.log`
    export PAGES=`echo $LINE | sed 's/[^0-9]*\([0-9]\+\).*/\1/'`
    export ODDEVEN=`expr $PAGES % 2`
    if test $ODDEVEN -eq 1
    then echo "Number of pages for $FAMILY is : $PAGES (odd)"
    ../cpdf  ./$FAMILY.pdf  ../gw2l_dist/Blank.pdf ./$FAMILY-annex.pdf -o ./$FAMILY-full.pdf
    fi
    if test $ODDEVEN -eq 0
    then echo "Number of pages for $FAMILY is : $PAGES (even)"
    ../cpdf  ./$FAMILY.pdf  ./$FAMILY-annex.pdf              -o ./$FAMILY-full.pdf
    fi
    mv ./$FAMILY-full.pdf ./$FAMILY.pdf
  }
else
  echo "cpdf tool not available"
fi
