GwToLaTeX is a set of tools to produce a "book" from a succession of pages
extracted from a GeneWeb data base.
GwToLaTeX will collect the necessary information to create a comprehensive
"table of contents" and an index of all the persons mentionned in the
document. Possible other cross references can be added using laTeX facilities.
GwToLaTeX incorporates in the LaTeX pages the images references in the
GeneWeb base in addition to the portraits, and provides an adittional
(potentially optionnal, wip) machanism to cross reference the appearance
of persons on those images.
LaTeX commands (footnotes, cross references, formatting data...) may be
included into the GeneWeb notes in "invisible" <span> tags.

The document "livres/test.pdf" provides a complete description of the
software and its environment, as well as examples and a sample
family.txt document. You may prefer to continue reading this .pdf
document rather than the README file.

A typical GeneWeb "page" can expand across several physical book pages.
Pages can be almost any GeneWeb query.
The source material for the book is a text file containing :
- some initialisation commands for LaTex (classical) and
  other LaTeX commands whiwch may be inserted anywhere in the document.
- a succession of <tag ...> 
  * <a href= ...>Nnn</a> queries to a GeneWeb data base.
      Creates a new section or subsection.
      Depending on the "Sub" command (see below), increments the
      subsection level or not.
      Analysis of p= and n= identifies person being queried. 
      Entry \index{N, p} is created.
      If Nnn is different from "N, p", then additional 
      Entry \index{Nnn, see n, p} is created
  * <x Command parameter> issues a command to GwToLaTeX.
      parameter can be "on/off" or a string
      | "Arbres" -> "on/off"
      | "Chapter" -> new chapter
      | "ImageLabel" -> nbr of items in image numbering (ch, sec, ssec, i_nbr)
      | "CollectImages" -> "on/off", collect_images to be printed at end of page
      | "Expand" -> In trees, expand cells width using neighbouring empty cells.
      | "FontSize" -> set font size (small, tiny, off -> returns to default)
      | "HighLight" -> highlight a first_name, occ, surname person
      | "Hrule" -> produce a hrule after each individual page
      | "ImageLabel" ->
      | "ImageWidth" -> set image width (string -> \textwidth, 5.1cm)
      | "NbImgPerLine" -> set the number of images across the page (default 3)
      | "Input" -> read the param file, scanning for %%%LIVRES%%%
      | "LaTeX" -> issues a LaTeX command
      | "NewLevel" ->
            "on/off" if off, do not increment subsection level.
            usefull is a section has been manually inserted and
            is followed by automatic <a ...> (sub)sections
      | "Newpage" -> newpage
      | "Print" -> print param
      | "Sideways" -> print page sideways (wip)
      | "Section" -> new section
      | "SubSection" -> new subsection
      | "SubSubSection" -> new subsubsection
      | "Trees" -> same as Arbres
      | "TwoPages" -> split a tree across two pages (WIP)
      | "TreeMode" -> 0 print tree as a list of cell lists, 1 print actual trees
      | "Unit" -> set width units (for parameters taking floats)
      | "Version" -> outputs version
      | "VignWidth" -> set vignettewidth (float)
      | "Wide" -> "on/off"
      | "WideImage" -> set imagewidth to \textwidth
      | "Width" -> sets image width to param (float)
  * <y ...> comment

This package includes historical test files and example files named 
gwtolatex-testn.{txt/html}. The html files contain only <a ... > tags
and do not produce any pdf output.

The tools include:
- mkImgDict -> creates an image database for further use
- mkNewGw -> inserts in a .gw file the necessary elements for image
    cross reference
- mkTex -> main tool: produces a .tex file from a family.txt file
- mkTweekInd -> manipulate the index files to integrate photo references
    
The tool mkBook encapsulates calls to the tools above:

Its launch parameters are:
 -base defines the base to be used
 -bases (wip, for the time being "."; mkBook is executed in bases)
 -livres location of input file and .pdf result
 -family name of the .txt file to be processed if not in test mode
 -famille idem
 -test n read file gwtolatex-testn.ext (wip)
 -o output file. if not specified livres/family.pdf
 -level debug level (for my personnal use!!)
 -debug (wip)
 -dev run from the gwtolatex repo otherwise from the distrib dir in bases
 -v run pdflatex in verbose mode
 -help as usual

In its current form, mkBook must run in the folder containing the target base
and GeneWeb must be available through a GW_BIN variable 

Install and test
- clone the gwtolatex repo
- make distrib or make install

Make distrib will create a folder "gw2l_dist" containing the necessary components.
Copy or move this folder into your bases folder.
Create a folder "livres" containing the main family description file "Family.txt",
and a folder Family-inputs containng
possible supplemental files associated to <x Input file> commands that
may appear in Family.txt.
In those files, the macro %%%LIVRES%%% will be replaced by the value of
the -livres start parameter, and %%%BASE%%% by the valur of the -base parameter.

- in your "bases" folder, run
./gw2l_dist/mkBook -base "Base" -livres "livres" -family "Family"

One of the first steps of mkBook is to create a new copy of the base with
additional images reference data obtained from the
"livres/family-inputs/who_is_where.txt" file.
Creation of this new base may fail if you introduce naming discrepancies
in this who_is_where file. Capitals and accents must be strictly observed.

To run a test case
- run ".setup-test.sh" to create a temporary test base in the gwtolatex folder
- run "./run-test.sh" should create ./gwtolatex/livres/test.pdf which is the
user  manual of GwToLaTeX.
  from ./gwtolatex/livres/test.txt

Warning:
Running pdftolatex is rather tricky and assumes that you have some knowledge
of TeX and LaTeX behaviours, especially if you have added some LaTeX commands in you
notes of source files.
The Web is a good source of information, but usually rather obscure.
Trial and error is your friend.

The -v option provides interactive mode for pdftolatex,
hopefully helping you to understand where the problem may come from.
A typical problem is the absence of an image file.
Another one is the spurious introduction of & characters.

Test files make reference to base-test for extraction of surnames/first_names.
Edit the test files according to your own base.

Report problems through the issue tracker,
or by mail to me (henri.gouraud@laposte.net).

- Images appearing within notes of a person are collected and
  printed at the end of the personnal page.
  The command <x Width n.pcm> controls the width of the displayed images.
  <x Width \textwidth> will display all images at \textwidth.
  <x Width off> resets the width at its default value
    (5.1cm allowing for 3 images per line).
- Images containing "-vignette" in their name are displayed "in line" with a
  width of 1cm (adjustable with VignWidth) and printed with the other images.
- Images containing "-wide" in their name are displayed across the full width 
  of the page ("\textwidth")

more to follow
