GwToLaTeX is a tool to produce a "book" from pages extracted from a GeneWeb data base.

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
      | "HighLight" ->
      | "ImageLabel" ->
      | "Input" -> read the param file, scanning for %%%LIVRES%%%
      | "LaTeX" -> issues a LaTeX command
      | "Newpage" -> newpage
      | "Sideways" -> print page sideways (wip)
      | "Sub" -> "on/off" if off, do not increment subsection level.
                  usefull is a section has been manually inserted and
                  is followed by automatic <a ...> (sub)sections
      | "Section" -> new section
      | "SubSection" -> new subsection
      | "SubSubSection" -> new subsubsection
      | "Version" -> outputs version
      | "Wide" -> "on/off"
      | "Width" -> sets image width to param
  * <y ...> comment
  * <b command> WIP, to be modified. Ignored for the time being

This package includes test and example files named gwtolatex-testn.{txt/html}
The html files contain only <a ... > tags and do not produce any pdf output.

The launch parameters are:
 -base (wip, is in fact defined in the <a commands in the input file)
 -bases (wip, for the time being "."; implies gwl is executed in bases)
 -livres location of input file and .pdf result
 -follow process the resulting LaTeX code with pdflatex
 -index n repeat n times the index construction
 -family name of the .txt file to be processed if not in test mode
 -famille idem
 -test n read file gwtolatex-testn.ext
 -o output file. if not specified livres/family.pdf
 -level debug level (for my personnal use!!)
 -debug (wip)
 -dev run from the repo otherwise from the distrib dir
 -v run pdflatex in verbose mode
 -help as usual

In its current form, GwToLaTeX must run in the folder containing the target base.

Install and test

- clone the gwtolatex repo
- dune build
- copy your base in the gwtolatex folder (interim solution)
- dune exec -- gwtolatex -test n to try gwttolatex-testn.ext
  where ext is txt or html

Make distribution will create a folder gw2l_dist containing the necessary components
Copy or move this folder into your bases folder.
Create a folder Livres containing the input file Xxxx.txt and possible supplemental files
associated to <x Input file> commands that may appear in Xxxx.txt.
In those files, the macro %%%LIVRES%%% will be replaced by the value of the -livres
start parameter.
Run the gwl.sh script after editing your preferences.
The resulting .pdf file would be moved into the Livres folder

Warning:
Running pdftolatex is rather tricky and assumes that you have some knowledge of TeX and LaTeX behaviours. The Web is a good source of information, but usually rather obscure.
Trial and error is your friend.

The -v option provides interactive mode for pdftolatex, helping you to understand where the problem may come from.

Test files make reference to my base for extraction of surnames/first_names.
Edit the test files according to your own base.

Report problems through the issue tracker,
or by mail to me (henri.gouraud@laposte.net).

- Images appearing within notes of a person are collected and
  printed at the end of the personnal page.
  The command <x Width n.pcm> controls the width of the displayed images.
  <x Width \textwidth> will display all images at \textwidth.
  <x Width off> resets the width at its default value
    (5.1cm allowing for 3 images per line).
- Images containing "-vignette" in their name are not
  collected and displayed "in line" with a width of 1cm.
- Images containing "-wide" in their name are displayed "\textwidth"

more to follow
