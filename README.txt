GwToLaTeX is a tool to produce a "book" from pages extracted from a GeneWeb data base.

A typical GeneWeb "page" can expand across several physical book pages.
Pages can be almost any GeneWeb query.
The sourve material for the book is a text file containing :
- some initialisation commands for LaTex (classical)
- a succession of <a href...> queries to a GeneWeb data base
- a collection of tool specific commands that contribute to the general layout of the book

In its current form, GwToLaTeX must run in the folder containing the target base.

Additionnal details will be provided as I progress.

In its current implementation, GwToLaTeX cannot yet produce a pdf file!