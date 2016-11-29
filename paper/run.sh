#!/bin/zsh

if [ $1 ]
then
    case "$1" in
    bib )       
        xelatex paper.tex
        bibtex paper     
    ;;
    * )
        echo "Usage (In terminal with zsh):"
        echo "\t\t./run.sh"
        echo "render and open a new pdf"
        echo "\t\t./run.sh bib"
        echo "render and open a new pdf with bib changes"
    esac   
fi

ruby computePositions.rb
xelatex paper.tex
open paper.pdf

rm footempfile.txt
rm *.bbl
rm *.blg
rm *.log
rm *-old