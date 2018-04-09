
latex $1.tex
latex $1.tex
bibtex $1.tex
bibtex $1.tex
latex $1.tex
latex $1.tex

##%%dvips -Ppdf -G0 -t letter -o $1.ps $1.dvi
dvips -Ppdf  -o $1.ps $1.dvi
ps2pdf $1.ps

# clean up working files 

rm -fr $1.aux
rm -fr $1.log
rm -fr $1.dvi
rm -fr $1.out
rm -fr $1.blg
