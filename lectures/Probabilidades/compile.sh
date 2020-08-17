#gnuplot ./plots/plot-onlineQRE.plt
#epstopdf onlineQRE.eps
#sed -i s/"includegraphics{"/"includegraphics[scale=0.7]{"/g onlineQRE.tex
#sed -i s/"onlineQRE"/"onlineQRE.pdf"/g onlineQRE.tex
#gnuplot ./plots/plot-online-tables.plt
#epstopdf online-tables.eps
#sed -i s/"online-tables"/"online-tables.pdf"/g online-tables.tex
gnuplot ./plot-spire.plt
epstopdf spire-plot.eps
sed -i s/"spire-plot"/"spire-plot.pdf"/g spire-plot.tex
pdflatex document.tex
bibtex document
pdflatex document.tex
acroread document.pdf &
