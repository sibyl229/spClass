all: README.md all-dynamic

INPATH="data/testemails.fake.txt"

dynamicMakefile:
	echo $(INPATH)
	python makeHelper.py -i $(INPATH)
-include dynamicMakefile

README.md: README.Rmd
        Rscript -e "knitr::knit('README.Rmd')"

clean:
	 rm -rf results/*.csv \
		data/*.Rda \
		dynamicMakefile \
		README.md	