all: prediction*.csv

*labeledEmails.Rda: GoodnSpam.txt *TestEmails.txt
        Rscript prep.r

*labeledFeatures.csv: labeledEmails.Rda
				Rscript genFeat.r
	

clean:
        rm -rf *.csv *.Rda