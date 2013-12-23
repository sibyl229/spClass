import argparse
import re

#
# Takes the file path for test emails, and use it in creation of a  makefile 
#


parser = argparse.ArgumentParser(description='Create Makefile')
parser.add_argument('-i', '--input', type=str, 
                    default='data/testemails.fake.txt',
                    help='text input containing emails to be classified')

args = parser.parse_args()
print "hey:"
print args


m = re.match('data/testemails\.([^.]+)\.txt', args.input)
if not m:
    raise Exception('Please rename the input file following the convention')
else:
    nickname = m.group(1)


makeContent = \
"""
all-dynamic: dynamicMakefile results/unlabeledFeatures.%(nickname)s.csv

data/testemails.%(nickname)s.Rda data/labeledEmails.Rda: \
    data/testemails.%(nickname)s.txt data/GoodnSpam.txt 
	Rscript spClassR/prepare.R -i data/testemails.%(nickname)s.txt

results/unlabeledFeatures.%(nickname)s.csv results/labeledFeatures.csv: \
    data/testemails.%(nickname)s.Rda data/labeledEmails.Rda
	Rscript spClassR/features.R -i data/testemails.%(nickname)s.Rda

results/prediction*.csv: data/*labeledFeatures.csv
	 Rscript spClassR/learn.R

""" % {'nickname': nickname}

with open('dynamicMakefile', 'w') as file:
    file.write(makeContent)
