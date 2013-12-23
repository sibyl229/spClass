README
========================================================
This is a simple spam email classifier, which I use to showcase a data analysis pipeline.


# How to reproduce this analysis
- Clone the repository!
- Place the test emails in [data/](data/) and follow the naming and format specification below.
- Run `make clean` to cleanup workspace from previous analysis.
- Run `make INPATH=data/testemails.SAMPLE_ID.txt`, replacing SAMPLE_ID to match with the path to file containing emails for testing.
- Classification results will be found in [results/](results/) for files with name prediction.CLS_ID.SAMPLE_ID.csv (where CLS_ID is the ID of the classifier used).

### test email format specification
- It should follow the same format as [the sample](
- the nmake INPATH=data/testemails.fake.txt

### Autotest
It is adviced to leave the auto-test running while making changes. At project root, run:
      Rscript tests/runtests.R
which runs all tests in the `tests/` directory first, and rerun a test if a change in either the test or the code in `spClassR/` is detected.
   
### Dependencies
- GNU make tool
- Python
- R
- R packages
    + tm
    + plyr
    + kernlab
    + randomForest
    + e1071
    + testthat
