README
========================================================
This is a simple spam email classifier, which I use to showcase a data analysis pipeline.


# How to reproduce this analysis

It is adviced to leave the auto-test running while making changes. At project root, run:
      Rscript tests/runtests.R
which runs all tests in the `tests/` directory first, and rerun a test if a change in either the test or the code in `spClassR/` is detected.
   
## Dependencies
- GNU make tool
- R
- R packages
    + tm
    + plyr
    + kernlab
    + randomForest
    + e1071
    + testthat
