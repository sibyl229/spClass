library(testthat)

wd <- getwd()
setwd('spClassR')
scripts <- list.files('.',
           pattern="*.[r|R]$", full.names=TRUE)
for (s in scripts){
  source(s)
  print(s)
}
setwd(wd)

test_dir("tests/") # run every tests in tests/

# turn on auto-test, which rerun tests if either codes or tests are modified
auto_test('spClassR', 'tests') 