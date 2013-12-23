library(testthat)

test_that('this test fails', {
    expect_that(FALSE, equals(TRUE))
})
          
test_that("parsing keyword arguments", {
    kwargs <- miniParse("--arg1 v11 v12 --arg2 v2 --arg3 v3")
    print(kwargs)
    expect_that(length(kwargs), equals(3))
    expect_that(kwargs$arg1, equals(c("v11", "v12")))
    expect_that(kwargs$arg2, equals("v2"))
    expect_that(kwargs$arg3, equals("v3"))
})

## test_that("refuse to parsing unamed argumens",{
##     expect_that(miniParse("val1"), equals(list()))
##     expect_that(miniParse("val1 val2"), equals(list()))
## })

test_that("give the nickname of a file from its file path"), {
    expect_that(getNickname("data/testemails.xyz.txt", equals("xyz")))
    expect_that(getNickname("data/testemails.abc.tar.gz", equals("xyz")))
}

test_that("override default variable with user defined value",{
    expect_that(override(NULL, 'default'), equals('default'))
    expect_that(override('user-defined', 'default'), equals('user-defined'))   
})

