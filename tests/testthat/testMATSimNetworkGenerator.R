source("../../NetworkGenerator.R")

test_that("Network generation from Sqlite input works", {
  set.seed(12345)
  wd<-getwd()
  
  setwd("../..")
  makeNetwork("test")
  setwd(wd)
  
  expect_true(file.exists('../../output/test/network.sqlite'))
})

  
