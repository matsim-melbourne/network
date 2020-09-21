source("../../MATSimNetworkGenerator.R")

test_that("MATSim Melbourne network generation from Sqlite input works", {
  set.seed(12345)
  wd<-getwd()
  
  setwd("../..")
  makeMatsimNetwork(F,20,F,F,F,F,T, "./tests/data/network.sqlite")
  setwd(wd)
  
  expect_true(file.exists('../../generatedNetworks/MATSimMelbNetwork.sqlite'))
})

  
