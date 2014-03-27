require(testthat)

setwd("../..")

source("src/dataValidation.R")
source("src/dataParsing.R")

context("Validation of enlistments")

test_that("Enlistments failing validation", {
  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "SvnRepository",
    URL = "http://svn6.assembla.com/svn/arcmanager/"
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(TRUE))

  enlistments <- data.table(data.frame(
    Project_ID = c("test1","test2","test2","test4"),
    Type = c("CvsRepository","SvnRepository","SvnRepository","CvsRepository"),
    URL = c("","http://svn6.assembla.com/svn/arcmanager/","http://svn.xp-dev.com/svn/ArcManager","")
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(c(FALSE,TRUE,TRUE,FALSE)))
  
  enlistments <- obtainEnlistmentsForProjects(list("firefox","mozilla", "apache", "arcmanager"))
  expect_that(validateEnlistments(enlistments)["arcmanager"]$Validation_Bad_Enlistment,
              equals(c(TRUE,TRUE)))
  
  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "SvnRepository",
    URL = "http://svn6.assembla.com/svn/arcmanager/branches/"
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(TRUE))
  
  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "SvnRepository",
    URL = "http://svn6.assembla.com/svn/arcmanager/branches"
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(TRUE))

  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "SvnSyncRepository",
    URL = "http://svn6.assembla.com/svn/arcmanager/branches"
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(TRUE))

  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "SvnRepository",
    URL = ""
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(TRUE))
  
  
})

test_that("VC configurations passing validation", {
  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "SvnRepository",
    URL = "http://svn6.assembla.com/svn/arcmanager/trunk"
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(FALSE))
  
  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "SvnRepository",
    URL = "http://svn6.assembla.com/svn/arcmanager/trunk/"
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(FALSE))
  
  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "SvnRepository",
    URL = "http://svn6.assembla.com/svn/arcmanager/branches/2.2"
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(FALSE))
  
  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "HgRepository",
    URL = "http://svn6.assembla.com/svn/arcmanager/master"
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(FALSE))
  
  enlistments <- data.table(data.frame(
    Project_ID = "test",
    Type = "GitRepository",
    URL = "http://svn6.assembla.com/svn/arcmanager/master"
  ))
  expect_that(validateEnlistments(enlistments)$Validation_Bad_Enlistment,
              equals(FALSE))
})

test_that("Augmenting facts data.table with results of enlistment validation", {
  projects <- c("firefox","arcmanager")
  activityAndSizeFacts <- obtainActivityAndSizeFactsForProjects(projects)
  enlistments <- obtainEnlistmentsForProjects(projects)
  augmentFactsWithValidatedEnlistments(activityAndSizeFacts, enlistments)
  expect_that(all(activityAndSizeFacts["arcmanager"]$Validation_Bad_Enlistment == TRUE),
              equals(TRUE))
  expect_that(all(activityAndSizeFacts["firefox"]$Validation_Bad_Enlistment == FALSE),
              equals(TRUE))
})
