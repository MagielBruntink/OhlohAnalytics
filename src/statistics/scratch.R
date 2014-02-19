source("src/statistics/regressionAnalysis.R")
source("src/statistics/testForNormality.R")

linearRegressionResults <- regressForAllProjects(yearlyFactsAfterCleaning,
                                             "project_name_fact",
                                             "age_in_years",
                                             "max_loc_fact",
                                             linearRegression,
                                             FALSE,
                                             prefix_str = "loc_lin_")

quadraticRegressionResults <- regressForAllProjects(yearlyFactsAfterCleaning,
                                             "project_name_fact",
                                             "age_in_years",
                                             "max_loc_fact",
                                             quadraticRegression,
                                             FALSE,
                                             prefix_str = "loc_qua_")

allRegressionResults <- cbind(linearRegressionResults,quadraticRegressionResults[2:4])
