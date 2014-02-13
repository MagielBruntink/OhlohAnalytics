source("src/statistics/regressionAnalysis.R")
source("src/statistics/testForNormality.R")

### Absolute LOC growth

monthlyFactsAfterCleaning[,abs_loc_growth_regress_coef:=
                            linearRegression(pruneOutliers(data.frame(X=.SD$age_in_months,
                                                                      Y=.SD$abs_loc_growth)))$coef[2],
                          by=project_name_fact]

monthlyFactsAfterCleaning[,abs_loc_growth_regress_coef_sign:=
                            sign(abs_loc_growth_regress_coef),
                          by=project_name_fact]

### Commits

monthlyFactsAfterCleaning[,commits_regress_coef:=
                            linearRegression(pruneOutliers(data.frame(X=.SD$age_in_months,
                                                                      Y=.SD$commits_fact)))$coef[2],
                          by=project_name_fact]

monthlyFactsAfterCleaning[,commits_regress_coef_sign:=
                            sign(commits_regress_coef),
                          by=project_name_fact]

regress_coefs <- subset(monthlyFactsAfterCleaning[,.SD[1],
                                                  by=project_name_fact],
                        select=c(project_name_fact,
                                 abs_loc_growth_regress_coef,
                                 abs_loc_growth_regress_coef_sign,
                                 commits_regress_coef,
                                 commits_regress_coef_sign))
