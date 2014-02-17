source("src/statistics/regressionAnalysis.R")
source("src/statistics/testForNormality.R")

regressionResults <- ddply(monthlyFactsAfterCleaning, .(project_name_fact), 
      .fun = function(df) {
        dfr = pruneOutliers(data.frame(X=df$age_in_months, Y=df$loc_fact))
        if (nrow(na.omit(dfr)) >= 2) {
          lin_regress_obj=linearRegression(dfr)
          qua_regress_obj=quadraticRegression(dfr)
          summarise(dfr,
                    loc_fact_lin_regress_coef = lin_regress_obj$coef[2],
                    loc_fact_lin_regress_coef_sign = sign(loc_fact_lin_regress_coef),
                    loc_fact_lin_adj_r_squarded = summary(lin_regress_obj)$adj.r.squared,
                    loc_fact_qua_regress_coef = qua_regress_obj$coef[3],
                    loc_fact_qua_regress_coef_sign = sign(loc_fact_qua_regress_coef),
                    loc_fact_qua_adj_r_squarded = summary(qua_regress_obj)$adj.r.squared)
        }
        else {
          warning("Fewer than 2 non-NA cases")
          summarise(dfr,
                    loc_fact_lin_regress_coef = NA,
                    loc_fact_lin_regress_coef_sign = NA,
                    loc_fact_lin_adj_r_squarded = NA,
                    loc_fact_qua_regress_coef = NA,
                    loc_fact_qua_regress_coef_sign = NA,
                    loc_fact_qua_adj_r_squarded = NA)
        }
      }
)
  


## Quadratic regression

monthlyFactsAfterCleaning[,loc_fact_qua_regress_coef:=
                            as.double(tryGettingRegressionCoefs(data.frame(X=.SD$age_in_months,
                                                                           Y=.SD$loc_fact),
                                                                quadraticRegression)[3]),
                          by=project_name_fact]

monthlyFactsAfterCleaning[,loc_fact_qua_regress_coef_sign:=
                            sign(loc_fact_qua_regress_coef),
                          by=project_name_fact]

## Combine all results in 1 data frame

regress_coefs <- subset(monthlyFactsAfterCleaning[,.SD[1],
                                                  by=project_name_fact],
                        select=c(project_name_fact,
                                 loc_fact_lin_regress_coef,
                                 loc_fact_lin_regress_coef_sign,
                                 loc_fact_qua_regress_coef,
                                 loc_fact_qua_regress_coef_sign))
