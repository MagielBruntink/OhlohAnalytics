require(data.table)
require(plyr)

analysis_dir <- "~/git/OhlohAnalytics/validation"

addGrowthFacts <- function(monthlyData) {
  monthlyData[,abs_loc_growth:=(loc_fact-previous_month_loc_fact)][]
  monthlyData[,ind_loc_growth:=((previous_month_loc_fact + abs_loc_growth) / previous_month_loc_fact)][]
}

groupByYear <- function(dataTable) {
  return (dataTable[,list(sum_loc_added_fact=sum(loc_added_fact),
                          sum_loc_deleted_fact=sum(loc_deleted_fact),
                          sum_comments_added_fact=sum(comments_added_fact),
                          sum_comments_deleted_fact=sum(comments_deleted_fact),
                          sum_blanks_added_fact=sum(blanks_added_fact),
                          sum_blanks_deleted_fact=sum(blanks_deleted_fact),
                          sum_commits_fact=sum(commits_fact),
                          med_contributors_fact=as.numeric(median(contributors_fact)),
                          max_loc_fact=max(loc_fact),
                          max_comments_fact=max(comments_fact),
                          max_blanks_fact=max(blanks_fact),
                          max_cumulative_commits_fact=max(cumulative_commits_fact),
                          sum_abs_loc_growth=sum(na.omit(abs_loc_growth)),
                          prod_ind_loc_growth=prod(na.omit(ind_loc_growth))),
                    by=list(project_name_fact,year_fact)])
}

addGrowthFacts(monthlyFactsWithValidationDataAfterCleaning)
yearlyFactsCleanCutOff<-subset(groupByYear(monthlyFactsWithValidationDataAfterCleaning),
                               year_fact<=2012)

addGrowthFacts(monthlyFactsWithValidationDataBeforeCleaning)
yearlyFactsUncleanCutOff<-subset(groupByYear(monthlyFactsWithValidationDataBeforeCleaning),
                                 year_fact<=2012)

