require(data.table)
require(plyr)

analysis_dir <- "~/git/OhlohAnalytics/validation"

addGrowthFacts <- function(monthlyData) {
  monthlyData[,abs_loc_growth:=(loc_added_fact - loc_deleted_fact)][]
  monthlyData[,ind_loc_growth:=(loc_fact / (loc_fact - abs_loc_growth))][]
}

addAgeFacts <- function(monthlyData) {
  monthlyData[,age_in_months:=
                (as.integer(month_fact) - as.integer(.SD[1]$month_fact)) +
                ((as.integer(year_fact) - as.integer(.SD[1]$year_fact)) * 12),
              by=project_name_fact][]
  monthlyData[,age_in_years:= floor(age_in_months/12)]
}

groupByCalendarYear <- function(dataTable) {
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

groupByAgeYear <- function(dataTable) {
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
                          prod_ind_loc_growth=prod(na.omit(ind_loc_growth)),
                          nr_months_in_year=length(.SD$month_fact)),
                    by=list(project_name_fact,age_in_years)])
}

monthlyFactsBeforeCleaningCutoff <- subset(monthlyFactsBeforeCleaning,
                                    year_fact<=2012)
monthlyFactsAfterCleaningCutoff <- subset(monthlyFactsAfterCleaning,
                                    year_fact<=2012)

addGrowthFacts(monthlyFactsBeforeCleaningCutoff)
addGrowthFacts(monthlyFactsAfterCleaningCutoff)

addAgeFacts(monthlyFactsBeforeCleaningCutoff)
addAgeFacts(monthlyFactsAfterCleaningCutoff)

yearlyFactsNotCleanedCutoff <- groupByAgeYear(monthlyFactsBeforeCleaningCutoff)
yearlyFactsCleanedCutoff <- groupByAgeYear(monthlyFactsAfterCleaningCutoff)

projectMetaData <- data.table(read.csv(paste(analysis_dir,"projectsMetaData.csv", sep="/")))


