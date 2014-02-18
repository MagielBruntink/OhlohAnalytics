require(data.table)
require(plyr)

analysis_dir <- "validation"

addGrowthFacts <- function(monthlyData) {
  monthlyData[,abs_loc_growth:=(loc_fact - previous_month_loc_fact)][]
  monthlyData[,ind_loc_growth:=(loc_fact / previous_month_loc_fact)][]
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
                          nr_months_in_year=length(.SD$month_fact),
                          max_calendar_year=max(year_fact)),
                    by=list(project_name_fact,age_in_years)])
}

augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning,"loc_fact","project_name_fact")
augmentWithPreviousMonthFeature(monthlyFactsAfterCleaning,"loc_fact","project_name_fact")

addGrowthFacts(monthlyFactsBeforeCleaning)
addGrowthFacts(monthlyFactsAfterCleaning)

addAgeFacts(monthlyFactsBeforeCleaning)
addAgeFacts(monthlyFactsAfterCleaning)

projectsMainLanguages <- data.table(read.csv(paste(analysis_dir,"projectsMainLanguages.csv", sep="/")))
setkey(projectsMainLanguages,project_name_fact)
monthlyFactsAfterCleaning <- projectsMainLanguages[monthlyFactsAfterCleaning]

yearlyFactsBeforeCleaning <- groupByAgeYear(monthlyFactsBeforeCleaning)
yearlyFactsAfterCleaning <- groupByAgeYear(monthlyFactsAfterCleaning)
yearlyFactsAfterCleaning <- projectsMainLanguages[yearlyFactsAfterCleaning]

# Project inactivity

# Max age of activity and min age of inactivity
projectActivityStatus <- yearlyFactsAfterCleaning[,
                         list(last_activity_age=max(subset(.SD,sum_commits_fact!=0)$age_in_years),
                              first_inactivity_age=min(subset(.SD,sum_commits_fact==0 & nr_months_in_year==12)$age_in_years)),
                         by=list(project_name_fact)]

projectActivityStatus[,yearOfEvent:=last_activity_age]
projectActivityStatus[,status:=1]
projectActivityStatus[!is.infinite(first_inactivity_age),yearOfEvent:=first_inactivity_age]
projectActivityStatus[!is.infinite(first_inactivity_age),status:=2]

# Output CSV files
write.csv(monthlyFactsAfterCleaning, paste(analysis_dir,"monthlyFactsAfterCleaningWithMetaData.csv", sep="/"))
write.csv(yearlyFactsAfterCleaning, paste(analysis_dir,"yearlyFactsAfterCleaningWithMetaData.csv", sep="/"))
write.csv(projectActivityStatus, paste(analysis_dir,"projectActivityStatus.csv", sep="/"))


