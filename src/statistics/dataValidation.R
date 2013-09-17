require(data.table)
require(plyr)

analysis_dir <- "~/git/OhlohAnalytics/validation"

monthlyFactsBeforeCleaning <- data.table(read.csv(paste(analysis_dir,"pre-monthly-validation-facts.csv", sep="/"),
                                      colClasses=c("character",
                                                   "character",
                                                   "character",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "integer",
                                                   "numeric",
                                                   "integer",
                                                   "integer")))

setkey(monthlyFactsBeforeCleaning, project_name_fact, year_fact, month_fact)

keyFeatures = c("project_name_fact",
               "year_fact",
               "month_fact")

countFeatures = c("loc_added_fact",
                  "loc_deleted_fact",
                  "comments_added_fact",
                  "comments_deleted_fact",
                  "blanks_added_fact",
                  "blanks_deleted_fact",
                  "commits_fact",
                  "contributors_fact",
                  "loc_fact",
                  "comments_fact",
                  "blanks_fact",
                  "cumulative_commits_fact",
                  "man_months_fact")

ratioFeatures = c("comment_ratio_fact")
		
# NUMBER OF PROJECTS

numberOfProjects <- length(unique(monthlyFactsBeforeCleaning$project_name_fact))
print(paste("Number of projects:", numberOfProjects))


print("MISSING VALUES")

missingValuesMatrix <- is.na.data.frame(monthlyFactsBeforeCleaning)
missingValuesCounts <- table(missingValuesMatrix)
print(paste("Total number of values:", missingValuesCounts["TRUE"] + missingValuesCounts["FALSE"]))
print(paste("Total number of missing values:", missingValuesCounts["TRUE"]))

print("Number of missing values per feature:")
print(sapply(monthlyFactsBeforeCleaning, function(x) sum(is.na(x))))

completeCasesCount <- table(complete.cases(monthlyFactsBeforeCleaning))
print(paste("Total number of cases:", completeCasesCount["FALSE"] + completeCasesCount["TRUE"]))
print(paste("Total number of cases with missing values (in any feature):", completeCasesCount["FALSE"]))


print("IMPLAUSIBLE VALUES")

print("Number of negative values per count or ratio feature:")
print(sapply(subset(monthlyFactsBeforeCleaning, select=c(countFeatures,ratioFeatures)),
       function(x) table(x<0,useNA="ifany")["TRUE"]))

augmentWithCheckResult <- function(dataTable, featureName, checkName, checkFun) {
  dataTable[,(paste(checkName,featureName,sep="_")) := checkFun(dataTable)][]
}

for(feature in c(countFeatures,ratioFeatures)) {
  augmentWithCheckResult(monthlyFactsBeforeCleaning,feature,
                         "negative_value",function(dataTable) {dataTable[[feature]] < 0})
  print(paste("Number of cases with negative values in ", feature, ": ", 
              table(monthlyFactsBeforeCleaning[[paste("negative_value",feature,sep="_")]])["TRUE"],
              sep=""))
}

print("Number of zero values per count or ratio feature:")
print(sapply(subset(monthlyFactsBeforeCleaning, select=c(countFeatures,ratioFeatures)),
             function(x) table(x==0,useNA="ifany")["TRUE"]))

augmentWithCheckResult <- function(dataTable, featureName, checkName, checkFun) {
  dataTable[,(paste(checkName,featureName,sep="_")) := checkFun(dataTable)][]
}

for(feature in c(countFeatures,ratioFeatures)) {
  augmentWithCheckResult(monthlyFactsBeforeCleaning,feature,
                         "zero_value",function(dataTable) {dataTable[[feature]] == 0})
  print(paste("Number of cases with zero values in ", feature, ": ", 
              table(monthlyFactsBeforeCleaning[[paste("zero_value",feature,sep="_")]])["TRUE"],
              sep=""))
}

monthlyFactsBeforeCleaning[,zero_values_size_and_activity_fact:=(zero_value_loc_fact==TRUE &
                                                                zero_value_loc_added_fact==TRUE &
                                                                zero_value_loc_deleted_fact==TRUE &
                                                                zero_value_blanks_fact==TRUE &
                                                                zero_value_blanks_added_fact==TRUE &
                                                                zero_value_blanks_deleted_fact==TRUE &
                                                                zero_value_comments_fact==TRUE &
                                                                zero_value_comments_added_fact==TRUE &
                                                                zero_value_comments_deleted_fact==TRUE)][]

print(paste("Number of cases with only zero values for size and activity facts: ", 
            table(monthlyFactsBeforeCleaning$zero_values_size_and_activity_fact)["TRUE"]))

print("CONSISTENCY CHECKS")

augmentWithPreviousMonthFeature <- function(dataTable, feature, groupByFeature) {
  
  dataTable[,(paste("previous_month",feature,sep="_")) :=
              c(NA,.SD[[feature]])[1:length(.SD[[feature]])],
            by=groupByFeature][]
}

## month_fact != (previous_month + 1) &&| !(month == 1 && previous_month != 12)
augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning, "month_fact", "project_name_fact")
monthlyFactsBeforeCleaning[,inconsecutive_month:=(
  (as.integer(month_fact) != (as.integer(previous_month_month_fact) + 1)) &
  !((as.integer(month_fact) == 1 & as.integer(previous_month_month_fact) == 12)))][]

print(paste("Number of cases where 'month_fact != (previous_month + 1) &&| !(month == 1 && previous_month != 12)' holds."))
print(table(monthlyFactsBeforeCleaning$inconsecutive_month,useNA="ifany"))

## loc_fact != previous_month_loc_fact + loc_added_fact - loc_deleted_fact

augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning, "loc_fact", "project_name_fact")
monthlyFactsBeforeCleaning[,inconsistent_loc:=(
  loc_fact!=previous_month_loc_fact+
    loc_added_fact-
    loc_deleted_fact)][]

print(paste("Number of cases where 'loc_fact != previous_month_loc_fact + loc_added_fact - loc_deleted_fact' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_loc,useNA="ifany"))

## comments_fact != previous_month_comments_fact + comments_added_fact - comments_deleted_fact

augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning, "comments_fact", "project_name_fact")
monthlyFactsBeforeCleaning[,inconsistent_comments:=(
  comments_fact!=previous_month_comments_fact+
    comments_added_fact-
    comments_deleted_fact)][]

print(paste("Number of cases where 'comments_fact != previous_month_comments_fact + comments_added_fact - comments_deleted_fact' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_comments,useNA="ifany"))


## blanks_fact != previous_month_blanks_fact + blanks_added_fact - blanks_deleted_fact

augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning, "blanks_fact", "project_name_fact")
monthlyFactsBeforeCleaning[,inconsistent_blanks:=(
  blanks_fact!=previous_month_blanks_fact+
    blanks_added_fact-
    blanks_deleted_fact)][]

print(paste("Number of cases where 'blanks_fact == blanks_fact != previous_month_blanks_fact + blanks_added_fact - blanks_deleted_fact' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_blanks,useNA="ifany"))

## cumulative_commits != previous_month_cumulative_commits + commits

augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning, "cumulative_commits_fact", "project_name_fact")
monthlyFactsBeforeCleaning[,inconsistent_commits:=(
  cumulative_commits_fact!=previous_month_cumulative_commits_fact + commits_fact)][]

print(paste("Number of cases where 'cumulative_commits != previous_month_cumulative_commits + commits' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_commits,useNA="ifany"))

## man_months != previous_month_man_months + contributors

augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning, "man_months_fact", "project_name_fact")
monthlyFactsBeforeCleaning[,inconsistent_man_months:=(
  man_months_fact!=man_months_fact + contributors_fact)][]

print(paste("Number of cases where 'man_months != previous_month_man_months + contributors' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_man_months,useNA="ifany"))


