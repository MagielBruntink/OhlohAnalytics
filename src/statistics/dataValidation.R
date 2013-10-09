require(data.table)
require(plyr)
require(lubridate)

analysis_dir <- "validation"

monthlyFactsBeforeCleaning <- data.table(read.csv(paste(analysis_dir,"monthlyFactsWithProperEnlistments.csv", sep="/"),
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

coreFeatures = c("loc_added_fact",
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
                  "cumulative_commits_fact")

droppedFeatures = c("comment_ratio_fact",
                  "man_months_fact")

augmentWithCheckResult <- function(dataTable, featureName, checkName, checkFun) {
  dataTable[,(paste(checkName,featureName,sep="_")) := checkFun(dataTable)][]
}

augmentWithPreviousMonthFeature <- function(dataTable, feature, groupByFeature) {
  
  dataTable[,(paste("previous_month",feature,sep="_")) :=
              c(NA,.SD[[feature]])[1:length(.SD[[feature]])],
            by=groupByFeature][]
}


projectsUpdateDate <- data.table(read.csv(paste(analysis_dir,"projectsUpdateDate.csv", sep="/")))
setkey(projectsUpdateDate,project_name_fact)
monthlyFactsBeforeCleaning <- projectsUpdateDate[monthlyFactsBeforeCleaning]


monthlyFactsBeforeCleaning <- subset(monthlyFactsBeforeCleaning,
                                     !(floor_date(as.Date(update_date_fact),"month") <= 
                                       floor_date(as.Date(paste(year_fact,month_fact,"01",sep="-")),"month")))

monthlyFactsBeforeCleaning[,update_date_fact:=NULL][]

setkey(monthlyFactsBeforeCleaning, project_name_fact, year_fact, month_fact)

# NUMBER OF PROJECTS

numberOfProjects <- length(unique(monthlyFactsBeforeCleaning$project_name_fact))
print(paste("Number of projects:", numberOfProjects))


print("MISSING VALUES")

missingValuesMatrix <- is.na.data.frame(subset(monthlyFactsBeforeCleaning,select=c(coreFeatures,droppedFeatures)))
missingValuesCounts <- table(missingValuesMatrix)
print(paste("Total number of values:", missingValuesCounts["TRUE"] + missingValuesCounts["FALSE"]))
print(paste("Total number of missing values:", missingValuesCounts["TRUE"]))
rm(missingValuesMatrix, missingValuesCounts)

print("Number of missing values per feature:")
print(sapply(monthlyFactsBeforeCleaning, function(x) sum(is.na(x))))

completeCasesCount <- table(complete.cases(monthlyFactsBeforeCleaning))
print(paste("Total number of cases:", completeCasesCount["FALSE"] + completeCasesCount["TRUE"]))
print(paste("Total number of cases with missing values (in any feature):", completeCasesCount["FALSE"]))

monthlyFactsBeforeCleaning <- cbind(monthlyFactsBeforeCleaning,
                                    data.frame(case_has_missing_values = 
                                                 !complete.cases(subset(
                                                   monthlyFactsBeforeCleaning,select=c(coreFeatures,droppedFeatures)))))

print("IMPLAUSIBLE VALUES")

print("Number of negative values per feature:")
print(sapply(subset(monthlyFactsBeforeCleaning, select=c(coreFeatures,droppedFeatures)),
       function(x) table(x<0,useNA="ifany")["TRUE"]))

for(feature in c(coreFeatures,droppedFeatures)) {
  augmentWithCheckResult(monthlyFactsBeforeCleaning,feature,
                         "negative_value",function(dataTable) {dataTable[[feature]] < 0})
  print(paste("Number of cases with negative values in ", feature, ": ", 
              table(monthlyFactsBeforeCleaning[[paste("negative_value",feature,sep="_")]])["TRUE"],
              sep=""))
}

monthlyFactsBeforeCleaning[,case_has_implausible_value:=(negative_value_loc_fact==TRUE |
                                                         negative_value_loc_added_fact==TRUE |
                                                         negative_value_loc_deleted_fact==TRUE |
                                                         negative_value_blanks_fact==TRUE |
                                                         negative_value_blanks_added_fact==TRUE |
                                                         negative_value_blanks_deleted_fact==TRUE |
                                                         negative_value_comments_fact==TRUE |
                                                         negative_value_comments_added_fact==TRUE |
                                                         negative_value_comments_deleted_fact==TRUE |
                                                         negative_value_commits_fact==TRUE |
                                                           negative_value_cumulative_commits_fact==TRUE |
                                                           negative_value_contributors_fact==TRUE |
                                                           negative_value_comment_ratio_fact==TRUE |
                                                           negative_value_man_months_fact==TRUE)][]

print(paste("Number of cases with implausible values: ", 
            table(monthlyFactsBeforeCleaning$case_has_implausible_value)["TRUE"],
            sep=""))

print("CONSISTENCY CHECKS")

for(feature in c(coreFeatures,droppedFeatures)) {
  augmentWithCheckResult(monthlyFactsBeforeCleaning,feature,
                         "zero_value",function(dataTable) {dataTable[[feature]] == 0})
  print(paste("Number of cases with zero values in ", feature, ": ", 
              table(monthlyFactsBeforeCleaning[[paste("zero_value",feature,sep="_")]])["TRUE"],
              sep=""))
}

monthlyFactsBeforeCleaning[,zero_values_size_facts:=(zero_value_loc_fact==TRUE &
                                                       zero_value_blanks_fact==TRUE &
                                                       zero_value_comments_fact==TRUE)][]

print(paste("Number of cases with only zero values for size facts: ", 
            table(monthlyFactsBeforeCleaning$zero_values_size_facts)["TRUE"]))


## month_fact != (previous_month + 1) &&| !(month == 1 && previous_month != 12)
augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning, "month_fact", "project_name_fact")
augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning, "year_fact", "project_name_fact")
monthlyFactsBeforeCleaning[,inconsecutive_month:=(
  (ymd(paste(year_fact,month_fact,"01",sep="-")) %m+% months(-1))
  !=
  (ymd(paste(previous_month_year_fact,previous_month_month_fact,"01",sep="-")))),
                          by=project_name_fact][]

print(paste("Total number of projects that have inconsecutive months:",
            table(monthlyFactsBeforeCleaning[,any(.SD$inconsecutive_month),by=project_name_fact][["V1"]])["TRUE"]))

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

## commits_fact == 0 & (loc_added_fact != 0 | loc_deleted_fact != 0)

monthlyFactsBeforeCleaning[,inconsistent_commits_loc:=(
  commits_fact==0 & (loc_added_fact != 0 | loc_deleted_fact != 0))][]

print(paste("Number of cases where 'commits_fact == 0 & (loc_added_fact != 0 | loc_deleted_fact != 0)' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_commits_loc,useNA="ifany"))

## commits_fact == 0 & (comments_added_fact != 0 | comments_deleted_fact != 0)

monthlyFactsBeforeCleaning[,inconsistent_commits_comments:=(
  commits_fact==0 & (comments_added_fact != 0 | comments_deleted_fact != 0))][]

print(paste("Number of cases where 'commits_fact == 0 & (comments_added_fact != 0 | comments_deleted_fact != 0)' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_commits_comments,useNA="ifany"))

## commits_fact == 0 & (blanks_added_fact != 0 | blanks_deleted_fact != 0)

monthlyFactsBeforeCleaning[,inconsistent_commits_blanks:=(
  commits_fact==0 & (blanks_added_fact != 0 | blanks_deleted_fact != 0))][]

print(paste("Number of cases where 'commits_fact == 0 & (blanks_added_fact != 0 | blanks_deleted_fact != 0)' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_commits_blanks,useNA="ifany"))


## man_months != previous_month_man_months + contributors

augmentWithPreviousMonthFeature(monthlyFactsBeforeCleaning, "man_months_fact", "project_name_fact")
monthlyFactsBeforeCleaning[,inconsistent_man_months:=(
  man_months_fact!=man_months_fact + contributors_fact)][]

print(paste("Number of cases where 'man_months != previous_month_man_months + contributors' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_man_months,useNA="ifany"))

## comment_ratio != comments / (comments + loc + blanks)

monthlyFactsBeforeCleaning[,inconsistent_comment_ratio:=(
  round(comment_ratio_fact,digits=10)!=round(comments_fact / (comments_fact + loc_fact),digits=10))][]

print(paste("Number of cases where 'round(comment_ratio_fact,10)!=round(comments_fact / (comments_fact + loc_fact),10)' holds."))
print(table(monthlyFactsBeforeCleaning$inconsistent_comment_ratio,useNA="ifany"))


monthlyFactsBeforeCleaning[,case_has_inconsistent_values:=(zero_values_size_facts==TRUE |
                                                             inconsistent_loc==TRUE |
                                                             inconsistent_comments==TRUE |
                                                             inconsistent_blanks==TRUE |
                                                             inconsistent_comment_ratio==TRUE |
                                                             inconsistent_commits==TRUE |
                                                             inconsistent_man_months==TRUE |
                                                             inconsistent_commits_loc==TRUE |
                                                             inconsistent_commits_comments==TRUE |
                                                             inconsistent_commits_blanks==TRUE)][]

print(paste("Number of cases with inconsistent features: ", 
            table(monthlyFactsBeforeCleaning$case_has_inconsistent_values)["TRUE"],
            sep=""))

write.csv(monthlyFactsBeforeCleaning, paste(analysis_dir,"monthlyFactsBeforeCleaningAnnotated.csv", sep="/"))

