require(XML)
require(data.table)
require(multicore)

DataPath_str <- "data/projects"

obtainActivityFacts <- function (Project_ID_str) {
  activityFacts_df <- obtainFacts(Project_ID_str, "ActivityFacts.xml", "//activity_fact")
  if(nrow(activityFacts_df) > 0) {
    data.frame(
      Project_ID         = as.character(Project_ID_str),
      Year_Month         = as.character(activityFacts_df$month),
      LOC_Added          = as.integer(activityFacts_df$code_added),
      LOC_Deleted        = as.integer(activityFacts_df$code_removed),
      Comments_Added     = as.integer(activityFacts_df$comments_added),
      Comments_Deleted   = as.integer(activityFacts_df$comments_removed),
      Blanks_Added       = as.integer(activityFacts_df$blanks_added),
      Blanks_Deleted     = as.integer(activityFacts_df$blanks_removed),
      Commits            = as.integer(activityFacts_df$commits),
      Contributors       = as.integer(activityFacts_df$contributors),
      stringsAsFactors = FALSE)
  }
  else {
    data.frame()
  }
}

obtainSizeFacts <- function (Project_ID_str) {
  sizeFacts_df <- obtainFacts(Project_ID_str, "SizeFacts.xml", "//size_fact")
  if(nrow(sizeFacts_df) > 0) {
    data.frame(
      Project_ID         = as.character(Project_ID_str),
      Year_Month         = as.character(sizeFacts_df$month),
      LOC                = as.integer(sizeFacts_df$code),
      Comments           = as.integer(sizeFacts_df$comments),
      Blanks             = as.integer(sizeFacts_df$blanks),
      Comment_Ratio      = as.double(sizeFacts_df$comment_ratio),
      Cumulative_Commits = as.integer(sizeFacts_df$commits),
      Man_Months         = as.integer(sizeFacts_df$man_months),
      stringsAsFactors = FALSE)
  }
  else {
    data.frame()
  }
}

obtainEnlistments <- function (Project_ID_str) {
  enlistments_df <- obtainFacts(Project_ID_str, "Enlistments.xml", "//repository")
  if(nrow(enlistments_df) > 0) {
      data.frame(
        Project_ID       = as.character(Project_ID_str),
        Type             = as.character(enlistments_df$type),
        URL              = as.character(enlistments_df$url),
        Logged_Date      = as.character(enlistments_df$logged_at),
        Commits_Total    = as.integer(enlistments_df$commits),
        stringsAsFactors = FALSE)
  }
  else {
    data.frame()
  }
}

obtainFacts <- function (Project_ID_str, XML_File_Name_str, XML_Node_str) {
  filePath <- paste(DataPath_str, Project_ID_str, XML_File_Name_str, sep="/")
  if(file.exists(filePath)) {
    xmlData <- xmlParse(filePath)
    facts <- xmlToDataFrame(xmlData[XML_Node_str])
  }
  else {
    data.frame()
  }
}

obtainActivityAndSizeFactsForAllProjects <- function() {
  projects <- list.files(DataPath_str, full.names=FALSE)
  obtainActivityAndSizeFactsForProjects(projects_list)
}

obtainActivityAndSizeFactsForProjects <- function(projects_list) {
  allActivityFacts <- rbindlist(mclapply(projects_list, obtainActivityFacts))
  setkey(allActivityFacts, Project_ID, Year_Month)
  allSizeFacts <- rbindlist(mclapply(projects_list,obtainSizeFacts))
  merge(allActivityFacts,allSizeFacts, all=TRUE)
}

obtainEnlistmentsForAllProjects <- function() {
  projects <- list.files(DataPath_str, full.names=FALSE)
  obtainEnlistmentsForProjects(projects)
}

obtainEnlistmentsForProjects <- function(projects_list) {
  allEnlistments <- rbindlist(mclapply(projects_list, obtainEnlistments))
  setkey(allEnlistments,Project_ID)
}
