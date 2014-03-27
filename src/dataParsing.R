require(XML)
require(data.table)
require(multicore)

DataPath_str <- "data/projects"

obtainActivityFacts <- function (Project_ID_str) {
  activityFacts_df <- obtainDataFromXML(Project_ID_str, "ActivityFacts.xml", "//activity_fact")
  if(nrow(activityFacts_df) > 0) {
    data.frame(
      Project_ID         = as.character(Project_ID_str),
      Year_Month         = as.character(activityFacts_df$month),
      LOC_Added          = as.integer(as.character(activityFacts_df$code_added)),
      LOC_Deleted        = as.integer(as.character(activityFacts_df$code_removed)),
      Comments_Added     = as.integer(as.character(activityFacts_df$comments_added)),
      Comments_Deleted   = as.integer(as.character(activityFacts_df$comments_removed)),
      Blanks_Added       = as.integer(as.character(activityFacts_df$blanks_added)),
      Blanks_Deleted     = as.integer(as.character(activityFacts_df$blanks_removed)),
      Commits            = as.integer(as.character(activityFacts_df$commits)),
      Contributors       = as.integer(as.character(activityFacts_df$contributors)),
      stringsAsFactors = FALSE)
  }
  else {
    data.frame()
  }
}

obtainSizeFacts <- function (Project_ID_str) {
  sizeFacts_df <- obtainDataFromXML(Project_ID_str, "SizeFacts.xml", "//size_fact")
  if(nrow(sizeFacts_df) > 0) {
    data.frame(
      Project_ID         = as.character(Project_ID_str),
      Year_Month         = as.character(sizeFacts_df$month),
      LOC                = as.integer(as.character(sizeFacts_df$code)),
      Comments           = as.integer(as.character(sizeFacts_df$comments)),
      Blanks             = as.integer(as.character(sizeFacts_df$blanks)),
      Comment_Ratio      = as.numeric(as.character(sizeFacts_df$comment_ratio)),
      Cumulative_Commits = as.integer(as.character(sizeFacts_df$commits)),
      Man_Months         = as.integer(as.character(sizeFacts_df$man_months)),
      stringsAsFactors = FALSE)
  }
  else {
    data.frame()
  }
}

obtainEnlistments <- function (Project_ID_str) {
  enlistments_df <- obtainDataFromXML(Project_ID_str, "Enlistments.xml", "//repository")
  if(nrow(enlistments_df) > 0) {
      data.frame(
        Project_ID       = as.character(Project_ID_str),
        Type             = as.character(enlistments_df$type),
        URL              = as.character(enlistments_df$url),
        Logged_Date      = as.character(enlistments_df$logged_at),
        Commits_Total    = as.integer(as.character(enlistments_df$commits)),
        stringsAsFactors = FALSE)
  }
  else {
    data.frame()
  }
}

obtainMetaData <- function (Project_ID_str) {
  analysis_df <- obtainDataFromXML(Project_ID_str, "MetaData.xml", "//analysis")
  if (nrow(analysis_df) > 0) {
    data.frame (
      Project_ID = as.character(Project_ID_str),
      Update_Date = as.character(analysis_df$updated_at),
      Main_Language = as.character(analysis_df$main_language_name),
      stringsAsFactors = FALSE)
  }
  else {
    data.frame()
  }
}

obtainDataFromXML <- function (Project_ID_str, XML_File_Name_str, XML_Node_str) {
  XML <- obtainXML (Project_ID_str, XML_File_Name_str)
  if(!is.null(XML)) {
      xmlToDataFrame(XML[XML_Node_str])
  }
  else {
    data.frame()
  }
}

obtainXML <- function (Project_ID_str, XML_File_Name_str) {
  filePath <- paste(DataPath_str, Project_ID_str, XML_File_Name_str, sep="/")
  if(file.exists(filePath)) {
    xmlData <- xmlParse(filePath)
  }
  else {
    NULL
  }
}

obtainDataForProjects <- function(projects_list, obtain_fun) {
  data_dt <- rbindlist(mclapply(projects_list, obtain_fun))
  setkey(data_dt,Project_ID)
}

obtainAllDataAsCSV <- function() {
  projects_list <- list.files(DataPath_str, full.names=FALSE)
  write.csv(obtainDataForProjects(projects_list, obtainActivityFacts), "output/ActivityFacts.csv", row.names=FALSE)
  write.csv(obtainDataForProjects(projects_list, obtainSizeFacts), "output/SizeFacts.csv", row.names=FALSE)
  write.csv(obtainDataForProjects(projects_list, obtainEnlistments), "output/Enlistments.csv", row.names=FALSE)
  write.csv(obtainDataForProjects(projects_list, obtainMetaData), "output/MetaData.csv", row.names=FALSE)
}

