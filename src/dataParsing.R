require(XML)
require(data.table)
require(multicore)

DataPath_str <- "data/projects"

ActivityFacts_dt <- data.table(data.frame(Project_ID = character(),
                                          Month = character(),
                                          LOC_Added = integer(),
                                          LOC_Deleted = integer(),
                                          Comments_Added = integer(),
                                          Comments_Deleted = integer(),
                                          Blanks_Added = integer(),
                                          Blanks_Deleted = integer(),
                                          Commits = integer(),
                                          Contributors = integer(),
                                          stringsAsFactors = FALSE))
ActivityFactsCols_vec <- as.vector(vapply(ActivityFacts_dt,class,character(1)))

SizeFacts_dt <- data.table(data.frame(Project_ID = character(),
                                      Month = character(),
                                      LOC = integer(),
                                      Comments = integer(),
                                      Blanks = integer(),
                                      Comment_Ratio = double(),
                                      Cumulative_Commits = integer(),
                                      Man_Months = integer(),
                                      stringsAsFactors = FALSE))
SizeFactsCols_vec <- as.vector(vapply(SizeFacts_dt,class,character(1)))

obtainActivityFacts <- function (Project_ID_str) {
  filePath <- paste(DataPath_str, Project_ID_str, "ActivityFacts.xml", sep="/")
  if(file.exists(filePath)) {
    xmlData <- xmlParse(filePath)
    res <- data.table(xmlToDataFrame(xmlData["//activity_fact"],
                              colClasses = tail(ActivityFactsCols_vec, -1)))
    if(nrow(res) > 0) {
      setnames(res,colnames(res),tail(colnames(ActivityFacts_dt),-1))
      res[,Project_ID:=Project_ID_str][]
      setcolorder(res, colnames(ActivityFacts_dt))
    }
    else {
      ActivityFacts_dt
    }
  }
  else {
    ActivityFacts_dt
  }
}

obtainSizeFacts <- function (Project_ID_str) {
  filePath <- paste(DataPath_str, Project_ID_str, "SizeFacts.xml", sep="/")
  if(file.exists(filePath)) {
    xmlData <- xmlParse(filePath)
    res <- data.table(xmlToDataFrame(xmlData["//size_fact"],
                                     colClasses = tail(SizeFactsCols_vec,-1)))
    if(nrow(res) > 0) {
      setnames(res,colnames(res),tail(colnames(SizeFacts_dt),-1))
      res[,Project_ID:=Project_ID_str][]
      setcolorder(res, colnames(SizeFacts_dt))
    }
    else {
      SizeFacts_dt
    }
  }
  else {
    SizeFacts_dt
  }
}

dataParsing.main <- function() {
  projects <- list.files(DataPath_str, full.names=FALSE)
  #projects <- c("apache","firefox","mozilla","altlinux","ab")
  allActivityFacts <- rbindlist(mclapply(projects, obtainActivityFacts))
  allSizeFacts <- rbindlist(mclapply(projects,obtainSizeFacts))
  allMergedFacts <- mergeFacts(allActivityFacts,
                               allSizeFacts)
}

mergeFacts <- function (facts1_dt, facts2_dt) {
  setkey(facts1_dt, Project_ID, Month)
  setkey(facts2_dt, Project_ID, Month)
  merge(facts1_dt,facts2_dt, all=TRUE)
}
