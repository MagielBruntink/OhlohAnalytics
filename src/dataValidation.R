require(data.table)
require(plyr)

source("src/dataParsing.R")

goodSVNPatterns <- list(".*/trunk/?",
                        ".*/head/?",
                        ".*/sandbox/?",
                        ".*/site/?",
                        ".*/branches/\\w+",
                        ".*/tags/\\w+")

validateAllData <- function() {
  projects_list <- c("firefox","arcmanager","mozila")
  #projects <- list.files(DataPath_str, full.names=FALSE)
  activityFacts <- obtainDataForProjects(projects_list, obtainActivityFacts)
  setkey(activityFacts, Project_ID, Year_Month)
  sizeFacts <- obtainDataForProjects(projects_list, obtainSizeFacts)
  setkey(sizeFacts, Project_ID, Year_Month)
  mergedFacts <- merge(activityFacts, sizeFacts, all=TRUE)
  enlistments <- obtainDataForProjects(projects_list, obtainEnlistments)
  metaData <- obtainDataForProjects(projects_list, obtainMetaData)
}

validateEnlistments <- function(enlistments_dt) {
  validationResult <- data.frame(
    Validation_Bad_Enlistment =
      mapply(function(Type, URL) {
        (Type == "SvnRepository" | Type == "SvnSyncRepository") &
          all(lapply(goodSVNPatterns,
                     function(p) {
                       any(grepl(p, URL, ignore.case = TRUE)
                           == TRUE)
                     })
              == FALSE)
      },
      enlistments_dt$Type,
      enlistments_dt$URL))
  return(cbind(enlistments_dt, data.table(validationResult)))
}

augmentFactsWithValidatedEnlistments <- function(facts_dt, enlistments_dt) {
  validatedEnlistments <- validateEnlistments(enlistments_dt)
  facts_dt[list(Project_ID, Year_Month), 
           Validation_Bad_Enlistment := any(validatedEnlistments[Project_ID]$Validation_Bad_Enlistment)]
}
