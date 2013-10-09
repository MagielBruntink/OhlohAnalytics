require(ggplot2)
require(survival)
require(plyr)
require(data.table)

options(scipen=1000)

theme_set(theme_bw(base_size = 24))

analysis_dir <- "validation"
output_dir <- "/Users/magielbruntink/Google Drive/UVA/Research/Writing/Quality of Software Evolution Data on Ohloh"

monthlyFacts <- data.table(read.csv(file=paste(analysis_dir,"monthlyFactsAfterCleaningWithMetaData.csv",sep="/"),header=TRUE,sep=","))
monthlyFactsBeforeCleaningCasesAnnotated <- data.table(read.csv(file=paste(analysis_dir,"monthlyFactsBeforeCleaningCasesAnnotated.csv",sep="/"),header=TRUE,sep=","))
setkey(monthlyFacts,project_name_fact,year_fact,month_fact)
setkey(monthlyFactsBeforeCleaningCasesAnnotated,project_name_fact,year_fact,month_fact)

projectsMainLanguages <- data.table(read.csv(paste(analysis_dir,"projectsMainLanguages.csv", sep="/")))
setkey(projectsMainLanguages,project_name_fact)
monthlyFactsBeforeCleaningCasesAnnotated <- projectsMainLanguages[monthlyFactsBeforeCleaningCasesAnnotated]
setkey(monthlyFactsBeforeCleaningCasesAnnotated,project_name_fact,year_fact,month_fact)

# TODO: get this from the actual data
frequentLanguages <- c("C","C++","Java","Python","PHP","JavaScript","C#","Perl","Ruby","shell script")

#projectDeath <- read.csv(file=paste(analysis_dir, "projectDeathStatus.csv",sep="/"),header=TRUE,sep=",")
#projectRepositoryFacts <- read.csv(file=paste(analysis_dir, "projectRepositoryFacts.csv",sep="/"),header=TRUE,sep=",")
#totalLOC <- sum(as.numeric(subset(yearlyFacts,year==cutOffYear)$max_loc_total))

### Boxplots
boxPlot <- function(dataFrame,xLabel,yLabel,fileName,zoom=TRUE,statsDetailLevel=2) {
  allWhiskerLimits = ddply(dataFrame, .(xData), summarise, 
                           whiskerLimMin = boxplot.stats(yData)$stats[c(1)], 
                           whiskerLimMax = boxplot.stats(yData)$stats[c(5)])
  allObsCounts     = ddply(dataFrame, .(xData), summarise, obsCount = length(yData))
  allOutlierCounts = ddply(dataFrame, .(xData), summarise, outlierCount = length(boxplot.stats(yData)$out))  
  allMedians       = ddply(dataFrame, .(xData), summarise, medianValue = round(median(yData),digits=3))
  allIQRs          = ddply(dataFrame, .(xData), summarise, iqr = round(IQR(yData),digits=3))
  allLabelData     = cbind(allObsCounts,allOutlierCounts,allMedians,allIQRs)
  
  plotObj <- ggplot(dataFrame,aes(x=xData,y=yData)) +
    geom_boxplot() +
    labs (x=xLabel,y=yLabel) +
    geom_hline(aes(yintercept=1.000))
  
  if(statsDetailLevel==2) {
    plotObj = plotObj + 
              geom_text(data=allLabelData,
                        aes(x = xData,
                            y = medianValue,
                              label = paste("Med. =", medianValue,
                                          "Obs. =", obsCount,
                                          "Out. =", outlierCount,
                                          "IQR =", iqr,
                                          sep=" ")),
              size = 5, vjust = -1.5)
  }
  
  if(statsDetailLevel==1) {
    plotObj = plotObj + 
      geom_text(data=allLabelData,
                aes(x = xData,
                    y = medianValue,
                    label = paste("Med. =", medianValue,
                                  "Obs. =", obsCount,
                                  "IQR =", iqr,
                                  sep=" ")),
                size = 3, vjust = -1.5)
  }
  
  if(zoom) {
    whiskerLimits = c(min(allWhiskerLimits$whiskerLimMin),max(allWhiskerLimits$whiskerLimMax))
    plotObj = plotObj + coord_cartesian(ylim = whiskerLimits*1.05)
  }
  
  ggsave(file=paste(output_dir,fileName,sep="/"),plot=plotObj)
}

### Multi boxplots
multiBoxplot <- function(dataFrame,xLabel,yLabel,yMin,yMax,fileName) {
  
  plotObj <- ggplot(dataFrame, aes(factor(xFactor),y=yData))
  plotObj <- plotObj + geom_boxplot()
  plotObj <- plotObj + labs(x=xLabel,y=yLabel)
  plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
  plotObj <- plotObj + coord_cartesian(ylim = c(yMin,yMax))
  plotObj <- plotObj + ggtitle(paste("N = ", nrow(dataFrame)))
  ggsave(file=paste(output_dir,fileName,sep="/"),plot=plotObj)
}

### Scatter plots
scatterPlot <- function(dataFrame,xLabel,yLabel,fileName,zoom=TRUE) {
  plotObj <- ggplot(dataFrame,aes(x=xData,y=yData)) +
    geom_point() +
    labs (x=xLabel,y=yLabel)
  
  ggsave(file=paste(output_dir,fileName,sep="/"),plot=plotObj)
}


######## Months per language before and after cleaning combined
dataToPlot <- melt(list(
  na.omit(data.frame(monthlyFactsBeforeCleaningCasesAnnotated[,month_fact,by=main_language_fact])),
  data.frame(monthlyFacts[,month_fact,by=main_language_fact])),
                   id.vars=c("main_language_fact"), measure.vars=c("month_fact"), value.name = "month_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  fill=factor(L1)))
plotObj <- plotObj + geom_bar(stat="bin",position="dodge")
plotObj <- plotObj + labs(x="",y="Number of months")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5))
plotObj <- plotObj + scale_fill_manual(values=c("gray","red"))
plotObj <- plotObj + theme(legend.position = "none") 
ggsave(file=paste(output_dir,"barchart-months-per-main-language-combined.pdf",sep="/"),plot=plotObj)


######## LOC Added per month before and after cleaning combined

dataToPlot <- melt(list(
  monthlyFactsBeforeCleaningCasesAnnotated,
  monthlyFacts
), id.vars=c("main_language_fact"), measure.vars=c("loc_added_fact"), value.name = "loc_added_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  ,y=loc_added_fact,fill=factor(L1),dodge=L1))
plotObj <- plotObj + geom_boxplot()
plotObj <- plotObj + labs(x="",y="LOC Added per month")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5),ylim = c(0,4500))
plotObj <- plotObj + scale_fill_manual(values=c("gray","red"))
plotObj <- plotObj + theme(legend.position = "none") 
ggsave(file=paste(output_dir,"boxplots-loc-added-per-programming-language-combined.pdf",sep="/"),plot=plotObj)

######## Commits per month before and after cleaning combined

dataToPlot <- melt(list(
  monthlyFactsBeforeCleaningCasesAnnotated,
  monthlyFacts
), id.vars=c("main_language_fact"), measure.vars=c("commits_fact"), value.name = "commits_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  ,y=commits_fact,fill=factor(L1),dodge=L1))
plotObj <- plotObj + geom_boxplot()
plotObj <- plotObj + labs(x="",y="Commits per month")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5),ylim = c(0,75))
plotObj <- plotObj + scale_fill_manual(values=c("gray","red"))
plotObj <- plotObj + theme(legend.position = "none") 
ggsave(file=paste(output_dir,"boxplots-commits_fact-per-programming-language-combined.pdf",sep="/"),plot=plotObj)


######## Projects per language
dataToPlot <- data.frame(monthlyFacts[,length(unique(.SD$project_name_fact)),
                                      by=main_language_fact])

dataToPlot$sorted_factors <- reorder(dataToPlot$main_language_fact, 
                                     dataToPlot$V1)
plotObj <- ggplot(dataToPlot[1:20,], aes(x=sorted_factors,y=V1))
plotObj <- plotObj +
  geom_bar(stat="identity") +
  labs(y="Number of projects",x="") +
  coord_flip()
ggsave(file=paste(output_dir,"barchart-projects-per-main-language.pdf",sep="/"),plot=plotObj)


######## Months per language 
dataToPlot <- data.frame(monthlyFacts[,length(.SD$month_fact),
                                      by=main_language_fact])

dataToPlot$sorted_factors <- reorder(dataToPlot$main_language_fact, dataToPlot$V1)
plotObj <- ggplot(dataToPlot[1:20,], aes(x=sorted_factors,y=V1))
plotObj <- plotObj +
  geom_bar(stat="identity") +
  labs(y="Number of months",x="") +
  coord_flip()
ggsave(file=paste(output_dir,"barchart-months-per-main-language-after-cleaning.pdf",sep="/"),plot=plotObj)

######## Duration in data set per project
dataToPlot <- data.frame(monthlyFacts[,max(.SD$age_in_months),
                                      by=project_name_fact])

plotObj <- ggplot(dataToPlot, aes(x=V1))
plotObj <- plotObj +
  geom_histogram() +
  labs(x="Number of months duration",y="Number of projects") +
  coord_flip()
ggsave(file=paste(output_dir,"barchart-months-duration-per-project.pdf",sep="/"),plot=plotObj)

######## Age of the projects (in months) in June 2013

dataToPlot <- subset(monthlyFacts,main_language_fact %in% frequentLanguages & year_fact=="2013" & month_fact=="6")
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$age_in_months
multiBoxplot(dataToPlot,"","Age in months in June 2013",
             0,300,
             "boxplots-age-june2013-per-programming-language.pdf")

######## Code size of the projects in June 2013 before cleaning

dataToPlot <- subset(monthlyFactsBeforeCleaningCasesAnnotated,main_language_fact %in% frequentLanguages & year_fact=="2013" & month_fact=="6")
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$loc_fact
multiBoxplot(dataToPlot,"","LOC in June 2013",
             0,250000,
             "boxplots-loc-june2013-per-programming-language-before-cleaning.pdf")


######## Code size of the projects in June 2013 after cleaning

dataToPlot <- subset(monthlyFacts,main_language_fact %in% frequentLanguages & year_fact=="2013" & month_fact=="6")
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$loc_fact
multiBoxplot(dataToPlot,"","LOC in June 2013",
             0,250000,
             "boxplots-loc-june2013-per-programming-language-after-cleaning.pdf")

######## LOC Added per month for various programming languages

dataToPlot <- subset(monthlyFacts,main_language_fact %in% frequentLanguages)
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$loc_added_fact
multiBoxplot(dataToPlot,"","LOC Added",
             0,5000,
             "boxplots-loc-added-per-programming-language.pdf")

######## LOC Deleted per month for various programming languages

dataToPlot <- subset(monthlyFacts,main_language_fact %in% frequentLanguages)
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$loc_deleted_fact
multiBoxplot(dataToPlot,"","LOC Deleted",
             0,5000,
             "boxplots-loc-deleted-per-programming-language.pdf")

######## LOC Absolute Growth per month for various programming languages

dataToPlot <- subset(monthlyFacts,main_language_fact %in% frequentLanguages)
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$abs_loc_growth
multiBoxplot(dataToPlot,"","Absolute LOC Growth",
             -250,4000,
             "boxplots-abs-loc-growth-per-programming-language.pdf")

######## LOC Absolute Growth per month for various programming languages, with commits > 0

dataToPlot <- subset(monthlyFacts,main_language_fact %in% frequentLanguages & commits_fact > 0)
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$abs_loc_growth
multiBoxplot(dataToPlot,"","Absolute LOC Growth",
             -250,4000,
             "boxplots-abs-loc-growth-per-programming-language-non-zero-commits.pdf")


######## LOC Indexed Growth per month for various programming languages

dataToPlot <- subset(monthlyFacts,main_language_fact %in% frequentLanguages)
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$ind_loc_growth
multiBoxplot(dataToPlot,"","Indexed LOC Growth",
             0.99,1.02,
             "boxplots-ind-loc_growth-per-programming-language.pdf")

######## Contributors per month for various programming languages

dataToPlot <- subset(monthlyFacts,main_language_fact %in% frequentLanguages)
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$contributors_fact
multiBoxplot(dataToPlot,"","Contributors",
             0,3,
             "boxplots-contributors-per-programming-language.pdf")

######## Commits per month for various programming languages

dataToPlot <- subset(monthlyFacts,main_language_fact %in% frequentLanguages)
dataToPlot$xFactor <- dataToPlot$main_language_fact
dataToPlot$yData <- dataToPlot$commits_fact
multiBoxplot(dataToPlot,"","Commits",
             0,75,
             "boxplots-commits-per-programming-language.pdf")

######### CODE SIZE

# dataSelection = subset(yearlyFacts,year==2012)
# dataToPlot <- data.frame(xData="Projects in 2012",yData=dataSelection$max_loc_total)
# boxPlot(dataToPlot,
#         "",
#         "Code Size (CS)",
#         "boxplot-codesize-2012.pdf")
# 
# dataToPlot <- data.frame(xData=factor(yearlyFacts$age),yData=yearlyFacts$max_loc_total)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Code Size (CS)",
#         "boxplots-codesize-by-age.pdf",
#         TRUE,0)
# 
# dataSelection = subset(yearlyFacts,age<=5)
# dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$max_loc_total)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Code Size (CS)",
#         "boxplots-codesize-by-age-5-years.pdf",
#         TRUE,0)
# 
# 
# ######### CODE GROWTH INDEXED
# 
# dataToPlot <- data.frame(xData="All project years",yData=yearlyFacts$prod_loc_growth_factor)
# boxPlot(dataToPlot,
#         "",
#         "Code Growth indexed (CGi)",
#         "boxplot-codegrowth-indexed.pdf")
# 
# dataToPlot <- data.frame(xData=factor(yearlyFacts$age),yData=yearlyFacts$prod_loc_growth_factor)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Code Growth indexed (CGi)",
#         "boxplots-codegrowth-indexed-by-age.pdf",
#         TRUE,0)
# 
# dataSelection = subset(yearlyFacts,age<=5)
# dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$prod_loc_growth_factor)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Code Growth indexed (CGi)",
#         "boxplots-codegrowth-indexed-by-age-5-years.pdf",
#         TRUE,0)
# 
# dataSelection = subset(yearlyFacts,age<=20)
# dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$prod_loc_growth_factor)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Code Growth indexed (CGi)",
#         "boxplots-codegrowth-indexed-by-age-20-years.pdf",
#         TRUE,0)
# 
# pdf(file=paste(output_dir,"hist-codegrowth-indexed-year.pdf",sep="/"))
# hist(yearlyFacts$prod_loc_growth_factor, xlab="Code Growth indexed (CGi)",
#      xlim=c(1,2),
#      ylab="Probability", 
#      main=paste("Probability density of relative code growth in a year.", "N =", length(yearlyFacts$projectName), sep=" " ),
#      freq=FALSE,
#      breaks=20)
# dev.off()
# 
# pdf(file=paste(output_dir,"hist-codegrowth-indexed-month.pdf",sep="/"))
# hist(monthlyFacts$loc_growth_factor, xlab="Code Growth indexed (CGi)",
#      #xlim=c(1,2),
#      ylab="Probability", 
#      main=paste("Probability density of relative code growth in a month.", "N =", length(monthlyFacts$projectName), sep=" " ),
#      freq=FALSE,
#      breaks=20)
# dev.off()
# 
# 
# ######### CODE GROWTH ABSOLUTE
# 
# dataToPlot <- data.frame(xData="All project years",yData=yearlyFacts$sum_abs_loc_growth)
# boxPlot(dataToPlot,
#         "",
#         "Code Growth absolute (CGa)",
#         "boxplot-codegrowth-absolute.pdf")
# 
# dataToPlot <- data.frame(xData=factor(yearlyFacts$age),yData=yearlyFacts$sum_abs_loc_growth)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Code Growth absolute (CGa)",
#         "boxplots-codegrowth-absolute.pdf-by-age.pdf",
#         TRUE,0)
# 
# dataSelection = subset(yearlyFacts,age<=5)
# dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$sum_abs_loc_growth)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Code Growth absolute (CGa)",
#         "boxplots-codegrowth-absolute.pdf-by-age-5-years.pdf",
#         TRUE,0)
# 
# pdf(file=paste(output_dir,"hist-codegrowth-absolute-year.pdf",sep="/"))
# hist(yearlyFacts$sum_abs_loc_growth, xlab="Code Growth absolute (CGa)",
#      #xlim=c(1,2),
#      ylab="Probability", 
#      main=paste("Probability density of absolute code growth in a year.", "N =", length(yearlyFacts$projectName), sep=" " ),
#      freq=FALSE,
#      breaks=20)
# dev.off()
# 
# pdf(file=paste(output_dir,"hist-codegrowth-absolute-month.pdf",sep="/"))
# hist(monthlyFacts$abs_loc_growth, xlab="Code Growth absolute (CGa)",
#      #xlim=c(1,2),
#      ylab="Probability", 
#      main=paste("Probability density of absolute code growth in a month.", "N =", length(monthlyFacts$projectName), sep=" " ),
#      freq=FALSE,
#      breaks=20)
# dev.off()
# 
# 
# ######### TEAM ACTIVITY
# 
# dataToPlot <- data.frame(xData="All project years",yData=yearlyFacts$sum_commits)
# boxPlot(dataToPlot,
#         "",
#         "Team Activity (TA)",
#         "boxplot-teamactivity.pdf")
# 
# dataToPlot <- data.frame(xData=factor(yearlyFacts$age),yData=yearlyFacts$sum_commits)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Team Activity (TA)",
#         "boxplots-teamactivity-by-age.pdf",
#         TRUE,0)
# 
# dataSelection = subset(yearlyFacts,age<=5)
# dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$sum_commits)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Team Activity (TA)",
#         "boxplots-teamactivity-by-age-5-years.pdf",
#         TRUE,0)
# 
# ######### TEAM ACTIVITY
# 
# dataToPlot <- data.frame(xData="All project years",yData=yearlyFacts$median_contributors)
# boxPlot(dataToPlot,
#         "",
#         "Team Size (TS)",
#         "boxplot-teamsize.pdf")
# 
# dataToPlot <- data.frame(xData=factor(yearlyFacts$age),yData=yearlyFacts$median_contributors)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Team Size (TS)",
#         "boxplots-teamsize-by-age.pdf",
#         TRUE,0)
# 
# dataSelection = subset(yearlyFacts,age<=5)
# dataToPlot <- data.frame(xData=factor(dataSelection$age),yData=dataSelection$median_contributors)
# boxPlot(dataToPlot,
#         "Project age (years)",
#         "Team Size (TS)",
#         "boxplots-teamsize-by-age-5-years.pdf",
#         TRUE,0)
# 
# ### Plot of project ages in 2012
# pdf(file=paste(output_dir,"hist-age-in-2012.pdf",sep="/"))
# dataToPlot <- subset(yearlyFacts,subset=year==2012,select=age)
# hist(dataToPlot$age, xlab="Project age in 2012",
#      ##xlim=c(0,40),
#      ylab="Number of occurrences", 
#      main=paste("Histogram of project ages in 2012, N =", length(dataToPlot$age), "projects, median =", median(dataToPlot$age), sep=" " ),
#      freq=TRUE,
#      breaks=40)
# dev.off()
# 
# ### Plot of project survival curve
# survivalCurve <- survfit(Surv(age,status) ~ 1, data=projectDeath)
# pdf(file=paste(output_dir,"surv-projects.pdf",sep="/"))
# plot(survivalCurve, xlab="Project age in years", ylab="Project Survival Probability (SP)",
#      main=paste("Kaplan-Meier survival of",length(projectDeath$projectName),"projects",sep=" ")
# )
# dev.off()
# 
# ### Age of death
# ageAtDeath <- subset(projectDeath, status==2, select=age)
# dataToPlot <- data.frame(xData="Dead projects",yData=ageAtDeath$age)
# boxPlot(dataToPlot,
#         "",
#         "Age of Death (AD)",
#         "boxplot-age-of-death.pdf",
#         TRUE,2)
# 
# ### Plot of age at projects deaths
# pdf(file=paste(output_dir,"hist-age-death.pdf",sep="/"))
# hist(ageAtDeath$age, xlab="Project age at death in years",
#      xlim=c(0,40),
#      ylab="Number of occurrences", 
#      main=paste("Histogram of the age at death of", length(ageAtDeath$age), "projects", sep=" " ),
#      freq=TRUE,
#      breaks=20)
# dev.off()
# 
# ### Plot of calendar year at project deaths
# yearOfDeath <- subset(projectDeath, status==2, select=yearOfEvent)
# pdf(file=paste(output_dir,"hist-year-death.pdf",sep="/"))
# hist(yearOfDeath$yearOfEvent, xlab="Year of death",
#      ylab="Number of occurrences", 
#      main=paste("Histogram of the year of death of", length(yearOfDeath$yearOfEvent), "projects", sep=" " ),
#      freq=TRUE)
# dev.off()
# 
# ### Plot of code size in 2012
# dataToPlot <- subset(yearlyFacts, year==2012, select=max_loc_total)
# pdf(file=paste(output_dir,"hist-code-size-2012.pdf",sep="/"))
# hist(dataToPlot$max_loc_total, xlab="Code size",
#      ylab="Number of occurrences", 
#      main=paste("Histogram of the code size in 2012", length(dataToPlot$max_loc_total), "projects", sep=" " ),
#      freq=TRUE)
# dev.off()
# 
# 
# ### Projects are dying
# projectsThatDied <- subset(projectDeath, status==2, select= c(projectName,yearOfEvent))
# isDeadYear<-function(row) {
#   m<-match(row[["projectName"]],projectsThatDied$projectName,FALSE)
#   if(m != FALSE) {
#     yearOfDeath <- projectsThatDied$yearOfEvent[[m[[1]]]]
#     row[["year"]] >= yearOfDeath
#   }
#   else FALSE
# }
# yearlyFacts$deadYear <- apply(yearlyFacts,1,isDeadYear)
# yearlyFactsOnDyingProjects <- subset(yearlyFacts, deadYear == FALSE & projectName %in% projectsThatDied$projectName)
# 
# ### CODE GROWTH INDEXED FOR DYING PROJECTS
# dataToPlot<-data.frame(xData=c(rep("Dying projects",length(yearlyFactsOnDyingProjects$projectName)),
#                     rep("Base rate",length(yearlyFacts$projectName))),
#                yData=c(yearlyFactsOnDyingProjects$prod_loc_growth_factor,
#                        yearlyFacts$prod_loc_growth_factor))
# 
# boxPlot(dataToPlot,
#         "",
#         "Code Growth indexed (CGi)",
#         "boxplot-codegrowth-indexed-dying-projects.pdf",
#         TRUE,1)
# 
# ### CODE GROWTH ABSOLUTE FOR DYING PROJECTS
# dataToPlot<-data.frame(xData=c(rep("Dying projects",length(yearlyFactsOnDyingProjects$projectName)),
#                                rep("Base rate",length(yearlyFacts$projectName))),
#                        yData=c(yearlyFactsOnDyingProjects$sum_abs_loc_growth,
#                                yearlyFacts$sum_abs_loc_growth))
# 
# boxPlot(dataToPlot,
#         "",
#         "Code Growth absolute (CGa)",
#         "boxplot-codegrowth-absolute-dying-projects.pdf",
#         TRUE,1)
# 
# ### TEAM ACTIVITY FOR DYING PROJECTS
# dataToPlot<-data.frame(xData=c(rep("Dying projects",length(yearlyFactsOnDyingProjects$projectName)),
#                                rep("Base rate",length(yearlyFacts$projectName))),
#                        yData=c(yearlyFactsOnDyingProjects$sum_commits,
#                                yearlyFacts$sum_commits))
# 
# boxPlot(dataToPlot,
#         "",
#         "Team Activity (TA)",
#         "boxplot-team-activity-dying-projects.pdf",
#         TRUE,1)
# 
# 
# 
# ######### EXAMPLES
# 
# dataSelection = subset(yearlyFacts,subset=projectName %in% c("apache","firefox","emacs","gcc","mysql") & age > 5)
# dataToPlot <- data.frame(xData=dataSelection$projectName,yData=dataSelection$prod_loc_growth_factor)
# boxPlot(dataToPlot,
#         "",
#         "Code Growth indexed (CGi)",
#         "boxplot-codegrowth-indexed-examples.pdf",
#         FALSE,0)
# 
# dataToPlot <- data.frame(xData=dataSelection$projectName,yData=dataSelection$sum_abs_loc_growth)
# boxPlot(dataToPlot,
#         "",
#         "Code Growth absolute (CGa)",
#         "boxplot-codegrowth-absolute-examples.pdf",
#         TRUE,0)
# 
# dataSelection = subset(yearlyFacts,select=c(prod_loc_growth_factor,median_contributors))
# dataToPlot <- data.frame(xData=dataSelection$median_contributors,yData=dataSelection$prod_loc_growth_factor)
# scatterPlot(dataToPlot,
#         "Yearly median contributors",
#         "Code Growth indexed (CGi)",
#         "scatterplot-contributors-codegrowth-indexed.pdf")
# 
# dataSelection = subset(yearlyFacts,select=c(sum_abs_loc_growth,median_contributors))
# dataToPlot <- data.frame(xData=dataSelection$median_contributors,yData=dataSelection$sum_abs_loc_growth)
# scatterPlot(dataToPlot,
#             "Yearly median contributors",
#             "Code Growth absolute (CGa)",
#             "scatterplot-contributors-codegrowth-absolute.pdf")
