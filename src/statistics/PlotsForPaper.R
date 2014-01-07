require(ggplot2)
require(survival)
require(plyr)
require(data.table)
require(reshape2)
source("src/statistics/ggsurv.R")

options(scipen=1000)

theme_set(theme_bw(base_size = 24))

analysis_dir <- "validation"
output_dir <- "/Users/magielbruntink/Google Drive/UVA/Research/Writing/Quality of Software Evolution Data on Ohloh-SQM2014"

monthlyFacts <- data.table(read.csv(file=paste(analysis_dir,"monthlyFactsAfterCleaningWithMetaData.csv",sep="/"),header=TRUE,sep=","))
monthlyFactsBeforeCleaningCasesAnnotated <- data.table(read.csv(file=paste(analysis_dir,"monthlyFactsBeforeCleaningCasesAnnotated.csv",sep="/"),header=TRUE,sep=","))
monthlyFacts[,X:=NULL][]
setkey(monthlyFacts,project_name_fact,year_fact,month_fact)
setkey(monthlyFactsBeforeCleaningCasesAnnotated,project_name_fact,year_fact,month_fact)

yearlyFacts <- data.table(read.csv(file=paste(analysis_dir,"yearlyFactsAfterCleaningWithMetaData.csv",sep="/"),header=TRUE,sep=","))
yearlyFacts[,X:=NULL][]
setkey(yearlyFacts,project_name_fact,age_in_years)

projectsMainLanguages <- data.table(read.csv(paste(analysis_dir,"projectsMainLanguages.csv", sep="/")))
setkey(projectsMainLanguages,project_name_fact)

monthlyFactsBeforeCleaningCasesAnnotated <- projectsMainLanguages[monthlyFactsBeforeCleaningCasesAnnotated]
setkey(monthlyFactsBeforeCleaningCasesAnnotated,project_name_fact,year_fact,month_fact)

projectsRepositories <- data.table(read.csv(paste(analysis_dir,"projectsRepositories.csv", sep="/")))
setkey(projectsRepositories,project_name_fact)
projectsRepositories[repository_type=="CvsRepository",repository_type:="CVS"]
projectsRepositories[repository_type=="SvnRepository",repository_type:="SVN"]
projectsRepositories[repository_type=="SvnSyncRepository",repository_type:="SVN sync"]
projectsRepositories[repository_type=="BzrRepository",repository_type:="Bazaar"]
projectsRepositories[repository_type=="GitRepository",repository_type:="Git"]
projectsRepositories[repository_type=="HgRepository",repository_type:="Mercurial"]

projectActivityStatus <- data.table(read.csv(paste(analysis_dir,"projectActivityStatus.csv", sep="/")))

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

######## Projects by language, before cleansing
dataToPlot <- melt(list(data.frame(projectsMainLanguages[,project_name_fact,by=main_language_fact])),
                   id.vars=c("main_language_fact"), measure.vars=c("project_name_fact"), value.name = "project_name_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  fill=factor(L1)))
plotObj <- plotObj + geom_bar(stat="bin",position="dodge",colour="black")
plotObj <- plotObj + labs(x="Main programming language",y="Number of projects")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5))
plotObj <- plotObj + scale_fill_manual(values=c("white","gray"))
plotObj <- plotObj + theme(legend.position = "none") 
ggsave(file=paste(output_dir,"barchart-projects-by-main-language-before-cleansing.pdf",sep="/"),plot=plotObj)

######## Projects by language, before and after cleansing
dataToPlot <- melt(list(
  data.frame(projectsMainLanguages[,project_name_fact,by=main_language_fact]),
  subset(data.frame(projectsMainLanguages[,project_name_fact,by=main_language_fact]),
         subset=project_name_fact %in% monthlyFacts$project_name_fact)),
                   id.vars=c("main_language_fact"), measure.vars=c("project_name_fact"), value.name = "project_name_fact")

dataToPlot$L1 <- factor(dataToPlot$L1,levels=c(1,2),labels=c("Before cleansing","After cleansing"))

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  fill=factor(L1)))
plotObj <- plotObj + geom_bar(stat="bin",position="dodge",colour="black")
plotObj <- plotObj + labs(x="Main programming language",y="Number of projects")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5))
plotObj <- plotObj + scale_fill_manual(values=c("white","gray"),name="")
plotObj <- plotObj + theme(legend.position = "top")
ggsave(file=paste(output_dir,"barchart-projects-by-main-language-before-after-cleansing.pdf",sep="/"),plot=plotObj)

######## Repositories count by type, before cleansing
dataToPlot <- melt(list(data.frame(projectsRepositories[,project_name_fact,by=repository_type])),
                   id.vars=c("repository_type"), measure.vars=c("project_name_fact"), value.name = "project_name_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(repository_type,L1,function(x) -length(x)),
                                  fill=factor(L1)))
plotObj <- plotObj + geom_bar(stat="bin",position="dodge",colour="black")
plotObj <- plotObj + labs(x="Repository type",y="Number of projects using repository type")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,6.5))
plotObj <- plotObj + scale_fill_manual(values=c("white","gray"))
plotObj <- plotObj + theme(legend.position = "none") 
ggsave(file=paste(output_dir,"barchart-repository-types-usage-before-cleansing.pdf",sep="/"),plot=plotObj)

######## Repositories count by type, before and after cleansing
dataToPlot <- melt(list(
  data.frame(projectsRepositories[,project_name_fact,by=repository_type]),
  subset(data.frame(projectsRepositories[,project_name_fact,by=repository_type]),
         subset=project_name_fact %in% monthlyFacts$project_name_fact)),
                    id.vars=c("repository_type"), measure.vars=c("project_name_fact"), value.name = "project_name_fact")

dataToPlot$L1 <- factor(dataToPlot$L1,levels=c(1,2),labels=c("Before cleansing","After cleansing"))

plotObj <- ggplot(dataToPlot, aes(x=reorder(repository_type,L1,function(x) -length(x)),
                                  fill=factor(L1)))
plotObj <- plotObj + geom_bar(stat="bin",position="dodge",colour="black")
plotObj <- plotObj + labs(x="Repository type",y="Number of projects using repository type")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,6.5))
plotObj <- plotObj + scale_fill_manual(values=c("white","gray"),name="")
plotObj <- plotObj + theme(legend.position = "top") 
ggsave(file=paste(output_dir,"barchart-repository-types-usage-before-after-cleansing.pdf",sep="/"),plot=plotObj)

######## Cases per language before and after cleaning combined
dataToPlot <- melt(list(
  na.omit(data.frame(monthlyFactsBeforeCleaningCasesAnnotated[,month_fact,by=main_language_fact])),
  data.frame(monthlyFacts[,month_fact,by=main_language_fact])),
                   id.vars=c("main_language_fact"), measure.vars=c("month_fact"), value.name = "month_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  fill=factor(L1)))
plotObj <- plotObj + geom_bar(stat="bin",position="dodge",colour="black")
plotObj <- plotObj + labs(x="",y="Number of cases (months)")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5))
plotObj <- plotObj + scale_fill_manual(values=c("gray","white"))
plotObj <- plotObj + theme(legend.position = "none") 
ggsave(file=paste(output_dir,"barchart-months-per-main-language-combined.pdf",sep="/"),plot=plotObj)


######## LOC Added per month before and after cleaning combined

dataToPlot <- melt(list(
  #monthlyFactsBeforeCleaningCasesAnnotated,
  monthlyFacts
), id.vars=c("main_language_fact"), measure.vars=c("loc_added_fact"), value.name = "loc_added_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  ,y=loc_added_fact,fill=factor(L1),dodge=L1))
plotObj <- plotObj + geom_boxplot()
plotObj <- plotObj + labs(x="",y="LOC Added per month")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5),ylim = c(-2500,5000))
plotObj <- plotObj + scale_fill_manual(values=c("gray","white"))
plotObj <- plotObj + theme(legend.position = "none") 
ggsave(file=paste(output_dir,"boxplots-loc-added-per-programming-language-combined.pdf",sep="/"),plot=plotObj)

######## Commits per month before and after cleaning combined

dataToPlot <- melt(list(
  #monthlyFactsBeforeCleaningCasesAnnotated,
  monthlyFacts
), id.vars=c("main_language_fact"), measure.vars=c("commits_fact"), value.name = "commits_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  ,y=commits_fact,fill=factor(L1),dodge=L1))
plotObj <- plotObj + geom_boxplot()
plotObj <- plotObj + labs(x="",y="Commits per month")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5),ylim = c(0,75))
plotObj <- plotObj + scale_fill_manual(values=c("gray","white"))
plotObj <- plotObj + theme(legend.position = "none") 
ggsave(file=paste(output_dir,"boxplots-commits_fact-per-programming-language-combined.pdf",sep="/"),plot=plotObj)

######## Yearly growth
dataToPlot <- melt(list(
  "1" = subset(yearlyFacts, subset = age_in_years == 1 & 
                               nr_months_in_year == 12),
  "2" = subset(yearlyFacts, subset = age_in_years == 2 & 
                               nr_months_in_year == 12),
  "3" = subset(yearlyFacts, subset = age_in_years == 3 & 
                               nr_months_in_year == 12)
), id.vars=c("main_language_fact"), measure.vars=c("prod_ind_loc_growth"), value.name = "prod_ind_loc_growth")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  ,y=prod_ind_loc_growth,fill=factor(L1),dodge=L1))
plotObj <- plotObj + geom_boxplot()
plotObj <- plotObj + labs(x="The 10 most used main programming languages in the data set",y="Yearly Code Growth Index")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5),ylim = c(0.9,2.0))
plotObj <- plotObj + scale_fill_manual(values=c("white","lightgray","darkgray"))
plotObj <- plotObj + theme(legend.position = "top")
plotObj <- plotObj + labs(fill = "Years of age")
ggsave(file=paste(output_dir,"boxplots-yearly-growth-per-programming-language.pdf",sep="/"),plot=plotObj)

######## Size
dataToPlot <- melt(list(
  "1" = subset(yearlyFacts, subset = age_in_years == 1 & 
                 nr_months_in_year == 12),
  "2" = subset(yearlyFacts, subset = age_in_years == 2 & 
                 nr_months_in_year == 12),
  "3" = subset(yearlyFacts, subset = age_in_years == 3 & 
                 nr_months_in_year == 12)
), id.vars=c("main_language_fact"), measure.vars=c("max_loc_fact"), value.name = "max_loc_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  ,y=max_loc_fact,fill=factor(L1),dodge=L1))
plotObj <- plotObj + geom_boxplot()
plotObj <- plotObj + labs(x="The 10 most used main programming languages in the data set",y="Max size in a year")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5))
plotObj <- plotObj + scale_y_log10()
plotObj <- plotObj + scale_fill_manual(values=c("white","lightgray","darkgray"))
plotObj <- plotObj + theme(legend.position = "top")
plotObj <- plotObj + labs(fill = "Years of age")
ggsave(file=paste(output_dir,"boxplots-yearly-size-per-programming-language.pdf",sep="/"),plot=plotObj)


######## Project inactivity

#dataToPlot <- subset(projectActivityStatus, subset=main_language_fact %in% c("Java","C","C++","Python","PHP"))
survivalCurve <- with(projectActivityStatus,
                      survfit(Surv(yearOfEvent,status,type="right") ~1))
plotObj <- ggsurv(survivalCurve)
plotObj <- plotObj + ylim(0,1)
plotObj <- plotObj + ggtitle(paste("Kaplan-Meier estimate for",
                                   length(projectActivityStatus$project_name_fact),"projects",sep=" ")) +
                     theme(plot.title = element_text(size=16))
plotObj <- plotObj + labs(x="Age of project in years",y="Probability of Continued Activity")
ggsave(file=paste(output_dir,"survival-curve-activity.pdf",sep="/"),plot=plotObj)
