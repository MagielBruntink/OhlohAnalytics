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
  monthlyFactsBeforeCleaningCasesAnnotated,
  monthlyFacts
), id.vars=c("main_language_fact"), measure.vars=c("loc_added_fact"), value.name = "loc_added_fact")

plotObj <- ggplot(dataToPlot, aes(x=reorder(main_language_fact,L1,function(x) -length(x)),
                                  ,y=loc_added_fact,fill=factor(L1),dodge=L1))
plotObj <- plotObj + geom_boxplot()
plotObj <- plotObj + labs(x="",y="LOC Added per month")
plotObj <- plotObj + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
plotObj <- plotObj + coord_cartesian(xlim = c(0.5,10.5),ylim = c(0,4500))
plotObj <- plotObj + scale_fill_manual(values=c("gray","white"))
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
plotObj <- plotObj + scale_fill_manual(values=c("gray","white"))
plotObj <- plotObj + theme(legend.position = "none") 
ggsave(file=paste(output_dir,"boxplots-commits_fact-per-programming-language-combined.pdf",sep="/"),plot=plotObj)

