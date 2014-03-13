require(data.table)
require(plyr)
require(ggplot2)

obtainCAGRs <- function(data_dt) {
  result_dt <- data_dt[,list(CAGR_Years = nrow(.SD),
                                 LOC_First = .SD[1]$max_loc_fact,
                                 LOC_Last = .SD[nrow(.SD)]$max_loc_fact),
                                 by=project_name_fact]
  result_dt[,CAGR := (LOC_Last / LOC_First) ^ (1 / CAGR_Years),
                by=project_name_fact]  
}

diffIndexedGrowthAndCAGR <- function (data_dt) {
  CAGRs <- obtainCAGRs(data_dt)
  dataWithCAGRs <- data_dt[subset(CAGRs,select=c(project_name_fact, CAGR, CAGR_Years))]
  dataWithCAGRs[,Diff_Indexed_Growth_And_CAGR := CAGR - prod_ind_loc_growth]
}

summariseDiffs <- function (data_dt) {
  ddply(subset(data_dt,subset=CAGR_Years>1),
        .(age_in_years),
        summarise,
        Min_Diff=min(Diff_Indexed_Growth_And_CAGR),
        Max_Diff=max(Diff_Indexed_Growth_And_CAGR),
        Mean_Diff=mean(Diff_Indexed_Growth_And_CAGR),
        Median_Diff=median(Diff_Indexed_Growth_And_CAGR),
        SD_Diff=sd(Diff_Indexed_Growth_And_CAGR))
}

plotDiffForProject <- function (data_dt, Project_ID_str) {

  dataToPlot <- subset(rbind(data_dt[,list(Project_ID = project_name_fact, Year_Number = age_in_years, Factor=CAGR, Type="CAGR")],
                             data_dt[,list(Project_ID = project_name_fact, Year_Number = age_in_years, Factor=prod_ind_loc_growth,Type="LOC Growth Indexed")]),
                       subset=Project_ID==Project_ID_str)
  
  plotObj <- ggplot(dataToPlot,
         aes(x=Year_Number,
             y=Factor,
             group=Type,
             colour=Type))
  plotObj <- plotObj + geom_line() + geom_point()
  show(plotObj)

}

plotMedianDiff <- function (data_dt) {
  
  dataToPlot <- rbind(data_dt[,list(Factor=median(CAGR), Type="Median CAGR"),by=age_in_years],
                             data_dt[,list(Factor=median(prod_ind_loc_growth),Type="Median LOC Growth Indexed"),by=age_in_years])
  
  plotObj <- ggplot(dataToPlot,
                    aes(x=age_in_years,
                        y=Factor,
                        group=Type,
                        colour=Type))
  plotObj <- plotObj + geom_line() + geom_point()
  show(plotObj)
  
}
