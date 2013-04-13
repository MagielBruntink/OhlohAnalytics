module exportProjectFacts

import processProjectFacts;
import Logging;
import lang::csv::IO;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;

public void exportFactsForAllProjects() {
	logToConsole("exportFactsForAllProject", "Getting Ohloh facts for all projects from cache...");
	OhlohFacts=getOhlohFactsRelFromCache();
	//logToConsole("exportFactsForAllProject", "Exporting Ohloh facts to CSV for all projects: " + "OhlohFacts.csv");
	//writeFactsToCSV(OhlohFacts,"OhlohFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Getting monthly facts for all projects from cache...");
	monthlyFacts=getMonthlyFactsRelFromCache(OhlohFacts);
	//logToConsole("exportFactsForAllProject", "Exporting monthly facts to CSV for all projects: " + "monthlyFacts.csv");
	//writeFactsToCSV(monthlyFacts,"monthlyFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Grouping monthly facts by year for all projects...");
	monthlyFactsByYear=getMonthlyFactsGroupedByYear(monthlyFacts);
	logToConsole("exportFactsForAllProject", "Exporting monthly facts grouped by year to CSV for all projects: " + "monthlyFactsByYear.csv");
	writeFactsToCSV(monthlyFactsByYear,"monthlyFactsByYear.csv");
}

public void writeFactsToCSV(facts,str fileName) {
	writeCSV(facts,
			 OutputFilesDirectory + fileName,
			 ("separator" : ","));
}
