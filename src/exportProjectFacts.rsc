module exportProjectFacts

import processProjectFacts;
import Logging;
import lang::csv::IO;

public void exportFactsForAllProjects() {
	logToConsole("exportFactsForAllProject", "Getting Ohloh facts for all projects from cache...");
	OhlohFacts=getOhlohFactsRelFromCache();
	//logToConsole("exportFactsForAllProject", "Outputting Ohloh facts for all projects...");
	//writeFactsToCSV(OhlohFacts,"OhlohFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Calculating monthly facts for all projects...");
	monthlyFacts=getMonthlyFacts(OhlohFacts);
	//logToConsole("exportFactsForAllProject", "Outputting monthly facts for all projects...");
	//writeFactsToCSV(monthlyFacts,"monthlyFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Grouping monthly facts grouped by year for all projects...");
	monthlyFactsByYear=getMonthlyFactsGroupedByYear(monthlyFacts);
	logToConsole("exportFactsForAllProject", "Outputting grouped monthly facts grouped by year for all projects...");
	writeFactsToCSV(monthlyFactsByYear,"monthlyFactsByYear.csv");	
}

public void writeFactsToCSV(facts,str fileName) {
	writeCSV(facts,
			 OutputFilesDirectory + fileName,
			 ("separator" : ","));
}
