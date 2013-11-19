module exportProjectFacts

import Prelude;
import processProjectFacts;
import projectFactsRepository;
import Logging;
import lang::csv::IO;
import util::Maybe;
import IO;
import analyzeProjectDeath;
import analyzeProjectMetaData;
import dataValidation;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;

public void validateAndOutputFacts() {
	exportProjectsMetaData(getProjectNamesInRepository());
	logToConsole("validateAndOutputFacts", "Validating all data in repository on project level...");
	remainingProjects = validateDataOnProjectLevel();
	logToConsole("validateAndOutputFacts", "Obtaining all merged facts form repository...");
	facts = mergeFactsForProjects(remainingProjects);
	writeFactsMapToCSV(facts, validationResultsDir + "monthlyFactsWithProperEnlistments.csv");
}

public void exportProjectsMetaData(list[str] projects) {
	logToConsole("exportProjectsMetaData", "Exporting meta data on all projects in repository...");
	rel[str project_name_fact, str main_language_fact] mainLanguages = getMetaDataElements(projects, "main_language_name", "");
	rel[str project_name_fact, str update_date_fact] updateDate = getMetaDataElements(projects, "updated_at", "analysis");
	repositoriesRel repos = getRepositoryFactsForProjects(projects);
	writeCSV(repos,validationResultsDir + "projectsRepositories.csv");
	writeCSV(mainLanguages,validationResultsDir + "projectsMainLanguages.csv");
	writeCSV(updateDate,validationResultsDir + "projectsUpdateDate.csv");
}

public void exportRepositoriesCount(list[str] projects, str fileName) {
	rel[str repoType, int count] repositoriesCount = getRepositoriesCount(projects);
	writeCSV(repositoriesCount,validationResultsDir + fileName);
}

public void writeValueToFile(v, str fileName) {
	writeFile(OutputFilesDirectory + fileName, v);
}

public void writeFactsMapToCSV (factsMap facts, loc outFile) {
	writeFactsMapToCSV(facts,outFile,identificationFactKeys + activityFactKeys + sizeFactKeys + metaDataFactKeys);
}

public void writeFactsMapToCSV (factsMap facts, loc outFile, list[maybeFactKey] factKeys) {
	separator = ",";
	header = "";
	
	for (factKey <- factKeys) {
		header += (factKey + separator);
	};
	header = substring(header, 0, size(header) - 1) + "\n";	
	writeFile(outFile, header);
	
	for (dataPoint <- facts) {
		line = "";
		factsForDataPoint = facts[dataPoint];
		for (factKey <- factKeys) {
			line += (maybeFactToString(factsForDataPoint[factKey]) + separator);
		};
		line = substring(line, 0, size(line) - 1) + "\n";
		appendToFile(outFile, line);
	};
}

private str maybeFactToString(Maybe[value] mv) {
	switch(mv) {
		case nothing(): return "NA";
		case just(value v): return toString(v);
	};
}
