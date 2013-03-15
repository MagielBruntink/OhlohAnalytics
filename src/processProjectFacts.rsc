module processProjectFacts

import Prelude;
import projectFactsRepository;
import lang::csv::IO;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;

public rel [str project,
	    str month,
		str loc_added,
		str loc_removed,
		str commits,
		str contributors,
		str total_loc]

mergeFactsForProjects (list[str] projectNames) 
{
	rel[str projectName,
	    str month,
		str loc_added,
		str loc_removed,
		str commits,
		str contributors,
		str total_loc] mergedFacts = {};
	
	for(str projectName <- projectNames) {
		activityFacts = getActivityFacts(projectName);
		sizeFacts = getSizeFacts(projectName);
		mergedFacts += {<projectName, month> + activityFact + <sizeFact> | 
						<str projectName,str month> <- activityFacts<projectName,month> +
													   sizeFacts<projectName,month>,
	                    activityFact <- activityFacts[projectName,month],
	                    sizeFact <- sizeFacts[projectName,month]};
	}
	return mergedFacts;
}

public rel [str project,
	    str month,
		str loc_added,
		str loc_removed,
		str commits,
		str contributors,
		str total_loc]

mergeFactsForAllProjects () 
{
	return mergeFactsForProjects(getProjectNamesInRepository());
}

public void generateCSVForAllMergedFacts() {
	writeCSV(mergeFactsForAllProjects(),
			 OutputFilesDirectory + "AllMergedFacts.csv",
			 ("header" : "true", "separator" : ","));
}
