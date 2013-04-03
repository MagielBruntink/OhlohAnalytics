module processProjectFacts

import Prelude;
import projectFactsRepository;
import lang::csv::IO;
import util::Math;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;

alias mergedFactsMap = 
		map[str, mergedFactsTuple];

alias mergedFactsTuple = 
				 tuple[str projectName,
		               str year,
		               str month,
		               str loc_added,
		               str loc_deleted,
		               str commits,
		               str contributors,
		               str loc_total];

alias factsRel =
		rel[str projectName,
		    datetime yearMonth,
		    str year,
		    str month,
		    int loc_added,
		    int loc_deleted,
		    int commits,
		    int contributors,
		    int loc_total];
		    
public mergedFactsMap mergeFactsForProjects (list[str] projectNames) {
     return (key : <projectName,year,month,loc_added,loc_deleted,commits,contributors,loc_total> |
                   str projectName <- projectNames,
                   activityFactsMap activityFacts <- [getActivityFacts(projectName)],
                   sizeFactsMap sizeFacts <- [getSizeFacts(projectName)],
                   str key <- activityFacts,
                   key in sizeFacts,
                   <_, str year, str month, str loc_added, str loc_deleted,str commits, str contributors> <- [activityFacts[key]],
                   <_, year, month, str loc_total> <- [sizeFacts[key]]
    );
}

public mergedFactsMap mergeFactsForAllProjects () { 
	return mergeFactsForProjects(getProjectNamesInRepository());
}

public factsRel convertFactsMapToRel(mergedFactsMap factsMap) { 
	return {
		<projectName,
			 parseDateTime(year + "-" + month,"yyyy-MM"),
			 year,
			 month,
			 toInt(loc_added),
			 toInt(loc_deleted),
			 toInt(commits),
			 toInt(contributors),
			 toInt(loc_total)> |
		str key <- factsMap,
		<str projectName,
		 str year,
		 str month,
		 str loc_added,
		 str loc_deleted,
		 str commits,
		 str contributors,
		 str loc_total> <- [factsMap[key]]
	};
}

public rel[str,datetime,int,real] getGrowthFacts(factsRel facts) {
	return {
		<projectName, thisYearMonth> + <locThisMonth - locPreviousMonth, 
										1.0 + toReal(locThisMonth - locPreviousMonth) / toReal(locPreviousMonth)> |
		monthlyLOCFacts := facts<projectName,yearMonth,loc_total>,
		str projectName <- monthlyLOCFacts<projectName>,								
		monthlyLOCFactsForProject := monthlyLOCFacts[projectName],
		<datetime thisYearMonth,int locThisMonth> <- monthlyLOCFactsForProject,
		int locPreviousMonth <- monthlyLOCFactsForProject[decrementMonths(thisYearMonth)]
	};
}

public void generateCSVForAllFacts() {

	writeCSV(convertFactsMapToRel(mergeFactsForAllProjects()),
			 OutputFilesDirectory + "AllMergedFacts.csv",
			 ("separator" : ","));
}
