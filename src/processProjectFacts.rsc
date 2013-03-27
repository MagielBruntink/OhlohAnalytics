module processProjectFacts

import Prelude;
import projectFactsRepository;
import lang::csv::IO;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;

//public rel [str project,
//		str year,
//	    str month,
//	    str yearMonth,
//		str loc_added,
//		str loc_removed,
//		str commits,
//		str contributors,
//		str total_loc]
//
//mergeFactsForProjects (list[str] projectNames) 
//{
//    return {
//        <projectName, year, month, year+"-"+month,loc_added,loc_removed,commits,contributors,total_loc> |
//        str projectName <- projectNames,
//        <_, str year, str month, str loc_added, str loc_removed, str commits, str contributors> <- getActivityFacts(projectName),
//        <_, year, month, str total_loc> <- getSizeFacts(projectName)
//	};
//}

public map[str, tuple[str projectName,
                      str yearMonth,
                      str year,
                      str month,
                      str loc_added,
                      str loc_deleted,
                      str commits,
                      str contributors,
                      str loc_total]]

mergeFactsForProjects (list[str] projectNames) 
{
     return (key : <projectName,yearMonth,year,month,loc_added,loc_deleted,commits,contributors,loc_total> |
                   projectName <- projectNames,
                   activityFacts <- [getActivityFacts(projectName)],
                   sizeFacts <- [getSizeFacts(projectName)],
                   key <- activityFacts,
                   key in sizeFacts,
                   <_, str yearMonth, str year, str month, str loc_added, str loc_deleted,str commits, str contributors> <- [activityFacts[key]],
                   <_, yearMonth, year, month, str loc_total> <- [sizeFacts[key]]
    );
}

public map[str, tuple[str projectName,
                      str yearMonth,
                      str year,
                      str month,
                      str loc_added,
                      str loc_deleted,
                      str commits,
                      str contributors,
                      str loc_total]]

mergeFactsForAllProjects () 
{
	return mergeFactsForProjects(getProjectNamesInRepository());
}

public rel[str projectName,
           str yearMonth,
           str year,
           str month,
           str loc_added,
           str loc_deleted,
           str commits,
           str contributors,
           str loc_total]
           
mergedFactsMapToRel(mergedFactsMap)
{ 
	return {
		fact |
		mergedFacts <- [mergedFactsMap],
		key <- mergedFacts,
		fact <- [mergedFacts[key]]
	};
}

public void generateCSVForAllMergedFacts() {

	writeCSV(mergedFactsMapToRel(mergeFactsForAllProjects()),
			 OutputFilesDirectory + "AllMergedFacts.csv",
			 ("separator" : ","));
}
