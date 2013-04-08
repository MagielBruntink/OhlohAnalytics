module processProjectFacts

import Prelude;
import projectFactsRepository;
import util::Math;

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

alias growthFactsRel =
		rel[str projectName,
		    datetime yearMonth,
		    str year,
		    str month,
		    int abs_loc_growth,
		    real loc_growth_factor];
		    
public mergedFactsMap mergeFactsForProjects (list[str] projectNames) {
     return (key : <projectName,year,month,loc_added,loc_deleted,commits,contributors,loc_total> |
                   str projectName <- projectNames,
                   activityFactsMap activityFacts := getActivityFacts(projectName),
                   sizeFactsMap sizeFacts := getSizeFacts(projectName),
                   str key <- activityFacts,
                   key in sizeFacts,
                   <_, str year, str month, str loc_added, str loc_deleted,str commits, str contributors> := activityFacts[key],
                   <_, year, month, str loc_total> := sizeFacts[key]
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

public growthFactsRel getMonthlyGrowthFacts(factsRel facts) {
	return {
		<projectName, thisYearMonth, year, month> +
										<locThisMonth - locPreviousMonth, 
										1.0 + toReal(locThisMonth - locPreviousMonth) / toReal(locPreviousMonth)> |
		monthlyLOCFacts := facts<projectName,yearMonth,year,month,loc_total>,
		str projectName <- monthlyLOCFacts<projectName>,								
		monthlyLOCFactsForProject := monthlyLOCFacts[projectName],
		<datetime thisYearMonth,str year,str month,int locThisMonth> <- monthlyLOCFactsForProject,
		<_,_,int locPreviousMonth> <- monthlyLOCFactsForProject[decrementMonths(thisYearMonth)]
	};
}

public growthFactsRel getMonthlyGrowthFactsByYear(growthFactsRel monthlyGrowthFacts) {
	monthlyGrowthFactsMap = (
		<projectName,year> : (<year,month> : (month : <monthlyAbsoluteGrowth, monthlyGrowthFactor>)) |
		<str projectName,
		 _,
		 str year,
		 str month,
		 int monthlyAbsoluteGrowth,
		 real monthlyGrowthFactor> <- monthlyGrowthFacts
	);
	
	return {
		<projectName, createDateTime(toInt(year),1,1,0,0,0,0), year, "01", 
					  toInt(sum(monthlyAbsoluteGrowthList)),
					  toReal(product(monthlyGrowthFactorList))> |
		<str projectName,str year> <- monthlyGrowthFactsMap,
		monthlyGrowthFactsMapForProject := monthlyGrowthFactsMap[<projectName,year>],
		<str year, str month> <- monthlyGrowthFactsMapForProject,
		monthlyGrowthFactsMapForProjectInYear := monthlyGrowthFactsMapForProject[<year,month>],
		str month <- monthlyGrowthFactsMapForProjectInYear,
	    list[int] monthlyAbsoluteGrowthList := [monthlyAbsoluteGrowth | <int monthlyAbsoluteGrowth,_> := monthlyGrowthFactsMapForProjectInYear[month]],
	    list[real] monthlyGrowthFactorList := [monthlyGrowthFactor | <_,real monthlyGrowthFactor> := monthlyGrowthFactsMapForProjectInYear[month]]		
	};  
}

private num product (list[num] listOfNumbers) {
	num result = 1;
	for (num number <- listOfNumbers) {
		result = result * number;
	}
	return result;
}


