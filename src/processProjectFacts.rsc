module processProjectFacts

import Prelude;
import projectFactsRepository;
import util::Math;
import ValueIO;
import Logging;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;
public loc CachedOhlohFactsRelLoc = OutputFilesDirectory + "OhlohFactsRel.txt";

alias monthlyFactsRel =
		rel[str projectName,
		    str year,
		    str month,
		    int loc_added,
		    int loc_deleted,
		    int commits,
		    int contributors,
		    int loc_total,
		    int abs_loc_growth,
		    real loc_growth_factor];

alias yearlyFactsRel = 
		rel[str projectName,
		    str year,
		    str month,
		    int loc_added,
		    int loc_deleted,
		    int commits,
		    int loc_total,
		    int abs_loc_growth,
		    real loc_growth_factor,
		    int age];
		    
public OhlohFactsRel getOhlohFactsRelFromCache() {
	if(exists(CachedOhlohFactsRelLoc)) {
		OhlohFactsRel f = readTextValueFile(#OhlohFactsRel, CachedOhlohFactsRelLoc);
		return (f);
	}
	else {
		return updateOhlohFactsRelInCache();
	}
}

public OhlohFactsRel updateOhlohFactsRelInCache() {
	logToConsole("updateFactsRelInCache", "Updating Ohloh facts relation cache from repository...");
	OhlohFactsRel newFactsRel = getOhlohFactsRelForAllProjects();
	writeTextValueFile(CachedOhlohFactsRelLoc, newFactsRel);
	return newFactsRel;
}

public monthlyFactsRel getMonthlyFacts(OhlohFactsRel facts) {
	factsMap = (
		<projectName,year,month> : monthlyFacts |
		monthlyFacts <- facts,
		<str projectName,str year,str month,_,_,_,_,_> := monthlyFacts
	);

	return {
		<projectName, year, month, loc_added, loc_deleted, commits, contributors, locThisMonth,
			locThisMonth - locPreviousMonth, 
			1.0 + toReal(locThisMonth - locPreviousMonth) / toReal(locPreviousMonth)> |
			
		<str projectName,str year, str month> <- factsMap,
		<projectName,year,month,int loc_added,int loc_deleted,int commits,int contributors,int locThisMonth> :=
		 	factsMap[<projectName, year, month>],
		datetime previousYearMonth := decrementMonths(createDateTime(toInt(year,10),toInt(month,10),1,0,0,0,0)),
		str previousYear := printDateTime(previousYearMonth,"yyyy"),
		str previousMonth := printDateTime(previousYearMonth,"MM"),
		<projectName,previousYear,previousMonth> in factsMap,
		<_,_,_,_,_,_,_,int locPreviousMonth> := factsMap[<projectName, previousYear, previousMonth>]
	};
}

public yearlyFactsRel getMonthlyFactsGroupedByYear(monthlyFactsRel monthlyFacts)
{
	monthlyFactsMap = (
		<projectName,year,month> : thisMonthlyFacts |
		thisMonthlyFacts <- monthlyFacts,
		<str projectName,str year,str month,_,_,_,_,_,_,_> := thisMonthlyFacts
	);

	months = monthlyFacts<month>;

	return {
		<projectName, year, "12",loc_added,loc_deleted,commits,loc_total,
					abs_loc_growth,loc_growth_factor,age> |
					
		yearsPerProject := monthlyFacts<projectName,year>,
		<str projectName,str year> <- yearsPerProject,
		 			
		monthlyFactsForProjectInYear := [
			<month,monthlyLocAdded,monthlyLocDeleted,monthlyCommits,monthlyLocTotal,
			 monthlyAbsoluteGrowth,monthlyGrowthFactor> |

			str month <- months,
		 	<projectName,year,month> in monthlyFactsMap,
		 	<_,_,_,int monthlyLocAdded,int monthlyLocDeleted,int monthlyCommits,
		 	 _,int monthlyLocTotal,int monthlyAbsoluteGrowth,real monthlyGrowthFactor>
		 	 := monthlyFactsMap[<projectName,year,month>]
		 ],
		 
		 int loc_added := sum([monthlyLocAdded | <_,int monthlyLocAdded,_,_,_,_,_> <- monthlyFactsForProjectInYear]),
		 int loc_deleted := sum([monthlyLocDeleted | <_,_,int monthlyLocDeleted,_,_,_,_> <- monthlyFactsForProjectInYear]),
		 int commits := sum([monthlyCommits | <_,_,_,monthlyCommits,_,_,_> <- monthlyFactsForProjectInYear]),
		 <"12",_,_,_,int loc_total,_,_> <- monthlyFactsForProjectInYear,
		 int abs_loc_growth := sum([monthlyAbsoluteGrowth | <_,_,_,_,_,int monthlyAbsoluteGrowth,_> <- monthlyFactsForProjectInYear]),
		 real loc_growth_factor := product([monthlyGrowthFactor | <_,_,_,_,_,_,real monthlyGrowthFactor> <- monthlyFactsForProjectInYear]),
		 int age := toInt(year) - min([ toInt(year) | str year <- yearsPerProject[projectName]])
	};  
}

private num product (list[num] listOfNumbers) {
	num result = 1;
	for (num number <- listOfNumbers) {
		result = result * number;
	}
	return result;
}


