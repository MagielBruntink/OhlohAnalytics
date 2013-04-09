module processProjectFacts

import Prelude;
import projectFactsRepository;
import util::Math;
import ValueIO;
import Logging;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;
public loc CachedFactsRelLoc = OutputFilesDirectory + "FactsRel.txt";

alias growthFactsRel =
		rel[str projectName,
		    datetime yearMonth,
		    str year,
		    str month,
		    int abs_loc_growth,
		    real loc_growth_factor];

public factsRel getFactsRelFromCache() {
	if(exists(CachedFactsRelLoc)) {
		factsRel f = readTextValueFile(#factsRel, CachedFactsRelLoc);
		return (f);
	}
	else {
		return updateFactsRelInCache();
	}
}

public factsRel updateFactsRelInCache() {
	logToConsole("updateFactsRelInCache", "Updating facts relation cache from repository...");
	factsRel newFactsRel = getFactsRelForAllProjects();
	writeTextValueFile(CachedFactsRelLoc, newFactsRel);
	return newFactsRel;
}

public growthFactsRel getMonthlyGrowthFacts(factsRel facts) {
	factsMap = (
		<projectName,yearMonth> : loc_total | 
		<str projectName,datetime yearMonth,_,_,_,_,_,_,int loc_total> <- facts
	);

	return {
		<projectName, thisYearMonth, printDateTime(thisYearMonth,"yyyy"),
									 printDateTime(thisYearMonth,"MM"),
									 locThisMonth - locPreviousMonth, 
									 1.0 + toReal(locThisMonth - locPreviousMonth) / toReal(locPreviousMonth)> |
		<str projectName,datetime thisYearMonth> <- factsMap,
		int locThisMonth := factsMap[<projectName, thisYearMonth>],
		datetime previousMonth := decrementMonths(thisYearMonth),
		<projectName, previousMonth> in factsMap,
		int locPreviousMonth := factsMap[<projectName, previousMonth>]
	};
}

public rel[str projectName,
		    datetime yearMonth,
		    str year,
		    str month,
		    int abs_loc_growth,
		    real loc_growth_factor,
		    int age]
getMonthlyGrowthFactsByYear(growthFactsRel monthlyGrowthFacts)
{
	monthlyGrowthFactsMap = (
		<projectName,year,month> : <monthlyAbsoluteGrowth, monthlyGrowthFactor> |
		<str projectName,
		 datetime yearMonth,
		 str year,
		 str month,
		 int monthlyAbsoluteGrowth,
		 real monthlyGrowthFactor> <- monthlyGrowthFacts
	);
	
	set[str] months = monthlyGrowthFacts<month>;

	return {
		<projectName, createDateTime(toInt(year),1,1,0,0,0,0), year, "01", 
					  toInt(sum(monthlyAbsoluteGrowthList)),
					  toReal(product(monthlyGrowthFactorList)),
					  toInt(year) - min(years)> |
		yearsPerProject := monthlyGrowthFacts<projectName,year>,
		<str projectName,str year> <- yearsPerProject,
		list[int] years := [ toInt(year) | str year <- yearsPerProject[projectName] ],
		list[int] monthlyAbsoluteGrowthList := [monthlyAbsoluteGrowth |
			str month <- months,
			<projectName,year,month> in monthlyGrowthFactsMap,
	    	<int monthlyAbsoluteGrowth,_> := monthlyGrowthFactsMap[<projectName,year,month>]],
	    list[real] monthlyGrowthFactorList := [monthlyGrowthFactor |
	    	str month <- months,
	    	<projectName,year,month> in monthlyGrowthFactsMap,
	    	<_,real monthlyGrowthFactor> := monthlyGrowthFactsMap[<projectName,year,month>]]
	};  
}

private num product (list[num] listOfNumbers) {
	num result = 1;
	for (num number <- listOfNumbers) {
		result = result * number;
	}
	return result;
}


