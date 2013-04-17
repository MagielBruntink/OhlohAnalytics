module analysis::AnalyzeProjectDeath

import processProjectFacts;

public rel[str projectName, str year] findDeadProjects () {
	monthlyFactsByYear = getYearlyFactsRelFromCache(
							getMonthlyFactsRelFromCache(
								getOhlohFactsRelFromCache()));
								
	return {
		<projectName, year> |
			 <str projectName, str year, _, _, 
			 int sum_commits,
			 _, _, _, _, _> <- monthlyFactsByYear,
			 sum_commits := 0
	};
}
