module AnalyzeProjectDeath

import processProjectFacts;

public rel[str projectName, str year] findProjectDeathEvents (monthlyFactsMap monthlyFactsByYear) {
							
	return {
		<projectName, year> |
			 <str projectName, str year, str month> <- monthlyFactsByYear,  
			 sum_commits_fact(num sum_commits) <- monthlyFactsByYear[<projectName, year, month>],
			 sum_commits := 0
	};
}

