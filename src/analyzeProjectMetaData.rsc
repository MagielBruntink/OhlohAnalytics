module analyzeProjectMetaData

import Prelude;
import projectFactsRepository;
import analysis::formalconcepts::FCA;
import lang::dot::Dot;

public void analyzeLanguages () {
	try
		langs=getMetaDataElements(getProjectNamesInRepository(),
							 	  "main_language_name");
		
	catch : ;
}

public str generateOALForTags(list[str] projectNames) {
	tags = getMetaDataElements(projectNames, "tag");
	return generateOAL(tags);
}

public str generateOAL(rel[str,str] R) {
	result = "";
	for (str object <- domain(R)) {
		result += object;
		result += ":";
		for (str attribute <- R[object]) {
			result += attribute;
			result += ";";
		};
		result += "\n";
	};
	return result;
}