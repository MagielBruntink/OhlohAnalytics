module analyzeProjectMetaData

import Prelude;
import projectFactsRepository;
import analysis::formalconcepts::FCA;
import lang::dot::Dot;

public str generateOALForTags(list[str] projectNames) {
	tags = getMetaDataStringAttributes(projectNames, "tag")<0,2>;
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