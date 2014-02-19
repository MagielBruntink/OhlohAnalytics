module listScientificProjects

import projectFactsRepository;

public rel[str,str] findScientificProjects (rel[str,str] projectTags) {
	
  	return { <projectName, projectTag> | 
           <str projectName, str projectTag> <- projectTags,
           /.*scientific.*/ := projectTag
  };
}
