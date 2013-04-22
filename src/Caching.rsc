module Caching

import Prelude;
import ValueIO;
import Logging;

public loc CacheDirectory = |project://OhlohAnalytics/cache|;

public &T getValueFromCache(str CachedValueName, type[&T] CachedObjectType, &T () getUpdatedValue) {
	if(exists(CacheDirectory + CachedValueName)) {
		&T f = readTextValueFile(CachedObjectType, CacheDirectory + CachedValueName);
		return f;
	}
	else {
		logToConsole("getValueFromCache", "Updating value in cache for: " + CachedValueName);
		&T newValue = getUpdatedValue();
		writeTextValueFile(CacheDirectory + CachedValueName, newValue);
		return newValue;
	}
}
