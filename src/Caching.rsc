module Caching

import Prelude;
import ValueIO;
import Logging;

public loc CacheDirectory = |project://OhlohAnalytics/cache|;

public &T getValueFromCache(str CachedValueName, &T () getUpdatedValue) {
	if(exists(CacheDirectory + CachedValueName)) {
		&T f = readTextValueFile(#&T, CacheDirectory + CachedValueName);
		return f;
	}
	else {
		logToConsole("getValueFromCache", "Updating value in cache for: " + CachedValueName);
		&T newValue = getUpdatedValue();
		writeTextValueFile(CacheDirectory + CachedValueName, newValue);
		return newValue;
	}
}

public &T getValueFromCache(str CachedValueName, &T (&S) getUpdatedValue, &S param) {
	if(exists(CacheDirectory + CachedValueName)) {
		&T f = readTextValueFile(#&T, CacheDirectory + CachedValueName);
		return f;
	}
	else {
		logToConsole("getValueFromCache", "Updating value in cache for: " + CachedValueName);
		&T newValue = getUpdatedValue(param);
		writeTextValueFile(CacheDirectory + CachedValueName, newValue);
		return newValue;
	}
}
