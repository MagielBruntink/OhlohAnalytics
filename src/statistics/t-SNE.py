#! /usr/bin/env python


import csv
from calc_tsne import *

csvFile = recfromtxt('/Users/magielbruntink/git/OhlohAnalytics/analysis/esamir/allYearlyFacts.csv', delimiter=',')
dataMat = csvFile[1:100,5:9].astype(float)
result = calc_tsne(dataMat,LANDMARKS=0.1)

with open('../../output/tSNE-result.csv', 'wb') as f:
    writer = csv.writer(f)
    writer.writerows(result[0])

with open('../../output/tSNE-landmarks.txt', 'wb') as f:
    for t in result[1] :
        f.write('%s\n' % t)
