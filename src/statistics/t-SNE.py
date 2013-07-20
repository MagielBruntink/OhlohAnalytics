#! /usr/bin/env python


import csv
from calc_tsne import *

NO_DIMS=2
LANDMARKS=1
PERPLEX=5

csvFile = recfromtxt('/Users/magielbruntink/git/OhlohAnalytics/analysis/tSNE-project-months-multi.csv', delimiter=',')
dataMat = csvFile[1:,1:].astype(float)
result = calc_tsne(dataMat,NO_DIMS=NO_DIMS,PERPLEX=PERPLEX,LANDMARKS=LANDMARKS)

with open('../../output/tSNE-result.csv', 'wb') as f:
    writer = csv.writer(f)
    if LANDMARKS != 1:
        writer.writerows(result[0])
    else:
        writer.writerows(result)

if LANDMARKS != 1:
    with open('../../output/tSNE-landmarks.txt', 'wb') as f:
        for t in result[1] :
            f.write('%s\n' % t)
