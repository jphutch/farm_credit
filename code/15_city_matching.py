#!/usr/bin/env python

# This script matches the PCA cities I scraped manually from the map (see the "supplements" directory) to a dataset of actia; {CA cotoes/}

import pandas as pd
import numpy as np
import os

with open("data_dir.txt") as f:
    data_dir = f.read()
    
city_list = pd.read_csv(data_dir+'spatial_data/us_cities.csv')
pca_list = pd.read_csv(data_dir+'pca_data/pca_cities.txt')

# Make the ID number
# Strip the white space for name and make lower case
city_list['NAME'] = city_list['NAME'].map(lambda x: x.replace(" ","").replace("'","").replace(".","").lower())
city_list['SFIPS'] = city_list['SFIPS'].map(lambda x: str(x).zfill(2))
city_list['CFIPS'] = city_list['CFIPS'].map(lambda x: str(x).zfill(3))
city_list['cityid'] = city_list['SFIPS'] + city_list['CFIPS'] + "_" + city_list['NAME'].map(lambda x: str(x[0:4]))

city_list = city_list[(pd.isnull(city_list.PL_FIPS)==False)]

new_list = city_list[city_list.cityid.isin(pca_list.cityid)]

if set(pca_list.cityid) != set(new_list.cityid):
	if len(new_list.cityid) > len(set(pca_list.cityid)):
		print("Duplicates found:")
		dups = set([x for x in list(new_list['cityid']) if list(new_list['cityid']).count(x) > 1])
		print(set([x for x in list(new_list['cityid']) if list(new_list['cityid']).count(x) > 1]))
		for i in dups:
			print(new_list.loc[new_list['cityid'] == i]['NAME'])
			count_d = count_d+len(new_list.loc[new_list['cityid'] == i]['NAME'])
	# Check for unmatched observations
	unmatch = list(set(pca_list.cityid) - set(new_list.cityid))
	count_u = len(unmatch)
	print("Unmatched observations found:")
	print(sorted(unmatch))

print("Unmatched:%s" %count_u)

pca_unmatch = pca_list[pca_list.cityid.isin(unmatch)]