import pickle
import json
import os

# Open pickled ad id to file id map
ad_id_file_id_map_path = '/Users/drewnleonard/Documents/thesis/data/pickle/ad_id_file_id_map.pickle'
with open(ad_id_file_id_map_path, 'rb') as f:
	ad_id_file_id_map = pickle.load(f)

# Open pickled file id to ad number map
file_id_ad_n_map_path = '/Users/drewnleonard/Documents/thesis/data/pickle/ad_id_map.pickle'
with open(file_id_ad_n_map_path, 'rb') as f:
	file_id_ad_n_map = pickle.load(f)

file_id_ad_n_values = file_id_ad_n_map.values()

image_path = '/Users/drewnleonard/Documents/thesis/data/png/png_small_trimmed'
for file in os.listdir(image_path):
	
	ad_n = filter(str.isdigit, file)

	# Check if ad n has associated file id
	if ad_n not in file_id_ad_n_values:
		print "Ad #{} has no associated file id".format(ad_n)

