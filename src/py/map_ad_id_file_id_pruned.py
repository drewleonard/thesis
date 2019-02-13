# File for mapping numbers 1..n for N images
# to image ids in dataset

import os
import json
import pickle

with open('/Users/drewnleonard/Documents/thesis/data/pickle/ad_id_map_pruned.pickle','rb') as f:
    id_map = pickle.load(f)

seen_ids = set()

ad_id_file_id_map = {}

json_root = '/Users/drewnleonard/Documents/thesis/data/json/ads/'
for root, dirs, files in os.walk(json_root):
    for file in files:
    	file_id = file[:-5]
    	
    	# If valid json file ...
    	if 'json' in file and file_id in id_map:
    		
    		# Load json file
    		path = "{}/{}".format(root,file)
    		with open(path) as f:
    			this_json = json.load(f)
			
			# Get ad id from json file
			all_text = this_json['first']['text']['fullTextAnnotation']['text']
			ad_id_line = all_text.split('\n', 1)[0]
			ad_id = [s for s in ad_id_line.split() if s.isdigit()][0]

			# Manual quality control
			if file_id == 'P(1)0006480':
				ad_id = '1005'
			if file_id == 'P(1)0004434':
				ad_id = '2197'
			
			# Check for and manage duplicate ids
			if ad_id in seen_ids:
				print "DUPLICATE ID: {0}\nOLD FILE: {1}\nNEW FILE: {2}\nNEW AD ID LINE: {3}\n".format(ad_id, ad_id_file_id_map[ad_id], file_id, ad_id_line)
			seen_ids.add(ad_id)

			ad_id_file_id_map[ad_id] = file_id

# Save image id map
with open('/Users/drewnleonard/Documents/thesis/data/pickle/ad_id_file_id_map_pruned.pickle', 'wb') as handle:
    pickle.dump(ad_id_file_id_map, handle, protocol=pickle.HIGHEST_PROTOCOL)

    		
