import os
import pickle

img_dir = "/Users/drewnleonard/Documents/thesis/data/png/png_small_keep_renumbered"
img_map_path = "/Users/drewnleonard/Documents/thesis/data/pickle/ad_id_map_pruned.pickle"

# Get max id
max_id = 0
for img_path in os.listdir(img_dir):
	img_id = int(img_path[7:-4])
	max_id = max(max_id,img_id)

print "MAX ID: {}".format(max_id)

# Check if all images exist and
# are numbered correctly
for n in range(0, max_id):
	if not os.path.isfile("{}/ira_ad_{}.png".format(img_dir,n)):
		print "FILE FOR INTEGER ... {} ... NOT PRESENT".format(n)

# Print dictionary
with open(img_map_path, 'rb') as handle:
    b = pickle.load(handle)
    print len(b)

