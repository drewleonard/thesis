import os
import shutil
import pickle

img_id_map = {}

image_dir = "/Users/drewnleonard/Documents/thesis/data/png/png_small_keep"
target_dir = "/Users/drewnleonard/Documents/thesis/data/png/png_small_keep_renumbered"

img_n = 0

# For each image in the flat directory
for image_path in os.listdir(image_dir):
    
    # Get full image source path and target path
    img_src = "{}/{}".format(image_dir,image_path)
    img_target = "{}/ira_ad_{}.png".format(target_dir,img_n)

    # Get image id (i.e., 'P(1)0003655')
    img_id = image_path[2:-4]

    # Move image
    shutil.copyfile(img_src, img_target)

    # Store old and new image ids in map
    img_id_map[img_id] = str(img_n)

    # Increment new image id count
    img_n += 1

# Save image id map
with open('/Users/drewnleonard/Documents/thesis/data/pickle/ad_id_map_pruned.pickle', 'wb') as handle:
    pickle.dump(img_id_map, handle, protocol=pickle.HIGHEST_PROTOCOL)


