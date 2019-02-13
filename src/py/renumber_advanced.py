from __future__ import division
import os
import shutil
import pickle
from PIL import Image
import numpy as np
from pylab import *


img_id_map = {}

image_dir = "/Users/drewnleonard/Documents/thesis/data/png/png_flat"
target_dir_keep = "/Users/drewnleonard/Documents/thesis/data/png/png_small_keep"
target_dir_throw = "/Users/drewnleonard/Documents/thesis/data/png/png_small_throw"

img_n = 0
img_n_master = 0

# For each image in the flat directory
for image_path in os.listdir(image_dir):
    
    # Get image type
    image_type = int(image_path[0])
    
    # If image is type 1 (i.e., graphic)
    if image_type == 1:

        pct_black_px = None

        try:

            # Validate image
            # Open image
            im = Image.open("{}/{}".format(image_dir, image_path))
            pix = np.asarray(im)[:-1000]

            # Do something
            pix = pix[:,:,0:3] # Drop the alpha channel
            idx = np.where(pix-255)[0:2] # Drop the color when finding edges
            box = map(min,idx)[::-1] + map(max,idx)[::-1]

            # Crop image
            region = im.crop(box)
            region_pix = np.asarray(region)

            # Calculate black pixels
            black_pixel_count = (region_pix == 0).sum()
            total_pixel_count = region_pix.shape[0] * region_pix.shape[1]
            pct_black_px = 100 * black_pixel_count/total_pixel_count

        except Exception as e:
            print e

        # If throw away ...
        if pct_black_px and pct_black_px > 75:

            # Get source and target paths
            img_src = "{}/{}".format(image_dir,image_path)
            img_target = "{}/{}.png".format(target_dir_throw,image_path)
            shutil.copyfile(img_src, img_target)

        else:

            # Get full image source path and target path
            img_src = "{}/{}".format(image_dir,image_path)
            img_target = "{}/{}.png".format(target_dir_keep,image_path)

            # Get image id (i.e., 'P(1)0003655')
            img_id = image_path[2:-4]

            # Move image
            shutil.copyfile(img_src, img_target)

            # Store old and new image ids in map
            img_id_map[img_id] = str(img_n)

            # Increment new image id count
            img_n += 1

        
        # Increment master image count
        img_n_master += 1
        print(img_n_master)

# # Save image id map
with open('/Users/drewnleonard/Documents/thesis/data/pickle/ad_id_map_small.pickle', 'wb') as handle:
    pickle.dump(img_id_map, handle, protocol=pickle.HIGHEST_PROTOCOL)


