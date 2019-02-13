"""
Drew N. Leonard
June 2018

Uploads files to GCS bucket.
Compatible with command line.
"""

import os
import argparse
import glob
import warnings

from google.cloud import storage


def upload_util(input_path, project_id, bucket):
    """Uploads files to GCS bucket in batch.

    Args:
        input_path: Path to input folder.
        project_id: ID of GCS project.
        bucket: Name of GCS project bucket.
    """
    client = storage.Client(project=project_id)
    bucket = client.get_bucket(bucket)

<<<<<<< HEAD
    subfolders = [f.path for f in os.scandir(input_path) if f.is_dir()]
=======
    subfolders = [
        roots for roots, dirs, files in os.walk(input_path) if not dirs
    ]
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19

    for subfolder in subfolders:

        paths = glob.glob(subfolder + "/*")
<<<<<<< HEAD
        # paths = glob.glob(input_path + "*")
        for i, path in enumerate(paths):
            print("File #{}".format(str(i)))
=======
        for i, path in enumerate(paths):
            print("FILE: #{0} OF {1}, {2}".format(str(i), len(files), path))
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19
            blob = bucket.blob(os.path.basename(os.path.normpath(
                subfolder)) + "/" + os.path.basename(path))
            blob.upload_from_filename(path)

if __name__ == "__main__":
    warnings.filterwarnings("ignore")
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "input_path", help="Path of folder containing input files.")
    parser.add_argument("project_id", help="Name of GCS project.")
    parser.add_argument("bucket", help="Name of GCS bucket.")
    args = parser.parse_args()
    upload_util(args.input_path, args.project_id, args.bucket)
