"""
Drew N. Leonard
June 2018

Annotates remotely-stored images with Google Cloud Vision API.
Compatible with command line.
"""

from google.cloud import storage
from google.cloud import vision
from google.protobuf.json_format import MessageToJson

import os
import argparse
import warnings
import re
import json


def detect_text_util(uri, file_type):
    """ Detects text in image.

    Args:
        client: Google Cloud Vision API client
        image: vision type of Image, set to image uri
    Returns:
        Formatted string of detected text.
    """
    client = vision.ImageAnnotatorClient()
    image = vision.types.Image()
    image.source.image_uri = uri
    text_response = client.text_detection(image=image)
    thisJson = json.loads(MessageToJson(text_response))
    if file_type == 0:
        thisJson["textAnnotations"] = None
        thisJson["fullTextAnnotation"]["pages"][0]["blocks"] = None

    return thisJson


def detect_label_util(uri, file_type):
    """ Labels images.

    Args:
        client: Google Cloud Vision API client
        image: vision type of Image, set to image uri
    Returns:
        Formatted string of labels.
    """
    client = vision.ImageAnnotatorClient()
    image = vision.types.Image()
    image.source.image_uri = uri
    response = client.label_detection(image=image)
<<<<<<< HEAD
=======

    return response
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19

    return response

<<<<<<< HEAD

=======
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19
def annotate_util(uri, file_type):
    """Annotates image stored in GCS remotely.

    Args:
        uri: GCS uri of image to annotate.
    Returns:
        text (str): formatted response
    """
    text = detect_text_util(uri, file_type)
<<<<<<< HEAD

    if file_type == 0:
        annotation = {"text": text, "label": None}
    elif file_type == 1:
        label = json.loads(MessageToJson(detect_label_util(uri, file_type)))
        annotation = {"text": text, "label": label}
=======

    if file_type == 0:
        annotation = {"text": text, "label": None}
    elif file_type == 1:
        label = json.loads(MessageToJson(detect_label_util(uri, file_type)))
        annotation = {"text": text, "label": label}

    return annotation
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19

    return annotation

<<<<<<< HEAD

def directory_util(project_id, bucket, output_directory):
=======
def directory_util(project_id, bucket, prefix, output_directory):
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19
    """Iterates over files in bucket directory

    Args:
        project_id: ID of GCS project.
        bucket: Name of GCS project bucket.
        output_directory: Path to output folder for text files.
    """

    client = storage.Client(project=project_id)
    this_bucket = client.bucket(bucket)
<<<<<<< HEAD
    bucket_iterator = this_bucket.list_blobs()
=======
    bucket_iterator = this_bucket.list_blobs(prefix=prefix)
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19

    for i, blob in enumerate(bucket_iterator):

        if "png" in blob.name:

            try:

                this_uri = "gs://" + bucket + "/" + blob.name
                head, tail = os.path.split(blob.name)

                file_type = int(tail[:1])
                tail = tail[2:]
<<<<<<< HEAD
                file_name = output_directory + head + "/" + tail.strip(".png") + ".json"
=======
                head = head.replace("png", "json")
                tail = tail.replace("png", "json")

                file_name = "{0}{1}/{2}".format(output_directory, head, tail)
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19

                if file_type >= 2:
                    continue

<<<<<<< HEAD
                print("FILE #{0}, NAME: {1}, TYPE: {2}".format(str(i), file_name, str(file_type)))
=======
                print "FILE #{0}, NAME: {1}, TYPE: {2}".format(
                    str(i), file_name, str(file_type))
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19

                if not os.path.exists(output_directory + head):
                    os.makedirs(output_directory + head)

                response = annotate_util(this_uri, file_type)
                if os.path.exists(file_name):
<<<<<<< HEAD
                    
=======

>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19
                    with open(file_name) as f:
                        data = json.load(f)
                    data.update({"second": response})
                    f.close()
                    with open(file_name, 'w') as f:
                        json.dump(data, f, separators=(',', ':'))
                    f.close()

                else:

                    with open(file_name, 'w') as f:
                        json.dump(
                            {
                                "first": response
                            }, f, separators=(',', ':'))
                    f.close()

            except Exception as e:

<<<<<<< HEAD
                print("ERROR: {0}, BLOB NAME: {1}".format(e, blob.name))
        
=======
                print "ERROR: {0}, BLOB NAME: {1}".format(e, blob.name)

>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19

if __name__ == "__main__":
    warnings.filterwarnings("ignore")
    parser = argparse.ArgumentParser()
    parser.add_argument("project_id", help="Name of GCS project.")
    parser.add_argument("bucket", help="Name of GCS bucket.")
<<<<<<< HEAD
    
    parser.add_argument(
        "output_directory", help="Path of output folder for annotation.")
    args = parser.parse_args()
    directory_util(args.project_id, args.bucket, 
=======
    parser.add_argument("prefix", help="Prefix of cloud-stored PNG folder.")
    parser.add_argument(
        "output_directory", help="Path of output folder for annotation.")
    args = parser.parse_args()
    directory_util(args.project_id, args.bucket, args.prefix,
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19
                   args.output_directory)
