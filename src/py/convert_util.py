"""
Drew N. Leonard
June 2018

Converts PDFs to PNGs.
Compatible with command line.
"""

import os
import glob
import argparse
import sys

from wand.image import Image


def convert_util(input_path, output_path):
    """ Converts files from PDFs to PNGs in batch.

    Converts PDFs from input folder into PNGs,
    and then deposits them in output folder.

    Args:
        input_path (str): Path to input folder.
        output_path (str): Path to output folder.
    """

<<<<<<< HEAD
    subdirectories = [
        roots for roots, dirs, files in os.walk(input_path) if not dirs
    ]
=======
    source = None
    images = None

    subdirectories = [
        roots for roots, dirs, files in os.walk(input_path) if not dirs
    ]
    
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19
    for x, subdirectory in enumerate(subdirectories):

        pdf_paths = glob.glob("{}/*".format(subdirectory))
        pdf_directory = os.path.split(subdirectory)[1]
        png_directory = os.path.join(output_path,
                                     "{}_png/".format(pdf_directory))

        if not os.path.exists(png_directory): os.makedirs(png_directory)

        for i, pdf_path in enumerate(pdf_paths):

<<<<<<< HEAD
            print("FILE #{0} OF {1}, SUBDIRECTORY #{2} of {3}".format(
                str(i + 1), len(pdf_paths), str(x + 1), len(subdirectories)))
=======
            print("FILE #{0} OF {1}, SUBDIRECTORY #{2} of {3}, SUBDIRECTORY: {4}".format(
                str(i + 1), len(pdf_paths), str(x + 1), len(subdirectories), subdirectory))
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19

            png_filename = os.path.basename(pdf_path).replace("pdf", "png")
            png_filepath = os.path.join(png_directory, png_filename)

<<<<<<< HEAD
            with (Image(filename=pdf_path, resolution=400)) as source:

                images = source.sequence
                pages = len(images)

                for n in range(pages):
                    this_path = os.path.join(
                        os.path.split(png_filepath)[0], "{0}_{1}".format(
                            str(n),
                            os.path.split(png_filepath)[1]))
                    Image(images[n]).save(filename=this_path)
=======
            try:
                with (Image(filename=pdf_path, resolution=400)) as source:

                    images = source.sequence
                    pages = len(images)

                    for n in range(pages):
                        this_path = os.path.join(
                            os.path.split(png_filepath)[0], "{0}_{1}".format(
                                str(n),
                                os.path.split(png_filepath)[1]))
                        Image(images[n]).save(filename=this_path)


            except Exception as e:
                print("ERROR: {0}, IMAGE: {1}".format(e, this_path))
>>>>>>> e3ccb0b4a7cbda245c311f4338a74fd8d9be4f19

            images = None
            source = None

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "input_path", help="Full path of folder containing input PDFs.")
    parser.add_argument(
        "output_path", help="Full path of output folder for PNGs.")
    args = parser.parse_args()
    convert_util(args.input_path, args.output_path)
