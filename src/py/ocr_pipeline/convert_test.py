import os
from PyPDF2 import PdfFileReader
import glob

count_map = {}

png_path = "./png/"
pdf_path = "./newpdf/"

png_subdirectories = [
        roots for roots, dirs, files in os.walk(png_path) if not dirs
]

pdf_subdirectories = [
        roots for roots, dirs, files in os.walk(pdf_path) if not dirs
]

for png_subdirectory in png_subdirectories:

	folder_name = os.path.split(png_subdirectory)[1].replace("_png", "")
	png_file_count = len(os.listdir(png_subdirectory))

	count_map[folder_name] = {
		"png_count": png_file_count
	}

for pdf_subdirectory in pdf_subdirectories:

	folder_name = os.path.split(pdf_subdirectory)[1]
	if folder_name not in count_map: 
		print "ERROR: PDF FOLDER {} NOT IN COUNT MAP".format(folder_name)
	else:
		page_count = 0
		pdf_paths = glob.glob("{}/*".format(pdf_subdirectory))
		for pdf_path in pdf_paths:
			pdf = PdfFileReader(open(pdf_path,'rb'))
			page_count += pdf.getNumPages()
		count_map[folder_name]["pdf_count"] = page_count

mismatches = 0
total = 0

for key, value in count_map.iteritems():
	total += value["pdf_count"]
	mismatches += value["pdf_count"] - value["png_count"]
	if value["png_count"] != value["pdf_count"]:
		print "MISMATCH: FOLDER: {0}, PNG: {1}, PDF: {2}".format(key, value["png_count"], value["pdf_count"])
		# matches += value["png_count"]
	else:
		print "MATCH: FOLDER: {0}, PNG: {1}, PDF: {2}".format(key, value["png_count"], value["pdf_count"])
		# mismatches += value["pdf_count"] - value["png_count"]
# print matches, total
print "{}% ACCURACY".format(str(float(total - mismatches)/total))