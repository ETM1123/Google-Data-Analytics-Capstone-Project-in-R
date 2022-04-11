# CONSTANT VARIABLES 

# Extension for url to download file
ZIPFILE_NAMES = [
"202004-divvy-tripdata.zip",
"202005-divvy-tripdata.zip",
"202006-divvy-tripdata.zip",
"202007-divvy-tripdata.zip",
"202008-divvy-tripdata.zip",
"202009-divvy-tripdata.zip",
"202010-divvy-tripdata.zip",
"202011-divvy-tripdata.zip",
"202012-divvy-tripdata.zip",
"202101-divvy-tripdata.zip",
"202102-divvy-tripdata.zip",
"202103-divvy-tripdata.zip",
"202104-divvy-tripdata.zip",
"202105-divvy-tripdata.zip",
"202106-divvy-tripdata.zip",
"202107-divvy-tripdata.zip",
"202108-divvy-tripdata.zip",
"202109-divvy-tripdata.zip",
"202110-divvy-tripdata.zip",
"202111-divvy-tripdata.zip",
"202112-divvy-tripdata.zip"
]
# Location of data (this may update and contain more information)
URL = "https://divvy-tripdata.s3.amazonaws.com/{}".format
# Formatting URL to the correct file
SOURCE = [URL(fileName) for fileName in ZIPFILE_NAMES]
# Directory path to store raw data
WORKING_DIRCETORY = "ENTER PATH TO WORKING DIR"

# test 
if __name__ == "__main__":
  print("\n".join(SOURCE))
  