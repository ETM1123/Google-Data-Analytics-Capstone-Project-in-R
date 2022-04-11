from config import config 
import urllib.request
import zipfile
import os
import time

# Unzips and extract csv files from source and stores data in file_destination

def extract_file(file_destination: str) -> None:
  # Iterate through zipfile names
  for source in config.SOURCE:
    # Retrieve document 
    file, _ = urllib.request.urlretrieve(source)
    # Extract and save file in working directory
    with zipfile.ZipFile(file) as f:
      f.extractall(file_destination)
    
    print(f"File extracted from: {source} complete.")
    time.sleep(5)

def run():
  files_in_working_directory_before = set([fn for fn in os.listdir(config.WORKING_DIRCETORY) if fn[-4:]== ".csv"])
  file_name = set([zfn[-4:] + ".csv" for zfn in config.ZIPFILE_NAMES])

  # Check if the files exist in the directory
  if len(file_name.intersection(files_in_working_directory_before) == len(file_name)):
    print("Files aready exist in directory")

  else:
    extract_file(config.WORKING_DIRCETORY)
    # Check the files are in the correct directory
    files_in_working_directory_after  = os.listdir(config.WORKING_DIRCETORY)
    message = "Some of the files are misssing!"
    assert len(files_in_working_directory_after) >= len(config.SOURCE), message
    # Display files in directory
    print(f"Current files in in directory: {config.WORKING_DIRCETORY} \n")
    print('\n'.join(files_in_working_directory_after))

if __name__ == "__main__":
  run()
  # test imports 
  # print(config.SOURCE)
  # print(config.WORKING_DIRCETORY)
  # print(config.ZIPFILE_NAMES)