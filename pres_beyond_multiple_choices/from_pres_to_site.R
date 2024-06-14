# Load necessary library
library(fs)

# Define paths
source_dir <- "/home/ral/Dropbox/git/beyond_multiple_choices/pres_beyond_multiple_choices"
dest_dir <- "/home/ral/Dropbox/git/laurenceoliviermfoisy/presentation_ollama"

# Check if the source directory exists
if (dir_exists(source_dir)) {
  
  # Create the destination directory if it doesn't exist
  dir_create(dest_dir, recurse = TRUE)
  
  # List all files and directories in the source directory
  items_to_copy <- dir_ls(source_dir, recurse = TRUE, type = "any")
  
  # Copy each file and directory to the destination directory
  for (item in items_to_copy) {
    # Create the corresponding path in the destination directory
    dest_item <- file.path(dest_dir, path_rel(item, start = source_dir))
    
    if (is_dir(item)) {
      # If the item is a directory, create it in the destination
      dir_create(dest_item, recurse = TRUE)
    } else {
      # Ensure the destination subdirectory exists
      dir_create(path_dir(dest_item), recurse = TRUE)
      
      # Copy the file to the destination directory
      file_copy(item, dest_item, overwrite = TRUE)
    }
  }
  
  message("Files and directories successfully copied to the website directory.")
  
} else {
  message("The source directory does not exist.")
}







