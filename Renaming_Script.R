library(tidyverse)
# First let's get all file paths 
# Specify our directory
path_csv <- "Dashboard/"
# List all files in the directory
all_files <- list.files(path_csv)
# Keep the csv ones 
csv_files <- all_files[which(str_detect(pattern = "csv", all_files))]
# To make them actual paths we're going to have 
# to paste Dashboard/ in front 
# Empty vector to fill with loop
all_path_files <- c()
# Loop to just paste 
for (i in 1:11) {
  new_path <- paste0("Dashboard/", csv_files[i])
  all_path_files[i] <- new_path
}
# Now loop to create our datasets 
for (j in 1:11) {
  df <- read_csv(all_path_files[j])
  df_title <- str_split(all_path_files[j], 
                        "\\/")[[1]][2]
  first_col <- df %>% 
    select(1) %>% 
    pull()
  renamed_df <- df %>% 
    mutate(name = first_col) %>% 
    select(name, everything())
  write_csv(renamed_df, 
            paste0("Dashboard/", "renamed_",df_title))
}
