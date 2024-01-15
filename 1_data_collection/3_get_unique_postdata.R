
rm(list = ls())

output_file_location <- "data"
output_file_name <- "3_mastodon_processed_post_data"

hashtags_dir_list <- list.dirs(path = "data\\2_postdata_raw", recursive = FALSE)
data_list <- list()
for (hashtag_dir in hashtags_dir_list){
  print(hashtag_dir)
  filenames = dir(path = hashtag_dir, full.names = TRUE, pattern ="*.RData")
  load(filenames)
  data_list[[hashtag_dir]] <- full_timeline_data
}
combined_df <- do.call(rbind, data_list)

# remove duplicated data based on the posts' id.
unique_data <- combined_df[!duplicated(combined_df$id),]

output_file_path_i <- paste(output_file_location, "\\", output_file_name,".RData", sep="")
save(unique_data, file=output_file_path_i)
