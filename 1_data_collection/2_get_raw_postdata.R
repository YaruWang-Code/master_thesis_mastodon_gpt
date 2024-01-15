
#### import ####

rm(list = ls())
# developer version
remotes::install_github("schochastics/rtoot")
# CRAN version
library(rtoot)
auth_setup()
# set the dataset output path
output_data_folder <- "data\\2_postdata_raw"

# import all the hashtags contain chatgpt
hashtag_list_path <- "data\\1_hashtag_list.csv"

# read list of hashtags
chatgpt_hashtags <- read.csv(hashtag_list_path, sep=",", encoding='utf-8', header=FALSE)
chatgpt_hashtags <- as.vector(t(chatgpt_hashtags))

# skip data if already existing
skip_if_exist <- TRUE

#### download data ####

# break down query in smaller parts to not exceed rate limit
query_limit <- 3000L
query_refresh_time <- 5

if(!dir.exists(output_data_folder)){
  dir.create(output_data_folder)
}



for (i in 1:length(chatgpt_hashtags)) {
  # prepare for export
  current_time <- format(Sys.time(), "%Y-%m-%d_%H%M")
  hashtag_i <- chatgpt_hashtags[i]
  message("-----------")
  message(paste("Downloading data for hashtag ", hashtag_i))
  hashtag_start_time <- Sys.time()
  
  output_file_dir_i <- paste(output_data_folder, "\\", hashtag_i, sep="")
  
  # check exist file
  if ((skip_if_exist) & (dir.exists(output_file_dir_i))) {
    message(paste("Skipping hashtag", hashtag_i))
    next
  }
  
  if(!dir.exists(output_file_dir_i)){
    dir.create(output_file_dir_i)
  }
  
  # get initial max_id
  latest_post <- get_timeline_hashtag(hashtag = "chatgpt", limit = 1L, retryonratelimit = FALSE)
  check_newer_posts <- get_timeline_hashtag(hashtag = "chatgpt", limit = 10L, retryonratelimit = FALSE, min_id = latest_post$id[1])
  stopifnot(nrow(check_newer_posts) == 0)
  query_max_id <- latest_post$id[1]
  
  posts_left <- TRUE
  
  full_timeline_data <- latest_post
  
  while (posts_left) {
    query_start_time <- Sys.time()
    part_timeline_data <- get_timeline_hashtag(hashtag = hashtag_i, limit = query_limit, retryonratelimit = FALSE, max_id = query_max_id)
    query_end_time <- Sys.time()
    query_duration <- as.numeric(difftime(query_end_time, query_start_time, units = "mins"))
    newest_date <- part_timeline_data$created_at[1]
    oldest_date <- part_timeline_data$created_at[nrow(part_timeline_data)]
    message(paste("Downloaded data between ", oldest_date, " and ", newest_date, " in ", query_duration, " minutes.", sep=""))
    full_timeline_data <- rbind(full_timeline_data, part_timeline_data)
    if (nrow(part_timeline_data) < query_limit) {
      posts_left <- FALSE
    } else {
      query_max_id <- part_timeline_data$id[query_limit]
      Sys.sleep((query_refresh_time - (query_duration %% query_refresh_time))*60)
    }
  }
  
  num_posts <- nrow(full_timeline_data)
  message(paste("Downloaded data for ", num_posts, " posts.", sep=""))
  output_file_path_i <- paste(output_file_dir_i, "\\", hashtag_i,"_post_", current_time, "_", num_posts, ".RData", sep="")
  
  full_timeline_data$request_tag <- hashtag_i
  full_timeline_data$account_id <- unlist(lapply(full_timeline_data$account, function(x){x$id}))
  save(full_timeline_data, file=output_file_path_i)
  message(paste("Saved data for hashtag", hashtag_i))
  hashtag_end_time <- Sys.time()
  hashtag_duration <- as.numeric(difftime(hashtag_end_time, hashtag_start_time, units = "mins"))
  message(paste("Hashtag", hashtag_i,"finished in", hashtag_duration, "minutes."))
  message(" ")
}


