rm(list = ls())

library(textcat)
library(dplyr)
library(stringr)
library(readxl)


# load unique dataset
load("data\\3_mastodon_processed_post_data.RData")

# 1. primary data clean ####
# check columns
colnames(unique_data)

## set timeline ####
# remove the post before 11.30.2022 and after 09.30.2023
min_datetime <- as.POSIXct("2022-11-30 00:00:00", format = "%Y-%m-%d %H:%M:%S")
max_datetime <- as.POSIXct("2023-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")

mastodon_dataset <- unique_data %>%
  filter((created_at >= min_datetime) & (created_at < max_datetime))

## check the instances ####

# privacy issue is difficult to consider...
# too many instances

## only keep the instance name
mastodon_dataset$instance <- mastodon_dataset$url
mastodon_dataset$instance <- str_extract_all(string = mastodon_dataset$instance, 
                                             pattern = "https://[^/]+/")

## check the unique value of instance
instance_df <- count(mastodon_dataset,instance)
instance_df <- apply(instance_df,2,as.character)

# Specify the path where you want to save the CSV file
output_path <- "data\\4_instance_raw.csv"  

# Save the DataFrame as a CSV file in the specified path
write.table(instance_df, file = output_path,sep=",", col.names=FALSE)


# input the dataset after checking the privacy policy
instance_df <- read_excel("data\\4.1_instance_notes.xlsx")
instance_df <- subset(instance_df,keep == "1")

# keep the data that from the kept instances
instances_kept <- instance_df$instances
mastodon_dataset_filtered <- subset(mastodon_dataset, instance %in% instances_kept)

rm(unique_data)
rm(mastodon_dataset)
rm(instances_kept)
rm(max_datetime)
rm(min_datetime)

save.image(file = "data\\5_mastodon_research.RData")
