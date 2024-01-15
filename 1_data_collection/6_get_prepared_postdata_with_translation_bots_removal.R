rm(list = ls())


library(textcat)
library(dplyr)
library(stringr)
library(readxl)
library(xml2)
library(rvest)
library(arrow)
library(data.table)


# prepraration ####

# load the primary dataset
load("data/6_mastodon_primary_research_data.RData")
rm(instance_df)
rm(translation)
rm(mastodon_dataset_filtered)


# 1. bots removal ####

# return the account lists
typeof(mastodon_primary$account)

# return the bot variable from list vatiable account to the dataframe
mastodon_primary <- mastodon_primary  %>%
  mutate(
    account_name = purrr::map_chr(account, "username"),
    account_bot = purrr::map_chr(account, "bot"),
    account_url = purrr::map_chr(account, "url")
  )

table(is.na(mastodon_primary$account_bot))
# there is no any missing value, every account was labeled

length(unique(mastodon_primary$account_id))

# build a bot df with url, bot, username, account_id information

bot_df <- mastodon_primary[mastodon_primary$account_bot == 'TRUE',
                           c("account_name","account_bot","account_url","account_id")]

# check how many bot accounts in the dataset
length(unique(bot_df$account_id))

sample(unique(bot_df$account_url), 20)
sample(unique(mastodon_primary$account_url[mastodon_primary$account_bot=="FALSE"]),50)
# both of the random sample checked by the researcher, and looks the results are correct.

nrow(bot_df)

# remove the bots related data

mastodon_primary <- subset(mastodon_primary,account_bot=="FALSE")
nrow(mastodon_primary)  
length(unique(mastodon_primary$account_id))

# remove the bot dataframe
rm(bot_df)



# 2. language check #### 

## number of Google Could translation ####

table(!is.na(mastodon_primary$gl_trans_text))
table(!is.na(mastodon_primary$gl_trans_text))/nrow(mastodon_primary)

length(unique(mastodon_primary$gl_detect_lang))

sort(table(mastodon_primary$gl_detect_lang))

# build a new language column in the dataset
mastodon_primary$language_type <- mastodon_primary$gl_detect_lang


# check what all the languages are
head(mastodon_primary[mastodon_primary$gl_detect_lang=="bn","clean_text"],20)

replacement_values <- c("en" = "English", "de" = "German", "fr" = "French", "es" = "Spanish",
                        "it" = "Italian","nl" = "Dutch","ja" = "Japanese","ja-Latn" = "English",
                        "pt" = "Portuguese","zh-TW" = "Chinese","fi" = "Finnish",
                        "zh-CN" = "Chinese","zh-Latn" = "English",
                        "cs" = "Czech","ca" = "Catalan","pl" = "Polish","ru" = "Russian",
                        "sv" = "Swedish","ar" = "Arabic", "da" = "Danish", "uk" = "Ukrainian",
                        "hi-Latn" = "Mix/Unclear", "vi" = "Vietnamese", "no" = "Norwegian", 
                        "hu" = "Hungarian", "ko" = "Korean",
                        "eu" = "Basque", "ta-Latn" = "Mix/Unclear", "lb" = "Luxembourgish",
                        "tr" = "Turkish", "la" = "Latin", "om" = "English", "hr" = "Croatian",
                        "fa" = "Persian", "ilo" = "English", "id" = "Indonesian", "gl" = "Galician",
                        "bs" = "Bosnian", "mr-Latn" = "English", "haw" = "Mix/Unclear", "gu-Latn" = "English",
                        "fil" = "Mix/Unclear", "ro" = "Romanian", "qu" = "German", "iw" = "Hebrew",
                        "hi" = "Hindi", "gn" = "Guarani", "ga" = "Irish", "bg" = "Bulgarian",
                        "sl" = "Slovenian", "sk" = "Slovak", "ms" = "Mix/Unclear", "lt" = "Mix/Unclear",
                        "kri" = "Krio", "gd" = "Mix/Unclear", "co" = "Mix/Unclear", "ta" = "Tamil",
                        "su" = "Mix/Unclear", "sn" = "English", "ml" = "Malayalam", "lv" = "Mix/Unclear",
                        "fy" = "Mix/Unclear", "bg-Latn" = "Mix/Unclear", "af" = "Mix/Unclear", 
                        "zu" = "Zulu",
                        "rw" = "Mix/Unclear", "mi" = "Mix/Unclear", "mg" = "Mix/Unclear", "jv" = "Mix/Unclear",
                        "is" = "Icelandic", "hmn" = "English", "et" = "Estonian", "eo" = "Mix/Unclear",
                        "el" = "Greek", "cy" = "English", "ceb" = "Mix/Unclear", "bn" = "Bangla") 

mastodon_primary$language_type[mastodon_primary$language_type %in% names(replacement_values)] <- replacement_values[mastodon_primary$language_type[mastodon_primary$language_type %in% names(replacement_values)]]

table(mastodon_primary$language_type)

# remove the data written in unclear language

mastodon_primary <- mastodon_primary[mastodon_primary$language_type != "Mix/Unclear",]

# combine the original language and translated language into a new column ####
mastodon_primary$text_combine <- NA
mastodon_primary$text_combine <- ifelse(mastodon_primary$language_type == "English",
                                        mastodon_primary$clean_text, mastodon_primary$gl_trans_text)

# check if there is missing value in the combine language column
table(is.na(mastodon_primary$text_combine))


# extract related information from the list varible "account" ####
mastodon_primary  <- mastodon_primary  %>%
  mutate(
    account_locked = purrr::map_chr(account, "locked"),
    account_discoverable = purrr::map_chr(account, "discoverable"),
    account_created_at = purrr::map(account, "created_at"),
    account_note = purrr::map_chr(account, "note"),
    account_followers_count = purrr::map_int(account, "followers_count"),
    account_following_count = purrr::map_int(account, "following_count"),
    account_statuses_count = purrr::map_int(account, "statuses_count"),
    account_fields = purrr::map(account, "fields"),
    account_group = purrr::map_chr(account, "group")
  )


# extract create time of the account ####
posixct_to_date <- function(x) {
  return(format(as.Date(as.POSIXct(x, tz=x$zone), tz=x$zone), format="%Y-%m-%d"))
}
mastodon_primary$account_created_at <- sapply(mastodon_primary$account, function(x) posixct_to_date(x$created_at))


mastodon_primary$account_fields <- NULL

# extract mentions data frame ####
mastodon_primary$mentions[[13]]

full_mention_df_ls = list()
for (i in 1:nrow(mastodon_primary)){
  mention_df <- mastodon_primary$mentions[[i]]
  if (length(mention_df) > 0) {
    mention_df <- as.data.frame(mention_df)
    mention_df$post_id <- unlist(mastodon_primary$id[i])
    mention_df$account_id <- unlist(mastodon_primary$account_id[i])
    
    full_mention_df_ls[[i]] <- mention_df
  }
}
full_mention_df <- rbindlist(full_mention_df_ls, fill = TRUE)
rm(full_mention_df_ls)


# extract hashtags data frame ####

full_tag_df_ls = list()
for (i in 1:nrow(mastodon_primary)){
  tag_df <- mastodon_primary$tags[[i]]
  if (length(tag_df) > 0) {
    tag_df <- as.data.frame(tag_df)
    tag_df$post_id <- unlist(mastodon_primary$id[i])
    tag_df$account_id <- unlist(mastodon_primary$account_id[i])
    
    full_tag_df_ls[[i]] <- tag_df
  }
}
full_tag_df = rbindlist(full_tag_df_ls, fill = TRUE)
rm(full_tag_df_ls)

# extract account_fields data frame ####

full_acct_fields_df_ls = list()
for (i in 1:nrow(mastodon_primary)){
  acct_fields_df <- mastodon_primary$account[[i]]$fields[[1]]
  if (length(acct_fields_df) > 0) {
    acct_fields_df <- as.data.frame(acct_fields_df)
    acct_fields_df$account_id <- unlist(mastodon_primary$account_id[i])
    
    full_acct_fields_df_ls[[i]] <- acct_fields_df
  }
}
full_acct_fields_df = rbindlist(full_acct_fields_df_ls, fill = TRUE)
rm(full_acct_fields_df_ls)


# remove html element in account_note ####

# remove the html elements and url links in the contents
rm_html_url <- function(text) {
  # replace elements with blank for following steps
  text <- gsub("<p>", " ", text)
  text <- gsub("<br>", " ", text)
  # Remove HTML elements
  text <- gsub("<.*?>", "", text)
  # Remove URL links (http and https)
  text <- gsub("https?://\\S+|www\\.\\S+", "", text)
  return(text)
}

mastodon_primary$account_descri <- sapply(mastodon_primary$account_note, rm_html_url)

# check the results
# choose random 50 data to check the results manually
random_num <- sample(1:56687,50)
long_text <- mastodon_primary[random_num, "account_descri"]
mastodon_primary$account_url[random_num]
long_text
rm(long_text,random_num,sample_uri)


# change the post creation time to character

posixct_to_date <- function(x) {
  return(format(as.Date(as.POSIXct(x, tz=x$zone), tz=x$zone), format="%Y-%m-%d"))
}
mastodon_primary$post_created_at <- sapply(mastodon_primary$created_at, function(x) posixct_to_date(x))





# create a copy of the data set and just keep the related information
mastodon_final_data <- mastodon_primary
colnames(mastodon_final_data)
mastodon_final_data = subset(mastodon_final_data, 
                             select = -c(created_at,account,mentions,tags,
                                         favourited,reblogged,muted,bookmarked,
                                         pinned,request_tag,account_note,reblog,
                                         gl_detect_lang,gl_trans_text,clean_text,
                                         account_bot) )
colnames(mastodon_final_data)


# account dataframe ####
# create an account df with account id and account description
account_df <- mastodon_final_data[,c("account_id","account_descri","instance",
                                     "account_url","language_type",
                                     "account_locked",
                                     "account_group" ,
                                     "account_discoverable","account_created_at",
                                     "account_followers_count",
                                     "account_following_count","account_statuses_count" )]
# remove duplicatation
account_df <- account_df[!duplicated(account_df$account_id), ]

rm(acct_fields_df)
rm(mention_df)
rm(tag_df)


save.image(file = "data\\7_mastodon_primary_data.RData")


write_feather(account_df, "data\\8_account_general_data.feather")
write_feather(mastodon_final_data, "data\\8_mastodon_final_data.feather")
write_feather(full_acct_fields_df, "data\\8_full_acct_fields.feather")
write_feather(full_mention_df, "data\\8_full_mention_df.feather")
write_feather(full_tag_df, "data\\8_full_tag_df.feather")




