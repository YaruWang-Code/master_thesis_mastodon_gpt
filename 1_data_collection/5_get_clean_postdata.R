rm(list = ls())

library(textcat)
library(dplyr)
library(stringr)
library(readxl)
library(xml2)
library(rvest)


# load unique dataset
load("data\\5_mastodon_research.RData")


# 1. Clean post data ####
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

mastodon_dataset_filtered$clean_text <- sapply(mastodon_dataset_filtered$content, rm_html_url)

# check the results
# choose random 50 data to check the results manually
random_num <- sample(1:56687,50)
long_text <- mastodon_dataset_filtered[random_num, "clean_text"]
mastodon_dataset_filtered$uri[random_num]
# the results looks good, all the html and url links are removed, and there is no text and emoji missing.
# remove the value that is not useful
rm(long_text,random_num,sample_uri)

# 2. translate texts into english ####

## 2.1 google cloud translate API ####
if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")}
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if(!require("googleLanguageR")) {install.packages("googleLanguageR"); library("googleLanguageR")}
if(!require("googleAuthR")) {install.packages("googleAuthR"); library("googleAuthR")}
theme_set(theme_bw())
gl_auth("credential.json")
gl_auth("credential_2.json")

# detect the posts language and save as a new column
detected_languages <- lapply(mastodon_dataset_filtered$clean_text, function(text) {
  result <- gl_translate_detect(text)
  return(result$language)
})


# Create a new column to store the detected languages
mastodon_dataset_filtered$detected_language <- unlist(detected_languages)
mastodon_dataset_filtered$gl_detect_lang <- mastodon_dataset_filtered$detected_language
mastodon_dataset_filtered$detected_language <- NULL
rm(detected_languages)

# translate other languages to english
mastodon_dataset_filtered$translated_text <- NA

# Translate non-English text to English and save in the new column with google translator

# check quality of small sample
for (i in 11:25) {
  if (mastodon_dataset_filtered$gl_detect_lang[i]!= "en"){
   translation <- gl_translate(mastodon_dataset_filtered$clean_text[i],target = "en")
    mastodon_dataset_filtered$gl_trans_text[i] <- translation$translatedText
  }
}

# get the uri and compare with the results
mastodon_dataset_filtered$uri[11:25]
# ===================== some of the results are broken, google translator ignored some texts, text missing!!! ===================== 

# but let's translate more
for (i in 1:nrow(mastodon_dataset_filtered)) {
  if (mastodon_dataset_filtered$gl_detect_lang[i]!= "en"){
    translation <- gl_translate(mastodon_dataset_filtered$clean_text[i],target = "en")
    mastodon_dataset_filtered$gl_trans_text[i] <- translation$translatedText
  }
}

# check how many languages
length(unique(mastodon_dataset_filtered$gl_detect_lang))

# check how many text that Google could translate
table(is.na(mastodon_dataset_filtered$gl_trans_text))


# 3 Save the dataset ####

# check the current columns
colnames(mastodon_dataset_filtered)

# create a new dataset as the research dataset with related data
mastodon_primary <- mastodon_dataset_filtered %>%
  select(id, created_at,url,reblogs_count, favourites_count, replies_count, in_reply_to_id,
         in_reply_to_account_id, account, reblog, mentions, tags, favourited,reblogged,
         muted, bookmarked, pinned, request_tag, account_id, instance, clean_text, 
         gl_detect_lang, gl_trans_text)



# save the dataset
# save current workspace
save.image("data\\6_mastodon_primary_research_data.RData")
