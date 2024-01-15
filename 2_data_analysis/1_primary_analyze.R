rm(list = ls())

input_path <- "../1_data_collection/data/7_mastodon_primary_data.RData"

load(input_path)

library(dplyr)
library(ggplot2)
library(scales)
library(igraph)
library(RColorBrewer)
library(arrow)
library(grid)
library(gridBase)


colnames(mastodon_final_data)
colnames(mastodon_primary)

# create final dataset for submitting####

post_df <- mastodon_final_data %>%
  select(id, url, reblogs_count,favourites_count,
         replies_count, account_id, instance,
         language_type,text_combine,post_created_at)

account_df <- mastodon_final_data %>%
  select(account_id,account_descri,instance,account_locked,
         account_discoverable,account_group,account_created_at,
         account_followers_count,account_following_count,
         account_statuses_count,account_url
        )

# account descriptive analysis ####

plot.new()

## setup and push viewport
par(mar= c(5, 10, 4, 2) + 0.1)
par(mfrow=c(1,3)) # Create a 1x3 layout for histograms
hist(account_df$account_following_count, main="Followings", 
     xlab="N of Following Accounts",
     ylab = "Frequency of Accounts",
     cex.lab=3, cex.axis=2.6, cex.main=4, cex.sub=1.5)

hist(account_df$account_followers_count, main="Followers", 
     xlab="N of Followers",
     ylab = "Frequency of Accounts",
     cex.lab=3, cex.axis=2.6, cex.main=4, cex.sub=1.5)
hist(account_df$account_statuses_count, main="Posts", 
     xlab="N of Posts",
     ylab = "Frequency of Accounts",
     cex.lab=3, cex.axis=2.6, cex.main=4, cex.sub=1.5)

round(sum(account_df$account_statuses_count < 2580)/nrow(account_df)*100,1)

round(sum(account_df$account_following_count == 32607)/nrow(account_df)*100,1)

max(account_df$account_following_count)




# 0.Dataset primary description ####


## number of languages in dataset ####


length(unique(mastodon_final_data$language_type))

sort(table(mastodon_final_data$language_type))



# 1. Posts primary analysis ####

## frequency of posts over days ####

# Convert 'creation_time' to a proper date format if it's not already in that format

mastodon_final_data$post_creation_time <- as.Date(mastodon_final_data$post_created_at, format = "%Y-%m-%d")



# Group the data by day and count the number of posts created on each day
posts_summary <- mastodon_final_data %>%
  group_by(post_creation_time) %>%
  summarise(posts_count = n())

posts_summary$year <- format(posts_summary$post_creation_time, "%Y")

# Create the plot with customized axis and legend labels ####
ggplot(posts_summary, aes(x = post_creation_time, y = posts_count)) +
  geom_line() +
  labs(x = "Date", y = "Number of Posts Each Day") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Adjust date labels and breaks
  theme_minimal(base_size = 35) +  # Adjust the theme as per your preference
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 35),  # Adjust font size here
        axis.text.y = element_text(size = 35),  # Adjust font size here
        )+
  annotate("text", x = as.Date("2023-03-10"), y = 842, label = "2022-12-05: ChatGPT reached 1 million users.", color = "blue",size = 10) +
  annotate("segment", x = as.Date("2022-12-25"), xend = as.Date("2022-12-05"), 
           y = 842, yend = 842, color = "blue", arrow = arrow(length = unit(0.7, "cm")), size = 0.7) +
  annotate("text", x = as.Date("2023-01-31"), y = 6, label = "2022-11-30: ChatGPT launched.", color = "blue",size = 10)+
  annotate("segment", x = as.Date("2022-12-07"), xend = as.Date("2022-11-30"), 
           y = 6, yend = 6, color = "blue", arrow = arrow(length = unit(0.7, "cm")), size = 0.7) +
  annotate("text", x = as.Date("2023-04-30"), y = 499, label = "2023-02-07: Microsoft released the new Bing, with ChatGPT built in.", 
           color = "blue",size = 10)+
  annotate("segment", x = as.Date("2023-02-07"), xend = as.Date("2023-02-07"), 
           y = 480, yend = 417, color = "blue", arrow = arrow(length = unit(0.7,"cm")), size = 0.7) +
  annotate("text", x = as.Date("2023-06-20"), y = 355, label = "2023-07-06: OpenAI announced the general availability of GPT-4.", 
           color = "blue",size = 10)+
  annotate("segment", x = as.Date("2023-07-06"), xend = as.Date("2023-07-06"), 
           y = 340, yend = 270, color = "blue", arrow = arrow(length = unit(0.7, "cm")), size = 0.7) 


posts_summary[posts_summary$post_creation_time=="2023-09-30",]
# checking the average posts daily.
sum(posts_summary$posts_count)/nrow(posts_summary)


"OpenAI announced the general availability of GPT-4 on July 6, 2023"

## distributation of posts in languages ####
data <- mastodon_final_data[,c("language_type")]

# Count the number of posts for each language
language_counts <- table(data$language_type)
language_counts <- sort(language_counts, decreasing = TRUE)  # Sort in descending order

# Convert language_counts back to a data frame
language_counts_df <- as.data.frame(language_counts)
names(language_counts_df) <- c("Language", "PostCount")
language_counts_df$Percentage <- language_counts_df$PostCount / sum(language_counts_df$PostCount) * 100

language_counts_df <- language_counts_df[order(language_counts_df$Percentage, decreasing = TRUE), ]

# Keep the top 10 rows
top_10 <- head(language_counts_df, 10)

# Calculate the sum of percentages for the "Others" category
others_percentage <- 100 - sum(top_10$Percentage)

# Create a data frame for the "Others" category with the same columns
others_data <- data.frame(Language = "Others", PostCount = sum(language_counts_df$PostCount[11:nrow(language_counts_df)]), Percentage = others_percentage)

# Combine the top 10 languages and the "Others" category
plot_data <- rbind(top_10, others_data)
color_palette <- brewer.pal(n = nrow(plot_data), name = "Set3")

ggplot(plot_data, aes(x = "", y = Percentage, fill = Language)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = color_palette,
                    labels = paste0(plot_data$Language, ": ", round(plot_data$Percentage, 1), "% (", plot_data$PostCount, " posts)"),
                    name = "Languages and Prevalence") +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),  # Adjust legend text size
        legend.title = element_text(size = 18),  # Adjust legend title size
        legend.key.height = unit(2, "lines"),    # Adjust the height of legend key
        legend.key.width = unit(2, "lines"),
        plot.caption = element_text(size = 15)) 



## check prevalence of hashtags ####
sort(table(mastodon_dataset_filtered$request_tag))


# 2. User primary analysis ####

## number of unique account ####
length(unique(mastodon_final_data$account_id))

## user creation time trend ####

# Convert 'creation_time' to a proper date format if it's not already in that format

mastodon_final_data$user_creation_time <- as.Date(mastodon_final_data$account_created_at, format = "%Y-%m-%d")



# Group the data by day and count the number of posts created on each day
account_unique <- mastodon_final_data[, c("account_id","user_creation_time")]
account_unique <- account_unique[!duplicated(account_unique$account_id), ]


account_summary <- account_unique %>%
  group_by(user_creation_time) %>%
  summarise(account_count=n())

account_summary$year <- format(account_summary$user_creation_time, "%Y")

# Create the plot with customized axis and legend labels ####
ggplot(account_summary, aes(x = user_creation_time, y = account_count)) +
  geom_line() +
  labs(x = "Date", y = "New Accounts Per Day") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +  # Adjust date labels and breaks
  theme_minimal(base_size = 35) +  # Adjust the theme as per your preference
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 35),  # Adjust font size here
        axis.text.y = element_text(size = 35)) + # Adjust font size here)
  annotate("text", x = as.Date("2019-10-01"), y = 284, label = "2022-04-26: Twitter accepted Elon Musk's deal the day before.", color = "blue",size = 10) +
  annotate("segment", x = as.Date("2022-02-01"), xend = as.Date("2022-04-20"), y = 284, yend = 284, color = "blue", arrow = arrow(length = unit(0.7, "cm"))) +
  annotate("text", x = as.Date("2020-06-15"), y = 524, label = "2022-10-28: Elon Musk completed Twitter takeover.", color = "blue",size = 10) +
  annotate("segment", x = as.Date("2022-06-01"), xend = as.Date("2022-10-25"), y = 524, yend = 524, color = "blue", arrow = arrow(length = unit(0.7, "cm"))) +
  annotate("text", x = as.Date("2019-07-15"), y = 622, label = "2022-11-06: Twitter launched paid subscription service Twitter Blue the day before.",color = "blue",size = 10) +
  annotate("segment", x = as.Date("2022-09-01"), xend = as.Date("2022-11-01"), y = 622, yend = 622, color = "blue", arrow = arrow(length = unit(0.7, "cm")))

# how many people joined since 2022-04-26
round(sum(account_summary[account_summary$user_creation_time>"2022-04-25","account_count"])/sum(account_summary$account_count)*100,1)

# try to know the biggest number of creation between 2022 apr to june.
max(account_summary[
  (account_summary$user_creation_time > "2022-04-01") & (account_summary$user_creation_time < "2022-06-30"), 
                "account_count"])
# know the data
account_summary[
  ((account_summary$user_creation_time > "2022-04-01") & (account_summary$user_creation_time < "2022-06-30") &
     account_summary$account_count==284), "user_creation_time"]
account_summary[account_summary$user_creation_time=="2022-04-25", "account_count"]

# try to know the biggest number of creation in 2022 october.
max(account_summary[
  (account_summary$user_creation_time > "2022-10-01") & (account_summary$user_creation_time < "2022-11-01"), 
  "account_count"])
# know the date
account_summary[account_summary$account_count=="524","user_creation_time"]

# try to know the biggest number of creation between 2022 novemeber to novemeber 6
max(account_summary[
  (account_summary$user_creation_time > "2022-11-01") & (account_summary$user_creation_time < "2022-11-15"), 
  "account_count"])
# know the data
account_summary[
  ((account_summary$user_creation_time > "2022-11-01") & (account_summary$user_creation_time < "2022-11-15") &
     account_summary$account_count==622), "user_creation_time"]

# try to know the biggest number of all time.
account_summary[account_summary$account_count==max(account_summary$account_count),"user_creation_time"]



# 3.Topic modeling ####

## decide which topic modelling model to use ####

# Function to count the number of words in a text
# Function to count words

count_words <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))  # Split the text into words
  return(length(words))  # Return the count of words
}

# Apply the function to the text_column

mastodon_final_data$word_count <- sapply(mastodon_final_data$text_combine, count_words)


ggplot(mastodon_final_data, aes(x = word_count)) +
  geom_histogram(aes(y = stat(count) / sum(stat(count))), 
                 binwidth = 5, fill = "skyblue", color = "black") + xlim(0, 500) +
  labs(x = "Word Count Per Post", y = "Percentage of Overall Posts",caption = "Data Source: Mastodon") +
  # scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal(base_size = 35) +  # Adjust the theme as per your preference
  theme(
    axis.text.x = element_text(size = 35),  # Adjust font size for x-axis labels
    axis.text.y = element_text(size = 35),  # Adjust font size for y-axis labels
    plot.title = element_text(size = 12, face = "bold"),  # Adjust font size and style for title
    plot.caption = element_text(size = 35),  # Adjust font size for caption
    plot.subtitle = element_text(size = 10)  # Adjust font size for subtitle
  )+
  geom_segment(aes(x = 256, xend = 256, y = 0, yend = 0.088), color = "red", linetype = "dashed", size = 1.7) +
  annotate("text", x = 256, y = 0.090, label = "256", color = "red",size = 10) 

  
# percentage of posts have less than 256 words
sum(mastodon_final_data$word_count<=256)
sum(mastodon_final_data$word_count<256) / nrow(mastodon_final_data) * 100


# topic modeling check
data <- mastodon_final_data[grepl("blurry", mastodon_final_data$text_combine), ]
data$text_combine


# 4. Account network prepare ####
## account description check ####
count_words <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))  # Split the text into words
  return(length(words))  # Return the count of words
}

# Apply the function to the text_column
# Translate non-English text to English

mastodon_final_data$word_count <- sapply(mastodon_final_data$account_descri, count_words)


ggplot(mastodon_final_data, aes(x = word_count)) +
  geom_histogram(aes(y = stat(count) / sum(stat(count))), 
                 binwidth = 5, fill = "skyblue", color = "black") + xlim(0, 200) +
  labs(x = "Word Count Per Post", y = "Percentage of Overall Posts",caption = "Data Source: Mastodon") +
  # scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal(base_size = 35) +  # Adjust the theme as per your preference
  theme(
    axis.text.x = element_text(size = 35),  # Adjust font size for x-axis labels
    axis.text.y = element_text(size = 35),  # Adjust font size for y-axis labels
    plot.title = element_text(size = 12, face = "bold"),  # Adjust font size and style for title
    plot.caption = element_text(size = 35),  # Adjust font size for caption
    plot.subtitle = element_text(size = 10)  # Adjust font size for subtitle
  )+
  geom_segment(aes(x = 256, xend = 256, y = 0, yend = 0.088), color = "red", linetype = "dashed", size = 1.7) +
  annotate("text", x = 256, y = 0.090, label = "256", color = "red",size = 10) 

## interaction dataset ####

# build reply dataset
inter_reply_df <- mastodon_final_data[,c("account_id","in_reply_to_account_id")]
inter_reply_df <- inter_reply_df[!is.na(inter_reply_df$in_reply_to_account_id),]
nrow(inter_reply_df)

# mention dataset
inter_mention_df <- full_mention_df[,c("account_id","id")]
nrow(inter_mention_df)

# change column names
new_colnames <- c("source", "target")
colnames(inter_reply_df) <- new_colnames
colnames(inter_mention_df) <- new_colnames

# add new column action record the actions
inter_reply_df$action <- "reply"
inter_mention_df$action <- "mention"

# combine to one dataset
user_interaction_df_self <- rbind(inter_mention_df, inter_reply_df)
nrow(user_interaction_df_self)

# check how many users self interacted
self_reply <- inter_reply_df %>%
  filter(source == target) 
length(unique(self_reply$source))

self_mention <- inter_mention_df %>%
  filter(source == target) 
length(unique(self_mention$source))

# check unique users
combined_vector <- c(user_interaction_df_self$source, user_interaction_df_self$target)

# Check unique values in the combined vector
unique_values_combined <- unique(combined_vector)
length(unique_values_combined)

## remove self interactions 
inter_reply_df_noself <- inter_reply_df %>%
  filter(source != target)
nrow(inter_reply_df_noself)
nrow(inter_reply_df)-nrow(inter_reply_df_noself)

inter_mention_df_noself <- inter_mention_df %>%
  filter(source != target)
nrow(inter_mention_df_noself)
nrow(inter_mention_df)-nrow(inter_mention_df_noself)


# combine to one dataset
user_interaction_df <- rbind(inter_mention_df_noself, inter_reply_df_noself)
nrow(user_interaction_df)

combined_vector <- c(user_interaction_df$source, user_interaction_df$target)

# Check unique values in the combined vector
unique_values_combined <- unique(combined_vector)
length(unique_values_combined)


# mention weight
mention_weight <- 1/(nrow(inter_mention_df)/nrow(user_interaction_df))
mention_weight

# reply weight
reply_weight <- 1/(nrow(inter_reply_df)/nrow(user_interaction_df))
reply_weight
