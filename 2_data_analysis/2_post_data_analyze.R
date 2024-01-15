rm(list = ls())

library(areaplot)
library(arrow)
library(dplyr)
library(ggplot2)
library(scales)
library(ggstream)
library(forcats)
library(tidyr)

# read post data
# Specify the path to your Feather file
feather_file_path <- "data\\9_mastodon_post_data.feather"
# Read data from the Feather file
post_df <- arrow::read_feather(feather_file_path)

# read topic data
csv_file_path <- "data\\topic_results.csv"

# Read data from the CSV file
topics <- read.csv(csv_file_path,colClasses = "character")

# remove the unnecessary column
topics$X <- NULL


post_topic <- merge(post_df, topics, by = "id", all = TRUE)
table(post_topic$topic_id)

post_topic <- post_topic %>%
  mutate(topic_name = case_when(
    topic_id == "0" ~ "AI & Big Tech",
    topic_id == "1" ~ "Ask ChatGPT to Generate Text",
    topic_id == "2" ~ "GPT Language Models",
    topic_id == "3" ~ "Coding with ChatGPT",
    topic_id == "4" ~ "Twitter & Mastodon",
    topic_id == "5" ~ "AI-Related Media Content",
    topic_id == "6" ~ "Alternative Access to ChatGPT",
    topic_id == "7" ~ "ChatGPT Answers' Quality",
    topic_id == "8" ~ "AI & Law & Trial",
    topic_id == "9" ~ "Political Topics",
    topic_id == "10" ~ "Data Privacy Issues",
    topic_id == "11" ~ "Paid GPT Service",
    topic_id == "-1" ~ "Outlier"
  ))

sort(table(post_topic$topic_name))
final_post_data <- post_topic

# load sentiment results
csv_file_path <- "data\\1_sentiment_results.csv"

# Read data from the CSV file
sentiment <- read.csv(csv_file_path,colClasses = "character")

# remove the unnecessary column
sentiment$gl_detect_lang <- NULL
colnames(final_post_data)[colnames(final_post_data) == "id"] <- "post_id"

final_post_data <- merge(final_post_data, sentiment, by = "post_id", all.x = TRUE)

rm(post_df,post_topic,sentiment,topics)

# check the text of topic ####

final_post_data[final_post_data$topic_name=="Ask ChatGPT to Generate Text","text_combine"]

# Build order dataset for topic, language, sentiment ####

## topic order dataset ####
# calculate the prevalence of topics
order_topic <- table(final_post_data$topic_name)
order_topic <- sort(order_topic,decreasing = TRUE)  # Sort in descending order

# Convert order_topic back to a data frame
order_topic <- as.data.frame(order_topic)
names(order_topic) <- c("topic_name", "post_count_per_topic")
order_topic$topic_percent_overall <- order_topic$post_count_per_topic / sum(order_topic$post_count_per_topic) * 100

order_topic <- order_topic[order(order_topic$topic_percent_overall, decreasing = TRUE), ]
order_topic <- order_topic %>%
  filter(topic_name != "Outlier")


## language order dataset ####
order_lang <- table(final_post_data$language_type)
order_lang <- sort(order_lang,decreasing = TRUE)  # Sort in descending order

# Convert order_lang back to a data frame
order_lang <- as.data.frame(order_lang)
names(order_lang) <- c("language_type", "post_count_per_lang")

order_lang <- order_lang[order(order_lang$post_count_per_lang, decreasing = TRUE), ]

# Keep the top 10 rows
order_lang <- order_lang[1:10,]


## sentiment order dataset ####
order_senti <- table(final_post_data$sentiment_class)
order_senti <- sort(order_senti,decreasing = TRUE)  # Sort in descending order

# Convert order_senti back to a data frame
order_senti <- as.data.frame(order_senti)
names(order_senti) <- c("sentiment_class", "post_count_per_senti")

order_senti <- order_senti[order(order_senti$post_count_per_senti, decreasing = TRUE), ]
order_senti$senti_percent_overall <- order_senti$post_count_per_senti / sum(order_senti$post_count_per_senti) * 100



# topic analysis ####
## topic overview: prevalence of each topic ####

sort(table(final_post_data$topic_name), decreasing = TRUE)

final_post_data$post_created_at <- as.POSIXct(final_post_data$post_created_at)

# Calculate the number of posts per topic
posts_per_topic <- final_post_data %>%
  group_by(post_created_at, topic_name) %>%
  summarise(num_posts = n(), .groups = "drop")

posts_per_topic <- posts_per_topic %>%
  filter(topic_name != "Outlier")


posts_per_topic <- left_join(posts_per_topic, order_topic, by = "topic_name")

# make plot
ggplot(posts_per_topic, aes(x = post_created_at, y = num_posts, fill = fct_reorder(topic_name, post_count_per_topic,.desc=TRUE))) +
  geom_stream(type = "proportional",color = 1, lwd = 0.25,bw = 1)+
  theme_minimal(base_size = 17)+
  guides(fill = guide_legend(title = "Topic & Overall Prevalence"))+
  labs(x = "Date", y = "Proportion of Topics")+
  scale_fill_manual(values = c("AI & Big Tech" = "#1f78b4", 
                               "Ask ChatGPT to Generate Text" ="#33a02c", 
                               "Coding with ChatGPT" = "#e31a1c",
                               "ChatGPT Answers' Quality" = "#ff7f00",
                               "GPT Language Models" = "#6a3d9a",
                               "Twitter & Mastodon" = "#a6cee3",
                               "Alternative Access to ChatGPT" = "#b2df8a",
                               "AI-Related Media Content" = "#fb9a99",
                               "Paid GPT Service" = "#fdbf6f",
                               "AI & Law & Trial" = "#cab2d6",
                               "Data Privacy Issues" = "#ffff99",
                               "Political Topics" = "#636363"
                               ),
                    labels = paste0(order_topic$topic_name, ": ", round(order_topic$topic_percent_overall, 1), "% (", order_topic$post_count_per_topic, " posts)"))+
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "2 month") + 
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.title = element_text(size = 20),  # Adjust legend title size
    legend.key.height = unit(2, "lines"),    # Adjust the height of the legend key
    legend.key.width = unit(2, "lines"),
    plot.caption = element_text(size = 15, hjust = 1),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 18),  # Adjust font size here
    axis.text.y = element_text(size = 18)
  )
                    
                

## topic & top 10 languages  ####

### keep the top 10 languages topics data ####  
top_10_languages_topic <- final_post_data[final_post_data$language_type %in% order_lang$language_type, ]
top_10_languages_topic <- top_10_languages_topic %>%
  group_by(language_type, topic_name) %>%
  summarise(num_posts = n(), .groups = "drop") 

### remove the outlier ####
top_10_languages_topic <- top_10_languages_topic %>%
  filter(topic_name != "Outlier")

### combine with topic order data order_topic ####
top_10_languages_topic <- left_join(top_10_languages_topic, order_topic, by = "topic_name")
### combine top 10 lang & topic dfs: 
top_10_languages_topic <- left_join(top_10_languages_topic, order_lang, by = "language_type")



ggplot(top_10_languages_topic, aes(fill = fct_reorder(topic_name, post_count_per_topic,.desc=TRUE), y=num_posts, 
                                   x=fct_reorder(language_type,post_count_per_lang,.desc=TRUE))) + 
  geom_bar(position="fill", stat="identity")+
  
  scale_fill_manual(values = c("AI & Big Tech" = "#1f78b4", 
                               "Ask ChatGPT to Generate Text" ="#33a02c", 
                               "Coding with ChatGPT" = "#e31a1c",
                               "ChatGPT Answers' Quality" = "#ff7f00",
                               "GPT Language Models" = "#6a3d9a",
                               "Twitter & Mastodon" = "#a6cee3",
                               "Alternative Access to ChatGPT" = "#b2df8a",
                               "AI-Related Media Content" = "#fb9a99",
                               "Paid GPT Service" = "#fdbf6f",
                               "AI & Law & Trial" = "#cab2d6",
                               "Data Privacy Issues" = "#ffff99",
                               "Political Topics" = "#636363"
  ))+
  labs(x = "Language**",
       y = "Proportion of Topics",
       fill = "Topic*")+
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.title = element_text(size = 20),  # Adjust legend title size
    legend.key.height = unit(2, "lines"),    # Adjust the height of the legend key
    legend.key.width = unit(2, "lines"),
    plot.caption = element_text(size = 15, hjust = 1),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 18),  # Adjust font size here
    axis.text.y = element_text(size = 18)
  )

  
  
    
# sentiment analysis ####

posts_per_sentiment_topic_lang <- final_post_data %>%
  group_by(post_created_at, sentiment_class,topic_name,language_type) %>%
  summarise(num_posts = n(), .groups = "drop")


## sentiment changing over time ####

senti_pre_overtime <- posts_per_sentiment_topic_lang  %>%
  group_by(post_created_at, sentiment_class) %>%
  summarise(n = sum(num_posts)) %>%
  mutate(percentage = n / sum(n))

senti_overall <- posts_per_sentiment_topic_lang  %>%
  group_by(sentiment_class) %>%
  summarise(n = sum(num_posts)) %>%
  mutate(perct_overall = n / sum(n))

senti_overall$senti_count_overall <- senti_overall$n 
senti_overall$n <- NULL

senti_overall$perct_overall<- senti_overall$perct_overall*100

color_palette <- brewer.pal(n = nrow(senti_overall), name = "Set1")
color_palette <- rev(color_palette)

senti_pre_overtime <- left_join(senti_pre_overtime, order_senti, by = "sentiment_class")
senti_pre_overtime <- left_join(senti_pre_overtime, senti_overall, by = "sentiment_class")

# Plot
ggplot(senti_pre_overtime, aes(x=post_created_at, y=percentage, 
                               fill=fct_reorder(sentiment_class, post_count_per_senti,.desc=TRUE))) + 
  geom_area(alpha=1 , size=0.5, colour="black")+
  theme_minimal(base_size = 17) +
  guides(fill = guide_legend(title = "Sentiment & Overall Prevalence")) +
  labs(x = "Date", y = "Proportion of Sentiments") +
  scale_fill_manual(values = color_palette,labels = paste0(order_senti$sentiment_class, ": ", round(order_senti$senti_percent_overall, 1), "% (", order_senti$post_count_per_senti, " posts)")) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "2 month") + 
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.title = element_text(size = 20),  # Adjust legend title size
    legend.key.height = unit(2, "lines"),    # Adjust the height of the legend key
    legend.key.width = unit(2, "lines"),
    plot.caption = element_text(size = 15, hjust = 1),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 18),  # Adjust font size here
    axis.text.y = element_text(size = 18)
  )


### check which day had more postitive, negative posts ####

# Group by post_created_at and sentiment_class, then summarize the count
summary_data <- senti_pre_overtime %>%
  group_by(post_created_at, sentiment_class) %>%
  summarize(n = sum(n))

# Create a new data frame with a column indicating if Negative posts are greater
result_data_negative <- summary_data %>%
  pivot_wider(names_from = sentiment_class, values_from = n, values_fill = 0) %>%
  mutate(Negative_Greater_Pos = Negative > Positive)

# Filter the days where Negative posts are greater than Positive + Neutral
days_with_more_negative <- result_data_negative %>%
  filter(Negative_Greater_Pos) %>%
  select(post_created_at)

# Print the result
print(days_with_more_negative)

# positive more than neutral
result_data_posi <- summary_data %>%
  pivot_wider(names_from = sentiment_class, values_from = n, values_fill = 0) %>%
  mutate(Positive_Greater_Neu = Positive > Neutral)

# Filter the days where Negative posts are greater than Positive + Neutral
days_with_more_posi <- result_data_posi %>%
  filter(Positive_Greater_Neu) %>%
  select(post_created_at)

# Print the result
print(days_with_more_negative)



## sentiment_per_topic ####


posts_per_sentiment_topic <- posts_per_sentiment_topic_lang 
posts_per_sentiment_topic$language_type <- NULL
posts_per_sentiment_topic$post_created_at <- NULL

posts_per_sentiment_topic <- posts_per_sentiment_topic  %>%
  filter(topic_name != "Outlier") %>%
  group_by(sentiment_class,topic_name) %>%
  summarise(num_posts = sum(num_posts), .groups = "drop")

topic_post_count <- posts_per_sentiment_topic %>% group_by(topic_name) %>% summarise(topic_post_count= sum(num_posts))
posts_per_sentiment_topic <- merge(posts_per_sentiment_topic, topic_post_count, by="topic_name")
posts_per_sentiment_topic$topic_sent_pct <- round(posts_per_sentiment_topic$num_posts / posts_per_sentiment_topic$topic_post_count,3)

posts_per_sentiment_topic <- left_join(posts_per_sentiment_topic, order_senti, by = "sentiment_class")
posts_per_sentiment_topic <- left_join(posts_per_sentiment_topic,  order_topic, by = "topic_name")


ggplot(posts_per_sentiment_topic, aes(
  x = fct_reorder(topic_name, post_count_per_topic,.desc=TRUE), 
  y = topic_sent_pct, 
  fill = fct_reorder(sentiment_class, post_count_per_senti,.desc=TRUE))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", topic_sent_pct*100), y = topic_sent_pct),
            position = position_stack(vjust = 0.5),size = 7) +
  scale_fill_manual(values = color_palette) +
  labs(x = "Topic**",
       y = "Proportion of Sentiments",
       fill = "Sentiment*") +
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.title = element_text(size = 20),  # Adjust legend title size
    legend.key.height = unit(2, "lines"),    # Adjust the height of the legend key
    legend.key.width = unit(2, "lines"),
    plot.caption = element_text(size = 15, hjust = 1),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 25),  # Adjust font size here
    axis.text.y = element_text(size = 25)
  )


## sentiment per language ####

# build sentiment and language dataset
posts_per_sentiment_lang <- posts_per_sentiment_topic_lang %>%
  group_by(sentiment_class,language_type) %>%
  summarise(num_posts = sum(num_posts), .groups = "drop")

# select top 10 language
posts_per_sentiment_lang <- posts_per_sentiment_lang[posts_per_sentiment_lang$language_type %in% order_lang$language_type, ]



lang_post_count <- posts_per_sentiment_lang %>% group_by(language_type) %>% summarise(lang_post_count= sum(num_posts))
posts_per_sentiment_lang <- merge(posts_per_sentiment_lang, lang_post_count, by="language_type")
posts_per_sentiment_lang$lang_sent_pct <- round(posts_per_sentiment_lang$num_posts / posts_per_sentiment_lang$lang_post_count,3)

posts_per_sentiment_lang <- left_join(posts_per_sentiment_lang,order_lang, by="language_type")
posts_per_sentiment_lang <- left_join(posts_per_sentiment_lang,order_senti, by="sentiment_class")

ggplot(posts_per_sentiment_lang, aes(x = fct_reorder(language_type, post_count_per_lang,.desc=TRUE),y = lang_sent_pct, 
                                     fill = fct_reorder(sentiment_class, post_count_per_senti,.desc=TRUE))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", lang_sent_pct*100), y = lang_sent_pct),
            position = position_stack(vjust = 0.5),size = 7) +
  scale_fill_manual(values = color_palette) +
  labs(x = "Language**",
       y = "Proportion of Sentiments",
       fill = "Sentiment*") +
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 20),  # Adjust legend text size
    legend.title = element_text(size = 20),  # Adjust legend title size
    legend.key.height = unit(2, "lines"),    # Adjust the height of the legend key
    legend.key.width = unit(2, "lines"),
    plot.caption = element_text(size = 15, hjust = 1),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 25),  # Adjust font size here
    axis.text.y = element_text(size = 25)
  )



