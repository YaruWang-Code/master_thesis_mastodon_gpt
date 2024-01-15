# preparation & load data ####
rm(list = ls())

library(arrow)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(forcats)
library(rcompanion)

# load data ####

# top nodes data
feather_file_path <- "data\\10_top_nodes_data.feather"
top_nodes_data <- arrow::read_feather(feather_file_path)

# post data
feather_file_path <- "data\\9_final__mastodon_post_data.feather"
post_data <- arrow::read_feather(feather_file_path)

# account data
feather_file_path <- "data\\9_mastodon_account_data.feather"
account_data <- arrow::read_feather(feather_file_path)
account_data <- account_data[!duplicated(account_data$account_id), ]

# central network data
feather_file_path <- "data\\10_central_network_data.feather"
central_network <- arrow::read_feather(feather_file_path)

# account description data
feather_file_path <- "data\\3_acct_descr_topics.feather"
account_descri <- arrow::read_feather(feather_file_path)

# community membership data
feather_file_path <- "data\\11_community_membership.feather"
communi_membership <- arrow::read_feather(feather_file_path)

# user interaction account
feather_file_path <- "data\\9_mastodon_user_interaction_data.feather"
user_interaction <- arrow::read_feather(feather_file_path)


# save datasets
write.csv(user_interaction, "data\\1201_user_interaction.csv", row.names=FALSE)

post_data_1201 <- post_data %>%
  select(post_id,reblogs_count,favourites_count,replies_count,account_id,
         instance,language_type,text_combine,post_created_at)
write.csv(post_data_1201, "data\\1201_post_data.csv", row.names=FALSE)
account_data_1201 <- account_data %>%
  select(account_id,account_descri,instance,account_locked,account_discoverable,
         account_group,account_created_at,account_followers_count,account_following_count,
         account_statuses_count)
write.csv(account_data_1201, "data\\1201_account_data.csv", row.names=FALSE)


# 0. check who interacted with top nodes ####
# user 1
list_node_target <- top_nodes_data$NodeID[1]
list_node <- user_interaction[user_interaction$target==list_node_target,source]
result <- list_node %in% top_nodes_data$NodeID
list_node_source <- list_node[result]
which(top_nodes_data == list_node_source, arr.ind = TRUE)
user_interaction[user_interaction$source== list_node_source & user_interaction$target == list_node_target,"action"]

# user 2
list_node_target <- top_nodes_data$NodeID[2]
list_node <- user_interaction[user_interaction$target==list_node_target,source]
result <- list_node %in% top_nodes_data$NodeID
list_node_source <- list_node[result]
which(top_nodes_data == list_node_source, arr.ind = TRUE)
user_interaction[user_interaction$source== list_node_source & user_interaction$target == list_node_target,"action"]

# user 3
list_node_target <- top_nodes_data$NodeID[3]
list_node <- user_interaction[user_interaction$target==list_node_target,source]
result <- list_node %in% top_nodes_data$NodeID
list_node_source <- list_node[result]
list_node_source
which(top_nodes_data == list_node_source, arr.ind = TRUE)
user_interaction[user_interaction$source== list_node_source & user_interaction$target == list_node_target,"action"]

# user 4
list_node_target <- top_nodes_data$NodeID[4]
list_node <- user_interaction[user_interaction$target==list_node_target,source]
result <- list_node %in% top_nodes_data$NodeID
list_node_source <- list_node[result]
list_node_source
which(top_nodes_data == list_node_source, arr.ind = TRUE)
user_interaction[user_interaction$source== list_node_source & user_interaction$target == list_node_target,"action"]

# user 5
list_node_target <- top_nodes_data$NodeID[5]
list_node <- user_interaction[user_interaction$target==list_node_target,source]
result <- list_node %in% top_nodes_data$NodeID
list_node_source <- list_node[result]
list_node_source
which(top_nodes_data == list_node_source, arr.ind = TRUE)
user_interaction[user_interaction$source== list_node_source & user_interaction$target == list_node_target,"action"]

# user 6
list_node_target <- top_nodes_data$NodeID[6]
list_node <- user_interaction[user_interaction$target==list_node_target,source]
result <- list_node %in% top_nodes_data$NodeID
list_node_source <- list_node[result]
list_node_source
which(top_nodes_data == list_node_source, arr.ind = TRUE)
user_interaction[user_interaction$source== list_node_source & user_interaction$target == list_node_target,"action"]

# user 7
list_node_target <- top_nodes_data$NodeID[7]
list_node <- user_interaction[user_interaction$target==list_node_target,source]
result <- list_node %in% top_nodes_data$NodeID
list_node_source <- list_node[result]
list_node_source
which(top_nodes_data == list_node_source, arr.ind = TRUE)
user_interaction[user_interaction$source== list_node_source & user_interaction$target == list_node_target,"action"]

# user 8
list_node_target <- top_nodes_data$NodeID[8]
list_node <- user_interaction[user_interaction$target==list_node_target,source]
result <- list_node %in% top_nodes_data$NodeID
list_node_source <- list_node[result]
list_node_source
which(top_nodes_data == list_node_source, arr.ind = TRUE)
user_interaction[user_interaction$source== list_node_source & user_interaction$target == list_node_target,"action"]


# user 9
user_interaction[user_interaction$target=="484383",source] %in% top_nodes_data$NodeID
# user 10
user_interaction[user_interaction$target==top_nodes_data$NodeID[10],source] %in% top_nodes_data$NodeID
# user 11
user_interaction[user_interaction$target==top_nodes_data$NodeID[11],source] %in% top_nodes_data$NodeID
# user 12
user_interaction[user_interaction$target==top_nodes_data$NodeID[12],source] %in% top_nodes_data$NodeID
# user 13
user_interaction[user_interaction$target==top_nodes_data$NodeID[13],source] %in% top_nodes_data$NodeID
# user 14
user_interaction[user_interaction$target==top_nodes_data$NodeID[14],source] %in% top_nodes_data$NodeID
# user 15
user_interaction[user_interaction$target==top_nodes_data$NodeID[15],source] %in% top_nodes_data$NodeID
# user 16
user_interaction[user_interaction$target==top_nodes_data$NodeID[16],source] %in% top_nodes_data$NodeID
# user 17
user_interaction[user_interaction$target==top_nodes_data$NodeID[17],source] %in% top_nodes_data$NodeID
# user 18
user_interaction[user_interaction$target==top_nodes_data$NodeID[18],source] %in% top_nodes_data$NodeID
# user 19
list_node_target <- top_nodes_data$NodeID[19]
list_node <- user_interaction[user_interaction$target==list_node_target,source]
result <- list_node %in% top_nodes_data$NodeID
list_node_source <- list_node[result]
list_node_source
which(top_nodes_data == list_node_source, arr.ind = TRUE)
user_interaction[user_interaction$source== list_node_source & user_interaction$target == list_node_target,"action"]

# user 20
user_interaction[user_interaction$target==top_nodes_data$NodeID[20],source] %in% top_nodes_data$NodeID

user_interaction[user_interaction$target==top_nodes_data$NodeID[19],source]

# 1. description analysis of accounts ####

table(is.na(account_descri$account_descri))
table(account_descri$acct_descri_topic)

account_descri_no_empty <- account_descri %>%
  filter(account_descri!="")

account_descri_no_empty <- account_descri_no_empty %>%
  filter(trl_account_descri!="")

round(nrow(account_descri_no_empty)/nrow(account_descri)*100,1)

text_descri <- account_descri_no_empty[account_descri_no_empty$acct_descri_topic=="0", "trl_account_descri"]
print(text_descri,n=20)

table(account_descri_no_empty$acct_descri_topic)


# 2. top 20 nodes analysis ####

top_nodes_head <- head(top_nodes_data,20)

top_nodes_posts <- post_data[post_data$account_id %in% top_nodes_head$NodeID, ]
length(unique(top_nodes_posts$account_id))

length(unique(top_nodes_posts$instance))

top_nodes_account <- account_data[account_data$account_id %in% top_nodes_head$NodeID, ]

# check the top 10 topic, sentiment, language
analy_top_nodes_posts <- top_nodes_posts %>%
  group_by(account_id,instance,language_type,topic_name,sentiment_class) %>%
  summarise(topic_sent_num_posts = n(), .groups = "drop")

analy_top_nodes_account <- top_nodes_posts %>%
  group_by(account_id) %>%
  summarise(num_posts = n(), sum_reblogs_count=sum(reblogs_count), sum_favourites_count=sum(favourites_count), sum_replies_count=sum(replies_count), .groups = "drop")
analy_top_nodes_account <- merge(analy_top_nodes_account, top_nodes_head, by.x="account_id", by.y = "NodeID")
analy_top_nodes_account <- merge(analy_top_nodes_account, top_nodes_account[, c("account_id", "account_followers_count", "account_url",
                                                                                "account_following_count", "account_statuses_count",
                                                                                "account_locked","account_discoverable")], by="account_id")

analy_top_nodes_acc_post <- left_join(analy_top_nodes_account,analy_top_nodes_posts,by="account_id")
analy_top_nodes_acc_post <- analy_top_nodes_acc_post[order(analy_top_nodes_acc_post$PagerankScore, decreasing = TRUE),]


# 3. community membership analysis ####

## build dataset ####
# get the post data of account with membership
node_membership <- post_data[post_data$account_id %in% communi_membership$name, ]

# combine the post data and membership data
node_membership <- merge(node_membership, communi_membership, by.x="account_id",by.y="name") 

# check the correlation and regression between membership and topics, sentiments

node_membership_statis <- node_membership[,c("account_id","instance","topic_id","topic_name",
                                             "Neutral","Positive","Negative","sentiment_class",
                                             "membership")]

# convert sentiment and topic to numeric value
node_membership_statis <- node_membership_statis %>%
  mutate(sentiment_class = case_when(
    sentiment_class == "Negative" ~ "0",
    sentiment_class == "Neutral" ~ "1",
    sentiment_class == "Positive" ~ "2"))

node_membership_statis$topic_id <- as.numeric(node_membership_statis$topic_id)

length(unique(node_membership_statis$instance))

# convert instance name to numeric number
instance_dataset <- node_membership_statis %>%
  select(instance) %>%
  group_by(instance) %>%
  mutate(count_column = n())

instance_dataset <- instance_dataset[!duplicated(instance_dataset$instance), ]
instance_dataset <- instance_dataset[order(-instance_dataset$count_column), ]
instance_dataset$instance_number <- row_number(instance_dataset)
instance_dataset$instance_number <- as.character(instance_dataset$instance_number)

length(unique(instance_dataset$instance_number))

node_membership_statis <- left_join(node_membership_statis, instance_dataset, by = "instance")

length(unique(node_membership_statis$instance_number))

node_membership_statis$membership <- as.character(node_membership_statis$membership)


## statistics analysis ####

### instance and membership ####
instance_member <- node_membership_statis %>%
  select(instance,membership) %>%
  count(instance,membership)

instance_member <- instance_member %>%
  pivot_wider(names_from = membership, values_from = n, values_fill = 0)

instance_member_mat <- as.matrix(instance_member[,-1])
row.names(instance_member_mat) <- as.matrix(instance_member[,1])

# chi-square test
Xsq <- chisq.test(instance_member_mat)
Xsq
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals

#calculate Cramer's V
cramerV(instance_member_mat)

### instance with topic ####

instance_topic <- node_membership_statis %>%
  select(instance,topic_name) %>%
  count(instance,topic_name)

instance_topic <- instance_topic %>%
  pivot_wider(names_from = topic_name, values_from = n, values_fill = 0)

instance_topic_mat <- as.matrix(instance_topic[,-1])
row.names(instance_topic_mat) <- as.matrix(instance_topic[,1])

# chi-square test
Xsq <- chisq.test(instance_topic_mat)
Xsq
Xsq$expected

#calculate Cramer's V
cramerV(instance_topic_mat)

### instance with sentiment ####

instance_sentiment <- node_membership_statis %>%
  select(instance,sentiment_class) %>%
  count(instance,sentiment_class)

instance_sentiment <- instance_sentiment %>%
  pivot_wider(names_from = sentiment_class, values_from = n, values_fill = 0)

instance_senti_mat <- as.matrix(instance_sentiment[,-1])
row.names(instance_senti_mat) <- as.matrix(instance_sentiment[,1])

# chi-square test
Xsq <- chisq.test(instance_senti_mat)
Xsq
Xsq$expected

#calculate Cramer's V
cramerV(instance_senti_mat)

### membership with topic ####

member_topic <- node_membership_statis %>%
  select(membership,topic_name) %>%
  count(membership,topic_name)

member_topic <- member_topic %>%
  pivot_wider(names_from = topic_name, values_from = n, values_fill = 0)

member_topic_mat <- as.matrix(member_topic[,-1])
row.names(member_topic_mat) <- as.matrix(member_topic[,1])

# chi-square test
Xsq <- chisq.test(member_topic_mat)
Xsq
Xsq$expected

#calculate Cramer's V
cramerV(member_topic_mat)

### membership with sentiment ####

member_senti <- node_membership_statis %>%
  select(membership,sentiment_class) %>%
  count(membership,sentiment_class)

member_senti <- member_senti %>%
  pivot_wider(names_from = sentiment_class, values_from = n, values_fill = 0)

member_senti_mat <- as.matrix(member_senti[,-1])
row.names(member_senti_mat) <- as.matrix(member_senti[,1])

# chi-square test
Xsq <- chisq.test(member_senti_mat)
Xsq
Xsq$expected

#calculate Cramer's V
cramerV(member_senti_mat)

### sentiemnt with topic ####

senti_topic <- node_membership_statis %>%
  select(sentiment_class,topic_name) %>%
  count(sentiment_class,topic_name)

senti_topic <- senti_topic %>%
  pivot_wider(names_from = topic_name, values_from = n, values_fill = 0)

senti_topic_mat <- as.matrix(senti_topic[,-1])
row.names(senti_topic_mat) <- as.matrix(senti_topic[,1])

# chi-square test
Xsq <- chisq.test(senti_topic_mat)
Xsq
Xsq$expected

#calculate Cramer's V
cramerV(senti_topic_mat)
