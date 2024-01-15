# get all the hashtags contain "chatgpt" on 10.01 16:00 - 16:30
# search hashtag "chatgpt" on mastodon and load all the results
# inspect the elements of searching results webpage and save as a html file

import csv
# get all the hashtags contain "chatgpt" from the searching webpage on Mastodon
from bs4 import BeautifulSoup

# load the html file
input_file_path = "1_data_collection\data\\0_hashtag_search_results_webpage.html"
output_file_path = "1_data_collection\data\\1_hashtag_list.csv"


with open(input_file_path, 'r', encoding="utf8") as f:
    page = f.read()

soup = BeautifulSoup(page, "html.parser")

trends_item_name_tags = soup.findAll("div", {"class": "trends__item__name"})

hashtag_list = []
substring= "chatgpt"

for hashtag_node in trends_item_name_tags:
    hashtag = hashtag_node.select("a")[0]["href"]
    hashtag = hashtag.replace("/tags/", "")
    if substring in hashtag.lower():
        hashtag_list.append(hashtag)
    
print(hashtag_list)
print(len(hashtag_list))

with open(output_file_path, "w", encoding="utf8") as f:
    write = csv.writer(f)
    write.writerow(hashtag_list)
