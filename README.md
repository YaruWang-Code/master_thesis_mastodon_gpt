# ChatGPT on Mastodon: Using Natural Language Processing and Network Analysis to Investigate Science Communication on Decentralized Platforms

The two folders are the code of data collection and data analysis. This research worked with R and Python.

In this thesis, Natural Language Processing (NLP) and Network Analysis were used to investigate the ChatGPT-related discourse on the decentralised platform Mastodon. The analysis considered both the content and user perspectives of the discourse. 50k+ Mastodon posts that used ChatGPT-related hashtags was collected and analysed, as well as the associated account information. For the content perspective, BERTopic was used for topic modeling, and the Twitter-roBERTa-base model for sentiment analysis. For the user analysis, network analysis methods like PageRank centrality and Walktrap community detection were used. Among the 12 topics identified, “AI & Big Tech” was the most popular topic across all languages (representing 50.6% of all the posts). The most common sentiment
of the posts was neutral, with positive sentiment being more common than negative. However, the prevalence of the topics differed between languages, as did the sentiment across both topics and languages. The most influential users were found to most commonly be related to academia, followed by tech and media. 66 communities were detected in the central interaction network, with a noticeable correlation between the communities and the Mastodon instances.


## Methodology of the research

Four research goals need to be achieved in this study: 1) identification of the main topics surrounding ChatGPT (RQ1), the evaluation of ChatGPT (RQ2), the influential users in the user network of ChatGPT sphere on Mastodon (RQ3), and the communities in ChatGPT discourse on Mastodon (RQ4). 

During analysis, an approach with mixed methodologies was applied. The researcher utilized computational methods, including natural language processing (also called automated content analysis) and network analysis, to extract relevant information from social media data on a large scale. Also, several qualitative analyses were conducted to supplement the research.

The following figure shows the workflow of this research.


*Figure 1 Workflow of ChatGPT Discourse on Mastodon Research*
![framework_diagram](https://github.com/YaruWang-Code/master_thesis_mastodon_gpt/assets/85878984/9b4c1c92-a424-443f-88e6-c453607fcda2)

The results of the research can be seen in [README.md](https://github.com/YaruWang-Code/ma_thesis_results)https://github.com/YaruWang-Code/ma_thesis_results.
