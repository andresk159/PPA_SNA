# -*- coding: utf-8 -*-
"""
Created on Mon Apr 18 13:37:35 2022

@author: ACMENDEZ
"""
#pip install GoogleNews
#pip install requests
#pip install textdistance
from GoogleNews import GoogleNews
import pandas as pd
import numpy as np
import seaborn as sb
import matplotlib.pyplot as plt
import os
import requests as rq
import urllib.parse
import textdistance
import nltk
from nltk import bigrams
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer
import networkx as nx
from bs4 import BeautifulSoup as bs
from html import unescape
from newspaper import Article
import re
import string
import time
import itertools
import collections




qry = urllib.parse.quote("partnership")



## usando la api de google search and crypto live prices

url = "https://google-search-and-crypto-live-prices-and-more.p.rapidapi.com/news/"+qry

headers = {
	"X-RapidAPI-Host": "google-search-and-crypto-live-prices-and-more.p.rapidapi.com",
	"X-RapidAPI-Key": "70799d51d6msh659ba2653b2749bp1cb260jsne124f5725b8d"
}

response_opt2 = rq.request("GET", url, headers=headers)

print(response_opt2.text)
res_json = response_opt2.json()
len(res_json)

news_link = [x["Link"] for x in res_json]

news_link_pd  = pd.Series(news_link)

news_link_pd = news_link_pd[ [not x for x in  news_link_pd.duplicated()] ].reset_index(drop=True)
news_link_pd = news_link_pd.drop(news_link_pd.index[-1])

news_dict = {}

os.makedirs("C:/Users/acmendez/AppData/Local/Temp/.newspaper_scraper/article_resources")

for i in range(10):
    print(i)

    try: 
        os.mkdir("C:/Users/acmendez/AppData/Local/Temp/.newspaper_scraper/article_resources") 
    except OSError as error: 
        print(error)  


for i in range(len(news_link_pd)):
    print("Crawling data from: "+ news_link_pd[i])
    article = Article(news_link_pd[i])
    try:
        article.download()
        article.parse()
    except: 
        print("Something went wrong")
    else:
        clean_text = re.sub(r'[^\w\s]', '', article.text.lower())
        clean_text = re.sub(r'[0-9]', '', clean_text)
        news_dict["news_"+ str(i)] = {"title" : article.title, "url": news_link_pd[i], "text_lowecase": article.text.lower(), "clean_text" : clean_text}
        
    time.sleep(1)

news_dict["news_31"]["clean_text"].encode("utf-8").decode("utf-8")

def check_qry(news_txt, lang):
    if lang == "en":
        to_check = ["platform", "amazon", "amazonia"]
        ret = sum([x in  news_txt  for x in to_check])
    else:
        ret = news_txt
    return ret

[check_qry(news_dict[x], lang = "pt") for x in news_dict.keys()]    



news_df = pd.DataFrame.from_dict(news_dict, orient = "index").reset_index().rename(columns={'index': 'id'})


news_df.to_csv("D:/OneDrive - CGIAR/Documents/PPA 2021/news_downloaded/abril 2022/ppa_pt_sep_abr_2022.csv", index = False)
news_dict


#tr.loc[0].at["clean_text"]

#### nlp section

def word_cleaner(word, stop_words):
    ret = len(word) >= 3 and not word in stop_words
    return ret


def word_preproc(text):
   
    pt_stm = nltk.stem.RSLPStemmer()
    
    tks = word_tokenize(text)
    tks = [i for i in tks if word_cleaner(i, stop_words = stop_words.union(stop_words_en) ) ]
    tks = [pt_stm.stem(i) for i in tks ]
    
    return tks


nltk.download('stopwords')
nltk.download('rslp')
stop_words = set(stopwords.words('portuguese'))
stop_words_en = set(stopwords.words('english'))

term_bigrams = [list(bigrams(word_preproc(text = i))) for i in news_df["clean_text"] ]


bgrms = list(itertools.chain(*term_bigrams))

edge_list = pd.DataFrame(bgrms, columns = ["from", "to"]).groupby(["from", "to"]).size().reset_index(name = "count").sort_values("count",ascending = False).iloc[0:20].reset_index(drop = True)


nt1 = nx.from_pandas_edgelist(edge_list, source = "from",  target = "to", edge_attr = "count")

bigram_counts = collections.Counter(bgrms)

list(itertools.chain(*bigram_counts.most_common(20)))



bigram_df = pd.DataFrame(bigram_counts.most_common(20), columns=['bigram', 'count'])

bigram_df

### plot network

d = bigram_df.set_index('bigram').T.to_dict('records')


# Create network plot 
G = nx.Graph()

# Create connections between nodes
for k, v in d[0].items():
    G.add_edge(k[0], k[1], weight=(v * 10))


ig, ax = plt.subplots(figsize=(10, 8))

pos = nx.spring_layout(G, k=2)

# Plot networks
nx.draw_networkx(G, pos,
                 font_size=16,
                 width=3,
                 edge_color='grey',
                 node_color='purple',
                 with_labels = False,
                 ax=ax)

# Create offset labels
for key, value in pos.items():
    x, y = value[0]+.135, value[1]+.045
    ax.text(x, y,
            s=key,
            bbox=dict(facecolor='red', alpha=0.25),
            horizontalalignment='center', fontsize=13)
    
plt.show()


qry = urllib.parse.quote("amazônia")



url = "https://google-search3.p.rapidapi.com/api/v1/news/q="+qry

headers = {
	"X-User-Agent": "desktop",
	"X-Proxy-Location": "US",
	"X-RapidAPI-Host": "google-search3.p.rapidapi.com",
	"X-RapidAPI-Key": "70799d51d6msh659ba2653b2749bp1cb260jsne124f5725b8d"
}

response = rq.request("GET", url, headers=headers)

rs = response.json()
rs["entries"] 
