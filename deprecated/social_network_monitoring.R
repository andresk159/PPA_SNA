###################################
# @autor: Andres camilo mendez
# This script comes with absolutely no warranty, feel free to use it 
#in the PPA brasil framework


if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)}
pacman::p_load(tidyverse, rtweet,httr, jsonlite, geojson, geojsonlint, httpuv, stringr, rvest, RSelenium, 
               udpipe,quanteda, lubridate, bit64, stringdist, magrittr, wordcloud, lattice, rtweet )

######################################
# access token to carlangasLab app ##
####################################

create_token(
  app = "carlangasLab",
  consumer_key = "1cbb9gRXqPpIYE012yOpjrFSH",
  consumer_secret = "rLEFXfrEpQWay3xDFnMzhrNobsZxvXhn3Af1oZrNzvBqsw7uc5")

#############################
## import company names ####
###########################


companys <- read.csv("Y:/PPA-SNA/input_data/actores_ppa/listado_empresas_A_2(new_3).csv")


##########################################
## search company twitter screen names  #
########################################

endpoint <- "https://api.twitter.com/1.1/users/search.json"

q.v <- "Cargill"

http <- paste0(endpoint, "?q=", q.v) %>% URLencode()
r <- GET(http, get_token())

r <- httr::content(r, as = "text", encoding = "UTF-8")
r <-     jsonlite::fromJSON(r)


stringsim(a = q.v  , b = r$screen_name , method = "osa") %>% which.max()



query <- '#ParceirosPelaAmazônia'
twts <- search_tweets(query, n = 15000, include_rts = T, type = "recent", retryonratelimit = TRUE, token = get_token())

#twts2 <- search_tweets("@CargillAnimal", n = 15500, include_rts = T, type = "recent", retryonratelimit = TRUE, token = get_token())

desc <- lookup_users(gsub("@", "", query))

#twts <-  bind_rows(twts, twts2)

tml <- get_timeline(query,  n =1000)

to_save <- list("tweets" = twts, "timeline" = tml, "descriptions" = desc)

saveRDS(to_save, paste0("Y:/PPA-SNA/input_data/social_networks_files/twt_", gsub("@", "", query), ".rds"))


#########

twts <- read.csv("Y:/PPA-SNA/input_data/social_networks_files/twitter_data/abril_13_2021_ppa_twitter.csv",fileEncoding = "UTF-8-BOM")


library(udpipe)   
## First step: Take the Spanish udpipe model and annotate the text. 
## Note: this takes about 3 minutes   

mycorpus <- corpus(twts$text)

## 4) a) TOKENIZATION (separar las palabras)
mycorpus_url_numb_punct_symb <- tokens(mycorpus, what = "word", remove_url = T, remove_numbers = T,
                                       remove_punct = T, remove_symbols = T, remove_separators = T, 
                                       remove_twitter = T, remove_hyphens = T)

twts_clenaed <- twts %>% 
  mutate( text = str_replace_all(text,"(http\\S*)|(\\n[a-z]*)|(\")|(#[a-z0-9]*)|(\\|\\|)|(\\|)|(@[a-z|0-9]*)|(_[a-z]*)|([0-9])", ""))

ud_model <- udpipe_download_model(language = "portuguese-gsd")  
ud_model <- udpipe_load_model(ud_model$file_model)   


x <- udpipe_annotate(ud_model, x = twts_clenaed$text) %>% 
  as.data.frame() %>% 
  mutate(lemma = char_wordstem(lemma, language = "pt"))

sustantivos <- x[x$upos == "NOUN",]
stats <- txt_freq(x = sustantivos$lemma)


#Co ocurrencia. que tan  frecuentemente las palabras ocurren en la misma sentencia, 
#en este caso sustantivos y adjetivos
sustantivosadjetivos <- x[x$upos == "NOUN" | x$upos == "ADJ",]
stats <- cooccurrence(sustantivosadjetivos,    
                      term = "lemma", 
                      group = c("doc_id", "paragraph_id", "sentence_id"))  

stats %>% 
  as_tibble() %>% 
  mutate(key = paste(term1, term2)) %>% 
  arrange(desc(cooc)) %>% 
  head(., 20) %>% 
  mutate(key = factor(key, levels = unlist(.[order(.$cooc),4] ))) %>% 
  barchart(key ~ cooc, data =., col = "cadetblue", main = "Keywords - simple noun phrases", xlab = "Frequency")





