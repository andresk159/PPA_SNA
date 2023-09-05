#############
# scripts to carry out text minig over SNA files (already downloaded)
# author: Andres camilo mendez alzate
# this script comes with absolutely no warranty, feel free to use it in the PPA-SNA context


#### import packages###
install.packages("pacman"); library(pacman)
  pacman::p_load(httr, jsonlite, geojson, geojsonlint, httpuv, rtweet, tidyverse, zoo, tidytext, 
                 qdapRegex, readr, stringi, spelling, hunspell, ggplot2, udpipe, stringdist, stopwords,
                 quanteda, GGally, widyr, SnowballC)


### make word barplot
f  %>% 
  count(words, sort = T) %>% top_n(20) %>% 
  mutate(words = reorder(words, n )) %>%
  ggplot(aes(x = words, y = n)) + 
  geom_col()+coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")
### make correlation graph
nms <- f %>% count(words, sort = T) %>% rename(freq = n) %>% dplyr::slice(1:20) %>% dplyr::pull(words)

cr <- f %>% group_by(words) %>% filter(n() > 20 ) %>%
  pairwise_cor(., words, id ,  sort = T,method = "pearson" ,use = "complete.obs") 
sim <- cr  %>%  spread( item1, correlation)

ggcorr(data = NULL, cor_matrix = sim[sim$item2 %in% nms, colnames(sim) %in% nms], nbreaks=10, palette='RdBu', label=TRUE, label_size=2, 
       label_color='black', label_round = 2)


#### function to clean and unnest_tokesn from twitter data (also works for twitter timelines)

clean_twt_txt <- function(txt, language = "es", ngrams = 1, add_stop_words = NULL, do_stemming = TRUE){
  data("stop_words")
  stop_words <- switch(language,
                       "es" =  tibble(word = stopwords::stopwords(language = "es")),
                       "en" =  stop_words,
                       "pt" =  tibble(word = stopwords::stopwords(language = "pt")))
  #stop_words_evolved <- paste0("[[:punct:][:blank:]]", t(stop_words), "[[:punct:][:blank:]]", collapse = "|")

  if(!is.null(add_stop_words)){
      stop_words <- stop_words %>% dplyr::add_row(word = add_stop_words)
  }
  
  if("list" %in% class(txt)){
    
    txt <- dplyr::bind_rows(txt)
  }
    
    if("text" %in% names(txt) &  ncol(txt) != 0 & !is.null(txt)){
      ## text mining using tidy text
      txt <- txt %>% 
        dplyr::filter(is_retweet == FALSE) %>% 
        dplyr::mutate(id = 1:nrow(.)) 
      
      to_clean <- txt %>% 
        dplyr::select(.,  text, lang, id) %>% 
        dplyr::filter(lang == language) %>%
        dplyr::mutate(text = str_to_lower(text),
                      text = str_conv(text, "UTF-8"), 
                      #text_cleaned = replace_non_ascii(text),# quita todos los emojis
                      text_cleaned = str_replace_all(text,"(http\\S*)|(\\n[a-z]*)|(\")|(#[a-z]*)|(\\|\\|)|(\\|)|(@[a-z|0-9]*)|(_[a-z]*)|([0-9])", ""))
    

        if(do_stemming){
          
          f <- to_clean %>% 
            dplyr::select(., -text) %>%
            tidytext::unnest_tokens(output = words, input = text_cleaned, token = "ngrams", n = ngrams, drop = F)%>%
            dplyr::mutate(words = SnowballC::wordStem(words, language = language) ) %>%
            filter(lang == language) %>% 
            anti_join(x = ., y = stop_words, by = c("words" = "word"))
        }else{
          
          f <- to_clean %>% 
            dplyr::select(., -text) %>%
            tidytext::unnest_tokens(output = words, input = text_cleaned, token = "ngrams", n = ngrams, drop = F)%>%
            filter(lang == language) %>% 
            anti_join(x = ., y = stop_words, by = c("words" = "word"))
          
        }
        return(f) 
    }else{
      cat("Dataframe 0 x 0 not allowed, returning NA. \n")
      return(NA)
    }
}


l <- list.files("Y:/PPA-SNA/input_data/social_networks_files/twitter_data", full.names = T)
nms <- list.files("Y:/PPA-SNA/input_data/social_networks_files/twitter_data") %>% substr(., 5, nchar(.)-4)
twt_raw <- lapply(l, function(x){readRDS(x)})

twt_data <- tibble(c_name =  nms,
                   tweets = purrr::map(twt_raw, function(i){
                       x <- i$tweets
                    }),
                   timeline = purrr::map(twt_raw, function(i){
                       x <- i$timeline
                     }),
                   description = purrr::map(twt_raw, function(i){
                       x <- i$descriptions
                    
                   })
)




### proccess text and store results in a multidimensional tibble
twt_data2 <- twt_data

all <- twt_data2 %>% 
  dplyr::mutate(twt_es = purrr::map(tweets,function(i){
    data <- clean_twt_txt(txt = i, language = "es", ngrams = 1, add_stop_words = NULL, do_stemming = TRUE)
    return(data)
  }),
  twt_en = purrr::map(tweets,function(i){
    data <- clean_twt_txt(txt = i, language = "en", ngrams = 1, add_stop_words = NULL, do_stemming = TRUE)
    return(data)
  }),
  twt_pt = purrr::map(tweets,function(i){
    data <- clean_twt_txt(txt = i, language = "pt", ngrams = 1, add_stop_words = NULL, do_stemming = TRUE)
    return(data)
  }),
  tml_es = purrr::map(timeline,function(i){
    data <- clean_twt_txt(txt = i, language = "es", ngrams = 1, add_stop_words = NULL, do_stemming = TRUE)
    return(data)
  }),
  tml_en = purrr::map(timeline,function(i){
    data <- clean_twt_txt(txt = i, language = "en", ngrams = 1, add_stop_words = NULL, do_stemming = TRUE)
    return(data)
  }),
  tml_pt = purrr::map(timeline,function(i){
    data <- clean_twt_txt(txt = i, language = "pt", ngrams = 1, add_stop_words = NULL, do_stemming = TRUE)
    return(data)
  })
  )



#calculate some metrics to summarize the data

key_words <- c("Conserva??o", "biodiversidade", "ambientais", "impacto","sustentabilidade" ,"sustent?vel", "sociobio", 
               "sociobiodiversidade", "amazonas", "Amaz?nia")

scores <- all %>% dplyr::select(c_name, tml_es:tml_pt)%>%
  dplyr::mutate(score1 = purrr::map(tml_pt, function(i){

    if( !"logical" %in% class(i) ){
    stemmed_key_words <- key_words %>% 
      SnowballC::wordStem(., language = "pt")

    words_ctn <- i %>% 
      dplyr::select(id, words) %>% 
      dplyr::count(words) %>%
      dplyr::filter(grepl(patter = paste(stemmed_key_words, collapse = "|") , words)) %>%
      dplyr::pull(n) %>%
      sum
    
    score <- words_ctn/length(unique(i$id))
    } else{ score <- NA    }
    
     return(score)
  })%>% unlist )

scores <- scores %>% dplyr::select(., c_name, score1) 

scores %>% 
  dplyr::filter(!is.na(score1) & !is.nan(score1)) %>%
  arrange(desc(score1)) %>%
  dplyr::mutate(c_name = reorder(c_name, score1 )) %>%
  ggplot(aes(x = c_name, y = score1)) + 
  geom_col()+coord_flip() +
  labs(x = "Miembros PPA",
       y = "Score",
       title = "Indicador del compromiso.")
