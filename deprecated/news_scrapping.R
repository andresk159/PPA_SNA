###################################
# @autor: Andres camilo mendez
# This script comes with absolutely no warranty, feel free to use it 
#in the PPA brasil framework


if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)}
pacman::p_load(tidyverse, rtweet,httr, jsonlite, geojson, geojsonlint, httpuv, stringr, rvest, RSelenium, 
               udpipe,quanteda, lubridate, bit64, stringdist, magrittr, wordcloud, lattice, lexicon, quanteda.textplots,
               igraph,networkD3, intergraph)

#kemixi3361@kembung.com

#################################################################
##### Download News from internet due some key words ############
#################################################################




endpoint <- "https://webhose.io//filterWebContent?"

apikey.webhose <- "66d3a68e-ec46-46eb-a6ab-8e814d39a358" # api key de brayan

format.v <- "json" 

#timestamp
now <- Sys.time()

#luego le le quito 30 dias
monthago <- now - days(30)

ts.v <- as.integer64(as.numeric(monthago)*1000, digits=15)

only.post <- "true"


q.v <-"%22parceiros%20pela%20amaz%C3%B4nia%22%20OR%20%22partnership%20platform%20for%20the%20amazon%22"
q.v <-  "parceiros pela amazônia OR partnership platform for the amazon" #"plataforma parceiros pela amazônia" " OR 
q.v <- URLencode(q.v)# %>% paste0(.,"%20is_first%3Atrue" )

sort.v <- "relevancy"

#volvemos a crear el http
http <- paste0(endpoint, "q=", q.v, "&token=", apikey.webhose, "&format=", 
               format.v, "&ts=", ts.v, "&sort=", sort.v, "&from=0")


#volvemos a hacer el request 
response <- GET(http, 
                add_headers("Accept" = "text/plain"))

articles.text <- content(response, as = "text", encoding = "UTF-8")
articles.flt <- fromJSON(articles.text, flatten = T)


articles.flt.df <- articles.flt[["posts"]]
articles.flt.tr <- articles.flt[["totalResults"]]
articles.flt.mra <- articles.flt[["moreResultsAvailable"]]
articles.flt.next <- articles.flt[["next"]]

#next es un parametro del body que recurre muchas veces en apis con paginacion
#a veces nos proporciona el codigo de la pagina siguiente
#en este caso nos da directamente el http para el request de la siguiente pagina

#articles.flt.dt <- as.data.table(articles.flt.df)

#inicializo el parametro de salida

### si se necesita descargar mas archivos

if(articles.flt.mra > 0){
  
  nout <- 100
  i <- 1
  while(nout == 100){
    
    http <- paste0("http://webhose.io", articles.flt.next)
    
    response <- tryCatch({
      GET(http, 
              add_headers("Accept" = "text/plain"))
     
    }, error = function(e){
      message(paste("Seems to be an error:", e, "\n"))
      return(NA)
    })
    
    articles.text.tmp <- content(response, as = "text", encoding = "UTF-8")
    articles.flt.tmp <- jsonlite::fromJSON(articles.text.tmp, flatten = T)
    
    articles.flt.dt.tmp <- articles.flt.tmp[["posts"]]
    articles.flt.next <- articles.flt.tmp[["next"]]
    nout <- nrow(articles.flt.dt.tmp)
    
    l <- list(articles.flt.df, articles.flt.dt.tmp)
    articles.flt.df <- do.call("rbind", l)
    
    articles.flt.mra <- articles.flt.tmp[["moreResultsAvailable"]]
    hd <- headers(response)
    apicall <- hd[["x-webhose-requests-left"]]
    msg <- paste0("Articulos restantes: ", articles.flt.mra, " - Requests restantes: ", apicall)
    print(msg)
    i <- i+1
    tm <- ifelse(i %% 5 == 0, 5, 8)
    Sys.sleep(tm)
  }
  
}


## guardar data frame
route <- "D:/OneDrive - CGIAR/Documents/PPA 2021/news_downloaded/ppa_pt_en_21_jun_20_ago"
saveRDS(articles.flt.df, paste0(route, ".rds"))
write_csv(articles.flt.df %>% dplyr::select(url, author, published , title, text, thread.site_type, language), paste0(route, ".csv"))



news.flt.dt <- articles.flt.df %>% 
  dplyr::mutate(., text.pr = tolower(text))%>% 
  dplyr::mutate(., text.pr = gsub( "( |^)\\| [a-z]+", "" , text.pr)) %>%
  dplyr::filter(., (thread.site_type == "news" | thread.site_type == "blogs"))

#### detectar textos que sean muy parecidos


  news.flt.dt %<>%
    group_by(thread.site_full) %>%
    dplyr::mutate(., repeated = table(thread.site_full)  ) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(., str_sim = 0, ID = 1:nrow(.)) %>%
      data.frame()
  #saveRDS(news.flt.dt, paste0("D:/OneDrive - CGIAR/Desktop/ppa_brasil/input_data/news_crawled/", "Conserva??o_da_biodiversidade_", Sys.Date()) )
  
  news.flt.dt <- readRDS( "D:/OneDrive - CGIAR/Documents/PPA 2021/news_downloaded/junio 2021/ppa_pt_en_1_jun_21.rds")
 
  news.flt.dt <- news.flt.dt %>% 
    dplyr::mutate(., text.pr = tolower(text))%>% 
    dplyr::mutate(., text.pr = gsub( "( |^)\\| [a-z]+", "" , text.pr)) %>%
    dplyr::filter(., (thread.site_type == "news" | thread.site_type == "blogs"))
  
  ### eliminar entradas que sean de la misma url y tengan mucha similaridad entre si
  news.flt.dt %>%
    dplyr::filter(., repeated > 1) %>%
    dplyr::pull(thread.site_full)%>%
    unique() %>% for(i in .){
      cat("Removing news duplicated by site:", i, "/", length(.), "\n")
      to_check <- news.flt.dt[which(news.flt.dt$repeated > 1 & news.flt.dt$thread.site_full == i), ] 
        
      comb <- combn(nrow(to_check), 2)
      j <- 1
    while(j != 0){
      cat("    Calculating similarity measures for:", j, "/", ncol(comb), " possible duplicated news. \n" )
      x <- stringsim(a = to_check[comb[1, j],  "text.pr"] , b = to_check[comb[2, j] , "text.pr"], method = "osa")
      names(x) <- NULL
      
      if(x >= 0.9){
        
        news.flt.dt <<- news.flt.dt[-which(news.flt.dt$repeated > 1 & news.flt.dt$thread.site_full == i)[comb[2, j] ], ]
        comb <- comb[, -which(comb[1, ] == comb[2, j]) ]
        
      }
      
      #Establecer criterio de parada
      if(j == ncol(comb) | ncol(comb) == 0){
        j<- 0
      }else{
        j <- j +1
      }
     
      }
    }
  
   
  
  ## 3) ahora creo el CORPUS 
  news.flt.dt <- news.flt.dt %>% 
    dplyr::filter(language == "portuguese")
  
  mycorpus <- corpus(news.flt.dt$text.pr)
  
  ## 4) a) TOKENIZATION (separar las palabras)
  mycorpus_url_numb_punct_symb <- tokens(mycorpus, what = "word", remove_url = T, remove_numbers = T,
                                         remove_punct = T, remove_symbols = T, remove_separators = T, 
                                         remove_twitter = T, remove_hyphens = T)
  
  
  ## 4)STOPWORDS
  ## vamos removiendo las stopwords
  # estas son las stopwords basicas del portuges
  stopwords.es <- stopwords(language = "pt")
  stopwords.es
  
  # podemos anadir otras stopwords ad hoc
  # 1) palabras relacionadas con el tema investigado
  # por ejemplo podemos quitar la palabra clave de la busqueda de los articulos
  stopwords.add <- c( "parceiros", "amazônia", "plataforma", "brasil", "ppa")
  
  stopwords.tot <- c(stopwords.es, stopwords.add)
  mycorpus_sw <- tokens_remove(mycorpus_url_numb_punct_symb, stopwords.tot) 
  
  ##### CREACION DE LA DOCUMENT TERM MATRIX
  #aqui ya podemos crear la document term matrix
  dtm_sw <- dfm(mycorpus_sw, tolower = T)
  View(dtm_sw[c(1:34), c(1:50)])
  
  ## 5) STEMMING
  # encuentro las raices de las palabras
  mycorpus_stem<-tokens_wordstem(mycorpus_sw, language = "pt")
  mycorpus[1]
  mycorpus_sw[10]
  mycorpus_stem[10]
  
  #tokens_replace(mycorpus_sw, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
  
  dtm_stem <- dfm(mycorpus_stem)
  dtm_stem
  ##### no STEMMING
  
  dtm_stem <-dtm_sw
  
  ##6) para disminuir la complejidad podemos quitar los stems de unos o dos caracteres 
  dtm_w2<-dfm_select(dtm_stem, min_nchar = 3)
  dtm_w2
  
  ##remuevo stems no frecuentes, en este caso que aparezcan en menos de 10 titulos 
  #y por lo menos en en cinco titulos distintos
  dtm_f10<-dfm_trim(dtm_w2, min_termfreq = 3, min_docfreq = 3)
  dtm_f10
  
  dtm_sw <- dtm_f10
  ## exploramos la document term matrix
  #las palabras mas frecuentes
  topfeatures(dtm_sw, 60)  
  ## similarity
  # es algo parecido a una correlacion calculada sobre palabras
  # que tanto quedan asociadas las palabras entre ellas?
  # tomo las diez palabras mas frecuentes
  tf <- topfeatures(dtm_sw, 20)
  
  freqfeatures <- names(tf)
  freqfeatures
  # individuo posibles palabras vacias extras dentro de las frecuentes
  stopfeatures <- c("to", "of" , "the", "can", "also", "one")
  # las quito de listado de las palabras frecuentes
  freqfeatures <- freqfeatures[!freqfeatures %in% stopfeatures]
  
  #calculo la matriz de correlaciones entre las palabras mas frecuentes y todas las demas palabras 
  sim <- quanteda.textstats::textstat_simil(dtm_sw, margin = "features", 
                        method = "correlation")
  
  library(GGally)
  
  gcor <- ggcorr(data = NULL, cor_matrix = as.matrix(sim[freqfeatures, freqfeatures]), 
           nbreaks=10, palette='RdBu', label=T, label_size= 1.5,layout.exp = 0 ,
         label_color='black', label_round = 1, digits = 1,size = 2)
  
  ggsave(filename = "D:/OneDrive - CGIAR/Documents/PPA 2021/news_downloaded/word_corr_plot.png", plot= gcor, dpi = 400, units="in", width=8, height=5)
  
  
  library(RColorBrewer)
  
  quanteda.textplots::textplot_wordcloud(dtm_sw, min_count = 15, random_order = FALSE,
                     rotation = .25, min_size =1, max_size = 5, max_words = 100,
                     color = RColorBrewer::brewer.pal(8,"Dark2"))
  
  
  mycorpus_ngram <- tokens_ngrams(mycorpus_sw, n = c(1:3), concatenator = " ")
  mycorpus_ngram[2]
  
  dtm_ngram <- dfm(mycorpus_ngram)  
  mycorpus_stem<-tokens_wordstem(mycorpus_ngram, language = "en")
  dtm_stem <- dfm(mycorpus_stem)
  dtm_stem
  
  
  dtm_w2<-dfm_select(dtm_stem, min_nchar = 3)
  dtm_w2
  
  
  ##remuevo stems no frecuentes, en este caso que aparezcan en menos de 10 titulos 
  #y por lo menos en en cinco titulos distintos
  dtm_f10<-dfm_trim(dtm_w2, min_termfreq = 15, min_docfreq = 5)
  dtm_f10
  
  ##7) ahora removemos los articulos que no tienen informacion
  # calculamos el numero de palabras por cada titulo
  rowTotals <- apply(X = dtm_f10, MARGIN = 1, FUN = sum) 
  #eliminamos todos los tweets que no tengan stems en las columnas
  dtm_delrow <- dtm_f10[rowTotals> 0, ]
  dtm_w2 <- dtm_delrow
  
  topfeatures(dtm_w2, 10)
  
  textplot_wordcloud(dtm_w2, min_count = 15, random_order = FALSE,
                     rotation = .25, min_size =1, max_size = 5, max_words = 37,
                     color = RColorBrewer::brewer.pal(8,"Dark2"))
  
  library(udpipe)   
  ## First step: Take the Spanish udpipe model and annotate the text. 
  ## Note: this takes about 3 minutes  
  #"portuguese-gsd"
  ud_model <- udpipe_download_model(language = "english-ewt")  
  ud_model <- udpipe_load_model(ud_model$file_model)   
  
  
  x <- udpipe_annotate(ud_model, x = news.flt.dt$text.pr) %>% 
    as.data.frame()
  
  sustantivos <- x[x$upos == "NOUN",]
  stats <- txt_freq(x = sustantivos$lemma)
  
  
  #Co ocurrencia. que tan  frecuentemente las palabras ocurren en la misma sentencia, 
  #en este caso sustantivos y adjetivos
  sustantivosadjetivos <- x[x$upos == "NOUN" | x$upos == "ADJ",]
  stats <- cooccurrence(sustantivosadjetivos,    
                        term = "lemma", 
                        group = c("doc_id", "paragraph_id", "sentence_id"))  
  
  stats %>% 
    as_tibble %>% 
    dplyr::filter(if_all(starts_with("term"), ~ . != "amazônia")) %>% 
    mutate(key = paste(term1, term2), key = factor(key)) %>% 
    filter(!term1 %in% c("grant", "hq")) %>% 
    filter(!term2 %in% c("grant", "hq")) %>% 
    arrange(desc(cooc)) %>%
    head(., 20) %>% 
    ggplot(., aes(x =  reorder(key, cooc), y = cooc))+
    geom_bar(stat = "identity", fill = "cadetblue", color = "black", width = 0.8)+
    coord_flip()+
    theme_bw()+
    xlab(element_blank())+
    ylab("Counts")
    #barchart(key ~ cooc, data =., col = "cadetblue", main = "Keywords - simple noun phrases", xlab = "Frequency")
##### crear grafico de redes
  ScaleWeight <- function(x, lambda) {
    x / lambda
  }
  
  network <-  stats %>% 
    as_tibble %>% 
    dplyr::filter(if_all(starts_with("term"), ~ . != "amazônia")) %>% 
    filter(!term1 %in% c("abrir", "janela", "clique")) %>% 
    mutate(weight = ScaleWeight(x = cooc, lambda = 2E3)) %>% 
    arrange(desc(cooc)) %>%
    head(., 60) %>% 
    graph_from_data_frame(directed = FALSE)
  
  V(network)$degree <- strength(graph = network)
  
  # Compute the weight shares.
  E(network)$width <- E(network)$weight/max(E(network)$weight)
  
 
  net1 <-  intergraph::asNetwork(simplify(network))
  nplot <- ggnet2(net1,
                  size = 100*V(network)$degree, 
                  shape = 19,
                  mode = "fruchtermanreingold", 
                  label = V(network)$name,
                  label.alpha = 1,
                  node.color = "dodgerblue",
                  label.size = 4,
                  edge.size = 1.5*E(network)$width,
                  arrow.size = 5,
                  edge.color = "grey40",
                  edge.alpha = 0.5,
                  arrow.gap = 0.025)+
    scale_fill_manual()+
    theme(legend.position = "none")
 
  ggsave(filename = "D:/OneDrive - CGIAR/Documents/PPA 2021/web_scp_results/wb_scp_21_jul_20_ago", plot= nplot, dpi = 400, units="in", width=8, height=5)
 
  
  net <- network
  net_mtrs <- list()
  net_mtrs$general_metrics<- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
         Value  = c(igraph::edge_density(net),
                    igraph::centralization.betweenness(net)$centralization,
                    igraph::centralization.degree (net, mode = "all")$centralization,
                    igraph:: cluster_walktrap(net) %>% igraph::modularity(),
                    mean(igraph::degree(net, mode="all", normalized = F)),
                    igraph::transitivity(net),
                    igraph::assortativity.degree(net, directed = F),
                    eigen_centrality(net, directed = F)$value
         )) %>%
    dplyr::mutate_if(is.numeric, round, digits = 2) %>%
    data.frame() 
  
  net_mtrs$indivicual_metrics <- tibble(nodes_id = V(net)$name) %>% 
    dplyr::mutate(degree         = igraph::degree(net, mode="all", normalized = F),
                  in_degree      = igraph::degree(net, v = V(net), mode = "in"),
                  out_degree     = igraph::degree(net, v = V(net), mode = "out"),
                  betwe          = igraph::betweenness(net) %>% round(.,1),
                  ClusWalk       = igraph::cluster_walktrap(net) %>% membership(),
                  transitivLocal = igraph::transitivity(net, type = "local")  %>% round(.,2),
                  Eigen_centr    = igraph::eigen_centrality(net, directed = F)$vector %>%  round(.,2)
    ) 
  writexl::write_xlsx(net_mtrs, "D:/OneDrive - CGIAR/Documents/PPA 2021/web_scp_results/wb_scp_21_jul_20_ago/word_net_mtrs_resultados_agosto.xlsx")
  
  
  
  
  
  x <- x[-which(x$token %in% c("", ".", ",","\"",stopwords.es, stopwords.add)),] 
  x <- d
  
  ## Using a sequence of POS tags (noun phrases / verb phrases)
  x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
  stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                            pattern = "(N|AD)*N(P+D*(N|AD)*A)*", 
                            is_regex = TRUE, detailed = FALSE,
                            ngram_max = 3)
  stats <- subset(stats, ngram == 1 & freq > 10 )
  stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
  barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
           main = "Keywords - Barchart", xlab = "Frequency")
  
  
  
  wordcloud(words = stats$keyword, freq = stats$freq, scale=c(2,0),
            max.words = 30, colors = RColorBrewer::brewer.pal(18,"Dark2"), random.color= TRUE)
  
  ymd_hms(news.flt.dt$thread.published) %>% 
    date() %>% 
    table() %>%
    as.data.frame()%>% 
    rename(., day = .) %>%
    #write.csv(., "D:/OneDrive - CGIAR/Desktop/text_mining_results/serie_tiempo2.csv")
  ggplot(.,  aes(x = day, y = Freq, group = 1)) + geom_line() + geom_point()
  

  
  news.flt.dt <- readRDS( "D:/OneDrive - CGIAR/Documents/PPA 2021/web_scp_results/wb_scp_1_jun_21/ppa_pt_en_1_jun_21.rds")
   
  news.flt.dt <- news.flt.dt %>% 
    dplyr::filter(language == "portuguese") %>% 
    mutate(freq_words = NA)
  
  
for(i in 1:nrow(news.flt.dt)){
  news.flt.dt$freq_words[i] <- dtm_f10[i,] %>%
    t() %>%
    convert(., "data.frame") %>% 
    rename_with(., function(i){gsub("[0-9]+", "", i)}) %>% 
    arrange(desc(text)) %>% 
    slice(1:3) %>% 
    pull(doc_id) %>% 
    paste(., sep = "-", collapse = "-") 
}
  
  news.flt.dt %>% 
    dplyr::select(url, title, text, language, thread.site_type,freq_words ) %>% 
    write_csv("D:/OneDrive - CGIAR/Documents/PPA 2021/web_scp_results/wb_scp_1_jun_21/freq_words_per_new_julio.csv")

  
  
  
  