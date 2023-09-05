if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)}
pacman::p_load(tidyverse, rtweet,httr, jsonlite, geojson, geojsonlint, httpuv, stringr, rvest, RSelenium, 
               udpipe,quanteda, lubridate, bit64, stringdist, magrittr, wordcloud, lattice, netdiffuseR, readr, igraph, statnet,
               GGally, intergraph, ggnet2)

options(scipen = 9999)
#edgelist from python

baseDir<- "D:/OneDrive - CGIAR/Documents/PPA 2021/instagram_results/"
mes <- "agosto"
edge_list_raw <- read.csv(paste0(baseDir, 'instagram_res_', mes, "_2021/Instagram_edge_list_", mes,".csv"), encoding = "latin-1")

ppa_member <- read.csv(paste0(baseDir, 'instagram_res_', mes, "_2021/Instagram_counts_", mes,".csv"), encoding = "latin-1") %>% 
  as_tibble() %>% 
  dplyr::select(c_name = company_name, everything())


edge_list_final <- edge_list_raw %>% 
  filter(to %in% unique(from))


members <- ppa_member %>% 
  drop_na() %>% 
  left_join(., edge_list_final %>% 
              filter(!duplicated(from)), by = c("c_name" = "from")) %>% 
  dplyr::mutate(to = ifelse(is.na(to), 0, 1))

#coopmel esta malo, hay que 
net <- graph_from_data_frame(d=  edge_list_final,
                             vertices = members %>% select(c_name, follower_number) ,
                             directed=T) %>%
  igraph::delete_vertices(., igraph::degree(.) == 0) #%>% 
  #igraph::delete_edges(., edges = which(igraph::which_multiple(.)))

v_color <- tibble(id = V(net)$name) %>% 
  mutate(color = ifelse(id == "parceirosamazonia", "#EB8055FF", '#73D055FF'))


v_size <-   tibble(id = 1:length(V(net)$name), fll  = V(net)$follower_number, v_sized = NA) 

to_change <- v_size %>% 
  filter(complete.cases(fll)) %>% 
  mutate(r_sized = rescale_vertex_igraph(fll, adjust = 300)) %>% 
  select(id, r_sized)

v_size$v_sized[to_change$id] <- to_change$r_sized
v_size <- v_size %>% 
  mutate(v_sized = ifelse(is.na(v_sized), 1, v_sized))


net1 <-  intergraph::asNetwork(simplify(net))
nplot <- ggnet2(net1,
                color = v_color$color, 
                size = v_size$v_sized, 
                shape = 19,
                mode = "fruchtermanreingold", 
                label = v_color$id,
                label.size = 3,
                label.alpha = 1,
                arrow.size = 5,
                arrow.gap = 0.025)+
  scale_fill_manual()+
  theme(legend.position = "none")

ggsave(filename = paste0(baseDir, 'instagram_res_', mes, "_2021/Instagram_net_plot_2", mes,".png"), plot= nplot, dpi = 400, units="in", width=8, height=5)


#### metricas de la red
insta_res <- list()

insta_res$general_metrics <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
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

insta_res$indivicual_metrics <- tibble(nodes_id = V(net)$name) %>% 
  dplyr::mutate(degree         = igraph::degree(net, mode="all", normalized = F),
                in_degree      = igraph::degree(net, v = V(net), mode = "in"),
                out_degree     = igraph::degree(net, v = V(net), mode = "out"),
                betwe          = igraph::betweenness(net) %>% round(.,1),
                closeness      = igraph::closeness(net, mode="all") %>% round(.,4),
                ClusWalk       = igraph::cluster_walktrap(net) %>% membership(),
                transitivLocal = igraph::transitivity(net, type = "local")  %>% round(.,2),
                Eigen_centr    = igraph::eigen_centrality(net, directed = F)$vector %>%  round(.,2)
  ) %>%
  dplyr::left_join(., members %>% select(c_name, everything()), by = c("nodes_id" = "c_name"))


#### los que no estan conectados con nadie


net_not_linked <- graph_from_data_frame(d=  edge_list_final,
                             vertices = members %>% select(c_name, everything()) ,
                             directed=T)

insta_res$not_linked_to_anyone <- data.frame(c_name = names(igraph::degree(net_not_linked)[igraph::degree(net_not_linked) == 0])  ) %>% 
  left_join(., members %>% select(c_name, starts_with("follow")), by = "c_name") %>% 
  add_row(c_name = "Totals", follower_number = sum(.$follower_number), following_number = sum(.$following_number) )
####  linkeados a la PPA

insta_res$PPA_linked <- tibble(
  c_name = unique(c(edge_list_final %>% 
    filter(from == "parceirosamazonia") %>% 
    pull(to),
    edge_list_final %>% 
      filter(to == "parceirosamazonia") %>% 
      pull(from)
    ))
 
) %>%
  left_join(., members %>% 
              select( c_name, starts_with("follow")), by = "c_name") %>% 
  add_row(c_name = "Totals", follower_number = sum(.$follower_number), following_number = sum(.$following_number) )

#### otros nodos conectados

insta_res$PPA_not_linked <- tibble(
  c_name = unique(c(edge_list_final %>% 
                      filter(from != "parceirosamazonia") %>% 
                      pull(from),
                    edge_list_final %>% 
                      filter(to != "parceirosamazonia") %>% 
                      pull(to)
  ))
  
) %>%
  left_join(., members %>% 
              select( c_name, starts_with("follow")), by = "c_name") %>% 
  add_row(c_name = "Totals", follower_number = sum(.$follower_number), following_number = sum(.$following_number) )



writexl::write_xlsx(insta_res, paste0(baseDir, 'instagram_res_', mes, "_2021/instagram_resultados_", mes,".xlsx"))





