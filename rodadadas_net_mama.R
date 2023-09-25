#############################################################
################## RED DE COLAB INST #######################
###########################################################
####### 2019 ##########

inv_2019 <- lapply(c("_invest", "_nego", "_coop", "_colab", "outras_org"), function(i){
  x <- raw_list_2019[[grep(i, names(raw_list_2019))]] %>% 
    dplyr::select(R_Start_ID, contains("R_Destiny_ID"), contains("R5b_Destiny_ID"), R_Start_ID_ego )
  names(x) <- c("R_Start_ID", "R_Destiny_ID", "R_Start_ID_ego" )
  x$type <- i
return(x)
}) %>% 
  bind_rows() %>% 
  drop_na()
  
outras_org_2019 <- raw_list_2019[[grep("outras_org", names(raw_list_2019))]]

name_rodada <- "rodada_2019"



######################################################################
########### REMOVIENDO LOS EGOS QUE DIJO FELIPE ##################### 
####################################################################

nt1 <- inv_2019 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) %>% 
  dplyr::filter(type != "outras_org")



d2 <- nt1 
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/madre_standard_net_graph.png"))


net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/madre_standard_net_graph_orgtype.png"))



###metricas
net_no_dir <- net_felipe_ego$net_no_dir
gnr_mtrs <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
                   Value  = c(igraph::edge_density(net_no_dir),
                              igraph::centralization.betweenness(net_no_dir)$centralization,
                              igraph::centralization.degree (net_no_dir, mode = "all")$centralization,
                              igraph:: cluster_walktrap(net_no_dir) %>% igraph::modularity(),
                              mean(igraph::degree(net_no_dir, mode="all", normalized = F)),
                              igraph::transitivity(net_no_dir),
                              igraph::assortativity.degree(net_no_dir, directed = F),
                              eigen_centrality(net_no_dir, directed = F)$value
                   )) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  data.frame()




to_rm <- d2 %>% 
  dplyr::mutate(ids = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::pull(ids) %>%
  unique()

role_ppa_rm <- role_ppa[[name_rodada]] %>%
  dplyr::mutate(ids = paste0(ID_node, "_", ID_ego)) %>% 
  dplyr::filter(ids %in% to_rm) %>% 
  dplyr::filter(!duplicated(ids)) %>% 
  dplyr::select(-ids, -ID_ego)


node_mtrs <- tibble(nodes_id = V(net_no_dir)$name) %>% 
  dplyr::left_join(., groups[[name_rodada]], by = c("nodes_id" = "ID_node")) %>%
  dplyr::select(nodes_id, Nome,V1) %>%
  dplyr::mutate(degree         = igraph::degree(net_no_dir, mode="all", normalized = F),
                betwe          = igraph::betweenness(net_no_dir) %>% round(.,1),
                closeness      = igraph::closeness(net_no_dir, mode="all") %>% round(.,4),
                ClusWalk       = igraph::cluster_walktrap(net_no_dir) %>% membership(),
                transitivLocal = igraph::transitivity(net_no_dir, type = "local")  %>% round(.,2),
                Eigen_centr    = igraph::eigen_centrality(net_no_dir, directed = T)$vector %>%  round(.,2)
  ) %>%
  dplyr::left_join(., entrevistados[[name_rodada]] %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
  dplyr::left_join(., type_org[[name_rodada]] %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>%
  dplyr::left_join(., role_ppa_rm, by = c("nodes_id" = "ID_node")) %>% 
  dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )


to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx,
  gnr_mtrs = gnr_mtrs,
  node_mtrs = node_mtrs
)



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/madre_standard_net_results.xlsx"))



#####################################################################
############# ADICIONANDO LOS EGOS C      ##########################
###################################################################


nt1 <- inv_2019 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) 



d2 <- nt1 

#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/madre_grC_net_graph.png"))

net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/madre_grC_net_graph_orgtype.png"))


###metricas
net_no_dir <- net_felipe_ego$net_no_dir
gnr_mtrs <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
                   Value  = c(igraph::edge_density(net_no_dir),
                              igraph::centralization.betweenness(net_no_dir)$centralization,
                              igraph::centralization.degree (net_no_dir, mode = "all")$centralization,
                              igraph:: cluster_walktrap(net_no_dir) %>% igraph::modularity(),
                              mean(igraph::degree(net_no_dir, mode="all", normalized = F)),
                              igraph::transitivity(net_no_dir),
                              igraph::assortativity.degree(net_no_dir, directed = F),
                              eigen_centrality(net_no_dir, directed = F)$value
                   )) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  data.frame()


to_rm <- d2 %>% 
  dplyr::mutate(ids = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::pull(ids) %>%
  unique()

role_ppa_rm <- role_ppa[[name_rodada]] %>%
  dplyr::mutate(ids = paste0(ID_node, "_", ID_ego)) %>% 
  dplyr::filter(ids %in% to_rm) %>% 
  dplyr::filter(!duplicated(ids)) %>% 
  dplyr::select(-ids, -ID_ego)


node_mtrs <- tibble(nodes_id = V(net_no_dir)$name) %>% 
  dplyr::left_join(., groups[[name_rodada]], by = c("nodes_id" = "ID_node")) %>%
  dplyr::select(nodes_id, Nome,V1) %>%
  dplyr::mutate(degree         = igraph::degree(net_no_dir, mode="all", normalized = F),
                betwe          = igraph::betweenness(net_no_dir) %>% round(.,1),
                closeness      = igraph::closeness(net_no_dir, mode="all") %>% round(.,4),
                ClusWalk       = igraph::cluster_walktrap(net_no_dir) %>% membership(),
                transitivLocal = igraph::transitivity(net_no_dir, type = "local")  %>% round(.,2),
                Eigen_centr    = igraph::eigen_centrality(net_no_dir, directed = T)$vector %>%  round(.,2)
  ) %>%
  dplyr::left_join(., entrevistados[[name_rodada]] %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
  dplyr::left_join(., type_org[[name_rodada]] %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>%
  dplyr::left_join(., role_ppa_rm, by = c("nodes_id" = "ID_node")) %>% 
  dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )


to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx,
  gnr_mtrs = gnr_mtrs,
  node_mtrs = node_mtrs
)



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/madre_grC_net_results.xlsx"))


##madre no tiene direccion

##################################################
################## 2020 #########################
################################################


inv_2020 <- lapply(c("_invest", "_nego", "_coop", "_colab", "outras_org"), function(i){
  x <- raw_list_2020[[grep(i, names(raw_list_2020))]] %>% 
    dplyr::select(R_Start_ID, contains("R_Destiny_ID"), contains("R5b_Destiny_ID") , R_Start_ID_ego)
  names(x) <- c("R_Start_ID", "R_Destiny_ID", "R_Start_ID_ego" )
  x$type <- i
  return(x)
}) %>% 
  bind_rows() %>% 
  drop_na()

outras_org_2020 <- raw_list_2020[[grep("outras_org", names(raw_list_2020))]]

name_rodada <- "rodada_2020"


############


nt1 <- inv_2020 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) %>% 
  dplyr::filter(type != "outras_org")


d2 <- nt1 
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/madre_standard_net_graph.png"))


net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/madre_standard_net_graph_orgtype.png"))




###metricas
net_no_dir <- net_felipe_ego$net_no_dir
gnr_mtrs <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
                   Value  = c(igraph::edge_density(net_no_dir),
                              igraph::centralization.betweenness(net_no_dir)$centralization,
                              igraph::centralization.degree (net_no_dir, mode = "all")$centralization,
                              igraph:: cluster_walktrap(net_no_dir) %>% igraph::modularity(),
                              mean(igraph::degree(net_no_dir, mode="all", normalized = F)),
                              igraph::transitivity(net_no_dir),
                              igraph::assortativity.degree(net_no_dir, directed = F),
                              eigen_centrality(net_no_dir, directed = F)$value
                   )) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  data.frame()

to_rm <- d2 %>% 
  dplyr::mutate(ids = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::pull(ids) %>%
  unique()

role_ppa_rm <- role_ppa[[name_rodada]] %>%
  dplyr::mutate(ids = paste0(ID_node, "_", ID_ego)) %>% 
  dplyr::filter(ids %in% to_rm) %>% 
  dplyr::filter(!duplicated(ids)) %>% 
  dplyr::select(-ids, -ID_ego)


node_mtrs <- tibble(nodes_id = V(net_no_dir)$name) %>% 
  dplyr::left_join(., groups[[name_rodada]], by = c("nodes_id" = "ID_node")) %>%
  dplyr::select(nodes_id, Nome,V1) %>%
  dplyr::mutate(degree         = igraph::degree(net_no_dir, mode="all", normalized = F),
                betwe          = igraph::betweenness(net_no_dir) %>% round(.,1),
                closeness      = igraph::closeness(net_no_dir, mode="all") %>% round(.,4),
                ClusWalk       = igraph::cluster_walktrap(net_no_dir) %>% membership(),
                transitivLocal = igraph::transitivity(net_no_dir, type = "local")  %>% round(.,2),
                Eigen_centr    = igraph::eigen_centrality(net_no_dir, directed = T)$vector %>%  round(.,2)
  ) %>%
  dplyr::left_join(., entrevistados[[name_rodada]] %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
  dplyr::left_join(., type_org[[name_rodada]] %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>%
  dplyr::left_join(., role_ppa_rm, by = c("nodes_id" = "ID_node")) %>% 
  dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )


to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx,
  gnr_mtrs = gnr_mtrs,
  node_mtrs = node_mtrs
)



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/madre_standard_net_results.xlsx"))

#####################################################################
############# ADICIONANDO LOS EGOS C  2020   #######################
###################################################################

nt1 <- inv_2020 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) 



d2 <- nt1 
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/madre_grC_net_graph.png"))

net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/madre_grC_net_graph_orgtype.png"))


###metricas
net_no_dir <- net_felipe_ego$net_no_dir
gnr_mtrs <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
                   Value  = c(igraph::edge_density(net_no_dir),
                              igraph::centralization.betweenness(net_no_dir)$centralization,
                              igraph::centralization.degree (net_no_dir, mode = "all")$centralization,
                              igraph:: cluster_walktrap(net_no_dir) %>% igraph::modularity(),
                              mean(igraph::degree(net_no_dir, mode="all", normalized = F)),
                              igraph::transitivity(net_no_dir),
                              igraph::assortativity.degree(net_no_dir, directed = F),
                              eigen_centrality(net_no_dir, directed = F)$value
                   )) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  data.frame()

to_rm <- d2 %>% 
  dplyr::mutate(ids = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::pull(ids) %>%
  unique()

role_ppa_rm <- role_ppa[[name_rodada]] %>%
  dplyr::mutate(ids = paste0(ID_node, "_", ID_ego)) %>% 
  dplyr::filter(ids %in% to_rm) %>% 
  dplyr::filter(!duplicated(ids)) %>% 
  dplyr::select(-ids, -ID_ego)


node_mtrs <- tibble(nodes_id = V(net_no_dir)$name) %>% 
  dplyr::left_join(., groups[[name_rodada]], by = c("nodes_id" = "ID_node")) %>%
  dplyr::select(nodes_id, Nome,V1) %>%
  dplyr::mutate(degree         = igraph::degree(net_no_dir, mode="all", normalized = F),
                betwe          = igraph::betweenness(net_no_dir) %>% round(.,1),
                closeness      = igraph::closeness(net_no_dir, mode="all") %>% round(.,4),
                ClusWalk       = igraph::cluster_walktrap(net_no_dir) %>% membership(),
                transitivLocal = igraph::transitivity(net_no_dir, type = "local")  %>% round(.,2),
                Eigen_centr    = igraph::eigen_centrality(net_no_dir, directed = T)$vector %>%  round(.,2)
  ) %>%
  dplyr::left_join(., entrevistados[[name_rodada]] %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
  dplyr::left_join(., type_org[[name_rodada]] %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>%
  dplyr::left_join(., role_ppa_rm, by = c("nodes_id" = "ID_node")) %>% 
  dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )


to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx,
  gnr_mtrs = gnr_mtrs,
  node_mtrs = node_mtrs
)



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/madre_grC_net_results.xlsx"))


#######################################
######      EDGE LIST JF    ##########
#####################################

entrev_2020 <- inv_2020 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::pull(R_Start_ID) %>% 
  unique

edge_lst <- inv_2019 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::filter(!R_Start_ID %in% entrev_2020) %>% 
  dplyr::filter(!R_Destiny_ID %in% entrev_2020) %>% 
  dplyr::select(-contains("ego"), - contains("comentario"))

net <- graph_from_data_frame(d=  edge_lst,
                             vertices = groups[["rodada_2019"]] %>% 
                               dplyr::select(ID_node, V1) ,
                             directed= T) %>%
  igraph::delete_vertices(., igraph::degree(.) == 0) %>% 
  igraph::delete_edges(., edges = which(igraph::which_multiple(.)))

saveRDS(net,  paste0(out_dirs[["rodada_2019"]],"/madre_edge_list_JF.rds"))



##################################################
################## 2021 #########################
################################################


inv_2021 <- lapply(c("_invest", "_nego", "_coop", "_colab", "outras_org"), function(i){
  x <- raw_list_2021[[grep(i, names(raw_list_2021))]] %>% 
    dplyr::select(R_Start_ID, contains("R_Destiny_ID"), contains("R5b_Destiny_ID") , R_Start_ID_ego)
  names(x) <- c("R_Start_ID", "R_Destiny_ID", "R_Start_ID_ego" )
  x$type <- i
  return(x)
}) %>% 
  bind_rows() %>% 
  drop_na()

outras_org_2021 <- raw_list_2021[[grep("outras_org", names(raw_list_2021))]]

name_rodada <- "rodada_2021"


############


nt1 <- inv_2021 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) %>% 
  dplyr::filter(type != "outras_org")


d2 <- nt1 
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/madre_standard_net_graph.png"))

net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/madre_standard_net_graph_orgtype.png"))


###metricas
net_no_dir <- net_felipe_ego$net_no_dir
gnr_mtrs <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
                   Value  = c(igraph::edge_density(net_no_dir),
                              igraph::centralization.betweenness(net_no_dir)$centralization,
                              igraph::centralization.degree (net_no_dir, mode = "all")$centralization,
                              igraph:: cluster_walktrap(net_no_dir) %>% igraph::modularity(),
                              mean(igraph::degree(net_no_dir, mode="all", normalized = F)),
                              igraph::transitivity(net_no_dir),
                              igraph::assortativity.degree(net_no_dir, directed = F),
                              eigen_centrality(net_no_dir, directed = F)$value
                   )) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  data.frame()

to_rm <- d2 %>% 
  dplyr::mutate(ids = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::pull(ids) %>%
  unique()

role_ppa_rm <- role_ppa[[name_rodada]] %>%
  dplyr::mutate(ids = paste0(ID_node, "_", ID_ego)) %>% 
  dplyr::filter(ids %in% to_rm) %>% 
  dplyr::filter(!duplicated(ids)) %>% 
  dplyr::select(-ids, -ID_ego)


node_mtrs <- tibble(nodes_id = V(net_no_dir)$name) %>% 
  dplyr::left_join(., groups[[name_rodada]], by = c("nodes_id" = "ID_node")) %>%
  dplyr::select(nodes_id, Nome,V1) %>%
  dplyr::mutate(degree         = igraph::degree(net_no_dir, mode="all", normalized = F),
                betwe          = igraph::betweenness(net_no_dir) %>% round(.,1),
                closeness      = igraph::closeness(net_no_dir, mode="all") %>% round(.,4),
                ClusWalk       = igraph::cluster_walktrap(net_no_dir) %>% membership(),
                transitivLocal = igraph::transitivity(net_no_dir, type = "local")  %>% round(.,2),
                Eigen_centr    = igraph::eigen_centrality(net_no_dir, directed = T)$vector %>%  round(.,2)
  ) %>%
  dplyr::left_join(., entrevistados[[name_rodada]] %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
  dplyr::left_join(., type_org[[name_rodada]] %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>%
  dplyr::left_join(., role_ppa_rm, by = c("nodes_id" = "ID_node")) %>% 
  dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )


to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx,
  gnr_mtrs = gnr_mtrs,
  node_mtrs = node_mtrs
)



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/madre_standard_net_results.xlsx"))

#####################################################################
############# ADICIONANDO LOS EGOS C  2021   #######################
###################################################################

nt1 <- inv_2021 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) 



d2 <- nt1 
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/madre_grC_net_graph.png"))


net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/madre_grC_net_graph_orgtype.png"))



###metricas
net_no_dir <- net_felipe_ego$net_no_dir
gnr_mtrs <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
                   Value  = c(igraph::edge_density(net_no_dir),
                              igraph::centralization.betweenness(net_no_dir)$centralization,
                              igraph::centralization.degree (net_no_dir, mode = "all")$centralization,
                              igraph:: cluster_walktrap(net_no_dir) %>% igraph::modularity(),
                              mean(igraph::degree(net_no_dir, mode="all", normalized = F)),
                              igraph::transitivity(net_no_dir),
                              igraph::assortativity.degree(net_no_dir, directed = F),
                              eigen_centrality(net_no_dir, directed = F)$value
                   )) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  data.frame()

to_rm <- d2 %>% 
  dplyr::mutate(ids = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::pull(ids) %>%
  unique()

role_ppa_rm <- role_ppa[[name_rodada]] %>%
  dplyr::mutate(ids = paste0(ID_node, "_", ID_ego)) %>% 
  dplyr::filter(ids %in% to_rm) %>% 
  dplyr::filter(!duplicated(ids)) %>% 
  dplyr::select(-ids, -ID_ego)


node_mtrs <- tibble(nodes_id = V(net_no_dir)$name) %>% 
  dplyr::left_join(., groups[[name_rodada]], by = c("nodes_id" = "ID_node")) %>%
  dplyr::select(nodes_id, Nome,V1) %>%
  dplyr::mutate(degree         = igraph::degree(net_no_dir, mode="all", normalized = F),
                betwe          = igraph::betweenness(net_no_dir) %>% round(.,1),
                closeness      = igraph::closeness(net_no_dir, mode="all") %>% round(.,4),
                ClusWalk       = igraph::cluster_walktrap(net_no_dir) %>% membership(),
                transitivLocal = igraph::transitivity(net_no_dir, type = "local")  %>% round(.,2),
                Eigen_centr    = igraph::eigen_centrality(net_no_dir, directed = T)$vector %>%  round(.,2)
  ) %>%
  dplyr::left_join(., entrevistados[[name_rodada]] %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
  dplyr::left_join(., type_org[[name_rodada]] %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>%
  dplyr::left_join(., role_ppa_rm, by = c("nodes_id" = "ID_node")) %>% 
  dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )


to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx,
  gnr_mtrs = gnr_mtrs,
  node_mtrs = node_mtrs
)



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/madre_grC_net_results.xlsx"))


##################################################
################## 2022 #########################
################################################


inv_2022 <- lapply(c("_invest", "_nego", "_coop", "_colab", "outras_org"), function(i){
  x <- raw_list_2022[[grep(i, names(raw_list_2022))]] %>% 
    dplyr::select(R_Start_ID, contains("R_Destiny_ID"), contains("R5b_Destiny_ID") , R_Start_ID_ego)
  names(x) <- c("R_Start_ID", "R_Destiny_ID", "R_Start_ID_ego" )
  x$type <- i
  return(x)
}) %>% 
  bind_rows() %>% 
  drop_na()

outras_org_2022 <- raw_list_2022[[grep("outras_org", names(raw_list_2022))]]

name_rodada <- "rodada_2022"


############


nt1 <- inv_2022 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) %>% 
  dplyr::filter(type != "outras_org")


d2 <- nt1 
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/madre_standard_net_graph.png"))

net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/madre_standard_net_graph_orgtype.png"))


###metricas
net_no_dir <- net_felipe_ego$net_no_dir
gnr_mtrs <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
                   Value  = c(igraph::edge_density(net_no_dir),
                              igraph::centralization.betweenness(net_no_dir)$centralization,
                              igraph::centralization.degree (net_no_dir, mode = "all")$centralization,
                              igraph:: cluster_walktrap(net_no_dir) %>% igraph::modularity(),
                              mean(igraph::degree(net_no_dir, mode="all", normalized = F)),
                              igraph::transitivity(net_no_dir),
                              igraph::assortativity.degree(net_no_dir, directed = F),
                              eigen_centrality(net_no_dir, directed = F)$value
                   )) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  data.frame()

to_rm <- d2 %>% 
  dplyr::mutate(ids = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::pull(ids) %>%
  unique()

role_ppa_rm <- role_ppa[[name_rodada]] %>%
  dplyr::mutate(ids = paste0(ID_node, "_", ID_ego)) %>% 
  dplyr::filter(ids %in% to_rm) %>% 
  dplyr::filter(!duplicated(ids)) %>% 
  dplyr::select(-ids, -ID_ego)


node_mtrs <- tibble(nodes_id = V(net_no_dir)$name) %>% 
  dplyr::left_join(., groups[[name_rodada]], by = c("nodes_id" = "ID_node")) %>%
  dplyr::select(nodes_id, Nome,V1) %>%
  dplyr::mutate(degree         = igraph::degree(net_no_dir, mode="all", normalized = F),
                betwe          = igraph::betweenness(net_no_dir) %>% round(.,1),
                closeness      = igraph::closeness(net_no_dir, mode="all") %>% round(.,4),
                ClusWalk       = igraph::cluster_walktrap(net_no_dir) %>% membership(),
                transitivLocal = igraph::transitivity(net_no_dir, type = "local")  %>% round(.,2),
                Eigen_centr    = igraph::eigen_centrality(net_no_dir, directed = T)$vector %>%  round(.,2)
  ) %>%
  dplyr::left_join(., entrevistados[[name_rodada]] %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
  dplyr::left_join(., type_org[[name_rodada]] %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>%
  dplyr::left_join(., role_ppa_rm, by = c("nodes_id" = "ID_node")) %>% 
  dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )


to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx,
  gnr_mtrs = gnr_mtrs,
  node_mtrs = node_mtrs
)



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/madre_standard_net_results.xlsx"))

#####################################################################
############# ADICIONANDO LOS EGOS C  2022   #######################
###################################################################

nt1 <- inv_2022 %>% 
  dplyr::filter(!R_Destiny_ID %in% c(25, 75, 900) ) %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) 



d2 <- nt1 
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/madre_grC_net_graph.png"))


net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/madre_grC_net_graph_orgtype.png"))



###metricas
net_no_dir <- net_felipe_ego$net_no_dir
gnr_mtrs <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
                   Value  = c(igraph::edge_density(net_no_dir),
                              igraph::centralization.betweenness(net_no_dir)$centralization,
                              igraph::centralization.degree (net_no_dir, mode = "all")$centralization,
                              igraph:: cluster_walktrap(net_no_dir) %>% igraph::modularity(),
                              mean(igraph::degree(net_no_dir, mode="all", normalized = F)),
                              igraph::transitivity(net_no_dir),
                              igraph::assortativity.degree(net_no_dir, directed = F),
                              eigen_centrality(net_no_dir, directed = F)$value
                   )) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  data.frame()

to_rm <- d2 %>% 
  dplyr::mutate(ids = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::pull(ids) %>%
  unique()

role_ppa_rm <- role_ppa[[name_rodada]] %>%
  dplyr::mutate(ids = paste0(ID_node, "_", ID_ego)) %>% 
  dplyr::filter(ids %in% to_rm) %>% 
  dplyr::filter(!duplicated(ids)) %>% 
  dplyr::select(-ids, -ID_ego)


node_mtrs <- tibble(nodes_id = V(net_no_dir)$name) %>% 
  dplyr::left_join(., groups[[name_rodada]], by = c("nodes_id" = "ID_node")) %>%
  dplyr::select(nodes_id, Nome,V1) %>%
  dplyr::mutate(degree         = igraph::degree(net_no_dir, mode="all", normalized = F),
                betwe          = igraph::betweenness(net_no_dir) %>% round(.,1),
                closeness      = igraph::closeness(net_no_dir, mode="all") %>% round(.,4),
                ClusWalk       = igraph::cluster_walktrap(net_no_dir) %>% membership(),
                transitivLocal = igraph::transitivity(net_no_dir, type = "local")  %>% round(.,2),
                Eigen_centr    = igraph::eigen_centrality(net_no_dir, directed = T)$vector %>%  round(.,2)
  ) %>%
  dplyr::left_join(., entrevistados[[name_rodada]] %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
  dplyr::left_join(., type_org[[name_rodada]] %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>%
  dplyr::left_join(., role_ppa_rm, by = c("nodes_id" = "ID_node")) %>% 
  dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )


to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx,
  gnr_mtrs = gnr_mtrs,
  node_mtrs = node_mtrs
)



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/madre_grC_net_results.xlsx"))








