#############################################################
################## RED DE NEGOCIOS ########################
###########################################################
####### 2019 ##########

inv_2019 <- raw_list_2019[[grep("_nego", names(raw_list_2019))]]
outras_org_2019 <- raw_list_2019[[grep("outras_org", names(raw_list_2019))]]

name_rodada <- "rodada_2019"


#####################################################################
############# ADICIONANDO LOS EGOS C      ##########################
###################################################################


nt1 <- inv_2019  %>% 
  dplyr::mutate(id = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::filter(!id  %in% c("42_a","42_c","49_a","49_b","76_a")) %>% 
  dplyr::select(-id, -contains("comentario")) 


d <- nt1 %>% 
  dplyr::select( R_Start_ID,  R_Destiny_ID, R1a, R1b, R_Start_ID_ego) %>%
  dplyr::mutate_all(as.character)



nt2 <- outras_org_2019 %>%
  dplyr::select(R_Start_ID, R_Destiny_ID = R5b_Destiny_ID, R5c, R_Start_ID_ego) %>% 
  drop_na() %>%  
  dplyr::filter( !R_Destiny_ID %in% c(900, 25)) %>% 
  mutate_all(as.character) %>% 
  dplyr::filter(R5c == "1") %>% 
  dplyr::select( -R5c)

nt_all <- bind_rows(d, nt2) %>% 
  dplyr::mutate(R1b = if_else(is.na(R1b), "9", R1b))%>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") )


net_felipe_ego_grC <- net_graphs(edge_list = nt_all, 
                                 groups = groups[[name_rodada]], 
                                 gr_colors = gr_colors[[name_rodada]], 
                                 preguntas = preguntas[[name_rodada]], 
                                 out_file = paste0(out_dirs[[name_rodada]], "/negocios_grC_net.png"))




###metricas

net_felipe_ego_mtrs_grC <- net_metrics(edge_list_raw = nt_all, 
                                       net = net_felipe_ego_grC$net, 
                                       net_no_dir = net_felipe_ego_grC$net_no_dir, 
                                       entrevistados = entrevistados[[name_rodada]], 
                                       type_org = type_org[[name_rodada]], 
                                       groups = groups[[name_rodada]], 
                                       preguntas = preguntas[[name_rodada]],
                                       role_ppa = role_ppa[[name_rodada]],
                                       Ra = "R1a", 
                                       Rb = "R1b",
                                       Rc = "R1c")


net_felipe_ego_mtrs_grC$nodes_to_add <- tibble(ID_node = unique(c(nt2$R_Start_ID, nt2$R_Destiny_ID))) %>% 
  dplyr::left_join(.,  type_org[[name_rodada]], by = "ID_node") %>% 
  dplyr::filter(V1 != "C") %>%
  dplyr::filter(!ID_node %in% c(d$R_Start_ID, d$R_Destiny_ID)) %>% 
  dplyr::select(ID_node, V1, `Type of organization`) %>% 
  dplyr::mutate(id = paste0(V1, "-",`Type of organization` ))


net_felipe_ego_mtrs_grC <- net_felipe_ego_mtrs_grC[!sapply(net_felipe_ego_mtrs_grC, is.null)]
to_save <- list(
  adj_mtx = net_felipe_ego_grC$adj_mtx
)
for(i in 1:length(net_felipe_ego_mtrs_grC)){
  
  to_save[[i+1]] <- net_felipe_ego_mtrs_grC[[i]]
  names(to_save)[i+1] <- names(net_felipe_ego_mtrs_grC[i])
}



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/negocios_grC_net_results.xlsx"))


##negocios no tiene direccion




######################################################################
########### REMOVIENDO LOS EGOS QUE DIJO FELIPE ##################### 
####################################################################

nt1 <- inv_2019 %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) 


nt1 <- nt1 %>% 
  dplyr::mutate(id = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::filter(!id  %in% c("42_a","42_c","49_a","49_b","76_a")) %>% 
  dplyr::select(-id, -contains("coment")) 


d2 <- nt1 %>%
  dplyr::mutate(across(everything(.), as.character)) %>% 
  # dplyr::filter(R1a != 2) %>%
  # dplyr::filter(R1b != 9) %>% 
  dplyr::select(R_Start_ID, R_Destiny_ID, starts_with("R1"))
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/negocios_standard_net_graph.png"))


net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/negocios_standard_net_graph_orgtype.png"))



###metricas

net_felipe_ego_mtrs <- net_metrics(edge_list_raw = nt1, 
                                   net = net_felipe_ego$net, 
                                   net_no_dir = net_felipe_ego$net_no_dir, 
                                   entrevistados = entrevistados[[name_rodada]], 
                                   type_org = type_org[[name_rodada]], 
                                   groups = groups[[name_rodada]], 
                                   preguntas = preguntas[[name_rodada]],
                                   role_ppa = role_ppa[[name_rodada]],
                                   Ra = "R1a", 
                                   Rb = "R1b",
                                   Rc = "R1c")


net_felipe_ego_mtrs$company_count <- net_felipe_ego_mtrs$company_count %>%
  dplyr::filter(V1 != "Total") %>% 
  mutate(id = paste0(V1 , "-",   `Type of organization`)) %>% 
  dplyr::left_join(., net_felipe_ego_mtrs_grC$nodes_to_add %>% 
                     dplyr::group_by(id ) %>% 
                     dplyr::tally() %>% 
                     dplyr::rename("n2" = n), by = "id" ) %>% 
  dplyr::mutate( n = rowSums(across(c(n,n2)), na.rm = T) ) %>% 
  dplyr::select(-id, -n2) %>% 
  dplyr::add_row(V1= "Total", `Type of organization`= "Total count of companies ",  n = sum(.$n))


net_felipe_ego_mtrs <- net_felipe_ego_mtrs[!sapply(net_felipe_ego_mtrs, is.null)]
to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx
)
for(i in 1:length(net_felipe_ego_mtrs)){

  to_save[[i+1]] <- net_felipe_ego_mtrs[[i]]
  names(to_save)[i+1] <- names(net_felipe_ego_mtrs[i])
}



writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/negocios_standard_net_results.xlsx"))




##################################################
################## 2020 #########################
################################################


inv_2020 <- raw_list_2020[[grep("_negocios", names(raw_list_2020))]]
outras_org_2020 <- raw_list_2020[[grep("outras_org", names(raw_list_2020))]]
name_rodada <- "rodada_2020"



#####################################################################
############# ADICIONANDO LOS EGOS C  2020   #######################
###################################################################

nt1 <- inv_2020 %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) %>% 
  dplyr::select(-contains("Coment")) 


d <- nt1 %>% 
  dplyr::select( R_Start_ID,  R_Destiny_ID, R1a, R1b, R_Start_ID_ego) %>%
  dplyr::mutate_all(as.character)


nt2 <- outras_org_2020 %>%
  dplyr::select(R_Start_ID, R_Destiny_ID = R5b_Destiny_ID, R5c, R_Start_ID_ego) %>% 
  drop_na() %>% 
  mutate_all(as.character) %>% 
  dplyr::filter(R5c == "1") %>% 
  dplyr::select( -R5c)


nt_all <- bind_rows(d, nt2) %>% 
  dplyr::mutate(R1b = if_else(is.na(R1b), "9", R1b))%>% 
  dplyr::left_join(., groups[[name_rodada]] %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") )  

net_felipe_ego_grC <-net_graphs(edge_list = nt_all, 
                                groups = groups[[name_rodada]], 
                                gr_colors = gr_colors[[name_rodada]], 
                                preguntas = preguntas[[name_rodada]], 
                                out_file = paste0(out_dirs[[name_rodada]], "/negocios_grC_net.png"))

###metricas

net_felipe_ego_mtrs_grC <- net_metrics(edge_list_raw = nt_all, 
                                       net = net_felipe_ego_grC$net, 
                                       net_no_dir = net_felipe_ego_grC$net_no_dir, 
                                       entrevistados = entrevistados[[name_rodada]], 
                                       type_org = type_org[[name_rodada]], 
                                       groups = groups[[name_rodada]], 
                                       preguntas = preguntas[[name_rodada]], 
                                       role_ppa = role_ppa[[name_rodada]],
                                       Ra = "R1a", 
                                       Rb = "R1b",
                                       Rc = "R1c")

net_felipe_ego_mtrs_grC$nodes_to_add <- tibble(ID_node = unique(c(nt2$R_Start_ID, nt2$R_Destiny_ID))) %>% 
  dplyr::left_join(.,  type_org[[name_rodada]], by = "ID_node") %>% 
  dplyr::filter(V1 != "C") %>%
  dplyr::filter(!ID_node %in% c(d$R_Start_ID, d$R_Destiny_ID)) %>%  
  dplyr::select(ID_node, V1, `Type of organization`) %>% 
  dplyr::mutate(id = paste0(V1, "-",`Type of organization` ))



net_felipe_ego_mtrs_grC <- net_felipe_ego_mtrs_grC[!sapply(net_felipe_ego_mtrs_grC, is.null)]
to_save <- list(
  adj_mtx = net_felipe_ego_grC$adj_mtx
)
for(i in 1:length(net_felipe_ego_mtrs_grC)){
  to_save[[i+1]] <- net_felipe_ego_mtrs_grC[[i]]
  names(to_save)[i+1] <- names(net_felipe_ego_mtrs_grC[i])
}


writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/negocios_grC_net_results.xlsx"))




############


nt1 <- inv_2020 %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups$rodada_2020 %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") )


d2 <- nt1 %>%
  dplyr::mutate(across(everything(.), as.character)) %>% 
  # dplyr::filter(R1a != 2) %>%
  # dplyr::filter(R1b != 9) %>% 
  dplyr::select(R_Start_ID, R_Destiny_ID, starts_with("R1"))
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/negocios_standard_net_graph.png"))


net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/negocios_standard_net_graph_orgtype.png"))


###metricas

net_felipe_ego_mtrs <- net_metrics(edge_list_raw = nt1, 
                                   net = net_felipe_ego$net, 
                                   net_no_dir = net_felipe_ego$net_no_dir, 
                                   entrevistados = entrevistados[[name_rodada]], 
                                   type_org = type_org[[name_rodada]], 
                                   groups = groups[[name_rodada]], 
                                   preguntas = preguntas[[name_rodada]], 
                                   role_ppa = role_ppa[[name_rodada]],
                                   Ra = "R1a", 
                                   Rb = "R1b",
                                   Rc = "R1c")


net_felipe_ego_mtrs$company_count <- net_felipe_ego_mtrs$company_count %>%
  dplyr::filter(V1 != "Total") %>% 
  mutate(id = paste0(V1 , "-",   `Type of organization`)) %>% 
  dplyr::left_join(., net_felipe_ego_mtrs_grC$nodes_to_add %>% 
                     dplyr::group_by(id ) %>% 
                     dplyr::tally() %>% 
                     dplyr::rename("n2" = n), by = "id" ) %>% 
  dplyr::mutate( n = rowSums(across(c(n,n2)), na.rm = T) ) %>% 
  dplyr::select(-id, -n2) %>% 
  dplyr::add_row(V1= "Total", `Type of organization`= "Total count of companies ",  n = sum(.$n))

net_felipe_ego_mtrs <- net_felipe_ego_mtrs[!sapply(net_felipe_ego_mtrs, is.null)]
to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx
)
for(i in 1:length(net_felipe_ego_mtrs)){
  
  to_save[[i+1]] <- net_felipe_ego_mtrs[[i]]
  names(to_save)[i+1] <- names(net_felipe_ego_mtrs[i])
}


writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/negocios_standard_net_results.xlsx"))




#######################################
######      EDGE LIST JF    ##########
#####################################

entrev_2020 <- inv_2020 %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::pull(R_Start_ID) %>% 
  unique

edge_lst <- inv_2019 %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::mutate(id = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
  dplyr::filter(!id  %in% c("42_a","42_c","49_a","49_b","76_a")) %>% 
  dplyr::filter(!R_Start_ID %in% entrev_2020) %>% 
  dplyr::filter(!R_Destiny_ID %in% entrev_2020) %>% 
  dplyr::select(-contains("ego"), - contains("comentario"))

net <- graph_from_data_frame(d=  edge_lst,
                             vertices = groups[["rodada_2019"]] %>% 
                               dplyr::select(ID_node, V1) ,
                             directed= T) %>%
  igraph::delete_vertices(., igraph::degree(.) == 0) %>% 
  igraph::delete_edges(., edges = which(igraph::which_multiple(.)))

saveRDS(net,  paste0(out_dirs[["rodada_2019"]],"/negocios_edge_list_JF.rds"))


########################################
#######     rodada 2021      ##########
#######################################

inv_2021 <- raw_list_2021[[grep("_nego", names(raw_list_2021))]]
outras_org_2021 <- raw_list_2021[[grep("outras_org", names(raw_list_2021))]]

name_rodada <- "rodada_2021"


#####################################################################
############# ADICIONANDO LOS EGOS C  2021   #######################
###################################################################


nt1 <- inv_2021 %>%
  dplyr::mutate(across(everything(.), as.character)) %>%
  dplyr::left_join(., groups[[name_rodada]] %>%
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") ) %>%
  dplyr::select(-contains("Coment"))


d <- nt1 %>%
  dplyr::select( R_Start_ID,  R_Destiny_ID, R1a, R1b, R_Start_ID_ego) %>%
  dplyr::mutate_all(as.character)


nt2 <- outras_org_2021 %>%
  dplyr::select(R_Start_ID, R_Destiny_ID = R5b_Destiny_ID, R5c, R_Start_ID_ego) %>%
  drop_na() %>%
  mutate_all(as.character) %>%
  dplyr::filter(R5c == "1") %>%
  dplyr::select( -R5c)

nt_all <- bind_rows(d, nt2) %>%
  dplyr::mutate(R1b = if_else(is.na(R1b), "9", R1b))%>%
  dplyr::left_join(., groups[[name_rodada]] %>%
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") )

net_felipe_ego_grC <-net_graphs(edge_list = nt_all,
                                groups = groups[[name_rodada]],
                                gr_colors = gr_colors[[name_rodada]],
                                preguntas = preguntas[[name_rodada]],
                                out_file = paste0(out_dirs[[name_rodada]], "/negocios_grC_net.png"))

###metricas

net_felipe_ego_mtrs_grC <- net_metrics(edge_list_raw = nt_all,
                                       net = net_felipe_ego_grC$net,
                                       net_no_dir = net_felipe_ego_grC$net_no_dir,
                                       entrevistados = entrevistados[[name_rodada]],
                                       type_org = type_org[[name_rodada]],
                                       groups = groups[[name_rodada]],
                                       preguntas = preguntas[[name_rodada]],
                                       role_ppa = role_ppa[[name_rodada]],
                                       Ra = "R1a",
                                       Rb = "R1b",
                                       Rc = "R1c")

net_felipe_ego_mtrs_grC$nodes_to_add <- tibble(ID_node = unique(c(nt2$R_Start_ID, nt2$R_Destiny_ID))) %>%
  dplyr::left_join(.,  type_org[[name_rodada]], by = "ID_node") %>%
  dplyr::filter(V1 != "C") %>%
  dplyr::filter(!ID_node %in% c(d$R_Start_ID, d$R_Destiny_ID)) %>%
  dplyr::select(ID_node, V1, `Type of organization`) %>%
  dplyr::mutate(id = paste0(V1, "-",`Type of organization` ))



net_felipe_ego_mtrs_grC <- net_felipe_ego_mtrs_grC[!sapply(net_felipe_ego_mtrs_grC, is.null)]
to_save <- list(
  adj_mtx = net_felipe_ego_grC$adj_mtx
)
for(i in 1:length(net_felipe_ego_mtrs_grC)){
  to_save[[i+1]] <- net_felipe_ego_mtrs_grC[[i]]
  names(to_save)[i+1] <- names(net_felipe_ego_mtrs_grC[i])
}


writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/negocios_grC_net_results.xlsx"))


####### red normal


nt1 <- inv_2021 %>% 
  dplyr::mutate(across(everything(.), as.character)) %>% 
  dplyr::left_join(., groups$rodada_2021 %>% 
                     dplyr::select(ID_node, V1), by = c("R_Start_ID" = "ID_node") )


d2 <- nt1 %>%
  dplyr::mutate(across(everything(.), as.character)) %>% 
  # dplyr::filter(R1a != 2) %>%
  # dplyr::filter(R1b != 9) %>% 
  dplyr::select(R_Start_ID, R_Destiny_ID, starts_with("R1"))
#dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019, var = "R2a", var_lab = "label"),  by = c("R2a", "Opcoes_numero"))

net_felipe_ego <-net_graphs(edge_list = d2, 
                            groups = groups[[name_rodada]], 
                            gr_colors = gr_colors[[name_rodada]], 
                            preguntas = preguntas[[name_rodada]], 
                            out_file = paste0(out_dirs[[name_rodada]], "/negocios_standard_net_graph.png"))


net_graphs_orgtype(edge_list = d2, 
                   groups = groups[[name_rodada]], 
                   type_org = type_org[[name_rodada]], 
                   preguntas = preguntas[[name_rodada]], 
                   out_file = paste0(out_dirs[[name_rodada]], "/negocios_standard_net_graph_orgtype.png"))


###metricas

net_felipe_ego_mtrs <- net_metrics(edge_list_raw = nt1, 
                                   net = net_felipe_ego$net, 
                                   net_no_dir = net_felipe_ego$net_no_dir, 
                                   entrevistados = entrevistados[[name_rodada]], 
                                   type_org = type_org[[name_rodada]], 
                                   groups = groups[[name_rodada]], 
                                   preguntas = preguntas[[name_rodada]], 
                                   role_ppa = role_ppa[[name_rodada]],
                                   Ra = "R1a", 
                                   Rb = "R1b",
                                   Rc = "R1c")


net_felipe_ego_mtrs$company_count <- net_felipe_ego_mtrs$company_count %>%
  dplyr::filter(V1 != "Total") %>% 
  mutate(id = paste0(V1 , "-",   `Type of organization`)) %>% 
  dplyr::left_join(., net_felipe_ego_mtrs_grC$nodes_to_add %>% 
                     dplyr::group_by(id ) %>% 
                     dplyr::tally() %>% 
                     dplyr::rename("n2" = n), by = "id" ) %>% 
  dplyr::mutate( n = rowSums(across(c(n,n2)), na.rm = T) ) %>% 
  dplyr::select(-id, -n2) %>% 
  dplyr::add_row(V1= "Total", `Type of organization`= "Total count of companies ",  n = sum(.$n))

net_felipe_ego_mtrs <- net_felipe_ego_mtrs[!sapply(net_felipe_ego_mtrs, is.null)]
to_save <- list(
  adj_mtx = net_felipe_ego$adj_mtx
)
for(i in 1:length(net_felipe_ego_mtrs)){
  
  to_save[[i+1]] <- net_felipe_ego_mtrs[[i]]
  names(to_save)[i+1] <- names(net_felipe_ego_mtrs[i])
}


writexl::write_xlsx(to_save, path = paste0(out_dirs[[name_rodada]],"/negocios_standard_net_results.xlsx"))



















