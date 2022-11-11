### script Andres Camilo Mendez Alzate
### sep 2021

### instalar paquetes
library(pacman)
pacman::p_load(tidyverse, readr, igraph, statnet, readxl, FactoMineR, zoo, likert, lubridate, hrbrthemes,
               igraph, statnet, qgraph, kableExtra, flextable, magrittr, RColorBrewer, xlsx, scales, rlang, writexl, plyr,
               intergraph, GGally, Redmonder, colorspace)



get_questions_opts <- function(data, var, var_lab, lan = "en"){
  
  var_lab <- sym(var_lab)
  
  ret <- data %>% 
    dplyr::filter(Variavel == var)
  
  if(grepl("múltipla escolha|múltiplas respostas|múltiplas", ret$Metrica)){
    
    ret <- ret %>%
      dplyr::pull(vars_opts) %>% 
      purrr::pluck(1) %>% 
      dplyr::filter(if_any(contains("Used_opcoes"), ~ as.logical(.))) 
    
    if(lan == "en"){
      
      ret <- ret %>% 
        dplyr::rename(!!var_lab := `Labels in English`) %>% 
        dplyr::select("Opcoes_numero" = Variavel , !!var_lab)
      
    }else{
      ret <- ret %>% 
        dplyr::rename(!!var_lab := Opcoes_nome) %>% 
        dplyr::select("Opcoes_numero" = Variavel , !!var_lab)
      
    }
    
    
  }else{
    
    ret <- ret %>%
      dplyr::pull(vars_opts) %>% 
      purrr::pluck(1) %>% 
      dplyr::filter(if_any(contains("Used_opcoes"), ~ as.logical(.)))
    
    if(lan == "en"){
      ret <- ret %>% 
        dplyr::rename(!!var_lab := `Labels in English`) %>% 
        dplyr::select(Opcoes_numero, !!var_lab)
    }else{
      ret <- ret %>% 
        dplyr::rename(!!var_lab := Opcoes_nome) %>% 
        dplyr::select(Opcoes_numero, !!var_lab)
    }
    
  }
  
  return(ret)
}

get_var_label      <- function(data, var, lan = "en"){
  
  if(lan == "en"){
    ret <- data %>% 
      dplyr::filter(Variavel == var) %>%
      dplyr::pull(`Labels in English`)
  }else{
    ret <- data %>% 
      dplyr::filter(Variavel == var) %>%
      dplyr::pull(Variavel_nome)
  }
  
  
  return(ret)
}

calc_counts        <- function(vec, val){
  ret <- length(vec[vec == val])
  return(ret)
}

net_graphs         <- function(edge_list, groups, gr_colors, preguntas, out_file){
  
  if(any(grepl("R[0-9](a|b)", names(edge_list)))){
    edge_list<- edge_list %>% 
      dplyr::arrange( across((matches("R[0-9](a|b)")) ))
    
  }
  
  
  net <- graph_from_data_frame(d=  edge_list,
                               vertices = groups %>% 
                                 dplyr::select(ID_node, V1) ,
                               directed= T) %>%
    igraph::delete_vertices(., igraph::degree(.) == 0) %>% 
    igraph::delete_edges(., edges = which(igraph::which_multiple(.)))
  
  saveRDS(net, gsub(".png", ".rds", out_file))
  
  
  net_no_dir <- graph_from_data_frame(d=  edge_list,
                                      vertices = groups %>% 
                                        dplyr::select(ID_node, V1) ,
                                      directed=F) %>%
    igraph::delete_vertices(., igraph::degree(.) == 0) %>% 
    igraph::delete_edges(., edges = which(igraph::which_multiple(.)))
  
  #save adjacencyu matrix
   adj_mtx <- as_adjacency_matrix(net_no_dir, type = "both", sparse = F) %>% 
    as.data.frame() 
  
  
  v_color <- tibble(id = V(net_no_dir)$name) %>% 
    dplyr::left_join(., groups, by = c("id" = "ID_node") ) %>% 
    dplyr::left_join(., gr_colors, by = c("V1" = "V1")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas, var = "V1_xxxx", var_lab = "Groups"), 
                     by = c("V1" = "Opcoes_numero"))
  
  
  net1 <-  intergraph::asNetwork(simplify(net_no_dir))
  network.vertex.names(net1) = v_color$id
  net1 %v% "color" = v_color$color
  net1 %v% "Groups" = v_color$Groups
  
  plt <- v_color %>% 
    dplyr::group_by(Groups, color) %>% 
    dplyr::tally() %>% 
    dplyr::pull( color, Groups) 
  
  nplot <- GGally::ggnet2(net1,
                          color = "Groups",
                          palette = plt ,#c("PPA members" = "#d6604d", "PPA Collaborators" = "#4393c3"),
                          size = 5, 
                          shape = 19,
                          mode = "fruchtermanreingold", 
                          label = "vertex.names",
                          color.legend = "Groups",
                          legend.zise = 12, 
                          legend.position = "left",
                          label.size = 3,
                          label.alpha = 1,
                          arrow.size = 5,
                          arrow.gap = 0.025) + theme(legend.position = "none")
  
  ggsave(filename = out_file, plot= nplot, dpi = 400, units="in", width=8, height=5)
  
  
  return(list(
    net = net,
    net_no_dir = net_no_dir,
    adj_mtx = adj_mtx,
    g_plot = nplot 
  ))
  
}

net_graphs_orgtype <- function(edge_list, groups, type_org, preguntas, out_file){
  
  if(any(grepl("R[0-9](a|b)", names(edge_list)))){
    edge_list<- edge_list %>% 
      dplyr::arrange( across((matches("R[0-9](a|b)")) ))
    
  }
  
  type_org <- type_org %>% 
    dplyr::select(ID_node, type = `Type of organization`) %>% 
    dplyr::mutate(type = case_when(
      type == "Company (consolidated or start-up)" ~ type, #azul
      type == "Civil society organization" ~ type,  #rojitp 
      TRUE ~ "Others"
      
    ))
  
  net <- graph_from_data_frame(d=  edge_list,
                               vertices = groups %>% 
                                 dplyr::select(ID_node, V1) ,
                               directed= T) %>%
    igraph::delete_vertices(., igraph::degree(.) == 0) %>% 
    igraph::delete_edges(., edges = which(igraph::which_multiple(.)))
  
  #saveRDS(net, gsub(".png", ".rds", out_file))
  
  
  net_no_dir <- graph_from_data_frame(d=  edge_list,
                                      vertices = groups %>% 
                                        dplyr::select(ID_node, V1) ,
                                      directed=F) %>%
    igraph::delete_vertices(., igraph::degree(.) == 0) %>% 
    igraph::delete_edges(., edges = which(igraph::which_multiple(.)))
  
  #save adjacencyu matrix
 
  orgs_types <- get_questions_opts(preguntas, var = "V39b", var_lab = "type")
  
  gr_colors <- tibble(type  = c(unique(type_org$type), "USAID"),
                      color = case_when(
                                        type == "Company (consolidated or start-up)" ~ "#5353c6", #azul
                                        type == "Civil society organization" ~ "#669900", #verde oliva
                                        type == "USAID" ~ "#ff0000", #rojitp 
                                        TRUE ~ "#999999"
                                        
                      ) )#pals::stepped(n = length(unique(type_org$type))))#c(Redmonder::redmonder.pal(8, "qMSOGn"), brewer.pal(0, "Blues") ))
  
  # color  = colorspace::diverging_hcl(length(unique(orgs_types$type)), 
  #                                    palette = "Cork") %>% rev
  
  
  #plot(1:4, 1:4, pch = 16, cex = 8, col = colorspace::sequential_hcl(4, palette = "Grays") )
  
  v_color <- tibble(id = V(net_no_dir)$name) %>% 
    dplyr::left_join(., type_org, by = c("id" = "ID_node")) %>%
    dplyr::mutate(type = ifelse(id == "76", "USAID", type),
                  type = ifelse(is.na(type), "Others", type)) %>% 
    dplyr::left_join(., groups, by = c("id" = "ID_node") ) %>% 
    dplyr::left_join(., gr_colors, by = c("type" = "type")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas, var = "V1_xxxx", var_lab = "Groups"), 
                     by = c("V1" = "Opcoes_numero"))
  
  #stopifnot("NA encontrados en type org: ", all(is.na(v_color$type)))
  
  net1 <-  intergraph::asNetwork(simplify(net_no_dir))
  network.vertex.names(net1) = v_color$id
  net1 %v% "color" = v_color$color
  net1 %v% "org_type" = v_color$type
  
  plt <- v_color %>% 
    dplyr::group_by(type, color) %>% 
    dplyr::tally() %>% 
    dplyr::pull( color, type) 
  
  
  nplot <- GGally::ggnet2(net1,
                          color = "org_type",
                          palette = plt ,#c("PPA members" = "#d6604d", "PPA Collaborators" = "#4393c3"),
                          size = 5, 
                          shape = 19,
                          mode = "fruchtermanreingold", 
                          label = "vertex.names",
                          color.legend = "Groups",
                          legend.zise = 12, 
                          legend.position = "left",
                          label.size = 3,
                          label.alpha = 1) + theme(legend.position = "bottom",
                                                   legend.text = element_text(size=5))
  
  ggsave(filename = out_file, plot= nplot, dpi = 400, units="in", width=8, height=6)
  
  
  
}



net_metrics <- function(edge_list_raw, net, net_no_dir, entrevistados, type_org, groups, preguntas, role_ppa, Ra, Rb, Rc){

  
  to_rm <- edge_list_raw %>% 
    dplyr::arrange(across(matches("R[0-9](a|b)"))) %>% 
    dplyr::mutate(ids = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>% 
    dplyr::pull(ids) %>%
    unique()
  
  role_ppa <- role_ppa %>%
    dplyr::mutate(ids = paste0(ID_node, "_", ID_ego)) %>% 
    dplyr::filter(ids %in% to_rm)
  
  attrs <- igraph::get.edge.attribute(net)
  
  
  tot <- edge_list_raw %>% 
    group_by(V1) %>% 
    dplyr::summarise(Total = n())
  
  
  non_res_Ra <- edge_list_raw %>% 
    group_by(V1) %>%
    dplyr::summarise(Count = calc_counts(!!sym(Ra), val =2)) %>%
    dplyr::mutate( label1 = "Data not available") %>% 
    dplyr::select(V1, label1, n = Count)
  
  
  non_res_Rb <- edge_list_raw %>% 
    group_by(V1) %>% 
    dplyr::summarise(Count = calc_counts(!!sym(Rb), val =9)) %>%
    dplyr::mutate(  label2 = "Data not available")  %>% 
    dplyr::select(V1, label2, n = Count)
  
  if(Rc %in% names(edge_list_raw)){
    non_res_Rc <- edge_list_raw %>% 
      group_by(V1) %>% 
      dplyr::summarise(Count = calc_counts(!!sym(Rc), val =9)) %>%
      dplyr::mutate(  label3 = "Data not available")  %>% 
      dplyr::select(V1, label3, n = Count)
    
  }else{
    non_res_Rc <- NULL
  }
 
  
  
  
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
  
  
  node_mtrs <- tibble(nodes_id = V(net_no_dir)$name) %>% 
    dplyr::left_join(., groups, by = c("nodes_id" = "ID_node")) %>%
    dplyr::select(nodes_id, Nome,V1) %>%
    dplyr::mutate(degree         = igraph::degree(net_no_dir, mode="all", normalized = F),
                  betwe          = igraph::betweenness(net_no_dir) %>% round(.,1),
                  closeness      = igraph::closeness(net_no_dir, mode="all") %>% round(.,4),
                  ClusWalk       = igraph::cluster_walktrap(net_no_dir) %>% membership(),
                  transitivLocal = igraph::transitivity(net_no_dir, type = "local")  %>% round(.,2),
                  Eigen_centr    = igraph::eigen_centrality(net_no_dir, directed = T)$vector %>%  round(.,2)
    ) %>%
    dplyr::left_join(., entrevistados %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
    dplyr::left_join(., type_org %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>% 
    dplyr::left_join(., role_ppa, by = c("nodes_id" = "ID_node")) %>% 
    dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )
  
  if(Rc %in% names(edge_list_raw)){
    summ <-  tibble(var1 = unlist(attrs[Ra]), 
                    var2 = unlist(attrs[Rb]),
                    var3 = unlist(attrs[Rc]))
  }else{
    summ <-  tibble(var1 = unlist(attrs[Ra]), 
                    var2 = unlist(attrs[Rb]))
  }
  
  summ <- summ %>%
    dplyr::mutate( start  = igraph::ends(net, es=E(net), names=T)[,1]) %>%
    left_join(., groups %>% dplyr::select(ID_node, V1), by = c("start" = "ID_node"))%>%
    dplyr::left_join(., get_questions_opts(data = preguntas, 
                                           var = Ra, 
                                           var_lab = "label1"), by = c("var1" = "Opcoes_numero")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas, 
                                           var = Rb, 
                                           var_lab = "label2"), by = c("var2" = "Opcoes_numero")) 
    
    if(Rc %in% names(edge_list_raw)){
      summ <- summ %>% 
        dplyr::left_join(., get_questions_opts(data = preguntas, 
                                               var = Rc, 
                                               var_lab = "label3"), by = c("var3" = "Opcoes_numero"))
    }
   
  
  
  Ra_summ <- summ %>%
    group_by( V1, label1) %>%
    tally()  %>% 
    ungroup() %>%
    dplyr::group_by(V1) %>% 
    dplyr::summarise(V1, label1, n, freq = round(prop.table(n)*100,2)) %>% 
    #bind_rows(non_res_Ra)%>% 
    #dplyr::left_join(., tot, by = "V1") %>% 
    dplyr::mutate( freq = paste0(freq, "%") ) %>% 
    dplyr::select(Group = V1, !!get_var_label(data = preguntas, var = Ra) := label1, Count = n, Frequency = freq) 
  
  
  Rb_summ <- summ %>%
    group_by( V1, label2) %>%
    tally() %>% 
    ungroup() %>% 
    dplyr::group_by(V1) %>% 
    dplyr::summarise(V1, label2, n, freq = round(prop.table(n)*100,2)) %>% 
    #bind_rows(non_res_Rb)%>% 
    #dplyr::left_join(., tot, by = "V1") %>% 
    dplyr::mutate(freq = paste0(freq, "%") ) %>% 
    dplyr::select(Group = V1, !!get_var_label(data = preguntas, var = Rb) := label2, Count = n, Frequency = freq) 
  
  if(Rc %in% names(edge_list_raw)){
    Rc_summ<- summ %>%
      group_by( V1, label3) %>%
      tally()  %>% 
      ungroup() %>% 
      dplyr::group_by(V1) %>% 
      dplyr::summarise(V1, label3, n, freq = round(prop.table(n)*100,2)) %>% 
      # bind_rows(non_res_Rc)%>% 
      #dplyr::left_join(., tot, by = "V1") %>% 
      dplyr::mutate(freq = paste0(freq, "%") ) %>% 
      dplyr::select(Group = V1, !!get_var_label(data = preguntas, var = Rc) := label3, Count = n, Frequency = freq) 
    
  }else{
    Rc_summ <- NULL
  }
 
  
  
  non_response<- non_res_Rb
  non_response$n <- non_res_Ra$n + non_res_Rb$n 
  non_response$label1 <- non_response$label2
  non_response$freq <- NULL
  
  Ra_vs_Rb_summ <- summ %>%  
    group_by( V1, label1, label2) %>%
    tally()  %>% 
    ungroup() %>% 
    dplyr::group_by(V1) %>% 
    dplyr::summarise(V1, label1, label2, n, freq = round(prop.table(n)*100, 2)) %>% 
    dplyr::select(Group = V1, 
                  !!get_var_label(data = preguntas, var = Ra):= label1,
                  !!get_var_label(data = preguntas, var = Rb):= label2, Count = n) 
  
  
  links_count <- as_tibble(ends(net, E(net), names = T)) %>% 
    dplyr::select(start = V1, end = V2) %>% 
    left_join(., groups %>% dplyr::select(ID_node, Nome, V1), by = c("start" = "ID_node")) %>% 
    dplyr::select( ID_start = V1, everything()) %>% 
    dplyr::left_join(., groups %>% dplyr::select(ID_node, Nome, V1), by = c("end"= "ID_node")) %>% 
    dplyr::select(start, end,  ID_start,ID_end = V1, everything()) %>% 
    dplyr::group_by(ID_start, ID_end) %>% 
    tally() %>%
    ungroup() %>%
    dplyr::mutate(Ratio = paste(round(n/sum(n)*100,1), "%")) %>% 
    dplyr::select(Group_start = ID_start, Group_end = ID_end, Links_count = n, Ratio)
  
  start_up_links_counts <- as_tibble(ends(net, E(net), names = T)) %>% 
    dplyr::select(start = V1, end = V2) %>% 
    left_join(., groups %>% dplyr::select(ID_node, Nome, V1), by = c("start" = "ID_node")) %>% 
    dplyr::select( ID_start = V1, Nome_start= Nome, start, end) %>% 
    dplyr::left_join(., groups %>% dplyr::select(ID_node, Nome, V1), by = c("end"= "ID_node")) %>% 
    dplyr::select(Nome_start, Nome_end = Nome,  ID_start,ID_end = V1, start, end) %>% 
    left_join(., type_org %>% dplyr::select(ID_node, org_type = `Subtype of organization`), by = c("start" = "ID_node")) %>% 
    dplyr::select(everything(), org_type_start = org_type) %>% 
    left_join(., type_org %>% dplyr::select(ID_node, org_type = `Subtype of organization`), by = c("end" = "ID_node")) %>% 
    dplyr::select(everything(), org_type_end = org_type) %>%
    dplyr::mutate(org_type_start = ifelse(Nome_start == "Tucum", paste0("not_remove_",org_type_start ), org_type_start ),
                  org_type_end = ifelse(Nome_end == "Tucum", paste0("not_remove_",org_type_end ), org_type_end )) %>% 
    dplyr::group_by(org_type_start ,   org_type_end) %>% 
    dplyr::tally() %>% 
    dplyr::ungroup() %>% 
    mutate(org_type_start = ifelse(is.na(org_type_start), "not_found", org_type_start),
           org_type_end = ifelse(is.na(org_type_end), "not_found", org_type_end)) 
  
  to_add1 <- start_up_links_counts %>% 
    dplyr::filter(org_type_start == "Start-up company" | org_type_end == "Start-up company") %>% 
    dplyr::pull(n) %>% 
    sum
  to_add2 <- start_up_links_counts %>%
    dplyr::filter(org_type_start != "Start-up company" ) %>% 
    dplyr::filter(org_type_end != "Start-up company") %>% 
    dplyr::pull(n) %>% 
    sum
  
  removed_startup_links_freq <- data.frame(org_type = c("Start-up company", "diff from Start-up company", "Total"),
             counts = c(to_add1, to_add2, to_add1+to_add2),
             frequency = c(to_add1/(to_add1+to_add2), to_add2/(to_add1+to_add2), 1 ) ) %>% 
    dplyr::mutate(frequency = paste0(round(frequency*100, 3),"%"))
  
  start_up_links_counts %>%
    dplyr::filter(org_type_start != "Start-up company" ) %>% 
    dplyr::filter(org_type_end != "Start-up company") %>% 
    dplyr::pull(n) %>% 
    sum
  
  
  
  company_count <- tibble(ID_node = V(net)$name) %>% 
    left_join(., groups %>% dplyr::select(ID_node, V1), by = "ID_node" ) %>% 
    left_join(., type_org %>% dplyr::select(-V1), by = c("ID_node"),  suffix = c("", ".to_remove")) %>%
    dplyr::group_by(V1, `Type of organization`) %>% 
    tally() %>% 
    ungroup() %>% 
    dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not defined")})) %>% 
    add_row(V1= "Total",`Type of organization` =  "Total count of companies", n = sum(.$n))
  
  return(list(
    gnr_mtrs = gnr_mtrs,
    node_mtrs= node_mtrs,
    Ra_summ = Ra_summ,
    Rb_summ = Rb_summ, 
    Rc_summ = Rc_summ,
    Ra_vs_Rb_summ = Ra_vs_Rb_summ,
    links_count = links_count,
    company_count = company_count,
    startup_links_counts = start_up_links_counts,
    removed_startup_links_freq = removed_startup_links_freq
  ))
  
}

net_graphs_dir <- function(edge_list, groups, gr_colors, preguntas, out_file){
  
  if(any(grepl("R[0-9](a|b)", names(edge_list)))){
    edge_list<- edge_list %>% 
      dplyr::arrange( across((matches("R[0-9](a|b)")) ))
    
  }
  
  
 
  
  net <- graph_from_data_frame(d=  edge_list,
                               vertices = groups %>% 
                                 dplyr::select(ID_node, V1) ,
                               directed= T) %>%
    igraph::delete_vertices(., igraph::degree(.) == 0) %>% 
    igraph::delete_edges(., edges = which(igraph::which_multiple(.)))
  
  #save adjacencyu matrix
  adj_mtx <- as_adjacency_matrix(net, type = "both", sparse = F) %>% 
    as.data.frame() 
  
  vtx_shp <-  tibble(vtx = V(net)$name) %>% 
    left_join(., tibble(nodos = character(0), shp = character(0)) %>% 
                add_row(nodos = intersect(edge_list$R_Start_ID, edge_list$R_Destiny_ID), shp = "Intermediary") %>%
                add_row(nodos = setdiff(edge_list$R_Start_ID, edge_list$R_Destiny_ID), shp = "Investor") %>%
                add_row(nodos = setdiff(edge_list$R_Destiny_ID, edge_list$R_Start_ID), shp = "Recipient"), by = c("vtx" = "nodos") )
  # %>% 
  #   dplyr::mutate( shape = dplyr::case_when(
  #     shp == "Intermediary" ~ "circle",
  #     shp == "Recipient" ~ "triangle",
  #     shp == "Investor" ~ "square"))
  # 

  
  v_color <- tibble(id = V(net)$name) %>% 
    dplyr::left_join(., groups, by = c("id" = "ID_node") ) %>% 
    dplyr::left_join(., gr_colors, by = c("V1" = "V1")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas, var = "V1_xxxx", var_lab = "Groups"), 
                     by = c("V1" = "Opcoes_numero"))
  
  
  net1 <-  intergraph::asNetwork(simplify(net))
  network.vertex.names(net1) = v_color$id
  net1 %v% "color" = v_color$color
  net1 %v% "Groups" = v_color$Groups
  net1 %v% "Function" = vtx_shp$shp
  
  plt <- v_color %>% 
    dplyr::group_by(Groups, color) %>% 
    dplyr::tally() %>% 
    dplyr::pull( color, Groups) 
  
  nplot <- GGally::ggnet2(net1,
                          color = "Groups",
                          palette = plt,
                          size = 5, 
                          shape = "Function",
                          mode = "fruchtermanreingold", 
                          label = "vertex.names",
                          color.legend = "Groups",
                          legend.zise = 12, 
                          legend.position = "left",
                          label.size = 3,
                          label.alpha = 1,
                          arrow.size = 5,
                          arrow.gap = 0.025) + theme(legend.position = "none")
  
  ggsave(filename = out_file, plot= nplot, dpi = 400, units="in", width=8, height=5)
  
  
  return(list(
    net = net,
    adj_mtx = adj_mtx,
    g_plot = nplot, 
    vtx_shp = vtx_shp
  ))
  
}

net_graphs_dir_orgtype <- function(edge_list, groups, type_org, preguntas, out_file){
  
  if(any(grepl("R[0-9](a|b)", names(edge_list)))){
    edge_list<- edge_list %>% 
      dplyr::arrange( across((matches("R[0-9](a|b)")) ))
    
  }
  
  type_org <- type_org %>% 
    dplyr::select(ID_node, type = `Type of organization`) %>% 
    dplyr::mutate(type = case_when(
      type == "Company (consolidated or start-up)" ~ type, #azul
      type == "Civil society organization" ~ type,  #rojitp 
      TRUE ~ "Others"
      
    ))
  
  net <- graph_from_data_frame(d=  edge_list,
                               vertices = groups %>% 
                                 dplyr::select(ID_node, V1) ,
                               directed= T) %>%
    igraph::delete_vertices(., igraph::degree(.) == 0) %>% 
    igraph::delete_edges(., edges = which(igraph::which_multiple(.)))
  
  #save adjacencyu matrix
  adj_mtx <- as_adjacency_matrix(net, type = "both", sparse = F) %>% 
    as.data.frame() 
  
  vtx_shp <-  tibble(id = V(net)$name) %>%
    dplyr::left_join(., groups %>% dplyr::select(ID_node, V1), by = c("id" = "ID_node")) %>% 
    dplyr::mutate(shp = case_when(
      V1 == "A" ~ "1",
      V1 == "B" ~ "1",
      V1 == "C" ~ "2",
      TRUE ~ "2"
    ))
    
    
    # left_join(., tibble(nodos = character(0), shp = character(0)) %>% 
    #             add_row(nodos = intersect(edge_list$R_Start_ID, edge_list$R_Destiny_ID), shp = "Intermediary") %>%
    #             add_row(nodos = setdiff(edge_list$R_Start_ID, edge_list$R_Destiny_ID), shp = "Investor") %>%
    #             add_row(nodos = setdiff(edge_list$R_Destiny_ID, edge_list$R_Start_ID), shp = "Recipient"), by = c("vtx" = "nodos") )
    # 
    
  # %>% 
  #   dplyr::mutate( shape = dplyr::case_when(
  #     shp == "Intermediary" ~ "circle",
  #     shp == "Recipient" ~ "triangle",
  #     shp == "Investor" ~ "square"))
  # 
  orgs_types <- get_questions_opts(preguntas, var = "V39b", var_lab = "type")
  
  gr_colors <- tibble(type  = c(unique(type_org$type), "USAID"),
                      color = case_when(
                        type == "Company (consolidated or start-up)" ~ "#5353c6", #azul
                        type == "Civil society organization" ~ "#669900", #verde oliva
                        type == "USAID" ~ "#ff0000", #rojitp 
                        TRUE ~ "#999999"
                        
                      ) )
  
  v_color <- tibble(id = V(net)$name) %>% 
    dplyr::left_join(., type_org, by = c("id" = "ID_node")) %>%
    dplyr::mutate(type = ifelse(id == "76", "USAID", type )) %>% 
    dplyr::left_join(., groups %>% dplyr::select(ID_node, V1 ), by = c("id" = "ID_node") ) %>% 
    dplyr::left_join(., vtx_shp %>% dplyr::select(id, shp), by = c("id" = "id")) %>%
    dplyr::left_join(., gr_colors, by = c("type" = "type")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas, var = "V1_xxxx", var_lab = "Groups"), 
                     by = c("V1" = "Opcoes_numero"))
  
  #stopifnot("NA encontrados en type org: ", all(is.na(v_color$type)))
  
  set.seed(123)
  net1 <-  intergraph::asNetwork(simplify(net))
  network.vertex.names(net1) = v_color$id
  net1 %v% "color" = v_color$color
  net1 %v% "Org_type" = v_color$type
  net1 %v% "shp" = v_color$shp
  net1 %v% "Group" = v_color$Groups
  
  plt <- v_color %>% 
    dplyr::group_by(type, color) %>% 
    dplyr::tally() %>% 
    dplyr::pull( color, type) 
  
  nplot <- GGally::ggnet2(net1,
                          shape = "shp",
                          color = "Org_type",
                          palette = plt,
                          size = 5, 
                          mode = "fruchtermanreingold", 
                          label = "vertex.names",
                          color.legend = "Org type",
                          legend.zise = 12, 
                          legend.position = "left",
                          label.size = 3,
                          label.alpha = 1,
                          arrow.size = 5,
                          arrow.gap = 0.025) +
    scale_shape_manual(name = "Groups",
                       labels = c("PPA members", "Organizations in PPA's sphere of influence "),
                       values = c(19, 15))+
    theme(legend.position = "bottom",
          legend.text = element_text(size=5))
    #guides(shape = guide_legend())
  nplot
  
  ggsave(filename = out_file, plot= nplot, dpi = 400, units="in", width=8, height=5)
  
  
  ret <- v_color %>% 
    dplyr::group_by(type, shp, V1) %>% 
    tally() %>% 
    dplyr::select(Org_type = type, node_type = shp, Group = V1, counts = n)
  
  return(ret)
  
}


net_metrics_dir <- function(edge_list_raw, net, vtx_shp, entrevistados, type_org, groups, preguntas, role_ppa, Ra, Rb){
  
  attrs <- igraph::get.edge.attribute(net)
  
  tot <- edge_list_raw %>% 
    dplyr::arrange(across(matches("R[0-9](a|b)"))) %>% 
    group_by(V1) %>% 
    dplyr::summarise(Total = n())
  
  
  non_res_Ra <- edge_list_raw %>% 
    group_by(V1) %>%
    dplyr::summarise(Count = calc_counts(!!sym(Ra), val =2)) %>%
    dplyr::mutate( label1 = "Data not available") %>% 
    dplyr::select(V1, label1, n = Count)
  
  
  non_res_Rb <- edge_list_raw %>% 
    group_by(V1) %>% 
    dplyr::summarise(Count = calc_counts(!!sym(Rb), val =9)) %>%
    dplyr::mutate(  label2 = "Data not available")  %>% 
    dplyr::select(V1, label2, n = Count)
  
 
  deg_analysis <- tibble(ID_node      = V(net)$name ,
                     type    = vtx_shp$shp,
                     group   = V(net)$V1,
                     in_deg  = igraph::degree(net, mode = 'in'), 
                     out_deg = igraph::degree(net, mode = 'out'), 
                     degree     = igraph::degree(net, mode = 'total') ) %>% 
    dplyr::mutate(in_deg_frq  = round(in_deg/degree,3)*100,
                  out_deg_frq = round(out_deg/degree,3)*100) %>% 
    dplyr::left_join(., groups %>% dplyr::select(ID_node, Nome) , by = "ID_node") 
  
  
  gnr_mtrs <- tibble(Metric = c("density", "centraBetw", "centraDeg", "modularidad", "meanDegree", "transitiv", "assorta", "EigenValue"),
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
  
  
  node_mtrs <- tibble(nodes_id = V(net)$name) %>% 
    dplyr::left_join(., groups, by = c("nodes_id" = "ID_node")) %>%
    dplyr::select(nodes_id, Nome,V1) %>%
    dplyr::mutate(degree         = igraph::degree(net, mode="all", normalized = F),
                  betwe          = igraph::betweenness(net) %>% round(.,1),
                  closeness      = igraph::closeness(net, mode="all") %>% round(.,4),
                  ClusWalk       = igraph::cluster_walktrap(net) %>% membership(),
                  transitivLocal = igraph::transitivity(net, type = "local")  %>% round(.,2),
                  Eigen_centr    = igraph::eigen_centrality(net, directed = T)$vector %>%  round(.,2)
    ) %>%
    dplyr::left_join(., entrevistados %>% dplyr::select(ID_node, `Labels in English`), by = c("nodes_id"= "ID_node")) %>% 
    dplyr::left_join(., type_org %>% dplyr::select(ID_node, `Type of organization`, `Subtype of organization` ), by = c("nodes_id"= "ID_node")) %>% 
    dplyr::left_join(., role_ppa, by = c("nodes_id" = "ID_node")) %>% 
    dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not participated")}) )
  
  summ <-  tibble(var1 = unlist(attrs[Ra]), 
                  var2 = unlist(attrs[Rb])) %>%
    dplyr::mutate( start  = igraph::ends(net, es=E(net), names=T)[,1]) %>%
    left_join(., groups %>% dplyr::select(ID_node, V1), by = c("start" = "ID_node"))%>%
    dplyr::left_join(., get_questions_opts(data = preguntas, 
                                           var = Ra, 
                                           var_lab = "label1"), by = c("var1" = "Opcoes_numero")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas, 
                                           var = Rb, 
                                           var_lab = "label2"), by = c("var2" = "Opcoes_numero"))
  
  
  
  Ra_summ <- summ %>%
    group_by( V1, label1) %>%
    tally()  %>% 
    ungroup() %>% 
    bind_rows(non_res_Ra)%>% 
    dplyr::left_join(., tot, by = "V1") %>% 
    dplyr::mutate(freq = round(n/Total,2)*100, freq = paste0(freq, "%") ) %>% 
    dplyr::select(Group = V1, !!get_var_label(data = preguntas, var = Ra) := label1, Count = n, Frequency = freq) 
  
  
  Rb_summ <- summ %>%
    group_by( V1, label2) %>%
    tally()  %>% 
    ungroup() %>% 
    bind_rows(non_res_Rb)%>% 
    dplyr::left_join(., tot, by = "V1") %>% 
    dplyr::mutate(freq = round(n/Total,2)*100, freq = paste0(freq, "%") ) %>% 
    dplyr::select(Group = V1, !!get_var_label(data = preguntas, var = Rb) := label2, Count = n, Frequency = freq) 
  
  
  
  non_response<- non_res_Rb
  non_response$n <- non_res_Ra$n + non_res_Rb$n 
  non_response$label1 <- non_response$label2
  non_response$freq <- NULL
  
  Ra_vs_Rb_summ <- summ %>%  
    group_by( V1, label1, label2) %>%
    tally()  %>% 
    ungroup() %>%
    bind_rows(non_response) %>% 
    dplyr::select(Group = V1, 
                  !!get_var_label(data = preguntas, var = Ra):= label1,
                  !!get_var_label(data = preguntas, var = Rb):= label2, Count = n) 
  
  
  links_count <- as_tibble(ends(net, E(net), names = T)) %>% 
    dplyr::select(start = V1, end = V2) %>% 
    left_join(., groups %>% dplyr::select(ID_node, Nome, V1), by = c("start" = "ID_node")) %>% 
    dplyr::select( ID_start = V1, everything()) %>% 
    dplyr::left_join(., groups %>% dplyr::select(ID_node, Nome, V1), by = c("end"= "ID_node")) %>% 
    dplyr::select(start, end,  ID_start,ID_end = V1, everything()) %>% 
    group_by(ID_start, ID_end) %>% 
    tally() %>%
    ungroup() %>%
    dplyr::mutate(Ratio = paste(round(n/sum(n)*100,1), "%")) %>% 
    dplyr::select(Group_start = ID_start, Group_end = ID_end, Links_count = n, Ratio)
  
  
  company_count <- tibble(ID_node = V(net)$name) %>% 
    left_join(., groups %>% dplyr::select(ID_node, V1), by = "ID_node" ) %>% 
    left_join(., type_org %>% dplyr::select(-V1), by = c("ID_node"),  suffix = c("", ".to_remove")) %>%
    dplyr::group_by(V1, `Type of organization`) %>% 
    tally() %>% 
    ungroup() %>% 
    dplyr::mutate(across(where(is.character), function(i){replace_na(i, "Not defined")})) %>% 
    add_row(V1= "Total",`Type of organization` =  "Total count of companies", n = sum(.$n))
  
  
 shape_counts <-  vtx_shp %>% 
    left_join(., groups, by = c("vtx" = "ID_node") ) %>% 
    group_by( shp) %>%
    tally() %>%
    bind_rows(tibble(shp = unique(non_res_Rb$label2), n = sum(non_res_Rb$n) )  ) %>% 
    dplyr::mutate(freq = round(n/sum(n), 2), frq = paste0(round(freq*100,1), "%")) %>% 
    dplyr::select(Type = shp, Count = n, Frequency = frq)
    
  
 
 
 shape_counts_by_gr <-  vtx_shp %>% 
    left_join(., groups, by = c("vtx" = "ID_node") ) %>% 
    group_by( V1, shp) %>%
    tally() %>%
    bind_rows(non_res_Rb %>% dplyr::select(V1, shp = label2, n)) %>% 
    dplyr::mutate(freq = round(n/sum(n), 2), frq = paste0(freq*100, "%")) %>%
    ungroup()
 
  return(list(
    gnr_mtrs = gnr_mtrs,
    node_mtrs = node_mtrs,
    deg_analysis = deg_analysis,
    Ra_summ = Ra_summ,
    Rb_summ = Rb_summ,
    Ra_vs_Rb_summ = Ra_vs_Rb_summ,
    links_count = links_count,
    company_count = company_count,
    shape_counts = shape_counts,
    shape_counts_by_gr = shape_counts_by_gr
  ))
  
}

to_vector <- function(choices, n_res){
  if(is.null(dim(choices))){
    vec <- as.integer(1:n_res %in% choices) 
  }else if(length(dim(choices))){
    vec <-  as.integer(1:n_res %in% dplyr::pull(choices))
  }
  return(vec)
}

correct_question_format <- function(input_data, var_opts, var ){
  
  n_opts <- nrow(var_opts)
  
  unq <- input_data %>% 
    dplyr::mutate(ID2 = paste0(ID,ID_ego)) %>% 
    dplyr::pull(ID2) %>% 
    unique()
  
  reshaped <- lapply(unq, function(i){
    var_ok <- input_data %>% 
      mutate(ID2 = paste0(ID,ID_ego)) %>% 
      filter(ID2 == i) %>% 
      dplyr::pull(!!var) %>% 
      to_vector(choices = ., n_res = n_opts)
    return(var_ok)
  }) %>% 
    do.call( rbind, .) %>% 
    as.data.frame()
  
  names(reshaped) <- paste0(var,"_",1:n_opts)
  
  return(reshaped)
  
}


### cargar archivos de datos

rodada_2019 <- "D:/OneDrive - CGIAR/Documents/Database SNA PPA 2019 final 2020 06 08.xlsx"
rodada_2020 <- "D:/OneDrive - CGIAR/Documents/PPA 2021/Database SNA PPA 2020 A_B (21ago21)_para_CIAT.xlsx"
rodada_2021 <- "D:/OneDrive - CGIAR/Documents/PPA 2021/Database SNA PPA 2021-2022 (21set22)_completa-Encaminhado CIAT.xlsx"

sheets_2019 <- excel_sheets(rodada_2019)
sheets_2020 <- excel_sheets(rodada_2020)
sheets_2021 <- excel_sheets(rodada_2021)

raw_list_2019 <- lapply(sheets_2019, function(i){
  readxl::read_xlsx(rodada_2019, sheet = i)
})

raw_list_2020 <- lapply(sheets_2020, function(i){
  readxl::read_xlsx(rodada_2020, sheet = i, col_types = "text")
})

raw_list_2021 <- lapply(sheets_2021, function(i){
  readxl::read_xlsx(rodada_2021, sheet = i, col_types = "text")
})

names(raw_list_2019) <- sheets_2019
names(raw_list_2020) <- sheets_2020
names(raw_list_2021) <- sheets_2021



opts <- raw_list_2021[[grep("Todos_detalhado", names(raw_list_2021))]]
#opts <- raw_list_2020[[grep("Todos_detalhado", names(raw_list_2020))]]
#names(opts2)

preguntas <- lapply(c("2019", "2020", "2021"), function(rodada){
  
  prefix <- "opcoes" # or startups
  fixed <- "2021"
  
  opts <- opts %>% 
    dplyr::filter(Variavel != "ID_node") %>% 
    dplyr::mutate(var_id = 1:nrow(.)) %>% 
    dplyr::filter(Variavel != "R_todas")
  
  vars_names <- opts %>% 
    dplyr::select(Variavel, Metrica,Variavel_nome, var_id, `Labels in English`) %>% 
    dplyr::filter(complete.cases(Metrica)) %>% 
    dplyr::mutate(root_var = ifelse(grepl("xxxx", Variavel), str_replace(Variavel, "xxxx", fixed ), Variavel)) 
  
  
  preguntas <- vars_names %>% 
    dplyr::mutate(vars_opts = purrr::map2(.x = root_var, .y = Metrica, .f = function(.x, .y){
      i <- .x
      cat("processing:", i, "\n")
      if(grepl(pattern ='múltipla escolha|múltiplas', .y)){    
        i <- paste0(i, "_")
        
        ret <- opts %>% 
          dplyr::filter(grepl(i, Variavel)) %>% 
          dplyr::select(Variavel, Variavel_nome, Opcoes_numero, Opcoes_nome, ends_with(paste0(prefix, "_", rodada)), `Labels in English`) %>% 
          filter(complete.cases(Opcoes_numero))
        
        if(any(table(stringr::str_extract(ret$Variavel, "[0-9]+$")) > 1 )| any(table(ret$Opcoes_nome)>1) ){
          stop("Numero de opciones de respuesta mal codificado")
        }
        
      }else{
        ret <- opts %>% 
          filter(Variavel == i) %>% 
          dplyr::select(Variavel, Variavel_nome, Opcoes_numero, Opcoes_nome, ends_with(paste0(prefix, "_", rodada)), `Labels in English`) %>% 
          filter(complete.cases(Opcoes_numero))
        
        if(any(table(ret$Opcoes_numero) > 1) | any(table(ret$Opcoes_nome) > 1) ){
          stop("Numero de opciones de respuesta mal codificado")
        }
        
      }
      
      if(nrow(ret) == 0){
        ret <- NA
      }
      return(ret)
    } ))
  
  return(preguntas)
  
})
names(preguntas) <- c("rodada_2019", "rodada_2020", "rodada_2021")

groups <- lapply(c("2019", "2020", "2021"), function(rodada){
  
  groups <- raw_list_2021[[grep("Orgs_grupo_rodadas", names(raw_list_2021))]] %>%
    dplyr::select(ID_node, Nome, matches(rodada)) %>%
    dplyr::mutate_all(as.character) %>% 
    dplyr::rename_at(vars(matches(rodada)), function(i){ gsub("_[0-9]+", "", i)}) %>% 
    dplyr::mutate(V1 = ifelse(rodada == "2019" & V1 == "A2", "A", V1))
  
  stopifnot("ID mal codificado" = !table(groups$ID_node)>1)
  
  return(groups)
})
names(groups) <- c("rodada_2019", "rodada_2020", "rodada_2021")

entrevistados <- lapply(c("rodada_2019", "rodada_2020", "rodada_2021"), function(i){
  cat(i)
  entrevistados <- groups[[i]] %>% 
    dplyr::left_join(., preguntas[[i]] %>% 
                       dplyr::filter(Variavel == "V1_Rodada_xxxx") %>% 
                       dplyr::pull(vars_opts) %>% 
                       purrr::pluck(1) %>% 
                       dplyr::select( Opcoes_numero, Opcoes_nome ,  `Labels in English`)
                     , by = c( "V1_Rodada" = "Opcoes_numero")) %>% 
    dplyr::mutate(status =  Opcoes_nome) %>% 
    dplyr::filter( status == "Participou")
  
  cat("Total de entrevistados en",i, "es de: ", nrow(entrevistados), "\n")
 return(entrevistados) 
})
names(entrevistados) <- c("rodada_2019", "rodada_2020", "rodada_2021")


gr_colors <- lapply(c("rodada_2019", "rodada_2020", "rodada_2021"), function(i){
  
  gr_colors <- groups[[i]] %>% 
    dplyr::select(V1) %>% 
    dplyr::group_by(V1) %>% 
    tally() %>% 
    dplyr::mutate(color = case_when(
      V1 == "A" ~ "olivedrab3",
      V1 == "B" ~ "dodgerblue2",
      V1 == "C" ~ "lavenderblush3",
      TRUE ~ "gray50"
    ))
  
  return(gr_colors)
})
names(gr_colors) <- c("rodada_2019", "rodada_2020", "rodada_2021")

out_dirs <- lapply(c("rodada_2019", "rodada_2020", "rodada_2021"), function(i){
  
  ret <- paste0("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/", i)
  if(!file.exists(ret)){dir.create(ret, recursive = T)}
  return(ret)
})
names(out_dirs) <- c("rodada_2019", "rodada_2020", "rodada_2021")

type_org <-  list(
  # raw_list_2020[[grep("Empresas_orgs_atributos", names(raw_list_2020))]] 
  
  rodada_2019 =raw_list_2019[[grep("Empresas_orgs_atributos", names(raw_list_2019))]] %>% 
    dplyr::select(starts_with("ID"), V39a, V39b) %>%
    dplyr::rename(ID_node = ID)  %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::left_join(., groups$rodada_2019 %>% dplyr::select(ID_node, Nome, V1)) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2019, var = "V39a", var_lab = "Type of organization"), by = c("V39a" = "Opcoes_numero")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2019, var = "V39b", var_lab = "Subtype of organization"), by = c("V39b" = "Opcoes_numero")),
  
  rodada_2020 = raw_list_2020[[grep("orgs_atributos_all", names(raw_list_2020))]] %>% 
    dplyr::select(starts_with("ID"), V39a, V39b) %>%
    dplyr::mutate_all(as.character) %>% 
    dplyr::left_join(., groups$rodada_2020 %>% dplyr::select(ID_node, Nome, V1), by = c("ID_node" = "ID_node")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2020, var = "V39a", var_lab = "Type of organization"), by = c("V39a" = "Opcoes_numero")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2020, var = "V39b", var_lab = "Subtype of organization"), by = c("V39b" = "Opcoes_numero")) ,
  
  rodada_2021 = raw_list_2021[[grep("Empresas_orgs_atributos", names(raw_list_2021))]] %>% 
    dplyr::select(starts_with("ID"), V39a, V39b) %>%
    dplyr::mutate_all(as.character) %>% 
    dplyr::left_join(., groups$rodada_2021 %>% dplyr::select(ID_node, Nome, V1), by = c("ID_node" = "ID_node")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2021, var = "V39a", var_lab = "Type of organization"), by = c("V39a" = "Opcoes_numero")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2021, var = "V39b", var_lab = "Subtype of organization"), by = c("V39b" = "Opcoes_numero")) 
  
  
  
)

type_org[["rodada_2021"]] <- type_org[["rodada_2021"]] %>% 
  dplyr::select(ID_node, everything(.), type_new = `Type of organization` ) %>% 
  dplyr::full_join(type_org[["rodada_2020"]] %>% 
                     dplyr::select(ID_node, type_old =  `Type of organization` ), by = "ID_node") %>% 
  dplyr::mutate(type_final = ifelse(is.na(type_new), type_old, type_new) ,
                type_final = ifelse(is.na(type_final), "others", type_final)) %>% 
  dplyr::select(ID_node, everything(.), -type_new, -type_old,`Type of organization` = type_final) %>% 
  dplyr::add_row(data.frame(ID_node = "237", V39a = NA, V39b = NA,  Nome = "Assaí Atacadista", V1 = "B",
                            `Subtype of organization` = "Consolidated company", `Type of organization` ="Company (consolidated or start-up)", check.names = F)) %>% 
  dplyr::add_row(data.frame(ID_node = "236", V39a = NA, V39b = NA,  Nome = "Nestlé", V1 = "B",
                            `Subtype of organization` = "Consolidated company", `Type of organization` ="Company (consolidated or start-up)", check.names = F))




informer <- list(
  rodada_2019 = raw_list_2019[[grep("Informer", names(raw_list_2019))]] %>% 
    dplyr::filter(!is.na(V3)) %>% 
    dplyr::mutate_all(as.character) %>% 
    bind_cols(., correct_question_format(input_data =raw_list_2019[[grep("Informer", names(raw_list_2019))]], 
                                      var_opts = get_questions_opts(data = preguntas$rodada_2019 , var = "V14", var_lab = "label"), 
                                      var = "V14" )) %>%
    dplyr::rename(ID_node = ID),
  
  rodada_2020 = raw_list_2020[[grep("Informer", names(raw_list_2020))]],
  rodada_2021 = raw_list_2021[[grep("Informer", names(raw_list_2021))]]
)

role_ppa <- list(
  rodada_2019 = informer$rodada_2019 %>% 
    dplyr::select(ID_node, ID_ego, starts_with("V14_")) %>% 
    tidyr::pivot_longer(., cols = -c(ID_node, ID_ego), names_to = "var", values_to = "opt") %>% 
    dplyr::left_join(.,get_questions_opts(data = preguntas$rodada_2019 , var = "V14", var_lab = "label"), by = c("var" = "Opcoes_numero")) %>% 
    dplyr::mutate(label = ifelse(opt == 0, "Not selected", label)) %>% 
    dplyr::select(-opt) %>% 
    tidyr::pivot_wider(., id_cols = c(ID_node, ID_ego), names_from = var, values_from = "label") %>% 
    dplyr::mutate(across(everything(.), as.character )), 
  
  rodada_2020 = informer$rodada_2020 %>% 
    dplyr::select(ID_node, ID_ego, starts_with("V14_")) %>% 
    tidyr::pivot_longer(., cols = -c(ID_node, ID_ego), names_to = "var", values_to = "opt") %>% 
    dplyr::left_join(.,get_questions_opts(data = preguntas$rodada_2020 , var = "V14", var_lab = "label"), by = c("var" = "Opcoes_numero")) %>% 
    dplyr::mutate(label = ifelse(opt == 0, "Not selected", label)) %>% 
    dplyr::select(-opt) %>% 
    tidyr::pivot_wider(., id_cols = c(ID_node, ID_ego), names_from = var, values_from = "label")%>% 
    dplyr::mutate(across(everything(.), as.character )),
  
  rodada_2021 = informer$rodada_2021 %>% 
    dplyr::select(ID_node, ID_ego, starts_with("V14_")) %>% 
    tidyr::pivot_longer(., cols = -c(ID_node, ID_ego), names_to = "var", values_to = "opt") %>% 
    dplyr::left_join(.,get_questions_opts(data = preguntas$rodada_2021 , var = "V14", var_lab = "label"), by = c("var" = "Opcoes_numero")) %>% 
    dplyr::mutate(label = ifelse(opt == 0, "Not selected", label)) %>% 
    dplyr::select(-opt) %>% 
    tidyr::pivot_wider(., id_cols = c(ID_node, ID_ego), names_from = var, values_from = "label")%>% 
    dplyr::mutate(across(everything(.), as.character ))
  
)


#############################################################
############## REMOVER START'ups ###########################
###########################################################
if(TRUE){
  raw_list_2019[grepl("^R[0-9]", names(raw_list_2019))]  <-raw_list_2019[grepl("^R[0-9]", names(raw_list_2019))] %>% 
    lapply(., function(l){
      start_ups <- type_org[["rodada_2019"]] %>% 
        dplyr::select(ID_node, V39b, `Subtype of organization`) %>% 
        dplyr::filter(V39b == 7) %>% 
        dplyr::filter(ID_node != "86") %>% 
        dplyr::filter(`Subtype of organization` == "Start-up company") %>% 
        dplyr::pull(ID_node)
      
      if(any("R5b_Destiny_ID" %in% names(l))){
        names(l)[which(names(l) == "R5b_Destiny_ID")] <- "R_Destiny_ID"
        control <-TRUE
      }else{
        control <- FALSE
      }
      
      tmp <- l %>% 
        dplyr::filter( !R_Start_ID %in% start_ups) %>% 
        dplyr::filter( !R_Destiny_ID %in% start_ups) %>% 
        dplyr::mutate(id = paste0(R_Start_ID , "_", R_Start_ID_ego)) %>% 
        dplyr::filter(!id  %in% c("42_a","42_c","49_a","49_b","76_a")) %>% 
        dplyr::select(-id)
      if(control){
        names(tmp)[which(names(tmp) == "R_Destiny_ID")] <- "R5b_Destiny_ID"
      }
      
      
      return(tmp)
      
    })
  
  raw_list_2020[grepl("^R[0-9]", names(raw_list_2020))] <- raw_list_2020[grepl("^R[0-9]", names(raw_list_2020))] %>% 
    lapply(., function(l){
      start_ups <- type_org[["rodada_2020"]] %>% 
        dplyr::select(ID_node, V39b, `Subtype of organization`) %>% 
        dplyr::filter(V39b == 7)%>%
        dplyr::filter(ID_node != "86") %>% 
        dplyr::filter(`Subtype of organization` == "Start-up company") %>% 
        dplyr::pull(ID_node)
      
      if(any("R5b_Destiny_ID" %in% names(l))){
        names(l)[which(names(l) == "R5b_Destiny_ID")] <- "R_Destiny_ID"
        control <-TRUE
      }else{
        control <- FALSE
      }
      
      tmp <- l %>% 
        dplyr::filter( !R_Start_ID %in% start_ups) %>% 
        dplyr::filter( !R_Destiny_ID %in% start_ups) %>% 
        dplyr::mutate(id = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>%  
        dplyr::filter(!id  %in% c("18_b", "18_c", "42_b","42_c","43_b" ,
                                  "49_b","49_c","76_c", "76_d","84_b", "84_c")) %>%
        dplyr::select(-id)
      if(control){
        names(tmp)[which(names(tmp) == "R_Destiny_ID")] <- "R5b_Destiny_ID"
      }
      
      return(tmp)
    })
  
  raw_list_2021[grepl("^R[0-9]", names(raw_list_2021))] <- raw_list_2021[grepl("^R[0-9]", names(raw_list_2021))] %>% 
    lapply(., function(l){
      start_ups <- type_org[["rodada_2021"]] %>% 
        dplyr::select(ID_node, V39b, `Subtype of organization`) %>% 
        dplyr::filter(V39b == 7)%>%
        dplyr::filter(ID_node != "86") %>% 
        dplyr::filter(`Subtype of organization` == "Start-up company") %>% 
        dplyr::pull(ID_node)
      
      if(any("R5b_Destiny_ID" %in% names(l))){
        names(l)[which(names(l) == "R5b_Destiny_ID")] <- "R_Destiny_ID"
        control <-TRUE
      }else{
        control <- FALSE
      }
      
      tmp <- l %>% 
        dplyr::filter( !R_Start_ID %in% start_ups) %>% 
        dplyr::filter( !R_Destiny_ID %in% start_ups) %>% 
        dplyr::mutate(id = paste0(R_Start_ID, "_", R_Start_ID_ego)) %>%  
        dplyr::filter(!id  %in% c("18_b", "18_c","76_c")) %>%
        dplyr::select(-id)
      if(control){
        names(tmp)[which(names(tmp) == "R_Destiny_ID")] <- "R5b_Destiny_ID"
      }
      
      return(tmp)
    })
  
}

#############################################################
############## REMOVER IDESAM node##########################
###########################################################
if(FALSE){
raw_list_2019[grepl("^R[0-9]", names(raw_list_2019))]  <- raw_list_2019[grepl("^R[0-9]", names(raw_list_2019))] %>% 
  lapply(., function(l){
   
    
    if(any("R5b_Destiny_ID" %in% names(l))){
      names(l)[which(names(l) == "R5b_Destiny_ID")] <- "R_Destiny_ID"
      control <-TRUE
    }else{
      control <- FALSE
    }
    
    tmp <- l %>% 
      dplyr::filter( R_Start_ID != 42 ) #42 IDESAM
    if(control){
      names(tmp)[which(names(tmp) == "R_Destiny_ID")] <- "R5b_Destiny_ID"
    }
    
    
    return(tmp)
    
  })

raw_list_2020[grepl("^R[0-9]", names(raw_list_2020))] <- raw_list_2020[grepl("^R[0-9]", names(raw_list_2020))] %>% 
  lapply(., function(l){

    
    if(any("R5b_Destiny_ID" %in% names(l))){
      names(l)[which(names(l) == "R5b_Destiny_ID")] <- "R_Destiny_ID"
      control <-TRUE
    }else{
      control <- FALSE
    }
    
    tmp <- l %>% 
      dplyr::filter( R_Start_ID != 42) 
    if(control){
      names(tmp)[which(names(tmp) == "R_Destiny_ID")] <- "R5b_Destiny_ID"
    }
    
    return(tmp)
  })
}

#############################################################
############## REMOVER nodos centrales######################
###########################################################

if(FALSE){
most_central_2019 <- readxl::read_excel("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/normal_treatment/rodada_2019/madre_standard_net_results.xlsx", sheet = "node_mtrs") %>% 
  dplyr::arrange(desc(degree)) %>% 
  dplyr::filter(!duplicated(Nome)) %>% 
  dplyr::slice(1:6) 

most_central_2020 <- readxl::read_excel("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/normal_treatment/rodada_2020/madre_standard_net_results.xlsx", sheet = "node_mtrs") %>% 
  dplyr::arrange(desc(degree)) %>% 
  dplyr::filter(!duplicated(Nome)) %>% 
  dplyr::slice(1:6) 

raw_list_2019[grepl("^R[0-9]", names(raw_list_2019))]  <- raw_list_2019[grepl("^R[0-9]", names(raw_list_2019))] %>% 
  lapply(., function(l){
    
    
    ids <- most_central_2019$nodes_id
    if(any("R5b_Destiny_ID" %in% names(l))){
      names(l)[which(names(l) == "R5b_Destiny_ID")] <- "R_Destiny_ID"
      control <-TRUE
    }else{
      control <- FALSE
    }
    
    tmp <- l %>% 
      dplyr::filter( !R_Start_ID %in% ids ) 
    if(control){
      names(tmp)[which(names(tmp) == "R_Destiny_ID")] <- "R5b_Destiny_ID"
    }
    
    
    return(tmp)
    
  })

raw_list_2020[grepl("^R[0-9]", names(raw_list_2020))] <- raw_list_2020[grepl("^R[0-9]", names(raw_list_2020))] %>% 
  lapply(., function(l){
    
    ids <- most_central_2020$nodes_id
    if(any("R5b_Destiny_ID" %in% names(l))){
      names(l)[which(names(l) == "R5b_Destiny_ID")] <- "R_Destiny_ID"
      control <-TRUE
    }else{
      control <- FALSE
    }
    
    tmp <- l %>% 
      dplyr::filter( !R_Start_ID %in% ids ) 
    if(control){
      names(tmp)[which(names(tmp) == "R_Destiny_ID")] <- "R5b_Destiny_ID"
    }
    
    return(tmp)
  })

}
####################################################################
############ RUN NETWORK ANALISYS   ###############################
##################################################################

list.files("D:/PPA_scripts/", pattern = "rodadadas_net_", full.names = T) %>% 
  lapply(., function(i){
    cat("Processing: ", i , "\n")
    source(i, encoding  = "utf-8")
  })



#################################################################
############# CALCULATE IN/OUT PPA #############################
###############################################################

for(y in c(2019, 2020)){


file_pths <- data.frame(file_paths = list.files(paste0("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_", y),
           pattern = "[a-z]+_standard_[a-z]+_results.xlsx",
           full.names = T) %>% 
  .[!grepl("madre", .)],
  file_names = c("Institutional collaboration", "Technical cooperation", "Investment", "Business")
  )  
  #lapply(., excel_sheets)
by_groups <-   apply(file_pths, 1, function(i){
    
    #print(i[2])
    x <- readxl::read_excel(i[1], sheet = "Ra_summ")
    
    ret1 <- x %>% 
      tidyr::pivot_wider(id_cols = 1, names_from = 2, values_from = 3) %>% 
      dplyr::select(Group, No, Yes, contains("Do not")) %>%
      dplyr::summarise(Group, No, Yes, across(contains("Do not")) ,
                       No_perc = paste0(round(prop.table(No)*100,2), "%"),
                       Yes_perc = paste0(round(prop.table(Yes)*100,2), "%" )
                       ) %>% 
      add_row(Group = "all groups", No = sum(.$No), Yes = sum(.$Yes), No_perc = "100%", Yes_perc = "100%") %>% 
      dplyr::mutate(`Type of relationship` = i[2], 
                    total = No + Yes,
                    total_perc = "100%") %>% 
      dplyr::select(`Type of relationship`,
                    Group,
                    `OUTSIDE the PPA` = No, 
                    `IN the PPA` = Yes,
                    `Grand Total` = total,
                    `OUTSIDE the PPA %`= No_perc,
                    `IN the PPA %` = Yes_perc,
                    `Grand Total %` = total_perc,
                    matches("Do not"))
  
    return(ret1)
  })


aggregados <- apply(file_pths, 1, function(i){
  
  x <- readxl::read_excel(i[1], sheet = "Ra_summ")
  
  not_know <- x %>% 
    dplyr::filter( if_any( where(is.character), ~ . == "Do not know")) %>% 
    dplyr::pull(Count) %>% 
    sum(., na.rm=T)
  
  ret2 <- x %>% 
    dplyr::filter(., if_all( where(is.character), ~ . != "Do not know") ) %>% 
    dplyr::select(Group,  "label" = 2, everything(.)) %>% 
    dplyr::group_by(across(2)) %>% 
    dplyr::summarise(number = sum(Count))  %>% 
    dplyr::mutate(perc = prop.table(number)) %>% 
    add_row(label = "total", number = sum(.$number), perc = 1) %>% 
    dplyr::mutate(perc = paste0(round(perc*100, 1), "%")) %>% 
    dplyr::mutate(across(everything(.), as.character)) %>% 
    dplyr::mutate(label = case_when(
      label == "No" ~ "OUTSIDE the PPA",
      label == "Yes"~ "IN the PPA",
      TRUE ~ label)) %>% 
    add_row(label = "Do not know", number = as.character(not_know), perc = NA) %>%
    add_row(label = "Type", number = i[2], perc = i[2]) %>%
    dplyr::rename("Context of the creations of relationships"  = label,`%`= perc) 
    
  
  
  return(ret2)
  
})


list(agregados = aggregados %>% 
       purrr::reduce(., left_join, by  = "Context of the creations of relationships"),
       
     by_groups  =  
         by_groups %>% 
         bind_rows %>% 
         dplyr::mutate(Group = case_when(
           Group == "A" ~ "Group A",
           Group == "B" ~ "Group B",
           TRUE  ~ "All groups"
         )) 
         ) %>% 
  writexl::write_xlsx(., paste0("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_", y , "/in_out_ppa_counts_", y, ".xlsx"))
} 
#################################################
######### create carlos tables #################
###############################################

rodadas_raw_files <- list(rodada_2019 = raw_list_2019,
                          rodada_2020 = raw_list_2020)


Rc_freqs <- list(
  rodada_2019 = lapply(c("negocios", "invest", "coop", "colab"), function(i){
    
    ano <- 2019
    rodada <- paste0("rodada" , "_", ano)
    if(i == "negocios"){
      query <- "R[0-9]b" 
      var <- "R1b"
    }else{
      query <- "R[0-9]c"
      
      if(i == "invest"){
        var <- "R2c"
      }else if(i == "coop"){
        var <- "R3c"
      }else{
        var <- "R4c"
      }
      
    }
    
    
    rd <- rodadas_raw_files[[rodada]]
    
    dm <- i
    
    Rc <-  rd[grepl(dm, names(rd))]%>% 
      purrr::pluck(1) %>% 
      dplyr::mutate(ids = paste0(R_Start_ID ,"-", R_Destiny_ID)) %>% 
      dplyr::select( - contains("comentario")) %>% 
      dplyr::select(ids, Rc = matches(query))
    
    pht <- list.files(paste0("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_", ano  ),
                      pattern = paste0("^", i),
                      full.names = T) %>% 
      .[grepl("_standard_net_graph.rds", tolower(.))]
    
    r <- readRDS(pht)
    
    ret <- tibble(as.data.frame(ends(r, E(r), names = T))) %>% 
      dplyr::mutate(ids = paste0(V1, "-", V2)) %>% 
      dplyr::left_join(., Rc) %>% 
      dplyr::group_by(V1, Rc) %>%
      dplyr::tally() %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(V1) %>% 
      dplyr::summarise(Rc, n, freq = round(prop.table(n),3)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(across(everything(.), as.character),
                    cat = dm,
                    time = ano) %>% 
      dplyr::left_join(., get_questions_opts(data= preguntas$rodada_2020,
                                             var = var,
                                             var_lab = "label"), by = c("Rc" = "Opcoes_numero")) %>% 
      dplyr::left_join(., groups[[rodada]] %>% dplyr::select( ID_node, Nome, V1), by = c("V1" = "ID_node")) %>% 
      dplyr::select(R_Start_ID = V1,
                    Nome,
                    group = V1.y,
                    time,
                    cat,
                    label,
                    conteo = n,
                    freq) 
    
    return(ret)
  }) %>% 
    bind_rows(),
  rodada_2020 = lapply(c("negocios", "invest", "coop", "colab"), function(i){
    
    ano <- 2020
    rodada <- paste0("rodada" , "_", ano)
    if(i == "negocios"){
      query <- "R[0-9]b" 
      var <- "R1b"
    }else{
      query <- "R[0-9]c"
      
      if(i == "invest"){
        var <- "R2c"
      }else if(i == "coop"){
        var <- "R3c"
      }else{
        var <- "R4c"
      }
      
    }
    
    rd <- rodadas_raw_files[[rodada]]
    
    dm <- i
    
    Rc <-  rd[grepl(dm, names(rd))]%>% 
      purrr::pluck(1) %>% 
      dplyr::mutate(ids = paste0(R_Start_ID ,"-", R_Destiny_ID)) %>% 
      dplyr::select( - contains("comentario")) %>% 
      dplyr::select(ids, Rc = matches(query))
    
    pht <- list.files(paste0("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_", ano  ),
                      pattern = paste0("^", i),
                      full.names = T) %>% 
      .[grepl("_standard_net_graph.rds", tolower(.))]
    
    r <- readRDS(pht)
    
    ret <- tibble(as.data.frame(ends(r, E(r), names = T))) %>% 
      dplyr::mutate(ids = paste0(V1, "-", V2)) %>% 
      dplyr::left_join(., Rc) %>% 
      dplyr::group_by(V1, Rc) %>%
      dplyr::tally() %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(V1) %>% 
      dplyr::summarise(Rc, n, freq = round(prop.table(n),3)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(across(everything(.), as.character),
                    cat = dm,
                    time = ano) %>% 
      dplyr::left_join(., get_questions_opts(data= preguntas$rodada_2020,
                                             var = var,
                                             var_lab = "label"), by = c("Rc" = "Opcoes_numero")) %>% 
      dplyr::left_join(., groups[[rodada]] %>% dplyr::select( ID_node, Nome, V1), by = c("V1" = "ID_node")) %>% 
      dplyr::select(R_Start_ID = V1,
                    Nome,
                    group = V1.y,
                    time,
                    cat,
                    label,
                    conteo = n,
                    freq) 
    
    return(ret)
  }) %>% 
    bind_rows()
)



writexl::write_xlsx(Rc_freqs, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/Rc_freqs.xlsx")

###############################################
####### conteos nodos por cada red ###########
############################################



standard_res <- lapply(c(2019, 2020), function(year){ list.files(paste0("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_", year),
                           pattern = "standard_net_results.xlsx",
                           full.names = T) %>% 
  .[!grepl("madre", .)] %>% 
    lapply(., function(k){
      read_excel(k, sheet = "company_count") %>% 
        dplyr::mutate(ano = year, 
                      type = stringr::str_extract(k, "[a-zA-Z]+_[a-zA-Z_]+.xlsx"))
    }) %>% bind_rows
  
  
  
  } ) %>% 
  bind_rows

grc_res <-  lapply(c(2019, 2020), function(year){list.files(paste0("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_", year),
                                                            pattern = "grC_net_results.xlsx",
                                                            full.names = T) %>% 
    .[!grepl("madre", .)] %>% 
    lapply(., function(k){
      read_excel(k, sheet = "company_count") %>% 
        dplyr::mutate(ano = year, 
                      type = stringr::str_extract(k, "[a-zA-Z]+_[a-zA-Z_]+.xlsx"))
    }) %>% bind_rows} ) %>% 
  bind_rows



standard_counts <- standard_res %>%
  dplyr::mutate(type = str_extract(type,"[a-zA-Z]+")) %>%
  dplyr::filter(V1 != "Total") %>% 
  dplyr::group_by(type, ano) %>% 
  dplyr::summarise(total = sum(n)) %>%
  ungroup() %>% 
  tidyr::pivot_wider(id_cols = ano, names_from = type, values_from = total) %>% 
  dplyr::mutate(type = "Standard net") %>% 
  dplyr::select(type, ano, investment, negocios, cooptec, colabInst)



grc_counts <- grc_res %>%
  dplyr::mutate(type = str_extract(type,"[a-zA-Z]+")) %>%
  dplyr::filter(V1 != "Total") %>% 
  dplyr::group_by(type, ano) %>% 
  dplyr::summarise(total = sum(n)) %>%
  ungroup() %>% 
  tidyr::pivot_wider(id_cols = ano, names_from = type, values_from = total) %>% 
  dplyr::mutate(type = "grC net") %>% 
  dplyr::select(type, ano, investment, negocios, cooptec, colabInst)

grc_news <- grc_res %>%
  dplyr::mutate(type = str_extract(type,"[a-zA-Z]+")) %>%
  dplyr::filter(V1 == "C") %>% 
  dplyr::group_by(type, ano) %>% 
  dplyr::summarise(total = sum(n)) %>%
  ungroup() %>% 
  tidyr::pivot_wider(id_cols = ano, names_from = type, values_from = total) %>% 
  dplyr::mutate(type = "C identified") %>% 
  dplyr::select(type, ano, investment, negocios, cooptec, colabInst)


bind_rows(standard_counts, grc_counts, grc_news) %>% 
  arrange(ano) %>% 
  write_xlsx(., "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/node_counts_all_nets.xlsx")


##################################################################
############# Correlaciones red mama y percepcion ###############
################################################################




perceps <- list( rodada_2019 = raw_list_2019[[grep("percepcoes", tolower(names(raw_list_2019)))]] %>% 
dplyr::select(-V20) %>% 
dplyr::filter(!is.na(V17)) %>% 
bind_cols(., correct_question_format(input_data = raw_list_2019 %>% 
                                       purrr::pluck(., grep("Percepcoes", names(raw_list_2019))) , 
                                     var_opts = get_questions_opts(data = preguntas[["rodada_2019"]] , var = "V20", var_lab = "label"), 
                                     var = "V20" ))  ,
rodada_2020 =  raw_list_2020[[grep("percepcoes", tolower(names(raw_list_2020)))]] %>% 
  dplyr::select(-matches("[0-9]a"))  )


ability <- lapply(c("rodada_2019","rodada_2020"), function(i){
  
  perceps[[i]] %>% 
    dplyr::select(starts_with("ID"), contains("V1_2020"), V21:V28) %>% 
    rename_with(function(x){return("ID")}, contains("ID_node")) %>% 
    dplyr::mutate(ID = as.character(ID)) %>% 
    dplyr::left_join(., groups[[i]] %>% dplyr::select(ID_node, V1), by = c("ID" = "ID_node")) %>% 
    dplyr::mutate(score = rowSums(dplyr::select(., V21:V28))  ) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::summarise(score_final = mean(score)) %>% 
    dplyr::rename(!!paste0("score_ability_",i):= score_final)
  
  
}) 
names(ability) <- c("rodada_2019","rodada_2020")

believes <- lapply(c("rodada_2019","rodada_2020"), function(i){
  
  perceps[[i]] %>% 
    dplyr::select(starts_with("ID"), contains("V1_2020"), V29:V36) %>% 
    rename_with(function(x){return("ID")}, contains("ID_node")) %>% 
    dplyr::mutate(ID = as.character(ID)) %>% 
    dplyr::left_join(., groups[[i]] %>% dplyr::select(ID_node, V1), by = c("ID" = "ID_node")) %>% 
    dplyr::mutate(score = rowSums(dplyr::select(., V29:V36))  ) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::summarise(score_final = mean(score)) %>% 
    dplyr::rename(!!paste0("score_believes_",i):= score_final)
  
  
}) 
names(believes) <- c("rodada_2019","rodada_2020")

madre_res <- list(rodada_2019 =  readxl::read_excel("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_2019/madre_grC_net_results.xlsx", sheet = "node_mtrs") %>% 
  dplyr::select(nodes_id, Nome, V1, degree, betwe), 
rodada_2020 =  readxl::read_excel("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_2020/madre_grC_net_results.xlsx", sheet = "node_mtrs") %>% 
  dplyr::select(nodes_id, Nome, V1, degree, betwe)
)


to_acp_2019 <- ability[["rodada_2019"]] %>% 
  left_join(., believes[["rodada_2019"]], by = "ID") %>% 
  left_join(., madre_res[["rodada_2019"]], by = c("ID" = "nodes_id")) %>% 
  left_join(., perceps[["rodada_2019"]] %>% 
              dplyr::select(ID, V17) %>% 
              group_by(ID) %>% 
              dplyr::summarise(score_knowledge_2019 = mean(V17)) %>% 
              dplyr::mutate(ID = as.character(ID)), by = "ID") %>% 
  dplyr::filter(!duplicated(ID)) %>% 
  drop_na


to_acp_2020 <- ability[["rodada_2020"]] %>% 
  left_join(., believes[["rodada_2020"]], by = "ID") %>% 
  left_join(., madre_res[["rodada_2020"]], by = c("ID" = "nodes_id")) %>% 
  left_join(., perceps[["rodada_2020"]] %>% 
              dplyr::select(ID = ID_node, V17) %>% 
              group_by(ID) %>% 
              dplyr::summarise(score_knowledge_2020 = mean(V17)) %>% 
              dplyr::mutate(ID = as.character(ID)), by = "ID") %>% 
  dplyr::filter(!duplicated(ID)) %>% 
  drop_na()
  
cor_rodada_2019 <- list(
 ability_score = to_acp_2019 %>% 
    dplyr::select(ID, score_ability_rodada_2019, degree, V1),
 
 cor_ability = to_acp_2019 %>% 
   dplyr::select(ID, score_ability_rodada_2019, degree, V1) %>% 
   dplyr::group_by(V1) %>% 
   dplyr::summarise(cor = cor(score_ability_rodada_2019, degree)),
 
 believe_score =  to_acp_2019 %>% 
    dplyr::select(ID, score_believes_rodada_2019, degree, V1),
 
 cor_believe = to_acp_2019 %>% 
   dplyr::select(ID, score_believes_rodada_2019, degree, V1) %>% 
   dplyr::group_by(V1) %>% 
   dplyr::summarise(cor = cor(score_believes_rodada_2019, degree))
)



cor_rodada_2020 <- list(
  ability_score = to_acp_2020 %>% 
    dplyr::select(ID, score_ability_rodada_2020, degree, V1),
  cor_score = to_acp_2020 %>% 
    dplyr::select(ID, score_ability_rodada_2020, degree, V1) %>% 
    dplyr::group_by(V1) %>% 
    dplyr::summarise(cor = cor(score_ability_rodada_2020, degree)),
  believe_score = to_acp_2020 %>% 
    dplyr::select(ID, score_believes_rodada_2020, degree, V1),
  cor_believe = to_acp_2020 %>% 
    dplyr::select(ID, score_believes_rodada_2020, degree, V1) %>% 
    dplyr::group_by(V1) %>% 
    dplyr::summarise(cor = cor(score_believes_rodada_2020, degree))
  
)

writexl::write_xlsx(cor_rodada_2019, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_2019/correlation_rodada_2019.xlsx")
writexl::write_xlsx(cor_rodada_2020,"D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/rodada_2020/correlation_rodada_2020.xlsx")


require(FactoMineR)
require(factoextra)


pca_2019 <- FactoMineR::PCA(to_acp_2019 %>% dplyr::select(starts_with("score"), degree, betwe))
plot(pca_2019)
pca_2019$var$contrib %>% write.table("clipboard", sep = "\t", row.names = T)


mfa_2019 <- FactoMineR::MFA(to_acp_2019 %>% dplyr::select(starts_with("score"), degree, betwe, V1),
                             group = c(5,1), type = c("s", "n"))

plot(mfa_2019, lab.ind = T)

to_acp_2019 %>% 
  dplyr::select(ID, V1, starts_with("score"), degree, betwe) %>% 
  ggplot(aes(x = score_ability_rodada_2019,  y = degree, colour = V1))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  scale_colour_manual(values = c("black", "gray50"))+
  theme_bw()
  



to_acp_2019 %>% 
  dplyr::select(ID, V1, starts_with("score"), degree, betwe) %>% 
  group_by(V1) %>% 
  dplyr::summarise(cor  = cor(score_ability_rodada_2019, degree)) %>% 
  write.table("clipboard", sep = "\t", row.names= F)






membership_2019 <- raw_list_2019[[grep("Informer", names(raw_list_2019))]] %>% 
  dplyr::select(ID, V12) %>% 
  drop_na() %>% 
  dplyr::mutate(V12 = lubridate::as_date(V12),
                ID = as.character((ID))) %>% 
  dplyr::left_join(., groups[["rodada_2019"]] %>% dplyr::select(ID_node, V1), by = c("ID"= "ID_node"))


membership_2020 <- raw_list_2020[[grep("Informer", names(raw_list_2020))]] %>% 
  dplyr::select(ID = ID_node, V12) %>% 
  drop_na() %>% 
  dplyr::filter(!V12 %in% c(0,1)) %>% 
  dplyr::mutate(V12 = lubridate::as_date(V12, origin = "1899-12-30"),
                ID = as.character((ID)) ) %>% 
  dplyr::left_join(., groups[["rodada_2020"]] %>% dplyr::select(ID_node, V1), by = c("ID"= "ID_node"))
                  


bind_rows(membership_2019, membership_2020) %>% 
  #dplyr::filter(!duplicated(ID)) %>%
  dplyr::mutate(ID = as.character(ID)) %>% 
  group_by(ID)%>%
  dplyr::slice(which.min(V12)) %>%
  ungroup() %>%
  dplyr::mutate(year = lubridate::year(V12)) %>% 
  group_by(year) %>%
  tally() %>% 
  dplyr::select(year, count = n) %>% 
  write.table("clipboard", sep  = "\t", row.names = F)



bind_rows(membership_2019, membership_2020) %>% 
  #dplyr::filter(!duplicated(ID)) %>%
  dplyr::mutate(ID = as.character(ID)) %>% 
  group_by(ID)%>%
  dplyr::slice(which.min(V12)) %>%
  ungroup() %>%
  dplyr::mutate(year = lubridate::year(V12)) %>% 
  group_by(year, V1) %>%
  tally() %>% 
  dplyr::select(year, V1, count = n) %>% 
  write.table("clipboard", sep  = "\t", row.names = F)


  

new_data$Informer %>%
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::inner_join(., groups, by = c("ID" = "ID")) %>%
  dplyr::mutate(year = lubridate::year(V12)) %>% 
  group_by(ID)%>%
  dplyr::slice(which.min(V12)) %>%
  ungroup() %>% 
  group_by(year) %>%
  dplyr::count(ID) %>%
  left_join(., groups %>% dplyr::select(ID, Nome), by = c("ID" = "ID")) %>% 
  dplyr::select(ID, Nome, year) %>% 
  write.csv(., paste0(tbl_questions, "pregunta_V12(Date interaction with PPA began (organization))", ".csv") )


############################################################
##extraer tab de archivos excel para enviar a carlos ######3
##########################################################

fls <- list.files("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/",
           pattern = "_net_results.xlsx",
           recursive = T, 
           full.names = T)


fls <- fls[!grepl("madre|Dir", fls)]

fls_short <- list.files("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/",
                  pattern = "_net_results.xlsx",
                  recursive = T, 
                  full.names = F)


fls_short <- fls_short[!grepl("madre|Dir", fls_short)] %>% 
  stringr::str_remove(., "rodada_") %>% 
  stringr::str_remove(., ".xlsx") %>% 
  stringr::str_replace(., pattern = "net_results", "removed_startup") %>% 
  stringr::str_replace(., pattern = "/", "_")




f_files <- lapply(fls, function(i){
  cat(i, "\n")
  c_file <- readxl::read_excel(i, sheet = "removed_startup_links_freq")
  
})

names(f_files) <- fls_short


writexl::write_xlsx(f_files, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/removed_startup_from_networks.xlsx")

