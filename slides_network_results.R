pacman::p_load(tidyverse, igraph, readxl, writexl)




root_dir <- "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/"

rodadas <- c("rodada_2019", "rodada_2020", "rodada_2021")

###evolucion by groups grupo C

evolucion_by_gr_grC <- lapply(c("investment_grC", "negocios_grC", "cooptec_grC", "colabInst_grC", "madre_grC"),
       function(pattern){
         cat("processing: ", pattern, "\n")
         df <- lapply(rodadas, function(rodada){
           baseDir <- paste0(root_dir, rodada)
           
           cat(rodada, "\n")
           to_ret <- list.files(baseDir, pattern = pattern, recursive = T, full.names = T) %>% 
             .[!grepl("old", .)] %>% 
             .[grepl(".xlsx", .)] %>% 
             readxl::read_excel(., sheet = "node_mtrs") %>% 
             dplyr::mutate(V1 = ifelse(V1 == "0", "C", V1))
           
           lg <- to_ret %>% 
             dplyr::filter(nodes_id != "86") %>% 
             pull(`Subtype of organization`) %>% 
             unique() %>% 
             grepl("[sS]tart-up|[sS]tart up", .) %>% 
             any() 
           
           glob_avg_deg <- mean(to_ret$degree)
           glob_avg_bet <- mean(to_ret$betwe)
           
           stopifnot("Start-up found in data" = !lg)
           
           to_ret <- to_ret %>% 
             dplyr::select(V1, degree, betwe) %>% 
             dplyr::group_by(V1) %>% 
             dplyr::summarise(number_of_orgs = n(),
                              avg_degree = mean(degree),
                              avg_betwe  = mean(betwe)) 
           
           to_ret <- to_ret %>% 
             add_row(V1= "Total", 
                     number_of_orgs = sum(to_ret$number_of_orgs),
                     avg_degree = glob_avg_deg,
                     avg_betwe  = glob_avg_bet)
           
           names(to_ret)[-1] <- paste0(names(to_ret)[-1], "_",rodada)
           
           return(to_ret)
         }) %>% 
           purrr::reduce(., left_join, by = "V1")
         
         return(df)
         
       })

names(evolucion_by_gr_grC) <- c("investment_grC", "negocios_grC", "cooptec_grC", "colabInst_grC", "madre_grC")

writexl::write_xlsx(evolucion_by_gr_grC, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/resultados_diapositivas/evolucion_by_grC.xlsx")


##### Evolution of the positioning of organization by subtypes 2019-2021



evolucion_posi_by_subtype <- lapply(c("investment_grC", "negocios_grC", "cooptec_grC", "colabInst_grC", "madre_grC"),
                              function(pattern){
                                cat("processing: ", pattern, "\n")
                                df <- lapply(rodadas, function(rodada){
                                  baseDir <- paste0(root_dir, rodada)
                                  
                                  cats <- get_questions_opts(data = preguntas$rodada_2021, var = "V39b", var_lab = "Subtype of organization") %>% 
                                    add_row(Opcoes_numero = "15" , `Subtype of organization` = "Not participated")
                                  
                                  cats$Opcoes_numero <- c(15, 5, 2, 1, 4, 3, 11, 8, 6,10, 8, 12, 13, 14, 9 )
                                  cats <- cats %>% 
                                    arrange(Opcoes_numero)
                                  
                                  cat(rodada, "\n")
                                  
                                  to_ret <- list.files(baseDir, pattern = pattern, recursive = T, full.names = T) %>% 
                                    .[!grepl("old", .)] %>% 
                                    .[grepl(".xlsx", .)] %>% 
                                    readxl::read_excel(., sheet = "node_mtrs") %>% 
                                    dplyr::mutate(V1 = ifelse(V1 == "0", "C", V1))
                                  
                                  lg <- to_ret %>% 
                                    dplyr::filter(nodes_id != "86") %>% 
                                    pull(`Subtype of organization`) %>% 
                                    unique() %>% 
                                    grepl("[sS]tart-up|[sS]tart up", .) %>% 
                                    any() 
                                  
                                  glob_avg_deg <- mean(to_ret$degree)
                                  glob_avg_bet <- mean(to_ret$betwe)
                                  
                                  stopifnot("Start-up found in data" = !lg)
                                  
                                  to_ret <- to_ret %>% 
                                    dplyr::select(`Subtype of organization`, degree, betwe) %>% 
                                    dplyr::group_by(`Subtype of organization`) %>% 
                                    dplyr::summarise(number_of_orgs = n(),
                                                     avg_degree = mean(degree),
                                                     avg_betwe  = mean(betwe)) 
                                  
                                  to_ret <- left_join(cats, to_ret, by = c("Subtype of organization")) %>% 
                                    dplyr::select(-Opcoes_numero)
                                  
                                  to_ret <- to_ret %>% 
                                    add_row(`Subtype of organization` = "Total", 
                                            number_of_orgs = sum(to_ret$number_of_orgs, na.rm = T),
                                            avg_degree = glob_avg_deg,
                                            avg_betwe  = glob_avg_bet)
                                  
                                  names(to_ret)[-1] <- paste0(names(to_ret)[-1], "_",rodada)
                                  
                                  return(to_ret)
                                }) %>% 
                                  purrr::reduce(., left_join, by = "Subtype of organization")
                                
                                return(df)
                                
                              })


names(evolucion_posi_by_subtype) <- c("investment_grC", "negocios_grC", "cooptec_grC", "colabInst_grC", "madre_grC")

writexl::write_xlsx(evolucion_posi_by_subtype, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/resultados_diapositivas/evolucion_by_subtype.xlsx")


#### Evolution of top 15 organizations 



evolucion_top_15 <- lapply(c("investment_grC", "negocios_grC", "cooptec_grC", "colabInst_grC", "madre_grC"),
                                    function(pattern){
                                      cat("processing: ", pattern, "\n")
                                      df <- lapply(rodadas, function(rodada){
                                        baseDir <- paste0(root_dir, rodada)
                                          
                                        cat(rodada, "\n")
                                        
                                        to_ret <- list.files(baseDir, pattern = pattern, recursive = T, full.names = T) %>% 
                                          .[!grepl("old", .)] %>% 
                                          .[grepl(".xlsx", .)] %>% 
                                          readxl::read_excel(., sheet = "node_mtrs") %>% 
                                          dplyr::mutate(V1 = ifelse(V1 == "0", "C", V1))
                                        
                                         
                                        to_ret <- tibble(Ranking = 1:15, 
                                                         ranked_by_degree = to_ret %>% 
                                                           arrange(desc(degree)) %>% 
                                                           slice(1:15) %>% 
                                                           pull(Nome),
                                                         ranked_by_betwe = to_ret %>% 
                                                           arrange(desc(betwe )) %>% 
                                                           slice(1:15) %>% 
                                                           pull(Nome))
                                        
                                      
                                        
                                        names(to_ret) <- paste0(names(to_ret), "_",rodada)
                                        
                                        return(to_ret)
                                      }) %>% 
                                        dplyr::bind_cols()
                                      
                                      return(df)
                                      
                                    })



names(evolucion_top_15) <- c("investment_grC", "negocios_grC", "cooptec_grC", "colabInst_grC", "madre_grC")

writexl::write_xlsx(evolucion_top_15, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/resultados_diapositivas/evolucion_top_15.xlsx")

##### Evolution of  relationships  Rb

evolucion_Rb <- lapply(c("investment_standard_net_results",  "cooptec_standard_net_results", "colabInst_standard_net_results"),
                       function(pattern){
                         cat("processing: ", pattern, "\n")
                         df <- lapply(rodadas, function(rodada){
                           baseDir <- paste0(root_dir, rodada)
                           
                           cat(rodada, "\n")
                           
                           to_ret <- list.files(baseDir, pattern = pattern, recursive = T, full.names = T) %>% 
                             .[!grepl("old", .)] %>% 
                             .[!grepl("~", .)] %>%
                             .[grepl(".xlsx", .)] %>% 
                             readxl::read_excel(., sheet = "Rb_summ") %>% 
                             dplyr::mutate(Group = ifelse(Group == "0", "C", Group))
                           
                           names(to_ret) <- c("Group", "var",  "Count", "Frequency")
                           
                           to_ret <- to_ret %>% 
                             dplyr::group_by(var) %>% 
                             dplyr::summarise(sum = sum(Count))
                           
                           
                           
                           if(pattern == "investment_standard_net_results"){
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R2b") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                             
                           }else if(pattern == "cooptec_standard_net_results"){
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R3b") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                             
                           }else{
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R4b") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                           }
                           
                           names(cats) <- "label"
                           
                           to_ret <- left_join(cats, to_ret,  by = c("label" = "var")) 
                           
                           names(to_ret)[-1] <- paste0(names(to_ret)[-1], "_",rodada)
                           
                           return(to_ret)
                         }) %>% 
                           purrr::reduce(., left_join, by = "label") %>% 
                           dplyr::mutate(across(everything(.), .fns = function(i){
                             ifelse(is.na(i), 0, i)
                           }))
                         
                         return(df)
                         
                       })




names(evolucion_Rb) <-c("investment_standard_net_results",  "cooptec_standard_net_results")

writexl::write_xlsx(evolucion_Rb, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/resultados_diapositivas/evolucion_Rb.xlsx")

### Evolution of relationships intensity 

evolucion_Rc <- lapply(c("investment_standard_net_results", 
                         "negocios_standard_net_results", 
                         "cooptec_standard_net_results",
                         "colabInst_standard_net_results"),
                       function(pattern){
                         cat("processing: ", pattern, "\n")
                         df <- lapply(rodadas, function(rodada){
                           baseDir <- paste0(root_dir, rodada)
                           
                           cat(rodada, "\n")
                           R_sheet <- "Rc_summ"
                           if(pattern == "investment_standard_net_results"){
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R2c") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                             
                           }else if(pattern == "negocios_standard_net_results"){
                             
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R1b") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                             
                             R_sheet <- "Rb_summ"
                             
                           }else if(pattern == "cooptec_standard_net_results"){
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R3c") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                             
                           }else if(pattern == "colabInst_standard_net_results"){
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R4c") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                           }
                           
                           names(cats) <- "label"
                           
                           
                           to_ret <- list.files(baseDir, pattern = pattern, recursive = T, full.names = T) %>% 
                             .[!grepl("old", .)] %>% 
                             .[!grepl("~", .)] %>%
                             .[grepl(".xlsx", .)] %>% 
                             readxl::read_excel(., sheet = R_sheet) %>% 
                             dplyr::mutate(Group = ifelse(Group == "0", "C", Group))
                           
                           names(to_ret) <- c("Group", "var",  "Count", "Frequency")
                           
                           to_ret <- to_ret %>% 
                             dplyr::group_by(var) %>% 
                             dplyr::summarise(sum = sum(Count))
                           
                           
                           
                           
                           to_ret <- left_join(cats, to_ret,  by = c("label" = "var")) 
                           
                           names(to_ret)[-1] <- paste0(names(to_ret)[-1], "_",rodada)
                           
                           return(to_ret)
                         }) %>% 
                           purrr::reduce(., left_join, by = "label") %>% 
                           dplyr::mutate(across(everything(.), .fns = function(i){
                             ifelse(is.na(i), 0, i)
                           }))
                         
                         return(df)
                         
                       })


names(evolucion_Rc) <- c("investment_standard_net_results", 
                         "negocios_standard_net_results", 
                         "cooptec_standard_net_results",
                         "colabInst_standard_net_results")

writexl::write_xlsx(evolucion_Rc, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/resultados_diapositivas/evolucion_Rc.xlsx")


### Advanced metrics: Data por thematic network 

advance_mtrs <- lapply(rodadas, function(rodada){
  
  to_ret <- tibble(a = c("investment_standard_net_results", 
                          "negocios_standard_net_results", 
                          "cooptec_standard_net_results",
                          "colabInst_standard_net_results",
                          "madre_standard_net_results"),
                    b = c("investment_grC", 
                          "negocios_grC", 
                          "cooptec_grC", 
                          "colabInst_grC", 
                          "madre_grC"),
                    dim = c("Investment",	"Business",	"Technical coop.",	"Institutional collab.", "red mama")) %>% 
    apply(., 1, function(dim){
      baseDir <- paste0(root_dir, rodada)
      
      to_reta <- list.files(baseDir, pattern = dim[1], recursive = T, full.names = T) %>% 
        .[!grepl("old", .)] %>% 
        .[!grepl("~", .)] %>%
        .[grepl(".xlsx", .)] %>% 
        readxl::read_excel(., sheet = "gnr_mtrs") 
      
      to_retb <- list.files(baseDir, pattern = dim[2], recursive = T, full.names = T) %>% 
        .[!grepl("old", .)] %>% 
        .[!grepl("~", .)] %>%
        .[grepl(".xlsx", .)] %>% 
        readxl::read_excel(., sheet = "gnr_mtrs") 
      
      
      
      adv_mrtrs <- tibble(Dimension = 
                            c("Density (a)",
                              "Density (b)",
                              "Mean Degree (a)",
                              "Mean Degree (b)",
                              "Betweenness centrality (a)",
                              "Betweenness centrality (b)",
                              "Central Degree (a)",
                              "Central Degree (b)",
                              "Eigen Value (a)",
                              "Eigen Value (b)",
                              "Modularity (a)",
                              "Modularity (b)",
                              "Transitivity (a)",
                              "Transitivity (b)",
                              "Assortativeness (a)",
                              "Assortativeness (b)"),
                          var = c(to_reta$Value[1],
                                  to_retb$Value[1],
                                  to_reta$Value[5],
                                  to_retb$Value[5],
                                  to_reta$Value[2],
                                  to_retb$Value[2],
                                  to_reta$Value[3],
                                  to_retb$Value[3],
                                  to_reta$Value[8],
                                  to_retb$Value[8],
                                  to_reta$Value[4],
                                  to_retb$Value[4],
                                  to_reta$Value[6],
                                  to_retb$Value[6],
                                  to_reta$Value[7],
                                  to_retb$Value[7]
                                  
                          )
      )
      
      names(adv_mrtrs)[2] <- paste0(dim[3],"_", rodada)
      
      return(adv_mrtrs)
    }) %>% 
    purrr::reduce(., left_join, by = "Dimension")
  
  
  return(to_ret)
}) %>% 
  purrr::reduce(., left_join, by = "Dimension")



writexl::write_xlsx(advance_mtrs, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/resultados_diapositivas/advance_mtrs_compar.xlsx")



###3 IN out PPA


in_out_ppa <- lapply(rodadas,
                       function(rodada){
                         cat("processing: ", pattern, "\n")
                         df <- lapply(c("investment_standard_net_results", 
                                        "negocios_standard_net_results", 
                                        "cooptec_standard_net_results",
                                        "colabInst_standard_net_results"), function(pattern){
                           
                           baseDir <- paste0(root_dir, rodada)
                           
                           cat(rodada, "\n")
                           
                           to_ret <- list.files(baseDir, pattern = pattern, recursive = T, full.names = T) %>% 
                             .[!grepl("old", .)] %>% 
                             .[!grepl("~", .)] %>%
                             .[grepl(".xlsx", .)] %>% 
                             readxl::read_excel(., sheet = "Ra_summ") %>% 
                             dplyr::mutate(Group = ifelse(Group == "0", "C", Group))
                           
                           names(to_ret) <- c("Group", "var",  "Count", "Frequency")
                           
                           to_ret <- to_ret %>% 
                             dplyr::group_by(var) %>% 
                             dplyr::summarise(sum = sum(Count))
                           
                           
                           
                           if(pattern == "investment_standard_net_results"){
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R2a") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                             
                           }else if(pattern == "cooptec_standard_net_results"){
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R3a") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                             
                           }else if(pattern == "negocios_standard_net_results"){ 
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R2a") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                           }else{
                             cats <- preguntas[["rodada_2021"]] %>% filter(Variavel == "R4a") %>% 
                               pull(vars_opts) %>% 
                               purrr::pluck(1) %>% 
                               dplyr::select(`Labels in English`)
                           }
                           
                           names(cats) <- "label"
                           
                           to_ret <- left_join(cats, to_ret,  by = c("label" = "var")) 
                           
                           names(to_ret)[-1] <- paste0(names(to_ret)[-1], "_",pattern)
                           
                           return(to_ret)
                         }) %>% 
                           purrr::reduce(., left_join, by = "label") %>% 
                           dplyr::mutate(across(everything(.), .fns = function(i){
                             ifelse(is.na(i), 0, i)
                           }))
                         
                         return(df)
                         
                       })


names(in_out_ppa) <- rodadas



in_out_ppa$totals <- in_out_ppa %>% 
  lapply(., function(l){
    #l <- in_out_ppa[[1]]
    to_ret <- tidyr::pivot_longer(l, cols = -label, names_to = "var", values_to = "vals") %>% 
      group_by(label) %>% 
      dplyr::summarise(counts = sum(vals))
    return(to_ret)
  }) %>% 
  purrr::reduce(., left_join, by = "label")
  

writexl::write_xlsx(in_out_ppa, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/resultados_diapositivas/in_out_ppa.xlsx")

##### red mama number of relationships (degree)



pattern = c("madre_grC" )

df <- lapply(rodadas, function(rodada){
  baseDir <- paste0(root_dir, rodada)
  
  cat(rodada, "\n")
  to_ret <- list.files(baseDir, pattern = pattern, recursive = T, full.names = T) %>% 
    .[!grepl("old", .)] %>% 
    .[grepl(".xlsx", .)] %>% 
    readxl::read_excel(., sheet = "node_mtrs") %>% 
    dplyr::mutate(V1 = ifelse(V1 == "0", "C", V1)) %>% 
    dplyr::select(nodes_id, Nome, V1, degree)
  
  
  
 
  names(to_ret) <- c("Node", paste0("Nome_", rodada), "V1", paste0("Deg_",  rodada))
  
  return(to_ret)
  
})

ranks_red_mama <- list()

 ranks <- purrr::reduce(df, right_join, by = c("Node")) %>% 
  dplyr::select(Node, starts_with("Nome"), V1,  starts_with("Deg")) %>% 
  tidyr::drop_na(Nome_rodada_2019,   Nome_rodada_2020,  Nome_rodada_2021) %>% 
  dplyr::mutate(Evol_Degree   = Deg_rodada_2021-Deg_rodada_2020,
                Evol_Degree_perc =  Evol_Degree/Deg_rodada_2020) 

 ranks_red_mama$rank_ascendente <-ranks %>% 
  dplyr::filter(V1 != "C") %>% 
  dplyr::arrange(desc(Evol_Degree)) %>% 
  dplyr::mutate(rank = 1:nrow(.)) %>% 
  dplyr::select(rank, Node,  Nome = Nome_rodada_2021, starts_with("Deg"), Evol_Degree, Evol_Degree_perc)
 
 ranks_red_mama$rank_ascendente_grC <- ranks %>% 
  dplyr::filter(V1 == "C") %>% 
  dplyr::arrange(desc(Evol_Degree)) %>% 
  dplyr::mutate(rank = 1:nrow(.)) %>% 
  dplyr::select(rank, Node, Nome = Nome_rodada_2021, starts_with("Deg"), Evol_Degree, Evol_Degree_perc)



 ranks_red_mama$rank_descendente <-ranks %>% 
  dplyr::filter(V1 != "C") %>% 
  dplyr::arrange((Evol_Degree)) %>% 
  dplyr::mutate(rank = 1:nrow(.)) %>% 
  dplyr::select(rank, Node, Nome = Nome_rodada_2021, starts_with("Deg"), Evol_Degree, Evol_Degree_perc)

 ranks_red_mama$rank_descendente_grC <-ranks %>% 
  dplyr::filter(V1 == "C") %>% 
  dplyr::arrange((Evol_Degree)) %>% 
  dplyr::mutate(rank = 1:nrow(.)) %>% 
  dplyr::select(rank, Node, Nome = Nome_rodada_2021, starts_with("Deg"), Evol_Degree, Evol_Degree_perc)

 writexl::write_xlsx(ranks_red_mama, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/resultados_diapositivas/ranks_red_mama.xlsx")
 
