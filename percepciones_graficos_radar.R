#' scripts para generar las tablas y graficos de la seccion de percepciones
#' @author andres Camilo Mendez
#' 2023
#' Se debe cargar las variables primero usando el script de rodadas_net.R

#####perceciones rodada 2021

become_vec <- function(choices, n_res){
  
  if(is.null(dim(choices))){
    vec <- as.integer(1:n_res %in% choices) 
  }else if(length(dim(choices))){
    vec <-  as.integer(1:n_res %in% dplyr::pull(choices))
  }
  return(vec)
}

#percep <- raw_list_2021[[grep("Percepcoes", names(raw_list_2021))]]
preg <- preguntas[["rodada_2022"]]
entrev <- entrevistados[["rodada_2022"]]

start_ups <- lapply(type_org, function(lst){
  lst %>% 
    dplyr::select(ID_node, V39b, `Subtype of organization`) %>% 
    dplyr::filter(V39b == 7) %>% 
    dplyr::filter(ID_node != "86") %>% 
    dplyr::filter(`Subtype of organization` == "Start-up company") %>% 
    dplyr::pull(ID_node)
})

raw_list_2019[[grep("Percepcoes", names(raw_list_2019))]] <- raw_list_2019[[grep("Percepcoes", names(raw_list_2019))]] %>%
  dplyr::mutate(ID2 = paste0(ID, ID_ego))


unq <- unique(raw_list_2019[[grep("Percepcoes", names(raw_list_2019))]]$ID2)

reshaped <- lapply(unq, function(i){
  raw_list_2019[[grep("Percepcoes", names(raw_list_2019))]] %>%
    dplyr::filter( ID2 == i) %>%
    dplyr::pull(V20) %>%
    become_vec(choices = ., n_res = 13)

}) %>%
  do.call( rbind, .) %>%
  as.data.frame()
names(reshaped) <- paste0("V20_", stringr::str_extract(names(reshaped), "[0-9]+"))

#se eliminan los egos de la misma empresa y solo se deja uno
#se desconoce porqué se elimina 76-USAID de los analisis
perceps <- list(rodada_2019 = raw_list_2019[[grep("Percepcoes", names(raw_list_2019))]] %>% 
                  dplyr::filter(!is.na(V17)) %>%
                  bind_cols(., reshaped)%>%
                  dplyr::rename("ID_node" = ID) %>% 
                  dplyr::filter(ID_node != "76") %>%
                  dplyr::mutate(id = paste0(ID_node, "_", ID_ego)) %>% 
                  dplyr::filter(!id  %in% c("42_a","42_c","49_a","49_b","76_a")) %>% 
                  dplyr::select(-id, -V20) %>% 
                  dplyr::filter(!ID_node %in% start_ups$rodada_2019),
                
                rodada_2020 = raw_list_2020[[grep("Percepcoes", names(raw_list_2020))]] %>% 
                  dplyr::filter(ID_node != "76") %>% 
                  dplyr::mutate(id = paste0(ID_node, "_", ID_ego)) %>%  
                  dplyr::filter(!id  %in% c("18_b", "18_c", "42_b","42_c","43_b" ,
                                            "49_b","49_c","76_c", "76_d","84_b", "84_c")) %>%
                  dplyr::select(-id) %>% 
                  dplyr::filter(!ID_node %in% start_ups$rodada_2020),
                
                rodada_2021 = raw_list_2021[[grep("Percepcoes", names(raw_list_2021))]] %>% 
                  dplyr::filter(ID_node != "76") %>% 
                  dplyr::mutate(id = paste0(ID_node, "_", ID_ego)) %>%  
                  dplyr::filter(!id  %in% c("18_b", "18_c","76_c")) %>%
                  dplyr::select(-id) %>% 
                  dplyr::filter(!ID_node %in% start_ups$rodada_2021) ,
                
                rodada_2022 = raw_list_2022[[grep("Percepcoes", names(raw_list_2022))]] %>% 
                  dplyr::filter(ID_node != "76") %>% 
                  dplyr::mutate(id = paste0(ID_node, "_", ID_ego)) %>%  
                  dplyr::filter(!id  %in% c("18_a", "18_c","76_c")) %>%
                  dplyr::select(-id) %>% 
                  dplyr::filter(!ID_node %in% start_ups$rodada_2022)
                
                )

 


##### GRAFICO  SLIDES-5
#vars_to_proces <- names(to_proces)[4:(length(names(to_proces))-2)]

gr1_vars <- c("V21", "V27", "V26", "V25", "V24","V22", "V23", "V28")

label_quest <- lapply(gr1_vars, get_var_label,data = preg) %>% unlist
#slide_5 = c("1", "2") se refieren al tipo de organización que se deben incluir en los conteos
sec_1 <- lapply(list(slide_5 = c("1", "2"), slide_6 = c("1"), slide_7 = c("2")), function(filt){
  
  ret <-  lapply(gr1_vars, function(vrs){
    
    purrr::map2( perceps, names(perceps), .f = function(db, rd){
      
      db %>%
        dplyr::mutate(across(everything(.), as.character)) %>% 
        dplyr::left_join(.,type_org[[rd]] %>% 
                           dplyr::select(ID_node, V39a), by = c("ID_node")) %>% 
        dplyr::filter(V39a %in% filt) %>% 
        dplyr::select(ID_node, Opcoes_numero = all_of(vrs)) %>% 
        dplyr::filter(Opcoes_numero != "0") %>% 
        dplyr::mutate(Opcoes_numero = ifelse(Opcoes_numero == "4" | Opcoes_numero  == "5", "5", Opcoes_numero)) %>% 
        group_by(Opcoes_numero) %>% 
        tally() %>% 
        arrange(Opcoes_numero) %>% 
        dplyr::mutate(freq  = n/sum(n)) %>% 
        dplyr::select(freq) %>% 
        dplyr::slice(nrow(.))
      
    }) %>% 
      bind_rows() %>% 
      dplyr::rename(!!vrs := freq)
    
    
    
  }) %>% 
    bind_cols() %>% 
    dplyr::mutate(rodada = names(perceps)) %>% 
    dplyr::select(rodada, everything(.))
  
  names(ret)[2:length(names(ret))] <-  label_quest 
  
  return(ret)
})

sec_1$slide_8 <- lapply(gr1_vars, function(vrs){
  
  perceps[["rodada_2021"]] %>%
    dplyr::mutate(across(everything(.), as.character)) %>% 
    dplyr::left_join(.,type_org[["rodada_2021"]] %>% 
                       dplyr::select(ID_node, V39a), by = c("ID_node")) %>% 
    dplyr::filter(V39a %in% c("1", "2")) %>% 
    dplyr::select(ID_node, Opcoes_numero = all_of(vrs), V39a) %>% 
    dplyr::filter(Opcoes_numero != "0") %>% 
    dplyr::mutate(Opcoes_numero = ifelse(Opcoes_numero == "4" | Opcoes_numero  == "5", "5", Opcoes_numero)) %>% 
    group_by(V39a, Opcoes_numero) %>% 
    dplyr::summarise(count = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(V39a) %>% 
    dplyr::mutate(freq = count/sum(count)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(V39a) %>% 
    dplyr::filter(Opcoes_numero == "5") %>% 
    dplyr::select(freq)
  
  
}) %>% 
  dplyr::bind_cols() %>% 
  dplyr::mutate(V39a = c("1","2")) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas[["rodada_2021"]], var = "V39a", var_lab = "V39a_label"), 
                   by = c("V39a" = "Opcoes_numero")) %>% 
  dplyr::select(V39a_label, everything(.), -V39a)

names(sec_1$slide_8) <- c("V39a", label_quest)

sec_1$slide_8_2022 <- lapply(gr1_vars, function(vrs){
  
  perceps[["rodada_2022"]] %>%
    dplyr::mutate(across(everything(.), as.character)) %>% 
    dplyr::left_join(.,type_org[["rodada_2022"]] %>% 
                       dplyr::select(ID_node, V39a), by = c("ID_node")) %>% 
    dplyr::filter(V39a %in% c("1", "2")) %>% 
    dplyr::select(ID_node, Opcoes_numero = all_of(vrs), V39a) %>% 
    dplyr::filter(Opcoes_numero != "0") %>% 
    dplyr::mutate(Opcoes_numero = ifelse(Opcoes_numero == "4" | Opcoes_numero  == "5", "5", Opcoes_numero)) %>% 
    group_by(V39a, Opcoes_numero) %>% 
    dplyr::summarise(count = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(V39a) %>% 
    dplyr::mutate(freq = count/sum(count)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(V39a) %>% 
    dplyr::filter(Opcoes_numero == "5") %>% 
    dplyr::select(freq)
  
  
}) %>% 
  dplyr::bind_cols() %>% 
  dplyr::mutate(V39a = c("1","2")) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas[["rodada_2022"]], var = "V39a", var_lab = "V39a_label"), 
                   by = c("V39a" = "Opcoes_numero")) %>% 
  dplyr::select(V39a_label, everything(.), -V39a)

names(sec_1$slide_8_2022) <- c("V39a", label_quest)

#graficos y tablas solicitadas por sylvia nov-2023
heat_map_lst <- lapply(gr1_vars, function(vr){
  to_ret1 <- lapply(rodadas, function(rd){
    to_ret <- perceps[[rd]] %>% 
      dplyr::select(ID_node, !!vr) %>% 
      dplyr::mutate(across(everything(.), as.character))
    
    names(to_ret)[2] <- rd#paste0(names(to_ret)[2], "_", rd)
    to_ret[which(to_ret[,2] == "0"),] <- NA
    return(to_ret)
  } ) %>% 
    purrr::reduce(., full_join, by = "ID_node") %>% 
    tidyr::drop_na(ID_node) %>% 
    dplyr::left_join(., type_org[["rodada_2022"]] %>% 
                                               dplyr::select(ID_node, Nome), by = "ID_node")
 
   dm <-to_ret1[, grepl("^rodada" ,names(to_ret1))]
   dm <- apply(dm, 1, function(vec){
     sq <- c(10000, 1000, 100, 10)
     to_ret <- ifelse(is.na(vec), NA, sq)
     return(to_ret)
   }) %>% t
   to_ret1$order <- rowSums(dm, na.rm = T)
   
   to_ret2 <- to_ret1 %>% 
     dplyr::arrange(desc(order)) %>% 
     dplyr::select(Nome, ID_node, everything(.),  -order,)
  return(to_ret2)
}) 

names(heat_map_lst) <- gr1_vars

## hacer los graficos

  ### tablas para los graficos de radar
#### preguntas strongly agree


gr2_vars <- c("V33", "V34", "V35", "V30", "V35","V32", "V29", "V34")

label_quest <- lapply(gr2_vars, get_var_label,data = preg) %>% unlist

sec_2 <- lapply(list(slide_10 = c("1", "2"), slide_11 = c("1"), slide_12 = c("2")), function(filt){
  
  ret <-  lapply(gr2_vars, function(vrs){
    
    purrr::map2( perceps, names(perceps), .f = function(db, rd){
      
      db %>%
        dplyr::mutate(across(everything(.), as.character)) %>% 
        dplyr::left_join(.,type_org[[rd]] %>% 
                           dplyr::select(ID_node, V39a), by = c("ID_node")) %>% 
        dplyr::filter(V39a %in% filt) %>% 
        dplyr::select(ID_node, Opcoes_numero = all_of(vrs)) %>% 
        dplyr::filter(Opcoes_numero != "0") %>% 
        dplyr::mutate(Opcoes_numero = ifelse(Opcoes_numero == "4" | Opcoes_numero  == "5", "5", Opcoes_numero)) %>% 
        group_by(Opcoes_numero) %>% 
        tally() %>% 
        arrange(Opcoes_numero) %>% 
        dplyr::mutate(freq  = n/sum(n)) %>% 
        dplyr::select(freq) %>% 
        dplyr::slice(nrow(.))
      
    }) %>% 
      bind_rows() %>% 
      dplyr::rename(!!vrs := freq)
    
    
    
  }) %>% 
    bind_cols() %>% 
    dplyr::mutate(rodada = names(perceps)) %>% 
    dplyr::select(rodada, everything(.))
  
  names(ret)[2:length(names(ret))] <-  label_quest 
  
  return(ret)
})

sec_2$slide_13 <- lapply(gr2_vars, function(vrs){
  
  perceps[["rodada_2021"]] %>%
    dplyr::mutate(across(everything(.), as.character)) %>% 
    dplyr::left_join(.,type_org[["rodada_2021"]] %>% 
                       dplyr::select(ID_node, V39a), by = c("ID_node")) %>% 
    dplyr::filter(V39a %in% c("1", "2")) %>% 
    dplyr::select(ID_node, Opcoes_numero = all_of(vrs), V39a) %>% 
    dplyr::filter(Opcoes_numero != "0") %>% 
    dplyr::mutate(Opcoes_numero = ifelse(Opcoes_numero == "4" | Opcoes_numero  == "5", "5", Opcoes_numero)) %>% 
    group_by(V39a, Opcoes_numero) %>% 
    dplyr::summarise(count = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(V39a) %>% 
    dplyr::mutate(freq = count/sum(count)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(V39a) %>% 
    dplyr::filter(Opcoes_numero == "5") %>% 
    dplyr::select(freq)
  
  
}) %>% 
  dplyr::bind_cols() %>% 
  dplyr::mutate(V39a = c("1","2")) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas[["rodada_2021"]], var = "V39a", var_lab = "V39a_label"), 
                   by = c("V39a" = "Opcoes_numero")) %>% 
  dplyr::select(V39a_label, everything(.), -V39a)

names(sec_2$slide_13) <- c("V39a", label_quest)



sec_2$slide_13_2022 <- lapply(gr2_vars, function(vrs){
  
  perceps[["rodada_2022"]] %>%
    dplyr::mutate(across(everything(.), as.character)) %>% 
    dplyr::left_join(.,type_org[["rodada_2022"]] %>% 
                       dplyr::select(ID_node, V39a), by = c("ID_node")) %>% 
    dplyr::filter(V39a %in% c("1", "2")) %>% 
    dplyr::select(ID_node, Opcoes_numero = all_of(vrs), V39a) %>% 
    dplyr::filter(Opcoes_numero != "0") %>% 
    dplyr::mutate(Opcoes_numero = ifelse(Opcoes_numero == "4" | Opcoes_numero  == "5", "5", Opcoes_numero)) %>% 
    group_by(V39a, Opcoes_numero) %>% 
    dplyr::summarise(count = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(V39a) %>% 
    dplyr::mutate(freq = count/sum(count)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(V39a) %>% 
    dplyr::filter(Opcoes_numero == "5") %>% 
    dplyr::select(freq)
  
  
}) %>% 
  dplyr::bind_cols() %>% 
  dplyr::mutate(V39a = c("1","2")) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas[["rodada_2022"]], var = "V39a", var_lab = "V39a_label"), 
                   by = c("V39a" = "Opcoes_numero")) %>% 
  dplyr::select(V39a_label, everything(.), -V39a)

names(sec_2$slide_13_2022) <- c("V39a", label_quest)


writexl::write_xlsx(c(sec_1, sec_2), paste0("D:/OneDrive - CGIAR/Documents/graficos_spider_2022.xlsx"))

###########################
########### V20 - V20 a ##
#########################

out_path <- "D:/OneDrive - CGIAR/Documents/PPA 2021/questionaire_results/rodada_2022/"

perc_pag_14 <- list()

perc_pag_14$V20 <- lapply(names(perceps), function(l){
  perceps[[l]] %>% 
  dplyr::select(ID_node, matches("V20_[0-9]+")) %>% 
    tidyr::pivot_longer(., cols = -ID_node, names_to = "var", values_to = "selected") %>% 
    dplyr::mutate(rodada = l) %>% 
    dplyr::mutate(across(everything(.), as.character))
  
}) %>% 
  bind_rows() %>% 
  dplyr::group_by(rodada, var) %>% 
  dplyr::summarise(total = n(), 
                   n_sel = sum(as.logical(as.numeric(selected)))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = round(n_sel/total, 2),
                perc = ifelse(n_sel == 0, 0, perc)) %>% 
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2021"]], var = "V20", var_lab = "lab"),
                   by = c("var" = "Opcoes_numero")) %>% 
  tidyr::drop_na() %>% 
  dplyr::select(rodada, lab, perc) %>% 
  tidyr::pivot_wider(., names_from = rodada, values_from = perc) %>%
  dplyr::arrange(desc(rodada_2019)) 


perc_pag_14$V20a <- perceps[["rodada_2022"]] %>% 
  dplyr::select(ID_node, matches("V20a_[0-9]" )) %>%
  tidyr::drop_na() %>% 
  tidyr::pivot_longer(., cols = -ID_node, names_to = "var", values_to = "selected") %>% 
  dplyr::group_by(var) %>% 
  dplyr::summarise(total = n(), 
                   n_sel = sum(as.logical(as.numeric(selected)))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = round(n_sel/total, 2),
                perc = ifelse(n_sel == 0, 0, perc)) %>% 
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2022"]], var = "V20a", var_lab = "lab"),
                   by = c("var" = "Opcoes_numero")) %>% 
  tidyr::drop_na() %>% 
  dplyr::select(lab, perc) %>% 
  dplyr::arrange(desc(perc)) 


writexl::write_xlsx(perc_pag_14, path = paste0(out_path, "perc_pag_14.xlsx"))



###########################
########### V17  #########
#########################


perc_pag_15 <- list()

perc_pag_15$V17 <- lapply(names(perceps), function(l){
  
  perceps[[l]] %>% 
  dplyr::select(ID_node, var = matches("V17")) %>% 
  dplyr::group_by(var) %>% 
  tally() %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = round(n/sum(n), 6),
                perc = ifelse(n == 0, 0, perc),
                var  = as.character(var),
                rodada = l) %>% 
  dplyr::select(rodada, var, perc)

  
}) %>% 
  bind_rows() %>% 
  tidyr::pivot_wider(., names_from = rodada, values_from = perc) %>%
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2021"]], var = "V17", var_lab = "lab"),
                   by = c("var" = "Opcoes_numero")) %>% 
  dplyr::select(lab, everything(.), -var)

writexl::write_xlsx(perc_pag_15, path = paste0(out_path, "perc_pag_15.xlsx"))

###########################
#######V35 y V36 #########
#########################


perc_pag_16 <- list()

perc_pag_16$V35 <- lapply(names(perceps), function(l){
  
  perceps[[l]] %>% 
    dplyr::select(ID_node, var = "V35") %>% 
    dplyr::filter(var != 0) %>% 
    dplyr::group_by(var) %>% 
    tally() %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = round(n/sum(n), 6),
                  perc = ifelse(n == 0, 0, perc),
                  var  = as.character(var),
                  rodada = l) %>% 
    dplyr::select(rodada, var, perc)
  
  
}) %>% 
  bind_rows() %>% 
  tidyr::pivot_wider(., names_from = rodada, values_from = perc) %>%
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2021"]], var = "V35", var_lab = "lab"),
                   by = c("var" = "Opcoes_numero")) %>% 
  dplyr::select(lab, everything(.), -var) %>% 
  tidyr::drop_na(lab) %>% 
  dplyr::mutate(across(everything(.), function(i){ifelse(is.na(i), 0, i)}))



perc_pag_16$V36 <- lapply(names(perceps), function(l){
  
  perceps[[l]] %>% 
    dplyr::select(ID_node, var = "V36") %>% 
    dplyr::filter(var != 0) %>%
    dplyr::filter(var != 6) %>%
    dplyr::group_by(var) %>% 
    tally() %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = round(n/sum(n), 6),
                  perc = ifelse(n == 0, 0, perc),
                  var  = as.character(var),
                  rodada = l) %>% 
    dplyr::select(rodada, var, perc)
  
  
}) %>% 
  bind_rows() %>% 
  tidyr::pivot_wider(., names_from = rodada, values_from = perc) %>%
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2021"]], var = "V36", var_lab = "lab"),
                   by = c("var" = "Opcoes_numero")) %>% 
  dplyr::select(lab, everything(.), -var) %>% 
  tidyr::drop_na(lab)%>% 
  dplyr::mutate(across(everything(.), function(i){ifelse(is.na(i), 0, i)}))




writexl::write_xlsx(perc_pag_16, path = paste0(out_path, "perc_pag_16.xlsx"))




###########################
#######  V37a    #########
#########################

perc_pag_17 <- list()
perc_pag_17$V37a <- lapply(names(perceps), function(l){
  
  perceps[[l]] %>% 
    dplyr::select(ID_node, var = "V37a") %>% 
    dplyr::filter(!var %in% c(3)) %>% 
    dplyr::group_by(var) %>% 
    tally() %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(perc = round(n/sum(n), 2),
                  perc = ifelse(n == 0, 0, perc),
                  var  = as.character(var),
                  rodada = l) %>% 
    dplyr::select(rodada, var, perc)
  
})%>% 
  bind_rows() %>% 
  tidyr::pivot_wider(., names_from = rodada, values_from = perc) %>%
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2022"]], var = "V37a", var_lab = "lab"),
                   by = c("var" = "Opcoes_numero")) %>% 
  dplyr::select(lab, everything(.), -var) %>% 
  tidyr::drop_na(lab)


writexl::write_xlsx(perc_pag_17, path = paste0(out_path, "perc_pag_17.xlsx"))

#################################
##### V46, V47, V48, V49 #######
###############################

#tener cuidado con las organizaciones registradas (sylvia se le olvida actualizarlas)
type_org_new <- type_org[["rodada_2022"]] %>% 
  dplyr::select(ID_node, `Type of organization` )
# type_new = `Type of organization` %>% 
#   dplyr::full_join(type_org[["rodada_2020"]] %>% 
#                      dplyr::select(ID_node, type_old =  `Type of organization` ), by = "ID_node") %>% 
#   dplyr::mutate(type_final = ifelse(is.na(type_new), type_old, type_new) ) %>% 
#   dplyr::select(ID_node, `Type of organization` = type_final)


desemp <- raw_list_2022[grepl("Desempenho", names(raw_list_2022))] %>% 
  purrr::pluck(1) %>% 
  dplyr::filter(ID_node != "76") %>% 
  dplyr::mutate(id = paste0(ID_node, "_", ID_ego)) %>%  
  dplyr::filter(!id  %in% c("18_a", "18_c","76_c")) 

perc_v46_v49<- list()




perc_v46_v49$V46 <- desemp %>% 
  dplyr::select(ID_node ,var =  "V46") %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  drop_na(var) %>% 
  dplyr::group_by(`Type of organization`, var) %>% 
  dplyr::summarise(counts = n()) %>% 
  dplyr::mutate(total = sum(counts),
                perc = round(counts/total, 2),
                var = as.character(var)) %>% 
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2022"]], var = "V46", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization`, lab, perc ) %>% 
  tidyr::pivot_wider(., names_from = lab, values_from = perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))

  

perc_v46_v49$V46a <- desemp %>% 
  dplyr::select(ID_node , matches("V46a")) %>% 
  drop_na() %>% 
  tidyr::pivot_longer(cols = -ID_node, names_to = "var", values_to = "vals") %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  dplyr::group_by(`Type of organization` , var) %>% 
  dplyr::summarise(total = n(), counts = sum(as.logical(as.numeric(vals)))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = round(counts/total,2)) %>%
  dplyr::left_join(., get_questions_opts(data =  preguntas[["rodada_2022"]], var = "V46a", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization` , lab, counts, total, perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))

perc_v46_v49$V47 <- desemp %>% 
  dplyr::select(ID_node ,var =  "V47") %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  drop_na(var) %>% 
  dplyr::group_by(`Type of organization`, var) %>% 
  dplyr::summarise(counts = n()) %>% 
  dplyr::mutate(total = sum(counts),
                perc = round(counts/total, 2),
                var = as.character(var)) %>% 
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2022"]], var = "V47", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization`, lab, perc ) %>% 
  tidyr::pivot_wider(., names_from = lab, values_from = perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))


perc_v46_v49$V47a <-desemp %>% 
  dplyr::select(ID_node , matches("V47a")) %>% 
  drop_na() %>% 
  tidyr::pivot_longer(cols = -ID_node, names_to = "var", values_to = "vals") %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  dplyr::group_by(`Type of organization` , var) %>% 
  dplyr::summarise(total = n(), counts = sum(as.logical(as.numeric(vals)))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = round(counts/total,2)) %>%
  dplyr::left_join(., get_questions_opts(data =  preguntas[["rodada_2022"]], var = "V47a", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization` , lab, counts, total, perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))


perc_v46_v49$V48 <-desemp %>% 
  dplyr::select(ID_node ,var =  "V48") %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  drop_na(var) %>% 
  dplyr::group_by(`Type of organization`, var) %>% 
  dplyr::summarise(counts = n()) %>% 
  dplyr::mutate(total = sum(counts),
                perc = round(counts/total, 2),
                var = as.character(var)) %>% 
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2022"]], var = "V48", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization`, lab, perc ) %>% 
  tidyr::pivot_wider(., names_from = lab, values_from = perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))

perc_v46_v49$V48a <-desemp %>% 
  dplyr::select(ID_node , matches("V48a")) %>% 
  drop_na() %>% 
  tidyr::pivot_longer(cols = -ID_node, names_to = "var", values_to = "vals") %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  dplyr::group_by(`Type of organization` , var) %>% 
  dplyr::summarise(total = n(), counts = sum(as.logical(as.numeric(vals)))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = round(counts/total,2)) %>%
  dplyr::left_join(., get_questions_opts(data =  preguntas[["rodada_2022"]], var = "V48a", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization` , lab, counts, total, perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))

perc_v46_v49$V49 <- desemp %>% 
  dplyr::select(ID_node , matches("v49_")) %>% 
  drop_na() %>% 
  tidyr::pivot_longer(cols = -ID_node, names_to = "var", values_to = "vals") %>% 
  dplyr::mutate(var = toupper(var)) %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  dplyr::group_by(`Type of organization` , var) %>% 
  dplyr::summarise(total = n(), counts = sum(as.logical(as.numeric(vals)))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = round(counts/total,2)) %>%
  dplyr::left_join(., get_questions_opts(data =  preguntas[["rodada_2022"]], var = "V49", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization` , lab, counts, total, perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))

############################################################
####### V28b - V28c - V28d - V28e - V28f - V28g ###########
##########################################################

perc_v28b_v28g<- list()

gr1_vars <- paste0("V28", c("b", "c", "d", "e", "f", "g"))
lapply(gr1_vars, function(vr){
  print(vr)
  to_ret <- lapply(c("rodada_2021", "rodada_2022"), function(rd){
    df <- perceps[[rd]]
    df <- df %>% 
      dplyr::select(ID_node , starts_with(vr)) %>% 
      tidyr::pivot_longer(., -ID_node, names_to = "var", values_to = "val") %>% 
      tidyr::drop_na(val) %>% 
      dplyr::group_by(var, val) %>% 
      dplyr::summarize(count = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(var) %>% 
      dplyr::mutate(total = sum(count),
                    val = ifelse(val == 0, "No", "Yes"),
                    freq = (count/total)) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(.,
                       get_questions_opts(data = preg, var = vr, var_lab = "lab", lan = "en" ),
                       by = c("var" = "Opcoes_numero")) %>% 
      dplyr::select(lab, val,count) %>% 
      tidyr::pivot_wider(., names_from = "lab", values_from = "count") %>% 
      dplyr::mutate(rodada  = rd)
    
    return(df)
  }) %>% 
    bind_rows() %>% 
    dplyr::mutate(across(everything(.), .fns = tidyr::replace_na, replace = 0))
  
  return(to_ret)
})


###################################
###         V19a       ###########
#################################

### las start-ups son las mimas para todos los años dado los cambios sugeridos por carlos y JF en 2023
start_ups <- type_org[["rodada_2022"]] %>% 
  dplyr::select(ID_node, V39b, `Subtype of organization`) %>% 
  dplyr::filter(V39b == 5) %>% #dplyr::filter(V39b == 5) %>% 
  dplyr::filter(ID_node != "86") %>% 
  dplyr::filter(`Subtype of organization` == "Company - Startup") %>% #dplyr::filter(`Subtype of organization` == "Start-up company")
  dplyr::pull(ID_node)


## se eliminan los nodos para cada rodada segun lo establecido en los analisis de redes por carlos, Jf y silvya
v19a_list <- list(

"rodada_2019" = raw_list_2019[[grep("Objetivo_org", names(raw_list_2019))]] %>% 
  dplyr::mutate(id = paste0(ID , "_", ID_ego)) %>% 
  dplyr::filter(!id  %in% c("42_a","42_c","49_a","49_b","76_a")) %>% 
  dplyr::select(-id) %>% 
  dplyr::filter(!ID %in% start_ups) %>% 
  dplyr::rename(ID_node = ID),



"rodada_2020" = raw_list_2020[[grep("V19a_Obj_Motivação_org", names(raw_list_2020))]] %>% 
  dplyr::mutate(id = paste0(ID_node , "_", ID_ego )) %>%  
  dplyr::filter(!id  %in% c("18_b", "18_c", "42_b","42_c","43_b" ,
                            "49_b","49_c","76_c", "76_d","84_b", "84_c")) %>%
  dplyr::select(-id) %>% 
  dplyr::filter(!ID_node  %in% start_ups),


"rodada_2021" = raw_list_2021[[grep("V19a_Obj_Motivação_org", names(raw_list_2021))]] %>% 
  dplyr::mutate(id = paste0(ID_node , "_", ID_ego )) %>%  
  dplyr::filter(!id  %in% c("18_b", "18_c","76_c")) %>%
  dplyr::select(-id) %>% 
  dplyr::filter(!ID_node  %in% start_ups),


"rodada_2022" = raw_list_2022[[grep("V19a_Obj_Motivação_org", names(raw_list_2022))]] %>% 
  dplyr::mutate(id = paste0(ID_node , "_", ID_ego )) %>%  
  dplyr::filter(!id  %in% c("18_a", "18_c","76_c")) %>%
  dplyr::select(-id) %>% 
  dplyr::filter(!ID_node  %in% start_ups)

)

v19a_counts <- lapply(c("rodada_2019", "rodada_2020" ,"rodada_2021", "rodada_2022"), function(rd){
  
  df <- v19a_list[[rd]]
  vr <- "V19a"
  df <- df %>% 
    dplyr::select(ID_node , starts_with("V19a")) %>% 
    tidyr::pivot_longer(., -ID_node, names_to = "var", values_to = "val") %>% 
    tidyr::drop_na(val) %>% 
    dplyr::group_by(var, val) %>% 
    dplyr::summarize(count = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(var) %>% 
    dplyr::mutate(total = sum(count),
                  val = ifelse(val == 0, "No", "Yes"),
                  freq = (count/total)) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(.,
                     get_questions_opts(data = preg, var = vr, var_lab = "lab", lan = "en" ),
                     by = c("var" = "Opcoes_numero")) %>% 
    dplyr::select(lab, val,count) %>% 
    tidyr::pivot_wider(., names_from = "lab", values_from = "count") %>% 
    dplyr::mutate(rodada  = rd)
  
  return(df)
})

names(v19a_counts) <- c("rodada_2019", "rodada_2020" ,"rodada_2021", "rodada_2022")

writexl::write_xlsx(v19a_counts, path = paste0(out_path, "V19a_counts.xlsx"))

##########################
###### HEATMAP ##########
########################


counts_order <- lapply(c("rodada_2019", "rodada_2020" ,"rodada_2021", "rodada_2022"), function(rd){
  
  entrevistados[[rd]] %>% 
    dplyr::select(ID_node, Nome) %>% 
    dplyr::mutate(rodada = rd)
  
 
}) %>% 
  bind_rows() %>% 
  dplyr::group_by( Nome) %>% 
  dplyr::reframe(count = n(),
                 ID_node = unique(ID_node),
                 rodada  = paste0(rodada, collapse = "-")) %>% 
  dplyr::arrange(desc(count)) %>% 
  dplyr::select(ID_node, count)

x11()
to_plot <- lapply(c("rodada_2019", "rodada_2020" ,"rodada_2021", "rodada_2022"), function(rd){
  to_ret <- perceps[[rd]] %>% 
    dplyr::select(ID_node ,  V23) %>% 
    dplyr::mutate(rodada = rd) %>% 
    dplyr::mutate(across(everything(.), as.character))
  
  return(to_ret)
}) %>% 
  bind_rows() %>% 
  dplyr::left_join(., type_org_v2[[4]] %>% dplyr::select(ID_node, Nome), by = c("ID_node")) %>% 
  dplyr::left_join(., counts_order) %>% 
  dplyr::arrange(desc(count)) %>% 
  dplyr::mutate(rodada = str_extract(rodada, "[0-9]+"))
  
g1 <- to_plot %>% 
  dplyr::mutate(Nome = factor(Nome, levels = rev(unique(to_plot$Nome)))) %>% 
  ggplot(aes(x = rodada, y = Nome, fill = V23))+
  geom_tile(colour="white", size=0.25)+
  theme_bw(base_size=8)+
  scale_y_discrete(expand=c(0, 0))+
  scale_x_discrete(expand=c(0, 0),
                   breaks=c("2019", "2020", "2021", "2022"))+
  scale_fill_brewer(palette = "YlOrRd" )+
  #theme options
  theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.6),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank()
  )+
  coord_fixed(ratio = 0.5)

x11();g1
ggsave( "C:/Users/acmendez/Downloads/prueba_heatmap.png", g1, dpi = 300, width = 12, height = 8, units = "in")








### no se uso en la rodada 2022
if(FALSE){
perc_v46_v49$V49a <- desemp %>% 
  dplyr::select(ID_node ,var =  "V49a") %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  drop_na(var) %>% 
  dplyr::group_by(`Type of organization`, var) %>% 
  dplyr::summarise(counts = n()) %>%  
  dplyr::mutate(total = sum(counts),
                perc = round(counts/total, 2),
                var = as.character(var)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2022"]], var = "V49a", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization`, lab, perc ) %>% 
  tidyr::pivot_wider(., names_from = lab, values_from = perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))
}
####

perc_v46_v49$V49b <- desemp %>% 
  dplyr::select(ID_node ,var =  "V49b") %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  drop_na(var) %>% 
  dplyr::group_by(`Type of organization`, var) %>% 
  dplyr::summarise(counts = n()) %>%  
  dplyr::mutate(total = sum(counts),
                perc = round(counts/total, 2),
                var = as.character(var)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2022"]], var = "V49b", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization`, lab, perc ) %>% 
  tidyr::pivot_wider(., names_from = lab, values_from = perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))


perc_v46_v49$V49c <- desemp %>% 
  dplyr::select(ID_node ,var =  "V49c") %>% 
  dplyr::left_join(., type_org_new %>% dplyr::select(ID_node, `Type of organization` ), by = "ID_node") %>% 
  drop_na(var) %>% 
  dplyr::group_by(`Type of organization`, var) %>% 
  dplyr::summarise(counts = n()) %>%  
  dplyr::mutate(total = sum(counts),
                perc = round(counts/total, 2),
                var = as.character(var)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(., get_questions_opts(preguntas[["rodada_2022"]], var = "V49c", var_lab= "lab"),  by =  c("var"  = "Opcoes_numero")) %>% 
  dplyr::select(`Type of organization`, lab, perc ) %>% 
  tidyr::pivot_wider(., names_from = lab, values_from = perc)%>% 
  dplyr::mutate(across(where(is.numeric), function(i){ifelse(is.na(i), 0, i)}))


writexl::write_xlsx(perc_v46_v49, path = paste0(out_path, "perc_v46_v49.xlsx"))

