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



##### tablas para los graficos de radar
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

