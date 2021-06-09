### script para procesar la encuesta a los miembros de la PPA en el ano 2020
### @author: Andres Camilo Mendez
### @date: MAyo - 2021


suppressWarnings(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(tidyverse, readr, readxl, magrittr, stringr, stringi, lubridate, writexl)

### funciones necesarias para organizar algunas cosas

get_questions_opts <- function(data, var, var_lab){
  
  var_lab <- sym(var_lab)
  
  ret <- data %>% 
    dplyr::filter(Variavel == var)
  
  if(grepl("múltipla escolha", ret$Metrica)){
    
    ret <- ret %>%
      dplyr::pull(vars_opts) %>% 
      purrr::pluck(1) %>% 
      dplyr::filter(if_any(contains("Used_opcoes"), ~ .)) %>% 
      dplyr::rename(!!var_lab := `Labels in English`) %>% 
      dplyr::select("Opcoes_numero" = Variavel , !!var_lab)
    
  }else{
    
    ret <- ret %>%
      dplyr::pull(vars_opts) %>% 
      purrr::pluck(1) %>% 
      dplyr::filter(if_any(contains("Used_opcoes"), ~ .)) %>% 
      dplyr::rename(!!var_lab := `Labels in English`) %>% 
      dplyr::select(Opcoes_numero, !!var_lab)
  }
  
  
  
  
  
  return(ret)
}

get_var_label <- function(data, var){
  ret <- data %>% 
    dplyr::filter(Variavel == var) %>%
    dplyr::pull(`Labels in English`)
  
  return(ret)
}
## Cargar database

dir_pth <- "D:/OneDrive - CGIAR/Documents/PPA 2021/Database SNA PPA 2020 A_B (20210505)_para CIAT.xlsx"

## hoja del archivo de excel
sheets <- readxl::excel_sheets(dir_pth)

rodada <- "2019"
prefix <- "opcoes" # or startups

### opciones de respuesta

opts <- readxl::read_xlsx(dir_pth, sheet = "Todos_detalhado")

opts <- opts %>% 
  dplyr::filter(Variavel != "ID_node") %>% 
  dplyr::mutate(var_id = 1:nrow(.))

vars_names <- opts %>% 
  dplyr::select(Variavel, Metrica,Variavel_nome, var_id, `Labels in English`) %>% 
  filter(complete.cases(Metrica)) %>% 
  dplyr::mutate(root_var = ifelse(grepl("xxxx", Variavel), str_replace(Variavel, "xxxx", "2020" ), Variavel))


preguntas <- vars_names %>% 
  dplyr::mutate(vars_opts = purrr::map2(.x = root_var, .y = Metrica, .f = function(.x, .y){
    i <- .x
    cat("processing:", i, "\n")
    if(grepl(pattern ='m?ltipla escolha', .y)){
      i <- paste0(i, "_")
      
      ret <- opts %>% 
        filter(grepl(i, Variavel)) %>% 
        select(Variavel, Variavel_nome, Opcoes_numero, Opcoes_nome, ends_with(paste0(prefix, "_", rodada)), `Labels in English`, `Labels in English simp`) %>% 
        filter(complete.cases(Opcoes_numero))
      
      if(any(table(stringr::str_extract(ret$Variavel, "[0-9]+$")) > 1 )| any(table(ret$Opcoes_nome)>1) ){
        stop("Numero de opciones de respuesta mal codificado")
      }
      
    }else{
      ret <- opts %>% 
        filter(Variavel == i) %>% 
        select(Variavel, Variavel_nome, Opcoes_numero, Opcoes_nome, ends_with(paste0(prefix, "_", rodada)), `Labels in English`, `Labels in English simp`) %>% 
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


### miembros de la PPA

miembros_sheet <- readxl::read_xlsx(dir_pth, sheet = "Orgs_grupo_rodadas")

groups <- miembros_sheet %>%
  dplyr::select(ID_node, Nome, matches(rodada)) %>%
  dplyr::mutate_all(as.character) %>% 
  dplyr::rename_at(vars(matches(rodada)), function(i){ gsub("_[0-9]+", "", i)})

# validaci?n de los ID's

stopifnot("ID mal codificado" = !table(groups$ID_node)>1)

#asignar colores dependiendo del grupo



gr_colors <- groups %>% 
  dplyr::select(V1) %>% 
  dplyr::group_by(V1) %>% 
  tally() %>% 
  dplyr::mutate(color = case_when(
    V1 == "A2" ~ "dodgerblue2",
    V1 == "A" ~ "olivedrab3",
    V1 == "B" ~ "gold",
    TRUE ~ "gray50"
  ))



entrevistados <- groups %>% 
  dplyr::left_join(., preguntas %>% 
                     dplyr::filter(Variavel == "V1_Rodada_xxxx") %>% 
                     dplyr::pull(vars_opts) %>% 
                     purrr::pluck(1) %>% 
                     dplyr::select( Opcoes_numero, Opcoes_nome ,  `Labels in English`, `Labels in English simp`)
                   , by = c( "V1_Rodada" = "Opcoes_numero")) %>% 
  dplyr::mutate(status = `Labels in English simp`) %>% 
  dplyr::filter(grepl('^Interviewed', status))

cat("Total de entrevistados en", rodada, "es de: ", nrow(entrevistados), "\n")
total_entrevistas <- nrow(entrevistados)


##### cargar todas las pestanas del archivo

file_pth <- "D:/OneDrive - CGIAR/Documents/Database SNA PPA 2019 final 2020 06 06.xlsx"
sheets_linea_base <- excel_sheets(file_pth)


raw_list <- lapply(sheets_linea_base[-c(1, 2, 3, 4)], function(i){
  readxl::read_xlsx(file_pth, sheet = i)
})

names(raw_list) <- sheets_linea_base[-c(1, 2, 3, 4)]

## definir una lista vacia para almacenar todos los resultados

results_lst <- list()

# extraer el tipo de organizaciones

type_org <-  raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>% 
  dplyr::select(ID, V39a, V39b) %>% 
  dplyr::mutate_all(as.character) %>% 
  dplyr::left_join(., groups, by = c("ID" = "ID_node")) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas,
                                         var = "V39a",
                                         var_lab = get_var_label(data = preguntas, var = 'V39a')),
                   by = c("V39a" = "Opcoes_numero")) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas,
                                         var = "V39b",
                                         var_lab = get_var_label(data = preguntas, var = 'V39b')),
                   by = c("V39b" = "Opcoes_numero")) 

stopifnot("Se encontraron NA en type_org" = all(complete.cases(type_org) ))

#### Question V39a - Tipo de empresa ou organização (Type of organization)

#lista para guardar todos los resultados de la seccion de atributosde las empresas
results_lst$Empresas_orgs_atributos <- list()

#tablas de frecuencia por tipo de organizacion
results_lst$Empresas_orgs_atributos$V39a <- type_org %>% 
  dplyr::group_by(`Type of organization`) %>% 
  dplyr::tally() %>%
  dplyr::mutate(freq = round(n/sum(n), 3)*100, Frequency = paste0(freq, "%"))%>% 
  dplyr::arrange(desc(freq)) %>% 
  dplyr::rename("Count" =  n) %>% 
  ungroup()


results_lst$Empresas_orgs_atributos$V39b <- type_org %>% 
  dplyr::group_by(`Subtype of organization`) %>% 
  dplyr::tally() %>%
  dplyr::mutate(freq = round(n/sum(n), 3)*100, Frequency = paste0(freq, "%"))%>% 
  dplyr::arrange(desc(freq)) %>% 
  dplyr::rename("Count" =  n)%>% 
  ungroup()

#tipo de organizacion por grupo

results_lst$Empresas_orgs_atributos$V39a_group <-type_org %>% 
  dplyr::group_by(  V1, `Type of organization`) %>% 
  dplyr::tally() %>%
  dplyr::mutate(freq = round(n/sum(n), 3)*100, Frequency = paste0(freq, "%"))%>% 
  dplyr::rename("Group" = V1, "Count" =  n)%>% 
  ungroup()

results_lst$Empresas_orgs_atributos$V39b_group <- type_org %>% 
  dplyr::group_by(  V1, `Subtype of organization`) %>% 
  dplyr::tally() %>%
  dplyr::mutate(freq = round(n/sum(n), 3)*100, Frequency = paste0(freq, "%"))%>% 
  dplyr::rename("Group" = V1, "Count" =  n)%>% 
  ungroup()

#tipo de organizacion por entrevistados

results_lst$Empresas_orgs_atributos$V39a_group_entrev <- type_org %>%
  dplyr::left_join(., entrevistados %>% 
              dplyr::select(ID_node, status), by = c("ID" = "ID_node")) %>% 
  dplyr::filter(!is.na(status)) %>%
  dplyr::group_by(V1, `Type of organization`) %>% 
  dplyr::tally() %>%
  dplyr::mutate(freq = round(n/sum(n), 3)*100, Frequency = paste0(freq, "%")) %>% 
  dplyr::rename("Group" = V1, "Count" = n)%>% 
  ungroup()

stopifnot("Conteo de entrevistados no coincide con el total" = sum(results_lst$Empresas_orgs_atributos$V39a_group_entrev$Count) == total_entrevistas)

results_lst$Empresas_orgs_atributos$V39b_group_entrev <- type_org %>%
  dplyr::left_join(., entrevistados %>% 
              dplyr::select(ID_node, status), by = c("ID" = "ID_node")) %>% 
  dplyr::filter(!is.na(status)) %>%
  dplyr::group_by(V1, `Subtype of organization`) %>% 
  dplyr::tally() %>%
  dplyr::mutate(freq = round(n/sum(n), 3)*100, Frequency = paste0(freq, "%")) %>% 
  dplyr::rename("Group" = V1, "Count" = n)%>% 
  ungroup()

stopifnot("Conteo de entrevistados no coincide con el total" = sum(results_lst$Empresas_orgs_atributos$V39b_group_entrev$Count) == total_entrevistas)

# grafico de spyder 

results_lst$Empresas_orgs_atributos$V39b_group %>% 
  pivot_wider(id_cols = Group, names_from = `Subtype of organization`, values_from = freq) %>% 
  dplyr::mutate_if(is.numeric, .funs = function(i)ifelse(is.na(i), 0, i/100)) %>%
  ggradar::ggradar(., group.colours  = gr_colors$color[-c(1,5)], group.line.width = 1,  group.point.size = 3)

#### pregunta V40a

var_label <- "V40a"

var_opts <- get_questions_opts(data = preguntas, 
                               var = var_label, 
                               var_lab = "label") %>% 
  dplyr::pull(Opcoes_numero)

#validacion de las opciones pregunta

stopifnot("Opciones de respuesta duplicadas" =  !any(gsub("[a-zA-Z0-9]+_","", var_opts) %>% table > 1)  ) 
stopifnot("Falta alguna opcion de respuesta" = all(abs(diff(  gsub("[a-zA-Z0-9]+_","", var_opts) %>% as.numeric  )) == 1))

tots <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%
  dplyr::select(ID, Nome, V1, !!var_opts) %>% 
  tidyr::drop_na() %>% 
  group_by(V1) %>% 
  tally()

results_lst$Empresas_orgs_atributos$V40a <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%
  dplyr::select(ID, Nome, V1, !!var_opts) %>% 
  tidyr::drop_na() %>% 
  pivot_longer(cols = !!var_opts) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas, var = "V40a", var_lab = "Label"), by= c("name" = "Opcoes_numero" )) %>% 
  dplyr::group_by( V1, Label) %>% 
  dplyr::summarise( Count = sum(as.numeric(value))) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(., tots , by ="V1") %>% 
  dplyr::mutate(freq = round(Count/n,3)*100, Frequency = paste0(freq, "%")) %>% 
  dplyr::rename("Group" = V1, !!(get_var_label(data = preguntas, var = "V40a")) := Label, "Total"= n)
 
#### pregunta v40b


var_label <- "V40b"

results_lst$Empresas_orgs_atributos$V40b <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%
  dplyr::select(ID, Nome, V1, !!var_label) %>%
  dplyr::mutate(across(everything(), as.character)) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas, 
                                         var = var_label, 
                                         var_lab = "label"), by = c("V40b" = "Opcoes_numero")) %>%
  dplyr::group_by(V1) %>% 
  dplyr::mutate(total = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(V1, label) %>% 
  dplyr::summarise(Count = n(), total = unique(total),freq = round(Count/total,3)*100, Frequency = paste0(freq, "%")) %>% 
  dplyr::rename(Group = V1, !!get_var_label(data = preguntas, var = var_label) := label)

#### pregunta v41a

var_label <- "V41a"

var_opts <- get_questions_opts(data = preguntas, 
                   var = var_label, 
                   var_lab = "label") %>% 
  dplyr::pull(Opcoes_numero)

#validacion de las opciones pregunta

stopifnot("Opciones de respuesta duplicadas" =  !any(gsub("[a-zA-Z0-9]+_","", var_opts) %>% table > 1)  ) 
stopifnot("Falta alguna opcion de respuesta" = all(abs(diff(  gsub("[a-zA-Z0-9]+_","", var_opts) %>% as.numeric  )) == 1))

df_trial <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%
  dplyr::select(ID, Nome, V1, !!var_opts) %>% 
  tidyr::drop_na() %>%
  tidyr::pivot_longer(cols = -(ID:V1), names_to= "nms") %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas, 
                                         var = var_label, 
                                         var_lab = "label"), by = c("nms" = "Opcoes_numero" ))
  

non_response <- df_trial %>% 
  dplyr::filter(nms == "V41a_0", value  != 0) %>%
  nrow()
non_res_frq <- round(non_response/length(unique(df_trial$Nome))*100)


results_lst$Empresas_orgs_atributos$V41a <- df_trial %>% 
  dplyr::mutate(value = as.numeric(value)) %>% 
  dplyr::filter(value != 0 ) %>% 
  dplyr::filter(nms != "V41a_0") %>%
  dplyr::group_by(V1, label) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  dplyr::left_join(., gr_colors %>% select(V1, n), by = "V1") %>% 
  dplyr::mutate(freq = paste0(round(count/n*100,2), "%") ) %>% 
  pivot_wider(id_cols = label, names_from = V1, values_from = c(count, freq)) %>% 
  dplyr::mutate(across(everything(), .fns = function(i){ifelse(is.na(i),0, i)})) %>% 
  dplyr::mutate(total =  rowSums(select(., starts_with("count")))) %>% 
  arrange(desc(total)) %>% 
  add_row(label = "No information count",  total = non_response ) %>%
  dplyr::rename(!!get_var_label(data = preguntas, var = var_label) := label)


#### pregunta v41a solo con los entrevistados


var_label <- "V41a"

var_opts <- get_questions_opts(data = preguntas, 
                               var = var_label, 
                               var_lab = "label") %>% 
  dplyr::pull(Opcoes_numero)

#validacion de las opciones pregunta

stopifnot("Opciones de respuesta duplicadas" =  !any(gsub("[a-zA-Z0-9]+_","", var_opts) %>% table > 1)  ) 
stopifnot("Falta alguna opcion de respuesta" = all(abs(diff(  gsub("[a-zA-Z0-9]+_","", var_opts) %>% as.numeric  )) == 1))

df_trial <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>% 
  dplyr::select(ID, Nome, V1, !!var_opts) %>% 
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::left_join(., entrevistados %>% dplyr::select(ID_node, status), by = c("ID" = "ID_node")) %>% 
  dplyr::filter(!is.na(status)) %>%
  dplyr::select(-status) 

stopifnot("Numero de empresas no coincide con el total de entrevistados" = nrow(df_trial) == total_entrevistas)

non_response <- df_trial %>% 
  dplyr::filter(!!sym(var_label)  != 0) %>%
  nrow()
non_res_frq <- round(non_response/length(unique(df_trial$Nome))*100)


results_lst$Empresas_orgs_atributos$V41a_entrev <- df_trial %>% 
  tidyr::pivot_longer(cols = -(ID:V1), names_to= "nms") %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas, 
                                         var = var_label, 
                                         var_lab = "label"), by = c("nms" = "Opcoes_numero" )) %>% 
  dplyr::mutate(value = as.numeric(value)) %>% 
  dplyr::filter(value != 0 ) %>% 
  dplyr::filter(nms != "V41a_0") %>%
  dplyr::group_by(V1, label) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  dplyr::left_join(., gr_colors %>% select(V1, n), by = "V1") %>% 
  dplyr::mutate(freq = paste0(round(count/n*100,2), "%") ) %>% 
  pivot_wider(id_cols = label, names_from = V1, values_from = c(count, freq)) %>% 
  dplyr::mutate(across(everything(), .fns = function(i){ifelse(is.na(i),0, i)})) %>% 
  dplyr::mutate(total =  rowSums(select(., starts_with("count")))) %>% 
  arrange(desc(total)) %>% 
  add_row(label = "No information count",  total = non_response ) %>%
  dplyr::rename(!!get_var_label(data = preguntas, var = var_label) := label)


##### pregunta V41b

var_label <- "V41b"

var_opts <- get_questions_opts(data = preguntas, 
                               var = var_label, 
                               var_lab = "label") %>% 
  dplyr::pull(Opcoes_numero)

#validacion de las opciones pregunta

stopifnot("Opciones de respuesta duplicadas" =  !any(gsub("[a-zA-Z0-9]+_","", var_opts) %>% table > 1)  ) 
stopifnot("Falta alguna opcion de respuesta" = all(abs(diff(  gsub("[a-zA-Z0-9]+_","", var_opts) %>% as.numeric  )) == 1))

df_trial <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%  
  dplyr::select(ID, Nome, V1, !!var_label) %>% 
  dplyr::mutate(across(everything(), as.character)) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas,
                                         var  = var_label,
                                         var_lab = "label"), by = c("V41b" = "Opcoes_numero")) %>% 
  tidyr::drop_na()



non_response <- df_trial %>% 
  dplyr::filter(!!sym(var_label) == 0) %>% 
  nrow()
non_res_frq <- round(non_response/nrow(df_trial)*100)

results_lst$Empresas_orgs_atributos$V41b <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != 0) %>% 
  dplyr::group_by(label) %>% 
  dplyr::tally() %>%  
  dplyr::mutate(freq = round(n/sum(n),3)*100, Frequency = paste0(freq, "%")) %>%  
  ungroup() %>% 
  add_row( label = "No information rate",  n = non_response,  freq = non_res_frq ,Frequency = paste0(non_res_frq,"%")) %>% 
  dplyr::select( !!get_var_label(data = preguntas, var = var_label) := label, Count = n, Percentage = Frequency) %>% 
  dplyr::arrange(desc(Count))



results_lst$Empresas_orgs_atributos$V41b_by_group <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != 0) %>% 
  dplyr::group_by(V1) %>% 
  dplyr::mutate(tot = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(V1, label) %>% 
  dplyr::summarise(Count = n(), tot = unique(tot),freq = round(Count/tot,3)*100, Frequency = paste0(freq, "%")) %>%  
  ungroup() %>% 
  add_row(V1 = "No information", label = "No information rate",  Count = non_response, tot = Count, freq = non_res_frq ,Frequency = paste0(non_res_frq,"%")) %>% 
  dplyr::select(Group = V1, !!get_var_label(data = preguntas, var = var_label) := label, Count, Percentage = Frequency)

##### pregunta V41b entrevistados

var_label <- "V41b"

var_opts <- get_questions_opts(data = preguntas, 
                               var = var_label, 
                               var_lab = "label") %>% 
  dplyr::pull(Opcoes_numero)

#validacion de las opciones pregunta

stopifnot("Opciones de respuesta duplicadas" =  !any(gsub("[a-zA-Z0-9]+_","", var_opts) %>% table > 1)  ) 
stopifnot("Falta alguna opcion de respuesta" = all(abs(diff(  gsub("[a-zA-Z0-9]+_","", var_opts) %>% as.numeric  )) == 1))

df_trial <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%  
  dplyr::select(ID, Nome, V1, !!var_label) %>% 
  dplyr::mutate(across(everything(), as.character)) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas,
                                         var  = var_label,
                                         var_lab = "label"), by = c("V41b" = "Opcoes_numero")) %>%
  dplyr::left_join(., entrevistados %>% dplyr::select(ID_node, status), by = c("ID" = "ID_node")) %>%
  tidyr::drop_na()

stopifnot("Numero de empresas no coincide con el total de entrevistados" = nrow(df_trial) == total_entrevistas)


non_response <- df_trial %>% 
  dplyr::filter(!!sym(var_label) == 0) %>% 
  nrow()
non_res_frq <- round(non_response/nrow(df_trial)*100)

results_lst$Empresas_orgs_atributos$V41b_entrev <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != 0) %>% 
  dplyr::group_by(label) %>% 
  dplyr::tally() %>%  
  dplyr::mutate(freq = round(n/sum(n),3)*100, Frequency = paste0(freq, "%")) %>%  
  ungroup() %>% 
  add_row( label = "No information rate",  n = non_response,  freq = non_res_frq ,Frequency = paste0(non_res_frq,"%")) %>% 
  dplyr::select( !!get_var_label(data = preguntas, var = var_label) := label, Count = n, Percentage = Frequency) %>% 
  dplyr::arrange(desc(Count))



results_lst$Empresas_orgs_atributos$V41b_by_group_entrev <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != 0) %>% 
  dplyr::group_by(V1) %>% 
  dplyr::mutate(tot = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(V1, label) %>% 
  dplyr::summarise(Count = n(), tot = unique(tot),freq = round(Count/tot,3)*100, Frequency = paste0(freq, "%")) %>%  
  ungroup() %>% 
  add_row(V1 = "No information", label = "No information rate",  Count = non_response, tot = Count, freq = non_res_frq ,Frequency = paste0(non_res_frq,"%")) %>% 
  dplyr::select(Group = V1, !!get_var_label(data = preguntas, var = var_label) := label, Count, Percentage = Frequency)

#### pregunta V42a


var_label <- "V42a"

var_opts <- get_questions_opts(data = preguntas, 
                               var = var_label, 
                               var_lab = "label") %>% 
  dplyr::pull(Opcoes_numero)

#validacion de las opciones pregunta

stopifnot("Opciones de respuesta duplicadas" =  !any(gsub("[a-zA-Z0-9]+_","", var_opts) %>% table > 1)  ) 
stopifnot("Falta alguna opcion de respuesta" = all(abs(diff(  gsub("[a-zA-Z0-9]+_","", var_opts) %>% as.numeric  )) == 1))


df_trial <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%  
  dplyr::select(ID, Nome, V1, !!var_label) %>% 
  dplyr::mutate(across(everything(), as.character)) %>% 
  tidyr::drop_na() %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas, 
                                         var = var_label, 
                                         var_lab = "label"), by = c("V42a" ="Opcoes_numero"))



non_response <- df_trial %>% 
  dplyr::filter(!!sym(var_label) == "0") %>%
  nrow()
non_res_frq <- round(non_response/nrow(df_trial)*100)

results_lst$Empresas_orgs_atributos$V42a <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != 0) %>% 
  dplyr::group_by(label) %>% 
  dplyr::tally() %>%  
  dplyr::mutate(freq = round(n/sum(n),3)*100, Frequency = paste0(freq, "%")) %>%  
  ungroup() %>% 
  add_row( label = "No information rate",  n = non_response,  freq = non_res_frq ,Frequency = paste0(non_res_frq,"%")) %>% 
  dplyr::select( !!get_var_label(data = preguntas, var = var_label) := label, Count = n, Percentage = Frequency) %>% 
  dplyr::arrange(desc(Count))

results_lst$Empresas_orgs_atributos$V42a_by_group <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != "0") %>%
  dplyr::group_by(V1) %>% 
  dplyr::mutate(tot = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(V1, label) %>% 
  dplyr::summarise(Count = n(), tot = unique(tot),freq = round(Count/tot,3)*100, Frequency = paste0(freq, "%")) %>%
  ungroup() %>% 
  add_row(V1 = "No information", label = "No information Rate",  Count = non_response, tot = Count, freq = non_res_frq,Frequency = paste0(non_res_frq,"%")) %>% 
  dplyr::select(Group = V1, !!get_var_label(data = preguntas, var = var_label) := label, Count, Percentage = Frequency) 


#### pregunta V42a entrevistados

var_label <- "V42a"

var_opts <- get_questions_opts(data = preguntas, 
                               var = var_label, 
                               var_lab = "label") %>% 
  dplyr::pull(Opcoes_numero)

#validacion de las opciones pregunta

stopifnot("Opciones de respuesta duplicadas" =  !any(gsub("[a-zA-Z0-9]+_","", var_opts) %>% table > 1)  ) 
stopifnot("Falta alguna opcion de respuesta" = all(abs(diff(  gsub("[a-zA-Z0-9]+_","", var_opts) %>% as.numeric  )) == 1))


df_trial <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%  
  dplyr::select(ID, Nome, V1, !!var_label) %>% 
  dplyr::mutate(across(everything(), as.character)) %>% 
  tidyr::drop_na() %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas, 
                                         var = var_label, 
                                         var_lab = "label"), by = c("V42a" ="Opcoes_numero")) %>% 
  dplyr::left_join(., entrevistados %>% dplyr::select(ID_node, status), by = c("ID" = "ID_node")) %>% 
  dplyr::filter(!is.na(status)) %>% 
  dplyr::select(-status)

stopifnot("Numero de empresas no coincide con el total de entrevistados" = nrow(df_trial) == total_entrevistas)


non_response <- df_trial %>% 
  dplyr::filter(!!sym(var_label) == "0") %>%
  nrow()
non_res_frq <- round(non_response/nrow(df_trial)*100)

results_lst$Empresas_orgs_atributos$V42a_entrev <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != 0) %>% 
  dplyr::group_by(label) %>% 
  dplyr::tally() %>%  
  dplyr::mutate(freq = round(n/sum(n),3)*100, Frequency = paste0(freq, "%")) %>%  
  ungroup() %>% 
  add_row( label = "No information rate",  n = non_response,  freq = non_res_frq ,Frequency = paste0(non_res_frq,"%")) %>% 
  dplyr::select( !!get_var_label(data = preguntas, var = var_label) := label, Count = n, Percentage = Frequency) %>% 
  dplyr::arrange(desc(Count))

results_lst$Empresas_orgs_atributos$V42a_by_group_entrev <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != "0") %>%
  dplyr::group_by(V1) %>% 
  dplyr::mutate(tot = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(V1, label) %>% 
  dplyr::summarise(Count = n(), tot = unique(tot),freq = round(Count/tot,3)*100, Frequency = paste0(freq, "%")) %>%
  ungroup() %>% 
  add_row(V1 = "No information", label = "No information Rate",  Count = non_response, tot = Count, freq = non_res_frq,Frequency = paste0(non_res_frq,"%")) %>% 
  dplyr::select(Group = V1, !!get_var_label(data = preguntas, var = var_label) := label, Count, Percentage = Frequency) 


#### pregunta v42b


var_label <- "V42b"

var_opts <- get_questions_opts(data = preguntas, 
                               var = var_label, 
                               var_lab = "label") %>% 
  dplyr::pull(Opcoes_numero)

#validacion de las opciones pregunta

stopifnot("Opciones de respuesta duplicadas" =  !any(gsub("[a-zA-Z0-9]+_","", var_opts) %>% table > 1)  ) 
stopifnot("Falta alguna opcion de respuesta" = all(abs(diff(  gsub("[a-zA-Z0-9]+_","", var_opts) %>% as.numeric  )) == 1))


df_trial <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%  
  dplyr::select(ID, Nome, V1, !!var_label) %>% 
  dplyr::mutate(across(everything(), as.character))  %>% 
  tidyr::drop_na() %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas, 
                                         var = var_label , 
                                         var_lab = "label"), by =  c("V42b" = "Opcoes_numero"))


non_response <- df_trial %>% 
  dplyr::filter(!!sym(var_label) == "0") %>%
  nrow()
non_res_frq <- round(non_response/nrow(df_trial)*100)

results_lst$Empresas_orgs_atributos$V42b <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != "0") %>% 
  dplyr::group_by(label) %>% 
  dplyr::tally(sort = TRUE) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( freq = round(n/sum(n),3)*100, Frequency = paste0(freq, "%")) %>% 
  dplyr::select( !!get_var_label(data = preguntas, var = var_label) := label, Count = n, Frequency) %>% 
  add_row( !!get_var_label(data = preguntas, var = var_label) := "No information Rate",  Count = non_response,  Frequency = paste0(non_res_frq,"%") ) 



results_lst$Empresas_orgs_atributos$V42b_by_group <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != "0") %>% 
  dplyr::group_by(V1) %>% 
  dplyr::mutate(tot = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(V1, label) %>% 
  dplyr::summarise(Count = n(), tot = unique(tot),freq = round(Count/tot,3)*100, Frequency = paste0(freq, "%")) %>% 
  ungroup() %>% 
  dplyr::select(Groups = V1, "Year of foundation" = label, Count, Frequency) %>% 
  add_row(Groups = "No information",    "Year of foundation" = "No information Rate",  Count = non_response,  Frequency = paste0(non_res_frq,"%") ) 


#### pregunta v42b por entrevistados

var_label <- "V42b"

var_opts <- get_questions_opts(data = preguntas, 
                               var = var_label, 
                               var_lab = "label") %>% 
  dplyr::pull(Opcoes_numero)

#validacion de las opciones pregunta

stopifnot("Opciones de respuesta duplicadas" =  !any(gsub("[a-zA-Z0-9]+_","", var_opts) %>% table > 1)  ) 
stopifnot("Falta alguna opcion de respuesta" = all(abs(diff(  gsub("[a-zA-Z0-9]+_","", var_opts) %>% as.numeric  )) == 1))


df_trial <- raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>%  
  dplyr::select(ID, Nome, V1, !!var_label) %>% 
  dplyr::mutate(across(everything(), as.character))  %>% 
  tidyr::drop_na() %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas, 
                                         var = var_label , 
                                         var_lab = "label"), by =  c("V42b" = "Opcoes_numero")) %>% 
  dplyr::left_join(., entrevistados %>% dplyr::select(ID_node, status), by = c("ID" = "ID_node")) %>% 
  dplyr::filter(!is.na(status)) %>% 
  dplyr::select(-status)

stopifnot("Numero de empresas no coincide con el total de entrevistados" = nrow(df_trial) == total_entrevistas)


non_response <- df_trial %>% 
  dplyr::filter(!!sym(var_label) == "0") %>%
  nrow()
non_res_frq <- round(non_response/nrow(df_trial)*100)

results_lst$Empresas_orgs_atributos$V42b_entrev <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != "0") %>% 
  dplyr::group_by(label) %>% 
  dplyr::tally(sort = TRUE) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( freq = round(n/sum(n),3)*100, Frequency = paste0(freq, "%")) %>% 
  dplyr::select( !!get_var_label(data = preguntas, var = var_label) := label, Count = n, Frequency) %>% 
  add_row( !!get_var_label(data = preguntas, var = var_label) := "No information Rate",  Count = non_response,  Frequency = paste0(non_res_frq,"%") ) 



results_lst$Empresas_orgs_atributos$V42b_by_group_entrev <- df_trial %>% 
  dplyr::filter(!!sym(var_label) != "0") %>% 
  dplyr::group_by(V1) %>% 
  dplyr::mutate(tot = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(V1, label) %>% 
  dplyr::summarise(Count = n(), tot = unique(tot),freq = round(Count/tot,3)*100, Frequency = paste0(freq, "%")) %>% 
  ungroup() %>% 
  dplyr::select(Groups = V1, "Year of foundation" = label, Count, Frequency) %>% 
  add_row(Groups = "No information",    "Year of foundation" = "No information Rate",  Count = non_response,  Frequency = paste0(non_res_frq,"%") ) 




