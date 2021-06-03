### script para procesar la encuesta a los miembros de la PPA en el ano 2020
### @author: Andres Camilo Mendez
### @date: MAyo - 2021


suppressWarnings(if(require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(tidyverse, readr, readxl, magrittr, stringr, stringi, lubridate, writexl)

### funciones necesarias para organizar algunas cosas

get_questions_opts <- function(data, var, var_lab){
  
  var_lab <- sym(var_lab)
  
  ret <- preguntas %>% 
    dplyr::filter(Variavel == var) %>%
    dplyr::pull(vars_opts) %>% 
    purrr::pluck(1) %>% 
    dplyr::filter(if_any(contains("Used_opcoes"), ~ .)) %>% 
    dplyr::rename(!!var_lab := `Labels in English`) %>% 
    dplyr::select(Opcoes_numero, !!var_lab)
  
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
  dplyr::select(Variavel, Metrica,Variavel_nome, var_id) %>% 
  filter(complete.cases(Metrica)) %>% 
  dplyr::mutate(root_var = ifelse(grepl("xxxx", Variavel), str_replace(Variavel, "xxxx", "2020" ), Variavel))


preguntas <- vars_names %>% 
  dplyr::mutate(vars_opts = purrr::map2(.x = root_var, .y = Metrica, .f = function(.x, .y){
    i <- .x
    cat("processing:", i, "\n")
    if(grepl(pattern ='múltipla escolha', .y)){
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

# validación de los ID's

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

labs <- preguntas %>% 
  dplyr::filter(Variavel == "V1_Rodada_xxxx") %>% 
  dplyr::pull(vars_opts) %>% 
  purrr::pluck(1) %>% 
  dplyr::select( Opcoes_numero, Opcoes_nome ,  `Labels in English`, `Labels in English simp`)


entrevistados <- groups %>% 
  dplyr::left_join(., labs, by = c( "V1_Rodada" = "Opcoes_numero")) %>% 
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


# extraer el tipo de organizaciones

type_org <-  raw_list %>% 
  purrr::pluck(., grep("Empresas_orgs_atributos", names(raw_list))) %>% 
  dplyr::select(ID, V39a, V39b) %>% 
  dplyr::mutate_all(as.character) %>% 
  dplyr::left_join(., groups, by = c("ID" = "ID_node")) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas,
                                         var = "V39a",
                                         var_lab = "V39a_lab"),
                   by = c("V39a" = "Opcoes_numero")) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas,
                                         var = "V39b",
                                         var_lab = "V39b_lab"),
                   by = c("V39b" = "Opcoes_numero")) 

stopifnot("Se encontraron NA en type_org" = all(complete.cases(type_org) ))

## validaciones al tipo de  empresas

for(i in c("V39a", "V39b")){
  opts <- preguntas %>% 
    dplyr::filter(Variavel == i) %>%
    dplyr::pull(vars_opts) %>% 
    purrr::pluck(1) %>% 
    dplyr::filter(if_any(contains("Used_opcoes"), ~ .)) %>%
    pull(Opcoes_numero)
  
  to_val <- type_org %>% 
    pull(i) %>% 
    unique
  cat("Validando pregunta: ", i, " \n")
  
  stopifnot("Respuestas no coinciden con las opciones de respuesta" = all(opts %in% to_val ) )
  
}







