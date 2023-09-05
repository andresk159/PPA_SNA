

library(pacman)
pacman::p_load(tidyverse, readxl, writexl, stringr, igraph)



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


type_org <-  list(
  # raw_list_2020[[grep("Empresas_orgs_atributos", names(raw_list_2020))]] 
  
  rodada_2019 =raw_list_2019[[grep("Empresas_orgs_atributos", names(raw_list_2019))]] %>% 
    dplyr::select(starts_with("ID"), starts_with("V40a"), V39a, V39b) %>%
    dplyr::rename(ID_node = ID)  %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::left_join(., groups$rodada_2019 %>% dplyr::select(ID_node, Nome, V1)) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2019, var = "V39a", var_lab = "Type of organization", lan = lan), by = c("V39a" = "Opcoes_numero")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2019, var = "V39b", var_lab = "Subtype of organization", lan = lan), by = c("V39b" = "Opcoes_numero")) ,
  
  rodada_2020 = raw_list_2020[[grep("orgs_atributos_all", names(raw_list_2020))]] %>% 
    dplyr::select(starts_with("ID"), V39a, V39b) %>%
    dplyr::mutate_all(as.character) %>% 
    dplyr::left_join(., groups$rodada_2020 %>% dplyr::select(ID_node, Nome, V1), by = c("ID_node" = "ID_node")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2020, var = "V39a", var_lab = "Type of organization", lan = lan), by = c("V39a" = "Opcoes_numero")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2020, var = "V39b", var_lab = "Subtype of organization", lan = lan), by = c("V39b" = "Opcoes_numero")) ,
  
  rodada_2021 = raw_list_2021[[grep("Empresas_orgs_atributos", names(raw_list_2021))]] %>% 
    dplyr::select(starts_with("ID"), V39a, V39b) %>%
    dplyr::mutate_all(as.character) %>% 
    dplyr::left_join(., groups$rodada_2021 %>% dplyr::select(ID_node, Nome, V1), by = c("ID_node" = "ID_node")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2021, var = "V39a", var_lab = "Type of organization", lan = lan), by = c("V39a" = "Opcoes_numero")) %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2021, var = "V39b", var_lab = "Subtype of organization", lan = lan), by = c("V39b" = "Opcoes_numero")) 
  
  
  
)

type_org[["rodada_2021"]] <- type_org[["rodada_2021"]] %>% 
  dplyr::select(ID_node, everything(.), type_new = `Type of organization` ) %>% 
  dplyr::full_join(type_org[["rodada_2020"]] %>% 
                     dplyr::select(ID_node, "Nome2" = Nome, "V12" = V1, type_old =  `Type of organization` ), by = "ID_node") %>% 
  dplyr::mutate(Nome       = ifelse(is.na(Nome), Nome2, Nome),
                V1         = ifelse(is.na(V1), V12, V1),
                type_final = ifelse(is.na(type_new), type_old, type_new) ,
                type_final = ifelse(is.na(type_final), "others", type_final)) %>% 
  dplyr::select(ID_node, everything(.), -type_new, -type_old, -Nome2, -V12,`Type of organization` = type_final) %>% 
  dplyr::add_row(data.frame(ID_node = "237", V39a = NA, V39b = NA,  Nome = "Assaí Atacadista", V1 = "B",
                            `Subtype of organization` = "Consolidated company", `Type of organization` ="Company (consolidated or start-up)", check.names = F)) %>% 
  dplyr::add_row(data.frame(ID_node = "236", V39a = NA, V39b = NA,  Nome = "Nestlé", V1 = "B",
                            `Subtype of organization` = "Consolidated company", `Type of organization` ="Company (consolidated or start-up)", check.names = F)) %>% 
  dplyr::add_row(data.frame(ID_node = "22", V39a = NA, V39b = NA,  Nome = "Coopmel", V1 = "B",
                            `Subtype of organization` = "Consolidated company", `Type of organization` ="Company (consolidated or start-up)", check.names = F))



################################################################################################
###########   CREACIÓN DE TABLAS POWER BI - PARA CARLOS #######################################
##############################################################################################

result <- list()

lan <- "en"

raw_list_2019$Percepcoes = raw_list_2019$Percepcoes %>% 
  dplyr::filter(!is.na(V17)) %>% 
  dplyr::mutate_all(as.character) %>% 
  bind_cols(., correct_question_format(input_data = raw_list_2019$Percepcoes, 
                                       var_opts = get_questions_opts(data = preguntas$rodada_2019 , var = "V20", var_lab = "label" , lan = lan), 
                                       var = "V20" )) %>%
  dplyr::rename(ID_node = ID)


raw_list_2019$Percepcoes <- raw_list_2019$Percepcoes %>% 
  dplyr::left_join(., type_org$rodada_2019, by = c("ID_node" = "ID_node" ) )

eco_sector_2019 <- raw_list_2019$Percepcoes %>% 
  dplyr::select(ID_node, starts_with("V40a_")) %>% 
  tidyr::pivot_longer(., cols = -ID_node, names_to = "V40a", values_to = "val") %>% 
  dplyr::mutate(V40a = stringr::str_extract(V40a, "_[0-9]+") %>% gsub("_", "", .)) %>% 
  dplyr::filter(val != 0) %>% 
  dplyr::filter(!duplicated(ID_node)) %>% 
  dplyr::left_join(.,  get_questions_opts(data = preguntas$rodada_2019 , var = "V40a", var_lab = "label", lan = lan), by = c("V40a" = "Opcoes_numero") )

result$db_2019 <- raw_list_2019$Percepcoes %>% 
  dplyr::select( -V20, -V39a, -V39b, -starts_with("V40a")) %>% 
   dplyr::left_join(., eco_sector_2019, by = "ID_node") %>% 
  dplyr::select(ID_node, name = Nome, 
                ID_ego, V1, Economic_sector = label, `Type of organization`, `Subtype of organization` , 
                V17, V19, V20_1:V20_14, everything(.)) 

for( i in paste0("V20_",1:14)){
  
  result$db_2019[i] <-
    ifelse(result$db_2019[i] == 1, 
           get_questions_opts(data = preguntas$rodada_2019 , var = "V20", var_lab = "label", lan = lan) %>% 
             dplyr::filter(Opcoes_numero == i) %>% 
             dplyr::pull(label)
           , 0)
}


for( i in c(paste0("V", c(17,21:36) ), "V37a","V38a") ){
  
  result$db_2019[i] <- result$db_2019[i] %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2019 , var = i, var_lab = "label", lan = lan) %>% 
                       dplyr::rename(!!i := "Opcoes_numero"  ) ) %>% 
    dplyr::select(-!!i) %>% 
    dplyr::rename( !!i := "label")
   
}


result$V20_2019 <-  result$db_2019 %>% 
  dplyr::select(ID_node, name, V1, starts_with("V20")) %>% 
  tidyr::pivot_longer(., cols =  starts_with("V20"), names_to = "cat", values_to = "val") 



#####################
### 2020 ###########
###################


result$db_2020 <- raw_list_2020$`V17_V20-V38_Percepcoes` %>% 
  dplyr::left_join(., type_org$rodada_2020, by = c("ID_node" = "ID_node" ) ) %>% 
  dplyr::left_join(., raw_list_2020$`V39-V45_Empresas_orgs_atributos` %>% 
                     dplyr::select(ID_node, V40a) ) %>% 
  dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2020 , var = "V40a", var_lab = "label", lan = lan), by = c("V40a" ="Opcoes_numero")) %>% 
  dplyr::select(ID_node, name = Nome, ID_ego, V1_2020, "Economic_sector" = label, `Type of organization`, `Subtype of organization`,
                V17, starts_with("V20_"), everything(.), -V39a, -V39b, -V1, -V40a)
  

for( i in paste0("V20_",1:14)){
  
  result$db_2020[i] <-
    ifelse(result$db_2020[i] == 1, 
           get_questions_opts(data = preguntas$rodada_2020 , var = "V20", var_lab = "label", lan = lan) %>% 
             dplyr::filter(Opcoes_numero == i) %>% 
             dplyr::pull(label)
           , 0)
}



for( i in c(paste0("V", c(17,21:36) ), "V37a","V38a", paste0("V", 21:36,"a")) ){
  
  result$db_2020[i] <- result$db_2020[i] %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2020 , var = i, var_lab = "label", lan = lan) %>% 
                       dplyr::rename(!!i := "Opcoes_numero"  ) ) %>% 
    dplyr::select(-!!i) %>% 
    dplyr::rename( !!i := "label")
  
}




result$V20_2020 <-  result$db_2020 %>% 
  dplyr::select(ID_node, name, V1_2020, starts_with("V20")) %>% 
  tidyr::pivot_longer(., cols =  starts_with("V20"), names_to = "cat", values_to = "val") 


#####################
### 2021 ###########
###################


result$db_2021 <- raw_list_2021$`V17_V20-V38a_Percepcoes` %>% 
  dplyr::left_join(., type_org$rodada_2021, by = c("ID_node" = "ID_node" ) ) %>% 
  dplyr::select(ID_node, name = Nome, ID_ego, V1_2021, `Type of organization`, `Subtype of organization`,
                V17, starts_with("V20_"), everything(.), -V39a, -V39b, -V1, -matches("V28[a-z]"),  -matches("V20[a-z]_")) 



for( i in names(result$db_2021)[grepl("V20_", names(result$db_2021))] ){
  
  result$db_2021[i] <-
    ifelse(result$db_2021[i] == 1, 
           get_questions_opts(data = preguntas$rodada_2021 , var = "V20", var_lab = "label", lan = lan) %>% 
             dplyr::filter(Opcoes_numero == i) %>% 
             dplyr::pull(label)
           , 0)
}

for( i in c(names(result$db_2021)[grepl("V[2-9]([1-9]|[1-9][a-z])", names(result$db_2021))], "V17", "V30") ){
  
  result$db_2021[i] <- result$db_2021[i] %>% 
    dplyr::left_join(., get_questions_opts(data = preguntas$rodada_2021 , var = i, var_lab = "label", lan = lan) %>% 
                       dplyr::rename(!!i := "Opcoes_numero"  ) ) %>% 
    dplyr::select(-!!i) %>% 
    dplyr::rename( !!i := "label")
  
}




result$V20_2021 <-  result$db_2021 %>% 
  dplyr::select(ID_node, name, V1_2021, starts_with("V20")) %>% 
  tidyr::pivot_longer(., cols =  starts_with("V20"), names_to = "cat", values_to = "val") 

writexl::write_xlsx(result,"D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/powerBI_tables.xlsx")

##################################################
######### Rc freqs ##############################
################################################


get_Rc <- function(dir_pth, rodada, type_org_rodada, preguntas_rodada){
  
  av_files <- list.files(dir_pth, pattern ="standard_[a-zA-Z_]+.rds", full.names = T)
  av_files <- av_files[!grepl("madre", av_files)]
  bd_inter <- lapply(av_files, function(i){
    
    year <- gsub("[a-zA-Z_]+","", rodada)
    cate <- stringr::str_extract(basename(i), "[a-zA-Z]+_") %>% 
      stringr::str_replace(., "_", "")
    
    nt1 <- readRDS(i)
    
    attrs    <- igraph::get.edge.attribute(nt1)
    
    if(grepl("nego",cate)){
      var_name <- names(attrs)[grepl("R[0-9]b", names(attrs))]
      
    }else{
      var_name <- names(attrs)[grepl("R[0-9]c", names(attrs))]
      
    }
    
    if(length(var_name) != 0){
      
      if(grepl("nego",cate)){
        Rc       <- attrs[grepl("R[0-9]b", names(attrs))] %>% unlist
        
      }else{
        Rc       <- attrs[grepl("R[0-9]c", names(attrs))] %>% unlist
        
      }
        
      
      res <- tibble(R_Start_ID = igraph::ends(nt1, es=E(nt1), names=T)[,1], Rc = Rc  ) %>% 
        dplyr::left_join(., type_org_rodada, by = c("R_Start_ID" = "ID_node")) %>% 
        dplyr::mutate(time = year,
                      cat = cate) %>% 
        dplyr::select(R_Start_ID, Nome, "group" = V1,  time,  cat, Rc) %>% 
        dplyr::left_join(., get_questions_opts(data = preguntas_rodada, var = var_name, var_lab= "label", lan = "en"), by = c("Rc" = "Opcoes_numero") ) %>% 
        dplyr::group_by(R_Start_ID, label) %>% 
        dplyr::mutate(conteo = n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::group_by(Nome) %>% 
        dplyr::mutate(freq = conteo/n()) %>% 
        dplyr::select(-Rc)
      
    }else{
      res <- tibble(R_Start_ID =NA, 
                    Nome = NA,
                    group = NA,
                    time = NA,
                    cat = NA, 
                    label =NA,
                    conteo = NA,
                    freq = NA)
    }
    return(res)
    
  })
  
  res_final <- bind_rows(bd_inter)
  
  return(res_final)
  }

rodadas <- c(paste0("rodada_", 2019:2021))

Rc_tables <- lapply(rodadas, function(rodada){
  get_Rc(dir_pth = paste0("D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/", rodada,"/"), 
         rodada = rodada,
         type_org_rodada = type_org[[rodada]],
         preguntas_rodada = preguntas[[rodada]])
  
})

names(Rc_tables) <- rodadas


writexl::write_xlsx(Rc_tables, "D:/OneDrive - CGIAR/Documents/PPA 2021/network_results/Rc_freqs.xlsx")

