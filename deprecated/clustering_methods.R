#### script to perform PCA over PPA SNA variables
### Author: Andres Camilo Mendez
### Year: 2020
### This script comes with absolutely no warranty

library(pacman)
pacman::p_load(FactoMineR, tidyverse, missMDA, purrr, magrittr, pvclust, mclust, xlsx)


data <- read.csv("C:/Users/acmendez/Downloads/pca_data.csv", stringsAsFactors = F, header = T)


#PCA reemplazando los NA por ceros
data %>% 
  dplyr::select(-(cod_node:clase_node)) %>% 
  dplyr::mutate_if(is.numeric, .funs = function(i){ifelse(is.na(i), 0, i)}) %>% 
  FactoMineR::PCA(., scale.unit = T, ncp = 3, graph = T) %>%
  cbind(x= purrr::pluck(.,3,2)  , clust =FactoMineR::HCPC(., nb.clust = -1, min = 3, max = 4, graph = F) %>% 
           purrr::pluck(1) %>% dplyr::pull(clust) %>% as.character) %>% 
  as.matrix(rownames.force = F) %>%
  as.data.frame() %>% 
  dplyr::select(-1) %>% 
  dplyr::select(x = Dim.1, y = Dim.2, clust) %>% 
  data.frame(company = data$name_node, group = data$clase_node, .) %>%  
  mutate_all(unlist) %>% 
  ggplot(aes(x = x , y = y , colour = factor(clust)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)


# PCA imputando los NA  
data %>% 
  dplyr::select(-(cod_node:clase_node))%>% 
  imputePCA(.,method="Regularized",ncp=3) %>% 
  purrr::pluck(1) %>% 
  FactoMineR::PCA(., scale.unit = T, ncp = 3, graph = F ) %>% 
  cbind(x= purrr::pluck(.,3,2)  , clust =FactoMineR::HCPC(., nb.clust = -1, min = 3, max = 4, graph = F) %>% 
          purrr::pluck(1) %>% dplyr::pull(clust) %>% as.character) %>% 
  as.matrix(rownames.force = F) %>%
  as.data.frame() %>% 
  dplyr::select(-1) %>% 
  dplyr::select(x = Dim.1, y = Dim.2, clust) %>% 
  data.frame(company = data$name_node, group = data$clase_node, .) %>%  
  mutate_all(unlist) %>% 
  ggplot(aes(x = x , y = y , colour = factor(clust)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  stat_ellipse(aes(group = factor(clust)), type = "norm", geom = "polygon", alpha = 0.3)

#PCA usando los datos  sin hacerles nada a los NA
data %>% 
  dplyr::select(-(cod_node:clase_node))%>% 
  na.omit() %>% 
  FactoMineR::PCA(., scale.unit = T, ncp = 3, graph = T ) 
  

## usando la matrix de distancias
  
cl <-  data %>% 
  dplyr::select(-(cod_node:clase_node)) %>% 
  dplyr::mutate_if(is.numeric, .funs = function(i){ifelse(is.na(i), 0, i)}) %>% 
  #imputePCA(.,method="Regularized",ncp=3) %>% 
  #purrr::pluck(1) %>% 
  stats::dist(., method = "euclidean") %>% 
  hclust(d =., method = "ward.D") %T>%
  plot() %>% 
  cutree(., k = 4)
  
data %>% 
  dplyr::select(-(cod_node:clase_node))%>% 
  imputePCA(.,method="Regularized",ncp=3) %>% 
  purrr::pluck(1) %>% 
  FactoMineR::PCA(., scale.unit = T, ncp = 3, graph = F ) %>% 
  purrr::pluck(3,2) %>% 
  data.frame() %>% 
  dplyr::select(x = Dim.1, y = Dim.3) %>% 
  data.frame(company = data$name_node, group = data$clase_node, ., clust = cl) %>%  
  ggplot(aes(x = x , y = y , colour = factor(clust), shape = factor(group)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  stat_ellipse(aes(group = factor(clust)), type = "norm", geom = "polygon", alpha = 0.3)
  

### clustering con k means

km_cl <- data %>% 
  dplyr::select(-(cod_node:clase_node))%>% 
  imputePCA(.,method="Regularized",ncp=3) %>% 
  purrr::pluck(1) %>% 
  kmeans(., centers = 4, iter.max = 1000, nstart = 100, algorithm = "Forgy") %>% 
  purrr::pluck(1) 
  

data %>% 
  dplyr::select(-(cod_node:clase_node))%>% 
  imputePCA(.,method="Regularized",ncp=3) %>% 
  purrr::pluck(1) %>% 
  FactoMineR::PCA(., scale.unit = T, ncp = 3, graph = F ) %>% 
  purrr::pluck(3,2) %>% 
  data.frame() %>% 
  dplyr::select(x = Dim.1, y = Dim.3) %>% 
  data.frame(company = data$name_node, group = data$clase_node, ., clust = km_cl) %>%  
  ggplot(aes(x = x , y = y , fill = factor(clust),  colour = factor(clust), shape = factor(group)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  stat_ellipse(aes(group = factor(clust)), type = "norm", geom = "polygon", alpha = 0.3)
 


final_data <- data %>% 
  dplyr::mutate(hclust = cl, kmeans_clust = km_cl)


xlsx::write.xlsx(final_data, file =  "D:/OneDrive - CGIAR/Attachments/Desktop/cluster_analysis.xlsx", sheetName = "data", append = TRUE, showNA = TRUE, row.names = FALSE)

as_tibble(data) %>% 
  dplyr::mutate(cod_node = as.character(cod_node), clust = km_cl) %>% 
  dplyr::group_by(clust) %>% 
  summarise_if(is.numeric, .funs = tibble::lst(mean, sd, min, max), na.rm = T) %>% 
  data.frame() %>% 
  xlsx::write.xlsx(., file =  "D:/OneDrive - CGIAR/Attachments/Desktop/cluster_analysis.xlsx", sheetName = "summary_kmeans", append = TRUE, showNA = TRUE, row.names = FALSE)

  
  as_tibble(data) %>% 
  dplyr::mutate(cod_node = as.character(cod_node), clust = cl) %>% 
  dplyr::group_by(clust) %>% 
  summarise_if(is.numeric, .funs = tibble::lst(mean, sd, min, max), na.rm = T) %>% 
   data.frame() %>% 
   xlsx::write.xlsx(., file =  "D:/OneDrive - CGIAR/Attachments/Desktop/cluster_analysis.xlsx", sheetName = "summary_hclsut", append = TRUE, showNA = TRUE, row.names = FALSE)
  



### usando pvclust
fit <- data %>% 
  dplyr::select(-(cod_node:clase_node))%>% 
  imputePCA(.,method="Regularized",ncp=3) %>%
  purrr::pluck(1) %>% 
  t() %>% 
  pvclust(., method.hclust="ward.D",
               method.dist="euclidean")

cutree(fit$hclust, k = 4)
plot(fit) 


fit2 <- data %>% 
  dplyr::select(-(cod_node:clase_node))%>% 
  imputePCA(.,method="Regularized",ncp=3) %>%
  purrr::pluck(1) %>%
  Mclust(.)
plot(fit2)





