## script par procesar bae de datos de las encuestas PPA SNA

## importar pacquetes
library(pacman)
pacman::p_load(tidyverse, readr, igraph, statnet, readxl, FactoMineR, zoo, likert)

# function to conver multioption question into  binary  columns
become_vec <- function(choices, n_res){
  
  if(is.null(dim(choices))){
    vec <- as.integer(1:n_res %in% choices) 
  }else if(length(dim(choices))){
    vec <-  as.integer(1:n_res %in% dplyr::pull(choices))
  }
  return(vec)
}


#### cargar datos
pth <- 'D:/OneDrive - CGIAR/Documents/Dados_entrevistas_2019_grupos_A_e_B_para_CIAT_(20nov19).xlsx'


categoria <- excel_sheets(pth)[-1]

raw_data <- lapply(categoria, function(i){
  read_excel(pth, sheet = i)
}) 
 
names(raw_data) <- categoria

##### Informer secction

### fix date format issues
f <- raw_data$Informer %>%
  dplyr::mutate(ID2 = paste0(ID, ID_ego)) %>%
  dplyr::mutate(V12 = as.numeric(V12), V12 = as.Date.numeric(V12, origin = "1899-12-30")) %>%
  dplyr::mutate(V13 = as.numeric(V13), V13 = zoo::as.Date(V13, origin = "1899-12-30"))

unq <- unique(f$ID2)  

reshaped <- lapply(unq, function(i){
  f %>% 
    dplyr::filter( ID2 == i) %>% 
    dplyr::pull(V14) %>%
    become_vec(choices = ., n_res = 9)
  
}) %>% 
  do.call( rbind, .) %>% 
  as.data.frame()
names(reshaped) <- paste0("V14_",1:9)


new_data <-list()

new_data$Informer <- f %>% 
  dplyr::filter(!is.na(V2)) %>%
  cbind(., reshaped)%>%
  as_tibble() %>%
  dplyr::select(-V14)

##### Percepcoes secction

new_data$Percepcoes <- raw_data$Percepcoes %>%
  dplyr::mutate(V1 = paste0(V1a, V1b)) 
  

unq <- unique(new_data$Percepcoes$V1)  

reshaped <- lapply(unq, function(i){
  new_data$Percepcoes %>% 
    dplyr::filter( V1 == i) %>% 
    dplyr::pull(V20) %>%
    become_vec(choices = ., n_res = 13)
  
}) %>% 
  do.call( rbind, .) %>% 
  as.data.frame()
names(reshaped) <- paste0("V20_",1:13)



new_data$Percepcoes <- new_data$Percepcoes %>% 
  dplyr::filter(!is.na(V17)) %>%
  cbind(., reshaped)%>%
  as_tibble() %>%
  dplyr::select(-V20)

res.mca <- new_data$Percepcoes %>% 
  dplyr::select(V20_1:V20_13) %>% 
  dplyr::mutate_all(.funs = as.character) %>%
  FactoMineR::MCA(.)
plot(res.mca)

#calcular cuales son las variables mas importantes
x <- dimdesc(res.mca, proba = 1)$`Dim 1`$quali %>% data.frame %>%
  dplyr::mutate(nms = substr(rownames(.),5, nchar(rownames(.)) ), nms = as.numeric(nms)) %>%
  arrange(nms)

y <- dimdesc(res.mca, proba = 1)$`Dim 2`$quali %>% data.frame %>%
  dplyr::mutate(nms = substr(rownames(.),5, nchar(rownames(.)) ), nms = as.numeric(nms)) %>%
  arrange(nms)
# calcular las preguntas mas importantes como el punto medio entre las dos dimensiones
weights <- data.frame(x, y) %>% dplyr::select(R2, R2.1) %>% dplyr::mutate(mn = (R2 + R2.1)/2 ) %>% pull(mn)

# hacer el grafico, coloreando las variables mas importantes
res.mca$var$eta2[,1:2] %>% 
  data.frame %>%
  cbind(., weight = weights ,nms = row.names(.)) %>%
  ggplot(aes(x = Dim.1, y = Dim.2, colour = weight, label = nms))+
  geom_point(shape = 17)+
  scale_color_gradient2(midpoint= mean(weights)-0.02, low="lightblue", mid="white",
                        high="red", space ="Lab" ) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_label(size = 3)+
  xlab(paste("Dim 1:", round(res.mca$eig[1, 1],3)*100 ,"%"))+
  ylab(paste("Dim 1:", round(res.mca$eig[2, 1],3)*100 ,"%"))+
  labs(title = "Pergunta 20")
  
freq <- new_data$Percepcoes %>% 
  dplyr::select(V20_1:V20_13) %>% 
  colSums()

data.frame(var = paste0("V20_",1:13), frq = freq/64) %>%
  arrange(desc(freq)) %>%
  ggplot(aes(x = var, y = frq))+
  geom_bar(stat = "identity")+
  coord_flip()
  


res.mca$var$eta2[,1:2] %>% 
  data.frame %>%
  cbind(., weight = weights ,nms = row.names(.)) %>%
  mutate(nms = reorder(nms, weight )) %>%
  ggplot(aes(x = nms, y = weight ))+
  geom_col() + 
  coord_flip()+
  theme_bw()+
  labs(title = "Important categories")+
  xlab("Category names")

###### pregunta V10 #######


groups <- raw_data$Empresas_orgs %>% dplyr::mutate(ID= as.character(ID)) %>% dplyr::select(-...6)
 
a1<-new_data$Informer %>%
  dplyr::mutate(ID = as.character(ID)) %>%
  dplyr::inner_join(., groups, by = c("ID" = "ID")) %>%
  dplyr::mutate(V10 = replace(V10, is.na(V10), "Empty"))%>%
  dplyr::mutate(V10 = as.factor(V10))%>%
  group_by(V1, V10) %>% 
  tally() %>% 
  mutate(fr1 = round(n/sum(n), 2)*100) %>%
  ggplot(aes(x = V1, y = fr1, fill = factor(V10)))+
  geom_bar(stat= "identity")+
  labs(x= "Group", y = "frecuency", title = "Pergunta V10")+
  scale_fill_discrete(name = "Setor",  labels = c("Generico", "Relacionado", "Não indet.","Empty"))+
  scale_y_continuous(limits=c(0,100), breaks= seq(0,100, by=10)) + 
  labs(caption = "CIAT - IPAM- CALPP")+
  theme(legend.position="bottom",
        # legend.direction ="horizontal",
        legend.text=element_text(size=14),
        legend.key.size = unit(3,"line"),
        plot.caption = element_text(size = 12, hjust = 0.8,
                                    family = "Garamond", color = "black", face = "bold"),
        # panel.background = element_rect(fill = "lightblue",
        #                                 colour = "lightblue",
        #                                 size = 0.5, linetype = "solid"),
        plot.title = element_text(face="bold", size=20),
        axis.title=element_text(face = "bold.italic", size = 20),
        strip.text.x = element_text(size = 16, angle = 0),
        axis.text= element_text(face = "bold", size = 20))
  
png("Y:/PPA-SNA/documents/images/pergunta_v10.png", width = 12, height = 10, units = 'in',bg = "transparent",res=620)
plot(a1)
dev.off()


###### pregunta V11 #######

groups <- raw_data$Empresas_orgs %>% dplyr::mutate(ID= as.character(ID)) %>% dplyr::select(-...6)

 a1<- new_data$Informer %>%
  dplyr::mutate(ID = as.character(ID)) %>%
  dplyr::inner_join(., groups, by = c("ID" = "ID")) %>%
  dplyr::mutate(V11 = replace(V11, is.na(V11), "Empty"))%>%
  dplyr::mutate(V11 = as.factor(V11))%>%
  group_by(V1, V11) %>% tally() %>% mutate(fr1 = round(n/sum(n), 2)*100) %>%
  ggplot(aes(x = V1, y = fr1, fill = factor(V11)))+
  geom_bar(stat= "identity")+
  labs(x= "Group", y = "frecuency", title = "Pergunta V11")+
  scale_fill_discrete(name = "Cargo",  labels = c("Diretoria", "Outros","Não ident."))+
  scale_y_continuous(limits=c(0,100), breaks= seq(0,100, by=10)) + 
  labs(caption = "CIAT - IPAM- CALPP")+
  theme(legend.position="bottom",
        # legend.direction ="horizontal",
        legend.text=element_text(size=14),
        legend.key.size = unit(3,"line"),
        plot.caption = element_text(size = 12, hjust = 0.8,
                                    family = "Garamond", color = "black", face = "bold"),
        # panel.background = element_rect(fill = "lightblue",
        #                                 colour = "lightblue",
        #                                 size = 0.5, linetype = "solid"),
        plot.title = element_text(face="bold", size=20),
        axis.title=element_text(face = "bold.italic", size = 20),
        strip.text.x = element_text(size = 16, angle = 0),
        axis.text= element_text(face = "bold", size = 20))

png("Y:/PPA-SNA/documents/images/pergunta_v11.png", width = 12, height = 10, units = 'in',bg = "transparent",res=620)
plot(a1)
dev.off()

###### pregunta  V12 

new_data$Informer[which(grepl( "1905-[0-9]*",new_data$Informer$V12)), "V12"]<- rep(as.Date("2019-11-20"), 6)

a1<- new_data$Informer %>%
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::inner_join(., groups, by = c("ID" = "ID")) %>%
  dplyr::mutate(year = year(V12)) %>%
  group_by(ID)%>%
  dplyr::slice(which.min(V12)) %>%
  ungroup() %>%
  group_by(year) %>%
  count(ID) %>%
  summarise(suma = sum(n)) %>%
  ggplot(aes(x = year, y = suma))+
  geom_line(size=1) +
  geom_point() +
  xlab("Años") + 
  ylab("Numero de empresas")+
  labs(title = "Pergunta V12")+
  scale_y_continuous(limits=c(0, 40),breaks=seq(0,40,5)) + 
  labs(caption = "CAIT- IPAM - CALPP")+
  scale_color_brewer(palette = "Dark2")+ theme(aspect.ratio = 0.6)+
  theme(legend.position="none",
        # legend.direction ="horizontal",
        legend.text=element_text(size=14),
        legend.key.size = unit(3,"line"),
        plot.caption = element_text(size = 12, hjust = 0.8,
                                    family = "Garamond", color = "black", face = "bold"),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        plot.title = element_text(face="bold", size=20),
        axis.title=element_text(face = "bold.italic", size = 20),
        strip.text.x = element_text(size = 16, angle = 0),
        axis.text= element_text(face = "bold", size = 20))
 
  
 
png("Y:/PPA-SNA/documents/images/pergunta_v12.png", width = 12, height = 10, units = 'in',bg = "transparent",res=620)
plot(a1)
dev.off()

###### pregunta V13

new_data$Informer[which(grepl( "1905-[0-9]*",new_data$Informer$V13)), "V13"]<- rep(as.Date("2019-11-20"), 5)

a1<-new_data$Informer %>%
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::inner_join(., groups, by = c("ID" = "ID")) %>%
  dplyr::mutate(month = as.yearmon(V13), ID_ego =paste0(ID, ID_ego)) %>%
  group_by(ID_ego) %>% 
  group_by(month) %>%
  count(ID) %>%
  summarise(suma = sum(n)) %>%
  ggplot(aes(x = month, y = suma))+
  geom_line(size=1) +
  geom_point() +
  xlab("Años") + 
  ylab("Numero de empresas")+
  labs(title = "Pergunta V12")+
  scale_y_continuous(limits=c(0, 8),breaks=seq(0,8,1)) + 
  #scale_x_continuous("A?os", breaks = seq(1, 27,1), labels = xlabs)+
  labs(caption = "CAIT- IPAM - CALPP")+
  scale_color_brewer(palette = "Dark2")+ theme(aspect.ratio = 0.6)+
  theme(legend.position="none",
        # legend.direction ="horizontal",
        legend.text=element_text(size=14),
        legend.key.size = unit(3,"line"),
        plot.caption = element_text(size = 12, hjust = 0.8,
                                    family = "Garamond", color = "black", face = "bold"),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        plot.title = element_text(face="bold", size=20),
        axis.title=element_text(face = "bold.italic", size = 20),
        strip.text.x = element_text(size = 16, angle = 0),
        axis.text= element_text(face = "bold", size = 20))



png("Y:/PPA-SNA/documents/images/pergunta_v13.png", width = 12, height = 10, units = 'in',bg = "transparent",res=620)
plot(a1)
dev.off()


#### pregunta V14

labs <- c("Comitê de Governana", 
         "organiza??o na Assembleia", 
         "Coordena??o Executiva",
         "GT1 - Empreendedorismo, investimento de impacto",
         "GT2 - Oportunidades estrat?gicas de investimento",
         "GT3 - Parcerias entre empresas",
         "GT4 - Interface com pol?ticas p?blicas",
         "Programa de Acelera??o",
         "Outros")

f<- new_data$Informer %>% 
  dplyr::mutate(ID = as.character(ID)) %>% 
  dplyr::inner_join(., groups, by = c("ID" = "ID")) %>%
  dplyr::select(V14_1:V14_9, V1)
  colnames(f) <- c(labs, "V1")
  
  a1<- f %>% 
    gather(-V1 ,key = "var", value = "value") %>%
    group_by(V1, var) %>%
    summarise(cnt = sum(value)) %>%
    mutate(fr1 = round(cnt/sum(cnt), 2)*100) %>%
    ungroup() %>%
    mutate(var = reorder(var, fr1 )) %>%
    ggplot(aes(x =var,  y= fr1, fill = factor(V1)))+
    geom_bar(stat = "identity")+
    xlab("Frecuency") + 
    ylab("Categories")+
    coord_flip()+
    scale_fill_discrete(name = "Grupo")+
    labs( title = "Pergunta V14", caption = "CAIT- IPAM - CALPP")+
    scale_color_brewer(palette = "Dark2")+ theme(aspect.ratio = 0.6)+
      theme(legend.position="bottom",
          # legend.direction ="horizontal",
          legend.text=element_text(size=14),
          legend.key.size = unit(3,"line"),
          plot.caption = element_text(size = 12, hjust = 0.8,
                                      family = "Garamond", color = "black", face = "bold"),
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid"),
          plot.title = element_text(face="bold", size=20),
          axis.title=element_text(face = "bold.italic", size = 20),
          strip.text.x = element_text(size = 16, angle = 0),
          axis.text= element_text(face = "bold", size = 20))
  
  png("Y:/PPA-SNA/documents/images/pergunta_v14.png", width = 12, height = 10, units = 'in',bg = "transparent",res=620)
  plot(a1)
  dev.off()
  

  #### pregunta 17
  
    to_likert <- new_data$Percepcoes %>% 
      dplyr::select(-V1)%>%
      dplyr::mutate(V1a = as.character(V1a)) %>% 
      dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
      dplyr::select(V1a, V1b, V17, V1) %>%
      dplyr::mutate(V17= replace(V17, V17 == "0", "Nenhum"), 
                    V17= replace(V17, V17 == "1", "Baixo"),
                    V17= replace(V17, V17 == "2", "Mediano"),
                    V17= replace(V17, V17 == "3", "Alto"),
                    V17= replace(V17, V17 == "4", "Muito alto")) %>%
      dplyr::select(V17,V1) %>%
      pivot_wider(names_from = V1, values_from = V17, values_fill = list(V17 = "Na")) 
    
    to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,31-25) ) ,A2 = c(unlist(to_likert$A2),rep(NA, 21)) ,B= unlist(to_likert$B))
    
    a1 <- to_likert%>% 
      mutate_all(factor, levels = c("Nenhum", "Baixo","Mediano", "Alto","Muito alto"  ) )%>% 
      likert(items = . , nlevels = 5) 
    
  
    png("Y:/PPA-SNA/documents/images/pergunta_v17.png", width = 12, height = 10, units = 'in',bg = "transparent",res=620)
    plot(a1)
    dev.off()
    
##### pregunta 20
    labs <- c("Ações que promovam o empreendedorismo",
    "Desenvolvimento de modelos positivos",
    "Relaçõeses com governo local",
    "Influência em políticas públicas",
    "Coordenaçao de açoes do setor privado",
    "Promoçao de iniciativas e projetos",
    "Ampliaçao do uso socioeconímico",
    "Alavancagem de compras locais",
    "Aumento do mercado e visibilidade",
    "Aumento da sustentabilidade de operaçoes",
    "Desenvolvimento de mecanismos financeiros",
    "Promoçao de mecanismos de desenvolvimento territorial",
    "Outros")
    
    
    f<- new_data$Percepcoes %>% 
      dplyr::mutate(ID = as.character(V1a)) %>% 
      dplyr::select(-V1a, -V1) %>%
      dplyr::inner_join(., groups, by = c("ID" = "ID")) %>%
      dplyr::select(V20_1:V20_13, V1)
    colnames(f) <- c(labs, "V1")
    
    a1<- f %>% 
      gather(-V1 ,key = "var", value = "value")%>%
      group_by(V1, var) %>%
      summarise(cnt = sum(value)) %>%
      mutate(fr1 = round(cnt/sum(cnt), 2)*100) %>%
      ungroup() %>%
      mutate(var = reorder(var, fr1 )) %>%
      ggplot(aes(x =var,  y= fr1, fill = factor(V1)))+
      geom_bar(stat = "identity")+
      geom_text(aes(label = fr1),
                position = position_stack(vjust = .5))+
      ylab("Frecuency") + 
      xlab("Categories")+
      scale_y_continuous(limits=c(0, 100),breaks=seq(0,100,10)) +
      coord_flip()+
      scale_fill_discrete(name = "Grupo")+
      labs( title = "Pergunta V20", caption = "CAIT- IPAM - CALPP")+
      scale_color_brewer(palette = "Dark2")+ theme(aspect.ratio = 0.6)+
      theme(legend.position="bottom",
            # legend.direction ="horizontal",
            legend.text=element_text(size=10),
            legend.key.size = unit(3,"line"),
            plot.caption = element_text(size = 12, hjust = 0.8,
                                        family = "Garamond", color = "black", face = "bold"),
            # panel.background = element_rect(fill = "lightblue",
            #                                 colour = "lightblue",
            #                                 size = 0.5, linetype = "solid"),
            plot.title = element_text(face="bold", size=20),
            axis.title=element_text(face = "bold.italic", size = 20),
            strip.text.x = element_text(size = 16, angle = 0),
            axis.text= element_text(face = "bold", size = 15))
  
    png("Y:/PPA-SNA/documents/images/pergunta_v20.png", width = 12, height = 6, units = 'in',bg = "transparent",res=620)
    plot(a1)
    dev.off()


##### pregunta V21
    
    
    to_likert <- new_data$Percepcoes %>% 
      dplyr::select(-V1)%>%
      dplyr::mutate(V1a = as.character(V1a)) %>% 
      dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
      dplyr::select(V1a, V1b, V21, V1) %>%
      dplyr::mutate(V21= replace(V21, V21 == "1", "Nenhum"), 
                    V21= replace(V21, V21 == "2", "Baixo"),
                    V21= replace(V21, V21 == "3", "Mediano"),
                    V21= replace(V21, V21 == "4", "Alto"),
                    V21= replace(V21, V21 == "5", "Muito alto"),
                    V21= replace(V21, V21 == "0", "N?o responde"),
                    V21= replace(V21, V21 == "6", "Outro")) %>%
      dplyr::select(V21,V1) %>%
      dplyr::filter(!V21 %in% c("Outro", "N?o responde")) %>%
      pivot_wider(names_from = V1, values_from = V21, values_fn = list(V21 = list)) 
    mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
    to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
    
    a1 <- to_likert%>% 
      mutate_all(factor, levels = c("Nenhum", "Baixo","Mediano", "Alto","Muito alto"  ) )%>% 
      likert(items = . , nlevels = 5) 
    plot(a1)
    
    ##### pregunta V22
    
    
    to_likert <- new_data$Percepcoes %>% 
      dplyr::select(-V1)%>%
      dplyr::mutate(V1a = as.character(V1a)) %>% 
      dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
      dplyr::select(V1a, V1b, V22, V1) %>%
      dplyr::mutate(V22= replace(V22, V22 == "1", "Nenhum"), 
                    V22= replace(V22, V22 == "2", "Baixo"),
                    V22= replace(V22, V22 == "3", "Mediano"),
                    V22= replace(V22, V22 == "4", "Alto"),
                    V22= replace(V22, V22 == "5", "Muito alto"),
                    V22= replace(V22, V22 == "0", "Não responde"),
                    V22= replace(V22, V22 == "6", "Outro")) %>%
      dplyr::select(V22,V1) %>%
      dplyr::filter(!V22 %in% c("Outro", "N?o responde")) %>%
      pivot_wider(names_from = V1, values_from = V22, values_fn = list(V22 = list)) 
    mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
    to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
    
    a1 <- to_likert%>% 
      mutate_all(factor, levels = c("Nenhum", "Baixo","Mediano", "Alto","Muito alto"  ) )%>% 
      likert(items = . , nlevels = 5) 
    
    plot(a1)
    
    ##### pregunta V23
    
    
    to_likert <- new_data$Percepcoes %>% 
      dplyr::select(-V1)%>%
      dplyr::mutate(V1a = as.character(V1a)) %>% 
      dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
      dplyr::select(V1a, V1b, V23, V1) %>%
      dplyr::mutate(V23= replace(V23, V23 == "1", "Nenhum"), 
                    V23= replace(V23, V23 == "2", "Baixo"),
                    V23= replace(V23, V23 == "3", "Mediano"),
                    V23= replace(V23, V23 == "4", "Alto"),
                    V23= replace(V23, V23 == "5", "Muito alto"),
                    V23= replace(V23, V23 == "0", "Não responde"),
                    V23= replace(V23, V23 == "6", "Outro")) %>%
      dplyr::select(V23,V1) %>%
      dplyr::filter(!V23 %in% c("Outro", "Não responde")) %>%
      pivot_wider(names_from = V1, values_from = V23, values_fn = list(V23 = list)) 
    mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
    to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
    
    a1 <- to_likert%>% 
      mutate_all(factor, levels = c("Nenhum", "Baixo","Mediano", "Alto","Muito alto"  ) )%>% 
      likert(items = . , nlevels = 5) 
    plot(a1)

    ##### pregunta V24
    
    
    to_likert <- new_data$Percepcoes %>% 
      dplyr::select(-V1)%>%
      dplyr::mutate(V1a = as.character(V1a)) %>% 
      dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
      dplyr::select(V1a, V1b, V24, V1) %>%
      dplyr::mutate(V24= replace(V24, V24 == "1", "Nenhum"), 
                    V24= replace(V24, V24 == "2", "Baixo"),
                    V24= replace(V24, V24 == "3", "Mediano"),
                    V24= replace(V24, V24 == "4", "Alto"),
                    V24= replace(V24, V24 == "5", "Muito alto"),
                    V24= replace(V24, V24 == "0", "Não responde"),
                    V24= replace(V24, V24 == "6", "Outro")) %>%
      dplyr::select(V24,V1) %>%
      dplyr::filter(!V24 %in% c("Outro", "Não responde")) %>%
      pivot_wider(names_from = V1, values_from = V24, values_fn = list(V24 = list)) 
    mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
    to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
    
    a1 <- to_likert%>% 
      mutate_all(factor, levels = c("Nenhum", "Baixo","Mediano", "Alto","Muito alto"  ) )%>% 
      likert(items = . , nlevels = 5) 
    plot(a1)
    
    
    ##### pregunta V25
    
    to_likert <- new_data$Percepcoes %>% 
      dplyr::select(-V1)%>%
      dplyr::mutate(V1a = as.character(V1a)) %>% 
      dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
      dplyr::select(V1a, V1b, V25, V1) %>%
      dplyr::mutate(V25= replace(V25, V25 == "1", "Nenhum"), 
                    V25= replace(V25, V25 == "2", "Baixo"),
                    V25= replace(V25, V25 == "3", "Mediano"),
                    V25= replace(V25, V25 == "4", "Alto"),
                    V25= replace(V25, V25 == "5", "Muito alto"),
                    V25= replace(V25, V25 == "0", "Não responde"),
                    V25= replace(V25, V25 == "6", "Outro")) %>%
      dplyr::select(V25,V1) %>%
      dplyr::filter(!V25 %in% c("Outro", "Não responde")) %>%
      pivot_wider(names_from = V1, values_from = V25, values_fn = list(V25 = list)) 
    mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
    to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
    
    a1 <- to_likert%>% 
      mutate_all(factor, levels = c("Nenhum", "Baixo","Mediano", "Alto","Muito alto"  ) )%>% 
      likert(items = . , nlevels = 5) 
    plot(a1)
    
    ##### pregunta V26
    
    to_likert <- new_data$Percepcoes %>% 
      dplyr::select(-V1)%>%
      dplyr::mutate(V1a = as.character(V1a)) %>% 
      dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
      dplyr::select(V1a, V1b, V26, V1) %>%
      dplyr::mutate(V26= replace(V26, V26 == "1", "Nenhum"), 
                    V26= replace(V26, V26 == "2", "Baixo"),
                    V26= replace(V26, V26 == "3", "Mediano"),
                    V26= replace(V26, V26 == "4", "Alto"),
                    V26= replace(V26, V26 == "5", "Muito alto"),
                    V26= replace(V26, V26 == "0", "Não responde"),
                    V26= replace(V26, V26 == "6", "Outro")) %>%
      dplyr::select(V26,V1) %>%
      dplyr::filter(!V26 %in% c("Outro", "Não responde")) %>%
      pivot_wider(names_from = V1, values_from = V26, values_fn = list(V26 = list)) 
    mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
    to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
    
    a1 <- to_likert%>% 
      mutate_all(factor, levels = c("Nenhum", "Baixo","Mediano", "Alto","Muito alto"  ) )%>% 
      likert(items = . , nlevels = 5) 
    plot(a1)
    
    ##### pregunta V27
      
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V27, V1) %>%
        dplyr::mutate(V27= replace(V27, V27 == "1", "Nenhum"), 
                      V27= replace(V27, V27 == "2", "Baixo"),
                      V27= replace(V27, V27 == "3", "Mediano"),
                      V27= replace(V27, V27 == "4", "Alto"),
                      V27= replace(V27, V27 == "5", "Muito alto"),
                      V27= replace(V27, V27 == "0", "Não responde"),
                      V27= replace(V27, V27 == "6", "Outro")) %>%
        dplyr::select(V27,V1) %>%
        dplyr::filter(!V27 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V27, values_fn = list(V27 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Nenhum", "Baixo","Mediano", "Alto","Muito alto"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)

      ##### pregunta V28
      
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V28, V1) %>%
        dplyr::mutate(V28= replace(V28, V28 == "1", "Nenhum"), 
                      V28= replace(V28, V28 == "2", "Baixo"),
                      V28= replace(V28, V28 == "3", "Mediano"),
                      V28= replace(V28, V28 == "4", "Alto"),
                      V28= replace(V28, V28 == "5", "Muito alto"),
                      V28= replace(V28, V28 == "0", "Não responde"),
                      V28= replace(V28, V28 == "6", "Outro")) %>%
        dplyr::select(V28,V1) %>%
        dplyr::filter(!V28 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V28, values_fn = list(V28 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Nenhum", "Baixo","Mediano", "Alto","Muito alto"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)
      
      ##### pregunta V29
      
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V29, V1) %>%
        mutate(V29 = dplyr::recode(V29, 
                      `1` = "Discordo fortemente", 
                      `2` = "Discordo",
                      `3` = "Incerto",
                      `4` = "Concordo",
                      `5` = "Concordo fortemente",
                      `0` = "Não responde",
                      `6`  = "Outro")) %>%
        dplyr::select(V29,V1) %>%
        dplyr::filter(!V29 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V29, values_fn = list(V29 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Discordo fortemente", "Discordo","Incerto", "Concordo","Concordo fortemente"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)
      ##### pregunta 30
      
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V30, V1) %>%
        mutate(V30 = dplyr::recode(V30, 
                                   `1` = "Discordo fortemente", 
                                   `2` = "Discordo",
                                   `3` = "Incerto",
                                   `4` = "Concordo",
                                   `5` = "Concordo fortemente",
                                   `0` = "Não responde",
                                   `6`  = "Outro")) %>%
        dplyr::select(V30,V1) %>%
        dplyr::filter(!V30 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V30, values_fn = list(V30 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Discordo fortemente", "Discordo","Incerto", "Concordo","Concordo fortemente"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)
      ##### pregunta V31
      
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V31, V1) %>%
        mutate(V31 = dplyr::recode(V31, 
                                   `1` = "Discordo fortemente", 
                                   `2` = "Discordo",
                                   `3` = "Incerto",
                                   `4` = "Concordo",
                                   `5` = "Concordo fortemente",
                                   `0` = "Não responde",
                                   `6`  = "Outro")) %>%
        dplyr::select(V31,V1) %>%
        dplyr::filter(!V31 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V31, values_fn = list(V31 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Discordo fortemente", "Discordo","Incerto", "Concordo","Concordo fortemente"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)
      
      ##### pregunta V32
      
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V32, V1) %>%
        mutate(V32 = dplyr::recode(V32, 
                                   `1` = "Discordo fortemente", 
                                   `2` = "Discordo",
                                   `3` = "Incerto",
                                   `4` = "Concordo",
                                   `5` = "Concordo fortemente",
                                   `0` = "Não responde",
                                   `6`  = "Outro")) %>%
        dplyr::select(V32,V1) %>%
        dplyr::filter(!V32 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V32, values_fn = list(V32 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Discordo fortemente", "Discordo","Incerto", "Concordo","Concordo fortemente"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)
      
      ##### pregunta V33
    
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V33, V1) %>%
        mutate(V33 = dplyr::recode(V33, 
                                   `1` = "Discordo fortemente", 
                                   `2` = "Discordo",
                                   `3` = "Incerto",
                                   `4` = "Concordo",
                                   `5` = "Concordo fortemente",
                                   `0` = "Não responde",
                                   `6`  = "Outro")) %>%
        dplyr::select(V33,V1) %>%
        dplyr::filter(!V33 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V33, values_fn = list(V33 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Discordo fortemente", "Discordo","Incerto", "Concordo","Concordo fortemente"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)
  
      ##### pregunta V34
      
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V34, V1) %>%
        mutate(V34 = dplyr::recode(V34, 
                                   `1` = "Discordo fortemente", 
                                   `2` = "Discordo",
                                   `3` = "Incerto",
                                   `4` = "Concordo",
                                   `5` = "Concordo fortemente",
                                   `0` = "Não responde",
                                   `6`  = "Outro")) %>%
        dplyr::select(V34,V1) %>%
        dplyr::filter(!V34 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V34, values_fn = list(V34 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Discordo fortemente", "Discordo","Incerto", "Concordo","Concordo fortemente"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)
      
      ##### pregunta V35
      
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V35, V1) %>%
        mutate(V35 = dplyr::recode(V35, 
                                   `1` = "Discordo fortemente", 
                                   `2` = "Discordo",
                                   `3` = "Incerto",
                                   `4` = "Concordo",
                                   `5` = "Concordo fortemente",
                                   `0` = "Não responde",
                                   `6`  = "Outro")) %>%
        dplyr::select(V35,V1) %>%
        dplyr::filter(!V35 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V35, values_fn = list(V35 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Discordo fortemente", "Discordo","Incerto", "Concordo","Concordo fortemente"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)
      
      ##### pregunta V36
      
      to_likert <- new_data$Percepcoes %>% 
        dplyr::select(-V1)%>%
        dplyr::mutate(V1a = as.character(V1a)) %>% 
        dplyr::inner_join(., groups, by = c("V1a" = "ID")) %>%
        dplyr::select(V1a, V1b, V36, V1) %>%
        mutate(V36 = dplyr::recode(V36, 
                                   `1` = "Discordo fortemente", 
                                   `2` = "Discordo",
                                   `3` = "Incerto",
                                   `4` = "Concordo",
                                   `5` = "Concordo fortemente",
                                   `0` = "Não responde",
                                   `6`  = "Outro")) %>%
        dplyr::select(V36,V1) %>%
        dplyr::filter(!V36 %in% c("Outro", "Não responde")) %>%
        pivot_wider(names_from = V1, values_from = V36, values_fn = list(V36 = list)) 
      mx <-lapply(to_likert, function(i) unlist(i)%>% length) %>% unlist %>% max
      to_likert <- data.frame(A = c(unlist(to_likert$A), rep(NA,mx- length(unlist(to_likert$A))) ) ,A2 = c(unlist(to_likert$A2),rep(NA, mx- length(unlist(to_likert$A2)))) ,B= unlist(to_likert$B, rep(NA, mx- length(unlist(to_likert$B)))))
      
      a1 <- to_likert%>% 
        mutate_all(factor, levels = c("Discordo fortemente", "Discordo","Incerto", "Concordo","Concordo fortemente"  ) )%>% 
        likert(items = . , nlevels = 5) 
      plot(a1)
      
      
      ## pregunta V37a
      f<- new_data$Percepcoes %>% 
        dplyr::mutate(ID = as.character(V1a)) %>% 
        dplyr::select(-V1a, -V1) %>%
        dplyr::inner_join(., groups, by = c("ID" = "ID")) %>%
        dplyr::select(V37a, V1)
      colnames(f) <- c(labs, "V1")
      
     f %>%
        dplyr::mutate(V37a = replace(V37a, is.na(V37a), "Empty"))%>% 
        group_by(V1, V37a) %>% 
        tally() %>% 
        mutate(fr1 = round(n/sum(n), 2)*100) %>%
        ggplot(aes(x = V1, y = fr1, fill = factor(V37a)))+
        geom_bar(stat= "identity")+
       geom_text(aes(label = paste0(fr1, "%")),
                 position = position_stack(vjust = .5), size = 3)+
        labs(x= "Group", y = "frecuency", title = "Pergunta V38a")+
        scale_fill_discrete(name = "Resultados",  labels = c("Não", "Sim", "Incerto","Não se aplica"))+
        scale_y_continuous(limits=c(0,100), breaks= seq(0,100, by=10)) + 
        labs(caption = "CIAT - IPAM- CALPP")+
        theme(legend.position="bottom",
              # legend.direction ="horizontal",
              legend.text=element_text(size=14),
              legend.key.size = unit(3,"line"),
              plot.caption = element_text(size = 12, hjust = 0.8,
                                          family = "Garamond", color = "black", face = "bold"),
              # panel.background = element_rect(fill = "lightblue",
              #                                 colour = "lightblue",
              #                                 size = 0.5, linetype = "solid"),
              plot.title = element_text(face="bold", size=20),
              axis.title=element_text(face = "bold.italic", size = 20),
              strip.text.x = element_text(size = 16, angle = 0),
              axis.text= element_text(face = "bold", size = 20))
      
     ## pregunta V38a
     f<- new_data$Percepcoes %>% 
       dplyr::mutate(ID = as.character(V1a)) %>% 
       dplyr::select(-V1a, -V1) %>%
       dplyr::inner_join(., groups, by = c("ID" = "ID")) %>%
       dplyr::select(V38a, V1)
    
     
     f %>%
       dplyr::mutate(V38a = replace(V38a, is.na(V38a), "Empty"))%>% 
       group_by(V1, V38a) %>% 
       tally() %>% 
       mutate(fr1 = round(n/sum(n), 2)*100) %>%
       ggplot(aes(x = V1, y = fr1, fill = factor(V38a)))+
       geom_bar(stat= "identity")+
       geom_text(aes(label = paste0(fr1, "%")),
                 position = position_stack(vjust = .5), size = 3)+
       labs(x= "Group", y = "frecuency", title = "Pergunta V38a")+
       scale_fill_discrete(name = "Resultados",  labels = c("Não", "Sim", "Incerto","Não se aplica"))+
       scale_y_continuous(limits=c(0,100), breaks= seq(0,100, by=10)) + 
       labs(caption = "CIAT - IPAM- CALPP")+
       theme(legend.position="bottom",
             # legend.direction ="horizontal",
             legend.text=element_text(size=14),
             legend.key.size = unit(3,"line"),
             plot.caption = element_text(size = 12, hjust = 0.8,
                                         family = "Garamond", color = "black", face = "bold"),
             # panel.background = element_rect(fill = "lightblue",
             #                                 colour = "lightblue",
             #                                 size = 0.5, linetype = "solid"),
             plot.title = element_text(face="bold", size=20),
             axis.title=element_text(face = "bold.italic", size = 20),
             strip.text.x = element_text(size = 16, angle = 0),
             axis.text= element_text(face = "bold", size = 20))