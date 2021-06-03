##### redes PPPA

g=gc;rm(list = ls())

suppressMessages(library(dplyr));suppressMessages(library(Hmisc))
suppressMessages(library(raster));suppressMessages(library(ggplot2))
suppressMessages(library(reshape));suppressMessages(library(RColorBrewer))
suppressMessages(library(maptools));suppressMessages(library(sp))
suppressMessages(library(maps));suppressMessages(library(rgdal))
suppressMessages(library(spdep));suppressMessages(library(viridis))
suppressMessages(library(broom));suppressMessages(library(readxl))
suppressMessages(library(spdep));suppressMessages(library(leaflet))
suppressMessages(library(classInt));suppressMessages(library(rjson))
suppressMessages(library(plyr));suppressMessages(library(tidyr))
suppressMessages(library(circlize));suppressMessages(library(sna))
suppressMessages(library(statnet));suppressMessages(library(igraph))
suppressMessages(library(network));suppressMessages(library(qgraph))
suppressMessages(library(magrittr));suppressMessages(library(cluster))
suppressMessages(library(cluster.datasets)); suppressMessages(library(cowplot))
suppressMessages(library(NbClust));suppressMessages(library(clValid))
suppressMessages(library(ggfortify));suppressMessages(library(clustree))
suppressMessages(library(dendextend));suppressMessages(library(factoextra))
suppressMessages(library(FactoMineR));suppressMessages(library(corrplot))
suppressMessages(library(GGally)); suppressMessages(library(knitr))
suppressMessages(library(kableExtra)); suppressMessages(library(intergraph))

#### rescale function ------------
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}


#####network R1_negocios
bb <- read.table("clipboard",sep="\t",header = T)
aa<- bb %>% dplyr::select(R_Start_V1a,R_Destiny_V1a) %>% arrange(R_Start_V1a)  %>% drop_na(.) #%>% mutate(val=1)
g<- network::network(aa,matrix.type="edgelist")

# get names para agregar variables
n<- g$gal$n
Nodes<- lapply(1:n, function(v){
  g$val[[v]]$vertex.names
  
})

nodesList<- as.data.frame(do.call(rbind,Nodes))
colnames(nodesList)[1]<- "Nodes" ## renombrando la columna 
g <- graph_from_data_frame(d=aa, vertices=nodesList, directed=TRUE)

ge<- asNetwork(g) ### exporto a version network
sub_g<- bb %>% filter(R_Start_V1a %in% nodesList[[1]])  ### subconjunto con los nodos correctos
ge<- set.edge.value(ge,"intensidad",sub_g[["R1b"]])     ### agrego variable de intensidad
gy <- asIgraph(ge)                                      ### convert network file to igraph

degi<- degree(graph = gy,mode = "out",loops = NA,normalized = TRUE)  ### obtenemos degree de la red

Isolated = which(degree(gy)==0)            ### eliminamos isolated
gy = delete.vertices(gy, Isolated)         ### borro nodos aislados 

e <- get.edgelist(gy, names=FALSE) ## creando visualizacion


l0 <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(gy),area=10*(vcount(gy)^5))  ### creando coordenadas para la visualizacion
LO2 = LO[-Isolated,]

degi<- degree(graph = gy,mode = "all",loops = NA,normalized = FALSE)  ### obtenemos degree de la red

E(gy)$arrow.size <- .3
# V(gy)$size<- 6
V(gy)$size<- (rescale(degi,1,20))/2
plot(gy, 
     layout = LO2,edge.width=(E(gy)$intensidad)*1.2,
     edge.color=adjustcolor("orange", alpha.f = .6),
     vertex.color="grey",
     vertex.label.color="black",
     vertex.label.dist=0.1,
     vertex.label.cex=1,
     main="Investimento") ### plotting










# LO = layout_with_fr(gy)
# 
# LO2 = LO[-Isolated,]






# g<- network::network(aa,matrix.type="edgelist")
# plot(g)



# # get names para agregar variables
# n<- g$gal$n
# Nodes<- lapply(1:n, function(v){
#   g$val[[v]]$vertex.names
#   
# })
# 
# nodesList<- as.data.frame(do.call(rbind,Nodes))
# colnames(nodesList)[1]<- "Nodes" ## renombrando la columna 

# sub_g<- bb %>% filter(R_Start_V1a %in% nodesList[[1]])
# 
# 
# ge<- set.edge.value(ge,"intensidad",sub_g[["R1b"]])
# gy <- asIgraph(ge)### convert network file to igraph
# 
# #### rescale function ------------
# rescale <- function(nchar,low,high) {
#   min_d <- min(nchar)
#   max_d <- max(nchar)
#   rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
#   rscl
# }
# 
# degi<- degree(graph = gy,mode = "out",loops = NA,normalized = TRUE)
# 
# Isolated = which(degree(gy)==0)
# yy = delete.vertices(gy, Isolated)
# LO = layout_with_fr(gy)
# 
# LO2 = LO[-Isolated,]
l02 <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g),area=5*(vcount(g)^2))

E(gy)$arrow.size <- .3


plot(gy, layout = LO2,edge.width=(E(gi)$intensidad))












#### atributos R1b
# Muito baixa - menos de uma vez por ano
# Baixa - uma ou duas vezes ao ano
# Regular - três a seis vezes ao ano
# Frequente - sete a doze vezes ao ano 
# Muito frequente - mais de uma vez por mês
# Outro - especificar
# 














g<- network::network(aa,matrix.type="edgelist")
# plot(g)



# get names para agregar variables
n<- g$gal$n
Nodes<- lapply(1:n, function(v){
  g$val[[v]]$vertex.names
  
})

nodesList<- as.data.frame(do.call(rbind,Nodes))
colnames(nodesList)[1]<- "Nodes" ## renombrando la columna 

sub_g<- bb %>% filter(R_Start_V1a %in% nodesList[[1]])


ge<- set.edge.value(ge,"intensidad",sub_g[["R1b"]])
gy <- asIgraph(ge)### convert network file to igraph

#### rescale function ------------
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}

degi<- degree(graph = gy,mode = "out",loops = NA,normalized = TRUE)

Isolated = which(degree(gy)==0)
yy = delete.vertices(gy, Isolated)
# LO = layout_with_fr(gy)
# 
# LO2 = LO[-Isolated,]
l02 <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g),area=5*(vcount(g)^2))

E(gy)$arrow.size <- .3


plot(gy, layout = LO2,edge.width=(E(gi)$intensidad))








####graficas
png("/dapadfs.cgiarad.org/workspace_cluster_13/PPA-SNA/documents/images/negocios.png",
    width = 12, height = 10, units = 'in', bg = "transparent",res=620)# width = 800, height = 500)

# op <- par(mar = c(2,0,2,0),mfrow=c(1,2),mgp=c(1,0.5,0))#,mgp=c(1,0.5,0)
e <- get.edgelist(gi, names=FALSE)
# V(gi)$size=(rescale(degi,0,max(degi)))
V(gi)$size<- 3
V(gi)$arrow.size <- .9

# l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(gi),area=2*(vcount(gi)^2))
l <- qgraph.layout.fruchtermanreingold(e,vcount =vcount(gi),niter = 500)

plot(gi,vertex.label=V(gi)$vertex.names, vertex.label.cex=1,
     edge.color=adjustcolor("orange"),
     layout =l,
     edge.width=E(gi)$intensidad/2,#vertex.frame.color="#ffffff",
     vertex.color="grey",  #edge.color="grey",
     # vertex.label.color="black",
     # vertex.label.dist=1,
     main="Relação de negócios intensidade")
# par(op)

dev.off()


