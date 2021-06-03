##############
# script para procesar las redes  de la PPA
# author: Andres camilo mendez
# author2: carlos gonzales

library(pacman)
pacman::p_load(tidyverse, dplyr, igraph, statnet,qgraph )

actores <- read.csv("Y:/PPA-SNA/input_data/actores_ppa/listado_empresas_A_2.csv")

ties_invest <- read.delim("clipboard", header = F, stringsAsFactors = F)

ties_invest <- raw_data$R2_investimento %>% 
  dplyr::mutate(R_start = paste0(R_Start_V1a), R_Destiny_V1a = as.character(R_Destiny_V1a))%>%
  dplyr::select(R_start, R_Destiny_V1a, R2a)

df <- ties_invest %>% mutate(c_names = gsub("[0-9]+\\. ", "", V1), V1 = NULL) %>% 
  dplyr::filter(V2 == "Sim")

edges <- data.frame(source = rep("IDESAM", nrow(df)), target = df$c_names, weigth = rep(1, nrow(df)))

nodes <- data.frame(name = c("IDESAM", df$c_names), id = 1:(nrow(df)+1))

g <- graph_from_data_frame(d=ties_invest, directed=FALSE)

plot(g)
coords <- layout_with_fr(g,dim=3)
rglplot(g, layout= coords)

ties_ct <- read.delim("clipboard", header = F, stringsAsFactors = F)
df_ct <- ties_ct %>% mutate(c_names = gsub("[0-9]+\\. ", "", V1), V1 = NULL) 

edges_ct <- data.frame(source = rep("IDESAM", nrow(df_ct)), target = df_ct$c_names, weigth = rep(1, nrow(df_ct)))

nodes_ct <- data.frame(name = c("IDESAM", df_ct$c_names), id = 1:(nrow(df_ct)+1))

g <- graph_from_data_frame(d=edges_ct, vertices=nodes_ct, directed=FALSE)

V(g)$color <- NA
V(g)$color[V(g)$name %in% df_ct[df_ct$V2 == "Sim", "c_names"] ] <- "red"
V(g)$color[V(g)$name %in% df_ct[df_ct$V2 == "Não" | df_ct$V2 == "não", "c_names"] ] <- "gold"
V(g)$color[V(g)$name %in% "IDESAM" ] <- "yellow"
plot(g)
legend("bottomleft", legend=c("PPA contexto", "Não PPA contexto", "IDESAM"), 
       pch=21, pt.bg=c("red", "gold", "yellow"), pt.cex=2, bty="n")



##########################################
## redecillas por ahi E##################
########################################

pacman::p_load(igraph, network, sna, ggraph, visNetwork, threejs, networkD3, ndtv, tidyverse, readxl)


nt1 <- read_excel("D:/OneDrive - CGIAR/Documents/Dados_entrevistas_2019_grupos_A_e_B_para_CIAT_(20nov19).xlsx", sheet = "R2_investimento")

#ntx <- read_excel("D:/OneDrive - CGIAR/Documents/Dados_entrevistas_2019_grupos_A_e_B_para_CIAT_(20nov19).xlsx", sheet = "R3_coop_técnica")

groups <- read_excel("D:/OneDrive - CGIAR/Documents/Dados_entrevistas_2019_grupos_A_e_B_para_CIAT_(20nov19).xlsx", sheet = "Empresas_orgs") %>% 
  dplyr::mutate(ID= as.character(ID)) 
#%>% dplyr::select(-...6)

d <- nt1 %>% 
  dplyr::select( R_Start_V1a,  R_Destiny_V1a, R2c, R2a) %>%
  dplyr::mutate_all(as.character)

net <- graph_from_data_frame(d=  d,
                             vertices = groups %>% dplyr::select(ID, V1),
                             directed=T)%>%
  igraph::delete.vertices(., igraph::degree(.) == 0)


all(unique(d$R_Start_V1a) %in% unique(groups$ID))
all(unique(d$R_Destiny_V1a) %in% unique(groups$ID))
unique(d$R_Destiny_V1a)


colrs <- c("olivedrab3", "tomato", "gold", "black")
V(net)$color <- colrs[as.numeric(as.factor(V(net)$V1))]
deg <- igraph::degree(net, mode="all", normalized = F)
## tamaño de los vertices
V(net)$size <- sqrt(deg)*10
## tamaño de los edges
E(net)$width <- as.numeric(E(net)$R2c)

minC <- rep(-Inf, vcount(net))
maxC <- rep(Inf, vcount(net))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(net, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC )
#co <- norm_coords(co, ymin=-1, ymax=1, xmin=-1, xmax=1)
#l <- qgraph.layout.fruchtermanreingold(e, vcount=vcount(g), area=5*(vcount(g)^2))

par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(net, 
     rescale = FALSE,
     layout= co, 
     edge.arrow.size=0.4, rescale=FALSE,
     edge.color=adjustcolor("gray50", alpha.f = .6),
     xlim=range(co[,1]), 
     ylim=range(co[,2]), 
     vertex.label.dist=0,
     vertex.label= V(net)$name,
     vertex.label.family = "Arial",
     vertex.label.font	= 2,
     vertex.label.cex = 0.7,
     vertex.label.dist = 150,
     vertex.label.degree = -pi/2,
     vertex.label.color="black")

legend("topright" , c("Group A","Group A2", "Group B", "Group C"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n")

##### cambiar color dependiendo de si la interaccion se da dentro de la PPA o no

# cambiar color dependiendo el grupo
edge.start <- ends(net, es=E(net), names=T)[,1]
edge.col <- nt1 %>% 
  dplyr::select( R_Start_V1a,  R_Destiny_V1a, R2a) %>%
  bind_cols(., edge.start = edge.start) %>%
  dplyr::mutate(col = if_else(R2a == 1, "yellow4", "grey70")) %>%
  pull(col)

plot(net, layout=co, 
     edge.arrow.size=0.125, rescale=FALSE,
     edge.color=edge.col,
     xlim=range(co[,1]), 
     ylim=range(co[,2]), 
     vertex.label.dist=0,
     vertex.label= V(net)$name,
     vertex.label.family = "Arial",
     vertex.label.font	= 2,
     vertex.label.cex = 0.7,
     vertex.label.dist = 150,
     vertex.label.degree = -pi/2,
     vertex.label.color="black",
     asp = 0)

legend("topright" , c("Group A","Group A2", "Group B", "Group C"), pch= c(21, 21, 21, 21),
       col="#777777", pt.bg= c(colrs), pt.cex=2, cex=.8, bty="n")
legend("bottomright", c( "In PPA", "Out PPA"), lty = c(1,1), col = c("yellow4", "grey70"),
       lwd = c(3,3), pt.cex=2, cex=.8, bty="n")


################ separar grafico dependiendo si el link esta dentro de la PPA o no

x <- data.frame(edg = get.edgelist(net),  cat = E(net)$R2a, stringsAsFactors = F) %>% 
  dplyr::filter(cat == "1") %>% pull(edg.1) %>% unique() 

y <- data.frame(edg = get.edgelist(net),  cat = E(net)$R2a, stringsAsFactors = F) %>% 
  dplyr::filter(cat == "1") %>% pull(edg.2) %>% unique() 

V(net)$dm <- "a"
V(net)$dm[ V(net)$name %in% c(x, y)  ]<- "x"

G_Grouped <- net
E(G_Grouped)$weight = 1
## Add edges with high weight between all nodes in the same group
for(i in unique(V(net)$dm)) {
  GroupV = which(V(net)$dm == i)
  G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=2))
} 

E(G_Grouped)$weight

## Now create a layout based on G_Grouped
set.seed(567)
co <- layout_with_fr(G_Grouped, weights = E(G_Grouped)$weight )

## Use the layout to plot the original graph
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(net, 
     layout=co, 
     edge.arrow.size=0.3,
     vertex.size = sqrt(igraph::degree(net, mode="all", normalized = F))*2,
     vertex.label= V(net)$name,
     vertex.color =  colrs[as.numeric(as.factor(V(net)$V1))],#rainbow(3, alpha=0.4)[as.numeric(as.factor(V(net)$dm))],
     edge.width = as.numeric(E(net)$R2c),
     edge.color=edge.col,
     asp = 0)


## Sankey diagram

mtx <- data.frame(id = V(net)$name, group = V(net)$V1, coords = co, stringsAsFactors = F) %>% as_tibble()

dsts <- nt1 %>% dplyr::select(R_Start_V1a, R_Destiny_V1a ) %>% mutate_all(as.character)  %>%
  apply(., 1, function(i){
    #cat(i, "\n")
    start <- mtx %>% dplyr::filter(id == as.character(i[1])) %>% dplyr::select(coords.1 ,coords.2)
    ends <- mtx %>% dplyr::filter(id == as.character(i[2])) %>% dplyr::select(coords.1 ,coords.2)
    dst <- dist(rbind(start, ends), method = "euclidean")
    return(dst)
  })



links <- nt1 %>% dplyr::select(R_Start_V1a, R_Destiny_V1a )%>%  
  add_column(dist = dsts*100, R2a = E(net)$R2a) %>% 
  as.data.frame() %>%
  dplyr::filter(R2a == 1)
nodes <- data.frame(id = V(net)$name, group = V(net)$V1, stringsAsFactors = F )%>% mutate(id = as.character(id))

links$IDsource <- match(links$R_Start_V1a, nodes$id)-1 
links$IDtarget <- match(links$R_Destiny_V1a, nodes$id)-1

networkD3::sankeyNetwork(
              Links = links, 
              Nodes = nodes,
              Source = "IDsource", 
              Target = "IDtarget", 
              NodeID = "id",
              NodeGroup = "group", 
              LinkGroup = "R2a",
              Value = "dist",
              fontSize = 12,
              unit = "ecd",
              nodeWidth = 30) 






## acomodar nodos sengun coordenadas

order_grp <- function(init = c(1,10), max.x = 3, ids = c("1", "5", "7", "8", "9") ){
  fn <- list()
  int <- init
  k <- 1
  for(i in 1:length(ids)){
    if(i %% (max.x + 1) == 0 ){
      int <- init
     int <-  int + c(0.5, -k)
     k <- k + 1
    }else{
     int <-  int + c(0.5, 0)
    }
   fn[[i]] <- data.frame(name = ids[i], x = int[1], y = int[2], stringsAsFactors = F)
  }
  return(fn)
}

dta <- data.frame(id = V(net)$name, deg = igraph::degree(net, mode="all", normalized = F), gr = V(net)$V1, stringsAsFactors = F)

gr_A2 <- order_grp(init = c(1,10), max.x = 3, ids =  dta %>% dplyr::filter(gr == "A2") %>% pull(id)  ) %>% do.call(rbind, .)
gr_A <- order_grp(init = c(1,7), max.x = 5, ids =  dta %>% dplyr::filter(gr == "A") %>% pull(id)) %>% do.call(rbind, .)
gr_B <- order_grp(init = c(4,8), max.x = 3, ids =  dta %>% dplyr::filter(gr == "B") %>% pull(id)) %>% do.call(rbind, .)
gr_c <- order_grp(init = c(6,8), max.x = 3, ids =  dta %>% dplyr::filter(gr == "C") %>% pull(id)) %>% do.call(rbind, .)

l <- rbind(gr_A2, gr_A, gr_B, gr_c)  %>% 
  right_join(., dta, by = c("name" = "id")) %>% 
  dplyr::select(x,y) %>%
  as.matrix()

grps <- list(gr_A2$name, gr_A$name, gr_B$name, gr_c$name)

# cambiar color a los links basados en los grupos
inc.edges <- incident(net,  V(net)[name =="42"], mode="all")
ecol <- rep("gray80", ecount(net))
ecol[inc.edges] <- "#E84E0E"
vcol <- rep("grey40", vcount(net))
vcol[V(net)$name == "42" ] <- "gold"


#edge.color= edge.col
plot(net, 
     edge.arrow.size = 0.8 , 
       #rgb(68/255, 67/255, 66/255, alpha = 0.5),
     edge.curved=.3, 
     layout = l, 
     vertex.size = plotrix::rescale(igraph::degree(net, mode="all", normalized = F), c(10, 20))*0.8,
     edge.color = "gray70",
     mark.groups = grps, 
     mark.col= c( "#ECC19A", "#D8D3CE", "#ECD89A", "#605E5C"), 
     mark.border= c( "#EE9647", "#C6C2BD", "#EBC652", "#3E3D3C")) 

legend(x=-1.5, y=-0.5, c("Group A","Group A2", "Group B", "Group C"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


net3 <- network(x = nt1 %>% dplyr::select( R_Start_V1a,  R_Destiny_V1a),  vertex.attr= groups %>% dplyr::select(ID, V1), matrix.type="edgelist", 
               loops=F, multiple=F, ignore.eval = F)

net3[,]
net3 %n% "net.name" <- "Media Network" #  network attribute
net3 %v% "V1"    # Node attribute


net3 %v% "col" <- c("gray70", "tomato", "gold")[net3 %v% "V1"]
plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col")

#################################################################
# red agrupando nodos con edges de la misma caracteristica #####
###############################################################

x <- data.frame(edg = get.edgelist(net),  cat = E(net)$R2a, stringsAsFactors = F) %>% 
  dplyr::filter(cat == "1") %>% pull(edg.1) %>% unique() 

y <- data.frame(edg = get.edgelist(net),  cat = E(net)$R2a, stringsAsFactors = F) %>% 
  dplyr::filter(cat == "1") %>% pull(edg.2) %>% unique() 

V(net)$dm <- "a"
V(net)$dm[ V(net)$name %in% c(x, y)  ]<- "x"

G_Grouped <- net
E(G_Grouped)$weight = 1
## Add edges with high weight between all nodes in the same group
for(i in unique(V(net)$dm)) {
  GroupV = which(V(net)$dm == i)
  G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=2))
} 


## Now create a layout based on G_Grouped
set.seed(289)
co <- layout_with_fr(G_Grouped, weights = E(G_Grouped)$weight )
# add triangle shape to shape list
igraph::add_shape("triangle", clip=igraph::shapes("circle")$clip,
                  plot=mytriangle)
## Use the layout to plot the original graph
par(mar = c(0.5, 0.5, 0.5, 0.5), bg = NA)
plot(net, 
     layout=co, 
     edge.arrow.size=0.85,
     vertex.shape = vtx_shp$shape,
     vertex.size = plotrix::rescale(igraph::degree(net, mode="all", normalized = F), c(10, 20))*0.8,
     vertex.label= V(net)$name,
     vertex.color =  colrs[as.numeric(as.factor(V(net)$V1))],#rainbow(3, alpha=0.4)[as.numeric(as.factor(V(net)$dm))],
     edge.width = as.numeric(E(net)$R2c),
     edge.color=edge.col,
     asp = 0)
legend("topright" , c("Group A","Group A2", "Group B", "Group C", "SHAPE","Partner", "Reporter", "Both"), pch = c(21, 21, 21,21, 15,15, 17, 16),
       col= c("white","black", "black", "black"), pt.bg= c(colrs, "white","black", "black", "black"), pt.cex=2, cex=.8, bty = "n",  title="GROUPS")
legend("bottomright", c( "In PPA", "Out PPA", "Não sei dizer", "Outro"), lty = c(1,1, 1, 1), col= c("darkgreen", "grey70", "orange", "red"), lwd = c(3,3,3,3), pt.cex=2, cex=.8, bty="n", title = "R2c")
 
#################################################################
## Read agrupada como JF lo pidio pero no lo miro ##############
###############################################################

order_grp <- function(init = c(1,10), max.x = 3, ids = c("1", "5", "7", "8", "9") ){
  fn <- list()
  int <- init
  k <- 1
  for(i in 1:length(ids)){
    if(i %% (max.x + 1) == 0 ){
      int <- init
      int <-  int + c(0.5, -k)
      k <- k + 1
    }else{
      int <-  int + c(0.5, 0)
    }
    fn[[i]] <- data.frame(name = ids[i], x = int[1], y = int[2], stringsAsFactors = F)
  }
  return(fn)
}

dta <- data.frame(id = V(net)$name, deg = igraph::degree(net, mode="all", normalized = F), gr = V(net)$V1, stringsAsFactors = F)

gr_A2 <- order_grp(init = c(1,10), max.x = 3, ids =  dta %>% dplyr::filter(gr == "A2") %>% pull(id)  ) %>% do.call(rbind, .)
gr_A <- order_grp(init = c(1,7), max.x = 5, ids =  dta %>% dplyr::filter(gr == "A") %>% pull(id)) %>% do.call(rbind, .)
gr_B <- order_grp(init = c(4,8), max.x = 3, ids =  dta %>% dplyr::filter(gr == "B") %>% pull(id)) %>% do.call(rbind, .)
gr_c <- order_grp(init = c(6,8), max.x = 3, ids =  dta %>% dplyr::filter(gr == "C") %>% pull(id)) %>% do.call(rbind, .)

l <- rbind(gr_A2, gr_A, gr_B, gr_c)  %>% 
  right_join(., dta, by = c("name" = "id")) %>% 
  dplyr::select(x,y) %>%
  as.matrix()

grps <- list(gr_A2$name, gr_A$name, gr_B$name, gr_c$name)

# add triangle shape to shape list
igraph::add_shape("triangle", clip=igraph::shapes("circle")$clip,
                  plot=mytriangle)
par(bg = NA)
plot(net, 
     edge.arrow.size = 0.85 , 
     #rgb(68/255, 67/255, 66/255, alpha = 0.5),
     edge.curved=.3, 
     layout = l, 
     vertex.size = plotrix::rescale(igraph::degree(net, mode="all", normalized = F), c(10, 20))*0.8,
     edge.color = edge.col,
     vertex.shape = vtx_shp$shape,
     mark.groups = grps, 
     mark.col= c( "#ECC19A", "darkolivegreen1", "#ECD89A", "gray90"), 
     mark.border= c( "#EE9647", "darkolivegreen3", "#EBC652", "#3E3D3C")) 

legend("topright" , c("Group A","Group A2", "Group B", "Group C", "SHAPE","Receptor", "Emisor", "Emisor/recep"), pch = c(21, 21, 21,21, 15,15, 17, 16),
       col= c("white","black", "black", "black"), pt.bg= c(colrs, "white","black", "black", "black"), pt.cex=2, cex=.8, bty = "n",  title="GROUPS")
legend("bottomright", c( "In PPA", "Out PPA", "Não sei dizer", "Outro"), lty = c(1,1, 1, 1), col= c("darkgreen", "grey70", "orange", "red"), lwd = c(3,3,3,3), pt.cex=2, cex=.8, bty="n", title = "R2a")





