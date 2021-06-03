################################################################# TEXT MINING
################################################################# TEXT MINING
################################################################# TEXT MINING
################################################################# TEXT MINING
################################################################# TEXT MINING
################################################################# TEXT MINING

### analisis de topicos y modelacion de topicos

library(stm)
library(quanteda)
w_no<- c("soil", "precision", "data","agriculture","data","agriculture", "system", "based", "field", "crop","system" )

df_filter_dfm<- df_abs_w %>% filter(!word %in% w_no) %>% anti_join(stop_words) %>%
  dplyr::count(Year,word,sort=TRUE) %>% 
  cast_dfm(Year,word,n)


tipic_model<- stm(df_filter_dfm,K = 10,init.type = "Spectral")
topic_mode_all<- tipic_model
summary(topic_mode_all)
tb_beta<- tidy(topic_mode_all)

write.csv(tb_beta %>% filter(beta==0),"./pic/beta_bid_ag_tech.csv")

png(filename = paste("./pic/beta_bid_ag_tech_apliados.png",sep = ""),bg = "transparent",
    width = 10, height = 10, units = 'in',res = 600)

tb_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics_ tittles")

dev.off()


################################################################# SNA
################################################################# SNA
################################################################# SNA
################################################################# SNA
################################################################# SNA
##### ejemplo de comercio de arroz

suppressMessages(library(igraph))
suppressMessages(library(intergraph))
suppressMessages(library(statnet))


netWorksTemp<- lapply(1: length(riceTpes),function(i){
  xdata<- riceTpes[[i]]
  
  
  for(y in 1:length(periododsList)){
    ## filtramos la los datos de tipo de commoditie por yr/reporter/partner/quantity
    xdataSub<- dplyr::filter(xdata, Year==periododsList[[y]]) %>% 
      dplyr::select(Reporter, Partner, Val) %>% 
      dplyr::filter(., Val!=0) 
    # xdataSub$Val<- log10(xdataSub$Val)
    xdataSub$Val<- xdataSub$Val/1000000
    #Filtramos por los valores menores al 1er quantile
    q<- quantile(xdataSub$Val,probs = 0.55)
    xdataSub<- dplyr::filter(xdataSub, Val>=q)
    attrValue<- xdataSub
    # attrValue$QL<- log10(attrValue$Q) # logaritmo de los niveles exportados
    
    
    ###### Filtrando solo datos de reportes and partner
    rRed<- xdataSub[c("Reporter","Partner")] ### nodos
    
    ###### crear objeto tipo red
    netWorksPeriod[[y]] <- network::network(rRed,matrix.type="edgelist") #### ojo andres aca creamos la red
    
    ######  esto es tonto pero se obtiene un objeto con los nodos en el mismo orden del objeto super para agregar atributos
    n<- netWorksPeriod[[y]]$gal$n
    Nodes<- lapply(1:n, function(v){
      netWorksPeriod[[y]]$val[[v]]$vertex.names
      
    })
    ##### apilamiento de los nodos
    nodesList<- as.data.frame(do.call(rbind,Nodes))
    colnames(nodesList)[1]<- "nodes" ## renombrando la columna 
    
    
    ###### Agregar atributo valor del edge, agregamos log(quatity export) 
    network::set.edge.attribute(netWorksPeriod[[y]], "Val", c(attrValue[,3]))
    
    list.edge.attributes(netWorksPeriod[[y]]) # lista de edge attributos
    summary(netWorksPeriod[[y]] %e% "Val") # summary de un atributo especifico
    as.sociomatrix(netWorksPeriod[[y]],"Val") # ver la matrix especifico del atributo
    
    ###### Creación de objectos regiones correspondientes a la red filtrada
    ### America
    treal1<- nodesList %>% dplyr::filter(Nodes %in% caribbean) %>% dplyr::mutate(., zone="caribbean")
    treal2<- nodesList %>% dplyr::filter(Nodes %in% Central_America) %>% dplyr::mutate(., zone="Central_America")
    treal3<- nodesList %>% dplyr::filter(Nodes %in% South_America) %>% dplyr::mutate(., zone="South_America")
    treal4<- nodesList %>% dplyr::filter(Nodes %in% Northern_America) %>% dplyr::mutate(., zone="Northern_America")
    
   
    ##### apilan los subsets por regiones
    r<- rbind(treal1,treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,treal15,
              treal16,treal17,treal18,treal19, treal20)
    r$nodes[duplicated(r$nodes)] # se eliminan nodos duplicados
    
    ##### join entre la lista de nodos y dataframe de las regiones
    rjoinRegion<- dplyr::left_join(nodesList, r, by = "nodes")
    
    
    ###### Agregated attribute to network file, region 
    network::set.vertex.attribute(netWorksPeriod[[y]], "region", c(rjoinRegion[,2]))
    
    
    ################################ calculo de indicadores sobre los atributos, estadistica descriptiva####
    ################################ calculo de indicadores sobre los atributos, estadistica descriptiva####
    
    #### Total exports by Reporter
    SumExports<- xdataSub %>% dplyr::filter(., Reporter %in% as.vector(nodesList[,1])) %>% 
      dplyr::group_by(Reporter) %>% 
      dplyr::mutate(., total=sum(Val)) %>% dplyr::select(Reporter,total) 
    SumExports<- SumExports[!duplicated(SumExports),]
    SumExports$Year<- periododsList[[y]]
    totalExports<- sum(SumExports$total) #### total exports by total year
    
    ##### Calculated of rate of exports
    SumExports$RateExpor<- (SumExports$total/totalExports)*100
    colnames(SumExports)[1]<- "nodes"
    
    
    #### agregar % exportaciones como atributo a cada nodo
    SumExports<- SumExports[c("nodes","RateExpor")]
    nodesList$nodes<- as.character(nodesList$nodes)
    
    #### Join local 
    rjoin1<- dplyr::left_join(nodesList, SumExports, by = "nodes")
    rjoin1$RateExpor[is.na(rjoin1$RateExpor)]<- 0
    
    #### agregar atributo de rateExports
    network::set.vertex.attribute(netWorksPeriod[[y]], "RateExpor", c(rjoin1[,2]))
    
    
    ##### agregar variable de liderazgo en importaciones, esta parte es para crear atributos apartir de los datos
    SumExports$Top<- NA
    SumExports$Top[SumExports$RateExpor>=1]<- 1
    SumExports$Top[is.na(SumExports$Top)]<- 0
    
    SumExports<- SumExports[c("nodes","Top")]
    
    ##### join local top
    rjoinT<- dplyr::left_join(nodesList, SumExports, by = "nodes")
    rjoinT$Top[is.na(rjoinT$Top)]<- 0
    
    #### agregar atributo de Ranking of exporters
    network::set.vertex.attribute(netWorksPeriod[[y]], "Top", c(rjoinT[,2]))
    
    
    ###########################  Aggregating shocks attributes to network objetc  #####################
    # Filtrar 
    nodesList$nodes<- as.character(nodesList$nodes)
    n<- nodesList[,1]
    YtempHWL<- shockYields %>% dplyr::filter(., year==periododsList[[y]]) %>% 
      dplyr:: filter(.,nodes %in% n) %>% dplyr::select(nodes, cat_localHV)
    
    YtempHWW<- shockYields %>% dplyr::filter(., year==periododsList[[y]]) %>% 
      dplyr:: filter(.,nodes %in% n) %>% dplyr::select(nodes, cat_f_hvW)
    
    rownames(YtempHWL)<- 1:nrow(YtempHWL)
    
    #### join local shocks local yields 
    rjoin2<- dplyr::left_join(nodesList, YtempHWL, by = "nodes")
    rjoin2$cat_localHV<- as.character(rjoin2$cat_localHV)
    rjoin2$cat_localHV[is.na(rjoin2$cat_localHV)]<- "NoProducers"
    
    network::set.vertex.attribute(netWorksPeriod[[y]], "cat_localHV", c(rjoin2[,2]))
    
    #join foreing shocks
    rjoin3<- dplyr::left_join(nodesList, YtempHWW, by = "nodes")
    rjoin3$cat_f_hvW<- as.character(rjoin3$cat_f_hvW)
    rjoin3$cat_f_hvW[is.na(rjoin3$cat_f_hvW)]<- "NoProducers"
    
    network::set.vertex.attribute(netWorksPeriod[[y]], "cat_f_hvW", c(rjoin3[,2]))
    
    #join dummies shocks
    dummies<- shockYields
    # require(dplyr)
    dummies<- dummies %>% dplyr::select(year, nodes,cat_localHV, cat_f_hvW)
    dummies$shock<- paste(dummies$cat_localHV, "+",dummies$cat_f_hvW)
    
    YtempShock<- dummies %>% dplyr::filter(., year==periododsList[[y]]) %>% 
      dplyr:: filter(.,nodes %in% n) %>% dplyr::select(nodes, shock)
    
    rjoin4<- dplyr::left_join(nodesList, YtempShock, by = "nodes")
    rjoin4$shock<- as.character(rjoin4$shock)
    rjoin4$shock[is.na(rjoin4$shock)]<- "NoProducers"
    
    network::set.vertex.attribute(netWorksPeriod[[y]], "Shock", c(rjoin4[,2]))
    
   
    #### eliminar isolates
    network::delete.vertices(ourregion,isolates(ourregion))

    
############################################# Calculando algunas metricas #############################################
############################################# Calculando algunas metricas #############################################
############################################# Calculando algunas metricas #############################################
    
    ourregion<- netWorksPeriod[[y]]
    
    ### centralidad
    deg <- sna::degree(ourregion,gmode="graph")
    cls <-  sna::closeness(ourregion,gmode="graph")  
    bet <-  sna::betweenness(ourregion,gmode="graph")
    detach(package:igraph)
    suppressMessages(library(statnet))
    CentraBet<-centralization(ourregion,betweenness)
    CentraDeg<- centralization(ourregion,degree)
    
    
    ### data igraph y generacion de metricas basicas 
    detach(package:statnet)
    iDHHS <- asIgraph(ourregion)### convert network file to igraph
    idensi<- graph.density(iDHHS)
    cw <- cluster_walktrap(iDHHS)
    clusMember<- as.vector(membership(cw))
    mod<- modularity(cw)
    gden<- graph.density(iDHHS)
    meanDeg<- mean(degree(iDHHS))
    trans<- transitivity(iDHHS)
    transLocal<- transitivity(iDHHS, type = "local")
    assor<- assortativity.degree(iDHHS, directed = T)
    
    ############## metricas basicas stacking ##########################
    dataSUM<- data.frame(nodes= nodesListLAC, 
                         degree= deg,
                         density= idensi,
                         between= bet, 
                         closeness=cls,
                         centraBetw= CentraBet,
                         centraDeg= CentraDeg,
                         ClusWalk= clusMember,
                         modelaridad= mod,
                         graphdens= gden,
                         meanDegree= meanDeg,
                         transitiv= trans,
                         transitivLocal=transLocal,
                         assorta= assor, 
                         ties= ourregion$gal$mnext,
                         nodesTotal= ourregion$gal$n,
                         Year= periododsList[[y]],
                         riceType= unique(riceTpes[[i]]$Commodity))
    
    ###### Export to cvs format
    write.csv(dataSUM,paste("./NetworkAnalysis/", periododsList[[y]], 
                            "_StactNetworks_",unique(riceTpes[[i]]$Commodity),
                            ".csv", sep = ""))
    
    
    ##### guardar datos en una lista
    netData[[y]]<- dataSUM
    
    
    
    #####################################################################################
    detach(package:igraph)
    suppressMessages(library(statnet))
    suppressMessages(library(intergraph))
    scatter.smooth(ourregion %v% 'RateExpor',
                   degree(ourregion,gmode='graph'),
                   xlab='Rate Export',
                   ylab='Degree')
    
    #             ### tables 
    #             table_local<- mixingmatrix(netWorksPeriod[[y]],'cat_localHV')
    #             table_fore<- mixingmatrix(netWorksPeriod[[y]],'cat_f_hvW')            
    #             table_shock<- mixingmatrix(netWorksPeriod[[y]],'Shock')
    
    
    
    
    #####################################################################################
    ### Eliminando  paises sin informacion de shocks de rendimiento
    test <- get.inducedSubgraph(ourregion,
                                which(ourregion %v% "cat_localHV"!="NoProducers"))
    
    
    ################################# modeling #########################
    suppressMessages(library(ergm))
    #### null model
    DSmod0 <- ergm(test ~ edges,
                   control=control.ergm(seed=40))
    
    #### model 1 ######
    DSmod1 <- ergm(test ~ edges +
                     nodefactor('cat_localHV')+
                     nodefactor('Top'), 
                   control=control.ergm(seed=40))
    
    
    ############################################## Dyadic Predictors
    
    #### model 2 ########
    DSmod2 <- ergm(test ~ edges +
                     nodefactor('Top')+
                     nodematch('cat_localHV'),
                   control=control.ergm(seed=40))
    
    #### model 3a #######
    DSmod3 <- ergm(test ~ edges +
                     nodefactor('Top')+
                     nodematch('cat_localHV', diff=TRUE),
                   control=control.ergm(seed=40))
    
    
    
    ### model 3b hypothesis of differential homophily.
    DSmod3a <- ergm(test ~ edges +
                      nodefactor('Top')+
                      nodemix('cat_localHV'), #, base=1
                    control=control.ergm(seed=40))
    
    ##### Creating functions to get coeficientes and results from models
    oddsratios <- function (mem) #based on Harris, 2014
    {  
      or <- exp( x = mem$coef )
      ste <- sqrt( diag( mem$covar ) ) 
      lci <- exp( mem$coef-1.96*ste ) 
      uci <- exp( mem$coef+1.96*ste )
      ors <- rbind( round( lci,digits = 4 ),round( or,digits = 4),round( uci,digits = 4 ) ) 
      ors <- t(ors)
      colnames(ors) <- c( "Lower","OR","Upper" ) 
      return(ors)
    }
    
    gof_pt.ego <- function(x) # x = ergm.gof object 
    {
      m <- x$summary.deg
      selm <- m[, "obs"] == 0 & m[, "min"] == 0 & m[, "max"] == 0 
      degcount<- x$pobs.deg[!selm] >= x$bds.deg[1,!selm] & x$pobs.deg[!selm] <= x$bds.deg[2,!selm]
      goftab <- rbind(c(sum(degcount)), c(length(degcount)), c(sum(degcount/length(degcount))))
      colnames(goftab) <- c("degree")
      return(goftab)
    }
    
    #### obtener parametros
    #### obtener parametros
    Bcoefstab <- rbind(
      cbind(summary(DSmod0)$coef, oddsratios(DSmod0), model="NULO", RiceT=unique(riceTpes[[i]]$Commodity)),
      cbind(summary(DSmod1)$coef, oddsratios(DSmod1),model="1",RiceT=unique(riceTpes[[i]]$Commodity)),
      cbind(summary(DSmod2)$coef, oddsratios(DSmod2),model="2", RiceT=unique(riceTpes[[i]]$Commodity)),
      cbind(summary(DSmod3)$coef, oddsratios(DSmod3),model="3", RiceT=unique(riceTpes[[i]]$Commodity)),
      cbind(summary(DSmod3a)$coef, oddsratios(DSmod3a),model="3a", RiceT=unique(riceTpes[[i]]$Commodity))
      
    )
    Bcoefstab$Year<- periododsList[[y]]
    
    
    ##### exporting results to csv files
    write.csv(Bcoefstab,paste("./NetworkAnalysis/", periododsList[[y]], 
                              "_ResultsModelNetworks_",unique(riceTpes[[i]]$Commodity),
                              ".csv", sep = ""))
    
    
    ####################################### generacion de graficos######################################## 
    detach(package:statnet)
    suppressMessages(library(igraph))
    suppressMessages(library(intergraph))
    iDHHS <- asIgraph(ourregion)
    
   
    V(iDHHS)$color<- FALSE
    ##### agregando color a los nodos
    ##### agregando color a los nodos
    V(iDHHS)[(V(iDHHS)$region=="Middle_Africa")]$color <- "green"
    V(iDHHS)[V(iDHHS)$region=="caribbean"]$color<- "blue"
    V(iDHHS)[V(iDHHS)$region=="South_America"]$color<- "blue"
    V(iDHHS)[V(iDHHS)$region=="Western_Asia"]$color<- "yellow"
    V(iDHHS)[V(iDHHS)$region=="Australia_and_New_Zealand"]$color<- "grey"
    V(iDHHS)[V(iDHHS)$region=="Western_Europe"]$color<- "red"
    V(iDHHS)[V(iDHHS)$region=="Eastern_Europe"]$color<- "red"
    V(iDHHS)[V(iDHHS)$region=="Central_America"]$color<- "blue"
    V(iDHHS)[V(iDHHS)$region=="Eastern_Africa"]$color<- "green"
    V(iDHHS)[V(iDHHS)$region=="Western_Africa"]$color<- "green"
    V(iDHHS)[V(iDHHS)$region=="Northern_America"]$color<- "purple"
    V(iDHHS)[V(iDHHS)$region=="Southern_Europe"]$color<- "red"
    V(iDHHS)[V(iDHHS)$region=="micro_poli_melanesia"]$color<- "black"
    V(iDHHS)[V(iDHHS)$region=="Northern_Europe"]$color<- "red"
    V(iDHHS)[V(iDHHS)$region=="Southern_Asia"]$color<- "blue"
    V(iDHHS)[V(iDHHS)$region=="Central_Asia"]$color<- "yellow"
    V(iDHHS)[V(iDHHS)$region=="Northern_Africa"]$color<- "green"
    V(iDHHS)[V(iDHHS)$region=="South_Eastern_Asia"]$color<- "yellow"
    V(iDHHS)[V(iDHHS)$region=="Eastern_Asia"]$color<- "yellow"
    V(iDHHS)[V(iDHHS)$region=="Southern_Africa"]$color<- "green"
    
    degi<- degree(graph = iDHHS,mode = "total")
    
    tiff(paste("./NetworkAnalysis/pic/",periododsList[[y]],
               unique(riceTpes[[i]]$Commodity),"World_Network" ,".tiff", sep = ""), width = 10, height = 10, units = 'in', 
         res=80)# width = 800, height = 500)
    
    op <- par(mar = c(1,0,3,1),mfrow=c(1,1))
    V(iDHHS)$size=(rescale(degi,1,8))*1
    E(iDHHS)$arrow.size <- .2
    plot(iDHHS,vertex.label=V(iDHHS)$vertex.names, layout=layout_with_fr, #layout.fruchterman.reingold,
         edge.width=E(iDHHS)$Val/2,edge.color="orange",
         vertex.color="orange", vertex.frame.color="#ffffff")
    #             legend(x=-1.8, y=1,c("caribbean", "South_America", "Central_America"), pch=21,
    #                    col="#777777", pt.cex=2, cex=0.8, bty="n", ncol=1,
    #                    title="Regions", pt.bg=c("green","blue","yellow" )) # pt.bg=unique(V(iDHHS)$color)
    #             
    par(op)
    dev.off()
    
    ### ensayos
    gplot(ourregion,gmode="graph",mode="fruchtermanreingold",
          vertex.cex=log(deg),edge.col="grey75")
    
    ############ grafico 2 ##############################
    
    tiff(paste("./NetworkAnalysis/pic/",periododsList[[y]],
               unique(riceTpes[[i]]$Commodity),"World_NetworkOtherGraph" ,".tiff", sep = ""), 
         width = 10, height = 10, units = 'in', 
         res=80)# width = 800, height = 500)
    op <- par(mar = c(1,0,3,1),mfrow=c(1,1))
    V(iDHHS)$size=(rescale(degi,1,8))*2
    E(iDHHS)$arrow.size <- .2
    l<- layout_randomly(iDHHS)
    plot(iDHHS,vertex.label=NA,layout=l,
         edge.width=E(iDHHS)$Val/2,edge.color="orange",
         vertex.color="grey", vertex.frame.color="#555555", edge.arrow.size=.,
         vertex.label.cex=.7,edge.curved=0)
    par(op)
    dev.off()
   
    cat(paste("proceso bien ", periododsList[[y]]," completed done!!!\n ", sep = ""))
  }
  networkRice[[i]]<- netWorksPeriod
  
  
} )
