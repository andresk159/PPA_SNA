##### SCRIPT TO GENERATE A DUMMY DATA BASE FROM SURVEY #############
#### AUTHOR: ANDRES CAMILO MENDEZ ALZATE 
####################################################################


# IMPORT PACKAGES
install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, magrittr, randomNames, lubridate, likert, brazilmaps, rgeos, rgdal, sf, sp)




actores <- read.csv("Y:/PPA-SNA/input_data/actores_ppa/listado_actores.csv", stringsAsFactors = F)


## section 1 from category 1
dummy_db <- actores %>% dplyr::mutate(c_id      = paste0(grupo, "_", round(runif(nrow(.), 1000, 9999), 0)),
                                      person_id = paste0(c_id, "_", round(runif(nrow(.), 100, 999), 0)),
                                      data_in   = sample(seq(dmy("15-01-2017"), dmy("08-05-2019"), by = "1 week"), nrow(.), replace = T),
                                      posicion  = sample(c("jefe de personal", "gerente", "encargado"), nrow(.), replace = T),
                                      sector    = sample(c("produccion", "logistica", "recursos humanos", "compras", "suministros", "gerencia"), nrow(.), replace = T),
                                      papel     = sample(c("comite de governanza", "asamblea", "GT", "otros"), nrow(.), replace = T),
                                      sexo      = rbinom(nrow(.),1 ,0.1),
                                      nombre    = randomNames(n = nrow(.), gender = sexo, ethnicity = "hispanic", which.names = "first"),
                                      apellido  = randomNames(n = nrow(.), gender = sexo, ethnicity = "hispanic", which.names = "last"),
                                      full.name = paste(nombre, apellido),
                                      email     = paste0(gsub(" ", "", full.name), "@gmail.com"),
                                      telefono  = paste0("+55", round(runif(nrow(.), 10000000, 99999999), 0)),
                                      sexo      = if_else(sexo == 1, "F", "M")
                                      )
names(dummy_db) <- c("company_name", 'grupo' ,"company_id", "c1_s1_person_id", "c1_s1_date_in", "c1_s1_posicion", "c1_s1_sector",
                     "c1_s1_papel_ppa", "c1_s1_sexo","first_name", "last_name", "c1_s1_nombre" ,"c1_s1_email",  "c1_s1_telefono")

## section 2 from category 1
dummy_db %<>% dplyr::mutate(c1_s2_conocimiento               = rbinom(nrow(.), 1, 0.9),
                           c1_s2_grado_conoc                 = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) ),
                           c1_s2_perc_utl_ppa                = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) ),
                           c1_s2_impc_gen_ppa                = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) ),
                           c1_s2_perc_compr_memb_ppa         = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) ),
                           c1_s2_perc_grado_infl_memb_ppa    = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) )
                           )


## section 3 from category 1

dummy_db %<>% dplyr::mutate(c1_s3_p1 = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) ),
                           c1_s3_p2 = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) ),
                           c1_s3_p3 = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) ),
                           c1_s3_p4 = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) ),
                           c1_s3_p5 = sample( 1:5, nrow(.), replace = T, prob = c(0.1, 0.1, 0.2, 0.3, 0.3) ) )


to_likert <- dummy_db[, grep("c1_s2_", names(dummy_db))] %>% 
  dplyr::select(-1) %>% dplyr::mutate_all(as.factor)

names(to_likert) <- c("Grau de conhecimento sobre a PPA",
                      "Percepção do indivíduo sobre a utilidade da PPA",
                      "Impacto gerado pela PPA em sua empresa/organização",
                      "Percepção sobre o protagonismo e compromisso dos membros da PPA com a iniciativa",
                     " Percepção do grau de influência dos membros da PPA")

lkt <- likert(items = to_likert, nlevels = 5)
plot(lkt)

## section 1 from category 2 
## organization
br_cities_nome <- brazilmaps::get_brmap(geo = "City",
                                        geo.filter = NULL,
                                        class = c("SpatialPolygonsDataFrame")) 

br_regions <- get_brmap(geo = "State", class= "SpatialPolygonsDataFrame")$nome %>% as.character()


br_cities <- cbind(as.character(br_cities_nome$nome), gCentroid(br_cities_nome,byid=TRUE) %>% as.data.frame())
br_cities$city <- as.character(br_cities$city)
names(br_cities) <- c("city", "long", "lat")

organization <- dummy_db %>% 
  dplyr::select(company_name, grupo, company_id) %>%
  dplyr::mutate(
    c2_s1_p1 = sample(seq(dmy("15-01-2017"), dmy("08-05-2019"), by = "1 week"), nrow(.), replace = T),
    c2_s1_p2 = sample(c("empresa_privada", "ONG", "Agencia_gov_brasilera", "agencia_gov_extranjera", "asociacion_empresarial", "cooperativa", "asociacion_comunitaria", "negocio de impacto", "emprendimiento social"), nrow(.), replace = T), 
    c2_s1_p3 = sample(c("local", "estadual", "mais de un estado na amazonia", "nacionalsampel", "internacional"), nrow(.), replace = T),
    c2_s1_p4 = sample(c("mineria", "varejo", "comunicaciones", "cosmeticos", "refrigerantes", "agricultura_pecuaria", "sociobiodiversidad", "infraestructura", "logistica", "finanzas", "defensa", "otros"), nrow(.), replace = T),
    n_smpl   = sample(1:nrow(.),nrow(.), replace = T),
    city     = br_cities[n_smpl, 1],
    long     = br_cities[n_smpl, 2],
    lat      = br_cities[n_smpl, 3],
    c2_s1_p5 = paste(city, long, lat, sep =";"),
    c2_s1_p6 = sample(br_regions, nrow(.), replace = T),
    c2_s1_p7 = sample(c("responsabilidad social", "sustentabilidad ambiental", "ambos"), nrow(.), replace = T),
    c2_s1_p8 = sample(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "AB", "BC", "DF", "FG", "HI", "DE", "FI", "HC"), nrow(.), replace = T)) %>% 
  dplyr::select(-n_smpl, -city, -long, -lat)

#### tamaho na empresa


organization %>% dplyr::mutate(c2_s2_p1 = round(runif(nrow(.), 10, 999), 0),
                               c2_s2_p2 = paste0(round(runif(nrow(.), 1000, 9999),0), ";", round(runif(nrow(.), 10000, 99999), 0)),
                               c2_s2_p3 = paste0(round(runif(nrow(.), 1000, 9999),0), ";", round(runif(nrow(.), 10000, 99999), 0)),
                               c2_s2_p4 = sample(1:999, nrow(.), replace = F),
                               c2_s2_p5 = sample(c("grande", "media", "pequena", "micro"), nrow(.), replace = T, prob = c(0.5,0.3, 0.1, 0.1)),
                               c2_s2_p6 = sample(0:5, nrow(.), replace = T))

### grau de compromiso con  la responsabilidad social e sustentabilidad ambiental

organization %>% dplyr::mutate( c2_s3_p1 = sample(c("S", "N"), nrow(.), replace = T, prob = c(0.7, 0.3)),
                                c2_s3_p2 = sample(c("S", "N"), nrow(.), replace = T, prob = c(0.7, 0.3)),
                                c2_s3_p3 = sample(1:5, nrow(.), replace = T),
                                c2_s3_p4 = sample(c("S", "N"), nrow(.), replace = T, prob = c(0.7, 0.3)),
                                c2_s3_p5 = sample(1:5, nrow(.), replace = T),
                                c2_s3_p6 = sample(c("alto", "medio", " bajo"), nrow(.), replace = T))


# compromiso en relacion  con la ppa

organization %>% dplyr::mutate(c2_s4_p1 = sample(0:5, nrow(.), replace = T, prob =c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)),
                               c2_s4_p2 = sample(0:5, nrow(.), replace = T, prob = c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)),
                               c2_s4_p3 = sample(0:5, nrow(.), replace = T, prob = c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)),
                               c2_s4_p4 = sample(0:5, nrow(.), replace = T, prob= c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)),
                               c2_s4_p5 = sample(c("S", "N"), nrow(.), replace = T, prob = c(0.3, 0.7)),
                               c2_s4_p6 = sample(c("S", "N"), nrow(.), replace = T, prob = c(0.3, 0.7)),
                               c2_s4_p7 = sample(0:5, nrow(.), replace = T, prob = c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)),
                               c2_s4_p8 = sample(0:5, nrow(.), replace = T, prob = c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)),
                               c2_s4_p9 = sample(c("aumento", "se mantuvo", "disminuyo", "no aplica"), nrow(.), replace = T, prob = c(0.2,0.4, 0.2, 0.2)),
                               c2_s4_p10= sample(c("A", "B", "C", "D", "E", "F", "G", "AB", "BC", "DF", "FG", "DE", "FB", "GC"), nrow(.), replace = T),
                               c2_s4_p11= sample( 1:4, nrow(.), replace = T, prob = c(0.1, 0.2, 0.4, 0.3) ),
                               c2_s4_p12= sample( 1:4, nrow(.), replace = T, prob = c(0.1, 0.2, 0.4, 0.3) ),
                               c2_s5_p1= sample( 1:4, nrow(.), replace = T, prob = c(0.1, 0.2, 0.4, 0.3) ),
                               c2_s5_p2= sample( 1:4, nrow(.), replace = T, prob = c(0.1, 0.2, 0.4, 0.3) ) )









