library(reshape2)
library(reshape)
library(plyr)
library(dplyr)
library(tidyr)
library(RMark)
library(ggplot2)
library(stringr)
library(purrr)


setwd("//kant/nhm-sfs-u2/franzif/paper2")

## https://www.paleobiodb.org/data1.2/occs/list.csv?taxon_reso=genus&idqual=certain&interval=Cambrian,Silurian&show=attr,class,genus,ecospace,coll,coords,loc,paleoloc,stratext,lithext,env,geo,acconly
## latest download 15.05.2019
genus <- read.csv("PBDB_C-S_data.csv", sep = ",", header=T)
# genus <- filter(genus, grepl("genus", accepted_rank))
genus <- cbind(genus, "diff"=genus$max_ma-genus$min_ma)


##
## in the following section, we extract the genus occurrences that belong to the different attaching echinoderm groups.
## if the filter is simply applied to a group, we filter for this group
## a ! in front of the taxa to be filtered indicates exceptions
## as an example - the class Crinoidea is filtered, with exception of the order Cladida, which are excluded, based on Sprinkle and Guensburg 1995.
## attaching echinoderms:
Crinoidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Crinoidea") %>% # Palmer and Palmer 1977; Brett and Liddell 1978; Palmer 1982; Sprinkle and Guensburg 1995; Holterhoff 1997
  filter(!order %in% "Cladida") %>% # Sprinkle and Guensburg 1995
  filter(!family %in% "Cupulocrinidae") %>% # pers. communication Selina Cole 13.08.2019 - they don't have holdfasts
  filter(!family %in% "Rhodocrinitidae") # Guensburg 1992

Edrioasteroidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Edrioasteroidea") %>% # Brett and Liddell 1978; Palmer 1982; Taylor and Wilson 2003; Taylor 2016 
  filter(!family %in% "Pyrgocystidae") %>% # pers comm. Mike Reich 12.03.2019 - please see below specific inclusions of three different Pyrgocystids (att.Pyrgocyst), that were attaching to hard substrates
  filter(!family %in% "Rhenopyrgidae") %>% # Lefebvre et al. 2013
  filter(!genus %in% c("Aragocystites", "Cambraster", "Edriodiscus", "Kailidiscus", "Walcottidiscus", "Stromatocystites", "Camptostroma")) %>% # Dornbos 2006
  filter(!genus %in% c("Totiglobus", "Paredriophus" , "Argodiscus", "Rhenopyrgus")) # Dornbos 2006

Eocrinoidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Eocrinoidea") %>% # Sprinkle 1973; Palmer 1982; Parsley and Prokop 2004
  filter(!order %in% "Ascocystitida") %>% # Zamora et al. 2017
  filter(!genus %in% c("Eocystites")) %>% # Durham and Caster 1963
  filter(!genus %in% c("Kinzercystis", "Lepidocystis", "Cymbionites", "Peridionites", "Acanthocystites", "Lichenoides", "Marjumicystites")) # Dornbos 2006

Diploporita <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Diploporita") %>% # Bockelie 1984 
  filter(!genus %in% "Triamara") # Frest 2005

Paracrinoidea <- genus %>% filter( # are attaching acc. to Brett and Liddel 1978
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Paracrinoidea") 
# 
# Callocystitidae <- genus %>% filter(
#   phylum %in% "Echinodermata") %>%
#   filter(family %in% "Callocystitidae") # Broadhead and Strimple 1978

att.Pyrgocyst <- genus %>% filter(
  genus %in% c("Epipaston", "Pyrgocystis", "Streptaster")) ## hard substrate attachers amongst previously excluded Pyrgocystidaa, according to Dornbos 2005, Sumrall and Zamora 2011

rest.attached <- genus %>% filter(
  genus %in% c("Vyscystis", # Nohejlova et al. 2019 # attached to debris on soft substrate
               "Coleicarpus")) # Daley E.J. 1996 # attaching exception from otherwise free living Solutes 


## the following command binds all rows (genus occurrences) that were filtered above together
##
## if the analysis is about to be run with or without Paracrinoidea, they can be turned off here, simply by putting an # in front of the line
##
attached <- rbind.data.frame(
  Crinoidea,
  Edrioasteroidea,
  Eocrinoidea,
  Diploporita,
  # Paracrinoidea,
  # Callocystitidae,
  att.Pyrgocyst,
  rest.attached
)
## the following command puts together only unique names from the above collected genus occurrences
## in this way, we get a list of unique genera names for attaching Echinoderms
attached.names <- unique(attached[,'genus'])


##
## this process is repeated below for all other considered groups
##
## filtering genus occurrences of non-attaching Echinoderms
Echinoidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Echinoidea")
Asteroidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Asteroidea")
Stylophora <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Stylophora") # Parsley and Prokop 2004 # Lefebvre et al. 2013
Ctenocystoidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Ctenocystoidea") # Parsley and Prokop 2004
Cincta <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Cincta") # Parsley and Prokop 2004
Rhombifera <- genus %>% filter( # Guensburg and Sprinkle 1992
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Rhombifera") %>%
  filter(!family %in% "Callocystitidae") # Broadhead and Strimple 1978
Soluta <- genus %>% filter( # Guensburg and Sprinkle 1992 
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Soluta") %>%
  filter(!genus %in% "Coleicarpus") # Daley E.J. 1996
Ophiuroidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Ophiuroidea")
Stenuroidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Stenuroidea")
Ophiocistioidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Ophiocistioidea")
Somasteroidea <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(class %in% "Somasteroidea")

exceptions.hard <- genus %>% filter(
  phylum %in% "Echinodermata") %>%
  filter(genus %in% c("Camptostroma", "Triamara","Eocystites")) # non attaching exceptions from generally attaching groups (see SI table S2 and the exceptions marked above)

rest.unattached <- genus %>% filter(
  class %in% c("Helicoplacoidea")) # Durham and Caster 1963

unattached <- rbind.data.frame(
  Echinoidea,
  Asteroidea,
  Stylophora,
  Ctenocystoidea,
  Cincta,
  Rhombifera,
  Soluta,
  Ophiuroidea,
  Stenuroidea,
  Ophiocistioidea,
  Somasteroidea,
  exceptions.hard,
  rest.unattached
)
unattached.names <- unique(unattached[,'genus'])


Bryozoa <- genus %>% filter(
  phylum %in% "Bryozoa"
) %>% filter(
  !order %in% "Cheilostomata"
)
Bryozoa.names <- unique(Bryozoa[,'genus'])

Corals <- genus %>% filter(
  class %in% "Anthozoa"
) %>% filter(
  !order %in% c("Actiniaria")
)
## filtering of tabulate corals in the following step
Corals <- Corals %>% filter(
  order %in% c("Favositida","Heliolitida", "Auloporida", "Lichenariida", "Sarcinulida", "Tetradiida")
)
Coral.names <- unique(Corals[,'genus'])


## genus occurrences of non-archaeocyathid sponges are collected in Porifera.a
Porifera.a <- genus %>% filter(
  phylum %in% "Porifera"
) %>% filter(
  class %in% c("Stromatoporoidea","Demospongea", "Hexactinellida")
)
## genus occurrences of archeocyathid sponges are collected in Porifera.b -- for completeness, as these are in the dataset
## However, they have no occurrences in the Ordovician (or even late Cambrian)
## hence, they could as good be excluded.
## inclusion of orders is based on Wood et al. 1992
Porifera.b <- genus %>% filter(
  order %in% c("Archaeocyathida", "Kazakhstanicyathida", ## irregulates
               "Monocyathida", "Ajacicyathida") ## regulares, plus ajacicyathida, which is not represented in our ds
)

Porifera <- rbind.data.frame(Porifera.a, Porifera.b)
Porifera.names <- unique(Porifera[,'genus'])

Brachiopoda.craniids <- genus %>% filter(
  phylum %in% "Brachiopoda"
) %>% filter(
  order %in% "Craniida"
)

Brachiopoda.discinids <- genus %>% filter(
  phylum %in% "Brachiopoda"
) %>% filter(
  family %in% "Discinidae"
)
Brachiopoda <- rbind.data.frame(Brachiopoda.craniids,Brachiopoda.discinids)
Brachiopoda.names <- unique(Brachiopoda[,'genus'])


Trilobita <- genus %>% filter(
  phylum %in% "Arthropoda") %>%
  filter(class %in% "Trilobita") %>%
  filter(!family %in% c("Cyclopygidae", "Telephinidae")) %>% # excluded based on Fortey 1985
  filter(!order %in% "Agnostida")

Trilobita.names <- unique(Trilobita[,'genus'])


Ostracoda <- genus %>% filter(
  phylum %in% "Arthropoda"
) %>%
  filter(class %in% "Ostracoda")
Ostracoda.names <- unique(Ostracoda[,'genus'])



Mollusca <- genus %>% filter(
  phylum %in% "Mollusca"
) %>% filter(
  !class %in% "Cephalopoda"
)
Mollusca.names <- unique(Mollusca[,'genus'])


## activate the following section if amount of genera that are not considered in the analysis is supposed to be calculated
# rest.Echinos <- genus %>% filter(
#   phylum %in% "Echinodermata"
# ) %>% filter(
#   !accepted_name %in% attached.names
# ) %>% filter(
#   !accepted_name %in% unattached.names
# )
# 
# rest.Echinos <- unique(rest.Echinos[,'accepted_name'])
# # rest.Echinos <- unique(rest.Echinos[,c('accepted_name',"phylum", "class", "order", "family", "genus")])
# 
# rest.Brachios <- genus %>% filter(
#   phylum %in% "Brachiopoda"
# ) %>% filter(
#   !genus %in% Brachiopoda.names)
# rest.Brachios <- unique(rest.Brachios[,'accepted_name'])
# 
# rest.Poris <- genus %>% filter(
#   phylum %in% "Porifera"
# ) %>% filter(
#   !genus %in% Porifera.names)
# rest.Poris <- unique(rest.Poris[,'accepted_name'])
# 
# rest.Corals <- genus %>% filter(
#   phylum %in% "Cnidaria"
# ) %>%
#   filter(class %in% "Anthozoa") %>%
#   filter(!genus %in% Coral.names)
# rest.Corals <- unique(rest.Corals[,'accepted_name'])
# 
# rest.Trilo.a <- genus %>% filter(
#   phylum %in% "Arthropoda") %>%
#   filter(class %in% "Trilobita") %>%
#   filter(family %in% c("Cyclopygidae", "Telephinidae")) #  based on Fortey 1985
# 
# rest.Trilo.b <- genus %>% filter(
#   phylum %in% "Arthropoda") %>%
#   filter(class %in% "Trilobita") %>%
#   filter(order %in% "Agnostida")
# 
# rest.Trilo <- rbind.data.frame(rest.Trilo.a,rest.Trilo.b)
# 
# rest.Trilo <- unique(rest.Trilo[,'accepted_name'])


reruns.modelcomparison <- list()
reruns.Phi.T.Gamma.t.p.T <- list()
reruns.Time.cov <- list()

for (i in 1:100){
  
  tryCatch({
    

genus.age <- genus %>% group_by(1:n()) %>% mutate(Ma=ifelse(diff<=12,runif(1,min_ma,max_ma),NA))


base_SS <- c(509,497,485.4,477.7,470,467.3,458.4,453,445.2,443.8,440.8,438.5)
name_SS<-c("C3","C4","Tr", "Fl","Dp","Dw","Sa","Ka","Hi","Si1","Si2")

## adding a new column with names of Stages for the randomly assigned ages
genus.age <- as.data.frame(genus.age %>% group_by(Ma) %>% mutate(SS=cut(Ma, 
                                                                        breaks = base_SS, 
                                                                        labels = rev(name_SS),
                                                                        right=FALSE)))

genus.age$SS <- factor(genus.age$SS,
                       levels = c("C3","C4","Tr", "Fl","Dp","Dw","Sa","Ka","Hi","Si1","Si2"))


# melt_genus <- melt(genus.age, id=c("genus", "SS"), na.rm = TRUE)
# melt_genus <- na.omit(melt_genus)
genus.age <- genus.age[!is.na(genus.age$SS),]
cast_genus <- dcast(genus.age, genus~SS, length)
cast_genus <- as.data.frame(cast_genus)
names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"


cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
  genus.name %in% attached.names ~ "attached",
  genus.name %in% Bryozoa.names ~ "attached",
  genus.name %in% Coral.names ~ "attached",
  genus.name %in% Porifera.names ~ "attached",
  genus.name %in% Brachiopoda.names ~ "attached",
  # genus.name %in% rest.Echinos ~ "attached",
  genus.name %in% Trilobita.names ~ "n.at",
  genus.name %in% Mollusca.names ~ "n.at",
  genus.name %in% unattached.names ~ "n.at"
))

## run the following section only if amount of genera (Tab. S1) should be calculated 
# cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
#   genus.name %in% attached.names ~ "att.Echino",
#   genus.name %in% Bryozoa.names ~ "Bryozoa",
#   genus.name %in% Brachiopoda.names ~ "att.Brachiopoda",
#   genus.name %in% Coral.names ~ "att.Coral",
#   genus.name %in% Porifera.names ~ "att.Pori",
#   genus.name %in% Trilobita.names ~ "n.at.trilo",
#   genus.name %in% Mollusca.names ~ "n.at.mollusca",
#   genus.name %in% unattached.names ~ "n.at.Echinos",
#   genus.name %in% rest.Echinos ~ "rest.Echino",
#   genus.name %in% rest.Brachios ~ "rest.Brachio",
#   genus.name %in% rest.Poris ~ "rest.Pori",
#   genus.name %in% rest.Corals ~ "rest.Corals",
#   genus.name %in% rest.Trilo ~ "rest.Trilo"
# ))

cast.gen.cov$cov <- as.factor(cast.gen.cov$cov)


summary(cast.gen.cov$cov)


inp1 <- cast.gen.cov %>% filter(
  cov %in% c("attached","n.at")
)

inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0),
                         "cov"=inp1[,13])


## the following sections calculated the amount of observations, presences (Tab. S2) per group. 
## run after the section right above for the calculation of genera per group was executed
# inp1 <- cast.gen.cov %>% filter(
#   cov %in% c(
#     # "att.Echino"
#     "Bryozoa"
#     # "att.Brachiopoda"
#     # "att.Coral"
#     # "att.Pori"
#     # "n.at.trilo"
#     # "n.at.mollusca"
#     # "n.at.Echinos"
#     )
# )
# 
# no.occurrences <- sum(colSums(inp1[2:12]))
# no.occurrences
# inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0),
#                          "cov"=inp1[,13])
# 
# no.presences <- sum(colSums(inp1[1:11]))
# no.presences
# ##
# ##

inp1 <- unite(as.data.frame(inp1), "ch", c(1:11), sep = "")

summary(inp1)


########
## MARK MODEL specificaitons
Phi.time <- list(formula=~time)
Gamma.time <- list(formula=~time)
p.time <- list(formula=~time)
L.time <- list(formula=~time)
Phi.time.cov <- list(formula=~time+cov)
p.time.cov <- list(formula=~time+cov)
Gamma.time.cov <- list(formula=~time+cov)
L.time.cov <- list(formula=~time+cov)

Phi.Time.cov <- list(formula=~time+cov+time*cov)
p.Time.cov <- list(formula=~time+cov+time*cov)
Gamma.Time.cov <- list(formula=~time+cov+time*cov)
L.Time.cov <- list(formula=~time+cov+time*cov)

Phi.const <- list(formula=~1)
p.const <- list(formula=~1)
Gamma.const <- list(formula=~1)
L.const <- list(formula=~1)

Phi.cov <- list(formula=~cov)
p.cov <- list(formula=~cov)
Gamma.cov <- list(formula=~cov)
L.cov <- list(formula=~cov)

#######################################################
#######################################################

proc.pradsen <- process.data(inp1, model= "Pradsen", groups = "cov")#Pradel's survival and "growth"

## running models for model comparison
Time.cov <- mark(proc.pradsen, model.parameters = list(Phi=Phi.Time.cov, p=p.Time.cov, Gamma=L.Time.cov))#, options="SIMANNEAL")

time.cov <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time.cov, p=p.time.cov, Gamma=L.time.cov))#, options="SIMANNEAL")
Phi.T.Gamma.T.p.t <- mark(proc.pradsen, model.parameters = list(Phi=Phi.Time.cov, p=p.time.cov, Gamma=L.Time.cov))#, options="SIMANNEAL")
Phi.t.Gamma.T.p.t <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time.cov, p=p.time.cov, Gamma=L.Time.cov))#, options="SIMANNEAL")
Phi.T.Gamma.t.p.t <- mark(proc.pradsen, model.parameters = list(Phi=Phi.Time.cov, p=p.time.cov, Gamma=L.time.cov))#, options="SIMANNEAL")
Phi.t.Gamma.T.p.T <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time.cov, p=p.Time.cov, Gamma=L.Time.cov))#, options="SIMANNEAL")
Phi.T.Gamma.t.p.T <- mark(proc.pradsen, model.parameters = list(Phi=Phi.Time.cov, p=p.Time.cov, Gamma=L.time.cov))#, options="SIMANNEAL")
 
Phi.t.p.t1.gamma.t1 <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time.cov, p=p.time, Gamma=L.time))#, options="SIMANNEAL")
Phi.t1.p.t.gamma.t1 <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time, p=p.time.cov, Gamma=L.time))#, options="SIMANNEAL")
Phi.t1.p.t1.gamma.t <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time, p=p.time, Gamma=L.time.cov))#, options="SIMANNEAL")

Phi.T.p.t1.gamma.t1 <- mark(proc.pradsen, model.parameters = list(Phi=Phi.Time.cov, p=p.time, Gamma=L.time))#, options="SIMANNEAL")
Phi.t1.p.T.gamma.t1 <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time, p=p.Time.cov, Gamma=L.time))#, options="SIMANNEAL")
Phi.t1.p.t1.gamma.T <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time, p=p.time, Gamma=L.Time.cov))#, options="SIMANNEAL")

Phi.T.p.T.gamma.t1 <- mark(proc.pradsen, model.parameters = list(Phi=Phi.Time.cov, p=p.Time.cov, Gamma=L.time))#, options="SIMANNEAL")
Phi.t1.p.T.gamma.T <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time, p=p.Time.cov, Gamma=L.Time.cov))#, options="SIMANNEAL")
Phi.T.p.t1.gamma.T <- mark(proc.pradsen, model.parameters = list(Phi=Phi.Time.cov, p=p.time, Gamma=L.Time.cov))#, options="SIMANNEAL")

Phi.t.p.t.gamma.t1 <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time.cov, p=p.time.cov, Gamma=L.time))#, options="SIMANNEAL")
Phi.t.p.t1.gamma.t <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time.cov, p=p.time, Gamma=L.time.cov))#, options="SIMANNEAL")
Phi.t1.p.t.gamma.t <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time, p=p.time.cov, Gamma=L.time.cov))#, options="SIMANNEAL")

time.1 <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time, p=p.time, Gamma=L.time))
const.cov <- mark(proc.pradsen, model.parameters = list(Phi=Phi.cov, p=p.cov, Gamma=L.cov))
const.1 <- mark(proc.pradsen, model.parameters = list(Phi=Phi.const, p=p.const, Gamma=L.const))

## the following command collects the models that were just run and compares them based on AICc
x=collect.models(type = "Pradsen")

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
## here, we extract the model comparison results and estimates from the best model as a list from all 100 replicate runs
  reruns.modelcomparison[[i]]=data.frame(x$model.table$model,x$model.table$weight)
  reruns.Phi.T.Gamma.t.p.T[[i]]=Phi.T.Gamma.t.p.T$results$real

  
}

AIC.table <- reduce(reruns.modelcomparison, full_join, by = "x.model.table.model")

save.image("//kant/nhm-sfs-u2/franzif/paper2/100.runs.AIC.minusParacrinoids.RData")

load("//kant/nhm-sfs-u2/franzif/paper2/100.runs.AIC.minusParacrinoids.RData")

AIC.mean <- rowMeans(AIC.table[2:101])
AIC.mean <- cbind.data.frame(AIC.table[1],AIC.mean)
AIC.mean <- AIC.mean[order(-AIC.mean$AIC.mean),]
AIC.mean$AIC.mean <- round(AIC.mean$AIC.mean,3)


## for SI - Median of model weights
AIC.median <- apply(AIC.table[2:101], 1, median)
AIC.median <- cbind.data.frame(AIC.table[1],AIC.median)
AIC.median <- AIC.median[order(-AIC.median$AIC.median),]
AIC.median$AIC.median <- round(AIC.median$AIC.median,3)

model.weights.100 <- full_join(AIC.mean, AIC.median, by="x.model.table.model")
model.weights.100

#####################################################################################################
#####################################################################################################

## collecting 100 replicate parameter estimates
estimates <- do.call(cbind, lapply(reruns.Phi.T.Gamma.t.p.T, function(x) x$estimate))
lcl <- do.call(cbind, lapply(reruns.Phi.T.Gamma.t.p.T, function(x) x$lcl))
ucl <- do.call(cbind, lapply(reruns.Phi.T.Gamma.t.p.T, function(x) x$ucl))

estimates[estimates<0.00001|estimates==1|estimates>=1] <- NA
lcl[is.na(estimates)] <- NA
ucl[is.na(estimates)] <- NA

estimates[ucl-lcl==0|ucl-lcl==1|ucl>1]<-NA
lcl[is.na(estimates)] <- NA
ucl[is.na(estimates)] <- NA

# estimate.means <- rowMeans(estimates, na.rm = T)
##
estimates.mean <- rowMeans(estimates, na.rm = T)
lcl.mean <- rowMeans(lcl, na.rm = T)
ucl.mean <- rowMeans(ucl, na.rm = T)

estimates.median <- apply(estimates, 1, median, na.rm=TRUE)
lcl.median <- apply(lcl,1,median, na.rm=TRUE)
ucl.median <- apply(ucl,1,median, na.rm=TRUE)

#####################################################################################################
## extracting parameter estimates medians from the results above
phi.median.att <- estimates.median[2:8]
phiu.median.att <- ucl.median[2:8]
phil.median.att <- lcl.median[2:8]

phi.median.n.at <- estimates.median[12:18]
phiu.median.n.at <- ucl.median[12:18]
phil.median.n.at <- lcl.median[12:18]

p.median.att <- estimates.median[23:29]
pu.median.att <- ucl.median[23:29]
pl.median.att <- lcl.median[23:29]

p.median.n.at <- estimates.median[34:40]
pu.median.n.at <- ucl.median[34:40]
pl.median.n.at <- lcl.median[34:40]

gam.median.att <- estimates.median[44:50]
gamu.median.att <- ucl.median[44:50]
gaml.median.att <- lcl.median[44:50]

gam.median.n.at <- estimates.median[54:60]
gamu.median.n.at <- ucl.median[54:60]
gaml.median.n.at <- lcl.median[54:60]

###########################################################################################
# following vector is time between midpoints of two neighbouring stageslices
t <- c(9.65,7.7,5.2,5.8,7.15,6.6,4.6)
# following vector is time per interval/stageslice
tp <- c(7.7,7.7,2.7,8.9,5.4,7.8,1.4)

###########################################################################################
origprob.median.att <- 1-gam.median.att
origCIl.median.att <- 1-gaml.median.att
origCIu.median.att <- 1-gamu.median.att
Orig_rate.median.att <- -log(1-origprob.median.att)/t
Orig_rate_CIl.median.att <- -log(1-origCIl.median.att)/t
Orig_rate_CIu.median.att <- -log(1-origCIu.median.att)/t

origprob.median.n.at <- 1-gam.median.n.at
origCIl.median.n.at <- 1-gaml.median.n.at
origCIu.median.n.at <- 1-gamu.median.n.at
Orig_rate.median.n.at <- -log(1-origprob.median.n.at)/t
Orig_rate_CIl.median.n.at <- -log(1-origCIl.median.n.at)/t
Orig_rate_CIu.median.n.at <- -log(1-origCIu.median.n.at)/t


extinctprob.median.att <- 1-phi.median.att
extinctCIl.median.att <- 1-phil.median.att
extinctCIu.median.att <- 1-phiu.median.att
Ext_rate.median.att <- -log(1-extinctprob.median.att)/t
Ext_rate_CIl.median.att <- -log(1-extinctCIl.median.att)/t
Ext_rate_CIu.median.att <- -log(1-extinctCIu.median.att)/t

extinctprob.median.n.at <- 1-phi.median.n.at
extinctCIl.median.n.at <- 1-phil.median.n.at
extinctCIu.median.n.at <- 1-phiu.median.n.at
Ext_rate.median.n.at <- -log(1-extinctprob.median.n.at)/t
Ext_rate_CIl.median.n.at <- -log(1-extinctCIl.median.n.at)/t
Ext_rate_CIu.median.n.at <- -log(1-extinctCIu.median.n.at)/t


rate_p.median.att <- -log(1-p.median.att)/tp
ratel_p.median.att <- -log(1-pl.median.att)/tp
rateu_p.median.att <- -log(1-pu.median.att)/tp

rate_p.median.n.at <- -log(1-p.median.n.at)/tp
ratel_p.median.n.at <- -log(1-pl.median.n.at)/tp
rateu_p.median.n.at <- -log(1-pu.median.n.at)/tp

#########################################################################################################
#########################################################################################################
#########################################################################################################

Stagebase <-c(485.4,477.7,470,467.3,458.4,453,445.2)
Stagemidpoints <- c(481.55,473.85,468.65,462.85,455.7,449.1,444.5)


tpx <- c("Tr", "Fl","Dp","Dw","Sa","Ka","Hi","Sil")
per <- data.frame(c(485.4,477.7,470,467.3,458.4,453,445.2,443.8))
Ord.Stage <- cbind.data.frame(tpx,"Stage"=per)

##
## timescale function from
## http://simpson-carl.github.io/articles/15/timescales.to.base

tscales.Ord <- function(top, bot, s.bot, ...){
  bases <- Ord.Stage
  cc <- rep(c("grey95","grey97 "),length(bases))
  
  rect(xleft = bases[-8, 2], ybottom = rep(bot,7), xright = bases[-1, 2],
       ytop = rep(top, 7), col = cc, border=NA)
  
  rect(xleft = bases[-8, 2], ybottom = rep(bot,7), xright = bases[-1, 2],
       ytop = rep(s.bot, 7), border = 'grey90')
  
  bt <- (bot+s.bot)/2
  tpl <- bases[,2]+c(diff(bases[,2])/2,0)
  
  text(x=tpl[-7], y=bt[-8], labels = bases[1:7, 1])
}


tiff("prefmod.tiff",
     res = 300,
     units = "cm",
     width = 10,
     height = 15,
     pointsize = 9)


##############################################################################
par(mfrow=c(3,1), mar = c(0.5,2,0.1,1), oma = c(3,2,0,0))
##############################################################################
plot(Stagebase-0.3, Orig_rate.median.att, type = "b", 
     pch=19,
     # main = "Origination Rate", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(0.5, -0.025, -0.075)

lines(Stagebase-0.3, Orig_rate.median.att, type = "b", pch=19, lwd=1, lty=2)
# lines(Stagebase[1]-1, Orig_rate.median.att[1]-0.02, type = "b", pch="1")
arrows(x0=Stagebase-0.3, y0=Orig_rate_CIl.median.att, x1=Stagebase-0.3, y1=Orig_rate_CIu.median.att, length=0.02, lwd = 1, angle = 90, code = 3, lty=2)

lines(Stagebase-0.2, Orig_rate.median.n.at, type = "b", lty = 1, pch=19,lwd=1)
# lines(Stagebase[1]-1, Orig_rate.median.n.at[1]-0.02, type = "b", pch="2")
arrows(x0=Stagebase-0.2, y0=Orig_rate_CIl.median.n.at, x1=Stagebase-0.2, y1=Orig_rate_CIu.median.n.at, length=0.02, lwd = 1, lty=1, angle = 90, code = 3)


# legend("topleft", legend="A", bty="n", cex = 1.25)

axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))

mtext("Origination events per myr", side = 2, line = 2)

# axis(1, col = 'grey75', line = 0.15, at = seq(445,485,10))
# mtext("Age (Ma)", side = 1, line = 2)

legend("topright",
       legend=c("hard substrate taxa","free-living benthic taxa"),
       lty=c(2,1),
       inset = 0.05)


##############################################################################
## Extinction Rate
plot(Stagebase-0.2, Ext_rate.median.att, type = "b",
     pch=19,
     # main = "Extinction Rate", 
     ylim = c(-0.05,0.25),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(0.25, -0.02, -0.05)

lines(Stagebase-0.3, Ext_rate.median.att, type = "b", pch=19, lwd=1, lty=2)
arrows(x0=Stagebase-0.3, y0=Ext_rate_CIl.median.att, x1=Stagebase-0.3, y1=Ext_rate_CIu.median.att, length=0.02, lwd = 1, angle = 90, code = 3, lty=2)

lines(Stagebase-0.2, Ext_rate.median.n.at, type = "b", lty = 1, lwd=1, pch=19)
arrows(x0=Stagebase-0.2, y0=Ext_rate_CIl.median.n.at, x1=Stagebase-0.2, y1=Ext_rate_CIu.median.n.at, length=0.02, lwd = 1,lty=1,  angle = 90, code = 3)

# legend("topleft", legend="B", bty="n", cex = 1.25)

axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.25, 0.05))

mtext("Extinction events per myr", side = 2, line = 2)




##############################################################################
# ## net diversification 
# plot(Stagebase-0.3, div.median.att-1, type = "b", 
#      # main = "Net Diversification", 
#      ylim = c(-2,8),
#      xlim = rev(c(444.18,485.4)),
#      axes = F,
#      xlab = "",
#      ylab = "")
# 
# tscales.Ord(8, -1.2, -2)
# abline(h = 0, col="darkgrey")
# 
# lines(Stagebase-0.3, div.median.att-1, type = "b", lwd=0.8, pch=19, lty=2)
# arrows(x0=Stagebase-0.3, y0=divl.median.att-1, x1=Stagebase-0.3, y1=divu.median.att-1, length=0.02, lwd = 1, angle = 90, code = 3, lty=2)
# 
# lines(Stagebase-0.2, div.median.n.at-1, type = "b", lty = 1, pch=19, lwd=1)
# arrows(x0=Stagebase-0.2, y0=divl.median.n.at-1, x1=Stagebase-0.2, y1=divu.median.n.at-1, length=0.02, lwd = 1, lty=1,  angle = 90, code = 3)
# 
# legend("topleft", legend="C", bty="n", cex = 1.25)
# 
# axis(1, col = 'grey75', line = 0.15, at = seq(445,485,10))
# axis(2, col = 'grey75', line = -0.2, at = seq(-1, 8, 1))
# 
# mtext("Age (Ma)", side = 1, line = 2.2)
# mtext("Net diversification rate", side = 2, line = 2)

##############################################################################
## Sampling rate
plot(Stagemidpoints-0.1, rate_p.median.att, type = 'b', 
     # main="Sampling Rate",
     xlim = rev(c(444.18,485.4)),
     ylim = c(-0.11,1),
     axes = F,
     xlab = "", ylab = "")

tscales.Ord(1, -0.02, -0.11)

lines(Stagemidpoints-0.3, rate_p.median.att, type = "b", pch=19,lwd=1, lty=2)
arrows(x0=Stagemidpoints-0.3, y0=ratel_p.median.att, x1=Stagemidpoints-0.3, y1=rateu_p.median.att, length=0.02, lwd = 1, angle = 90, code = 3, lty=2)

lines(Stagemidpoints-0.2, rate_p.median.n.at, type = "b" , lty = 1, lwd=1, pch=19)
arrows(x0=Stagemidpoints-0.2, y0=ratel_p.median.n.at, x1=Stagemidpoints-0.2, y1=rateu_p.median.n.at, length=0.02, lwd = 1, lty=1, angle = 90, code = 3)

# legend("topleft", legend="C", bty="n", cex = 1.25)

axis(1, col = 'grey75', line = 0.15, at = seq(445,485,10))
axis(2, col = 'grey75', line = -0.15, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.2)
mtext("Sampling events per myr", side = 2, line = 2)

dev.off()