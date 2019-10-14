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

########
## MARK MODEL specificaitons
## ~time defines that the parameter can vary through time
## ~1 defines that the parameter is constant through time
Phi.time <- list(formula=~time)
Gamma.time <- list(formula=~time)
p.time <- list(formula=~time)
L.time <- list(formula=~time)

Phi.const <- list(formula=~1)
p.const <- list(formula=~1)
Gamma.const <- list(formula=~1)
L.const <- list(formula=~1)


## importing PBDB dataset
## https://www.paleobiodb.org/data1.2/occs/list.csv?taxon_reso=genus&idqual=certain&interval=Cambrian,Silurian&show=attr,class,genus,ecospace,coll,coords,loc,paleoloc,stratext,lithext,env,geo,acconly
## download date 15.05.2019
genus <- read.csv("PBDB_C-S_data.csv", sep = ",", header=T)
## filtering only genus-level occurrences (by only filtering for "genus" as accepted rank)
# genus <- filter(genus, grepl("genus", accepted_rank))
## calculating the length of time bins in PBDB
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


rest.Echinos <- genus %>% filter(
  phylum %in% "Echinodermata"
) %>% filter(
  !accepted_name %in% attached.names
) %>% filter(
  !accepted_name %in% unattached.names
)

rest.Echinos <- unique(rest.Echinos[,'genus'])


## we will be creating four individual lists, where we store the results from 100 individual model runs
## the list is created by the command list() and filled in at the end of the loop (scroll down to the end of {})
reruns=list()

for (i in 1:100){
  
  tryCatch({
    
    ## in the following step, we create a random number between the maximum and minimum age, if the difference between those two is smaller or the same as 12
    genus.age <- genus %>% group_by(1:n()) %>% mutate(Ma=ifelse(diff<=12,runif(1,min_ma,max_ma),NA))
    
    base_SS <- c(509,497,485.4,477.7,470,467.3,458.4,453,445.2,443.8,440.8,438.5)
    name_SS<-c("C3","C4","Tr", "Fl","Dp","Dw","Sa","Ka","Hi","Si1","Si2")
    
    ## then we add a new column with names of stages for the randomly assigned ages
    ## the bases and names of the stages were defined in the vectors above
    genus.age <- as.data.frame(genus.age %>% group_by(Ma) %>% mutate(SS=cut(Ma, 
                                                                            breaks = base_SS, 
                                                                            labels = rev(name_SS),
                                                                            right=FALSE)))
    
    genus.age$SS <- factor(genus.age$SS,
                           levels = c("C3","C4","Tr", "Fl","Dp","Dw","Sa","Ka","Hi","Si1","Si2"))
    
    ## in the following step, we create an observation/non-observation matrix
    genus.age <- genus.age[!is.na(genus.age$SS),]
    cast_genus <- dcast(genus.age, genus~SS, length)
    cast_genus <- as.data.frame(cast_genus)
    names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    
    ## in addition to our observations/non-observations, we need to know which groups the individual lines (genera) belong to
    ## possible groups are hard substrate taxa (attached) or free living benthos (n.at - abbr. for non attached)
    ## these assignments are added as an extra column at the end of the observation/non-observation matrix
    ## to assign a genus, we use the genus names that we extracted above, before the loop started
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

    cast.gen.cov$cov <- as.factor(cast.gen.cov$cov)
    
    summary(cast.gen.cov$cov)
    
    ## in this first step, we are only interested in hard substrate taxa
    ## hence, we only include the "attached" into our input file
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("attached")
    )
    
    ## then we modify the input file in a way that it can be read and used as an input for MARK
    inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0))
    inp1 <- unite(as.data.frame(inp1), "ch", c(1:11), sep = "")
    
    summary(inp1)
    
    ## now we run the model.
    ## we have to process the data first (process.data), in order to run it in MARK (mark)
    proc.pradsen <- process.data(inp1, model= "Pradsen")  #Pradel's survival and "growth"
    time.1 <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time, p=p.time, Gamma=L.time))
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  ## after running the model, we collect the results from the model in the list reruns
  reruns[[i]]=time.1$results$real
  
}


## commands and steps are exactly as for the loop of 100 replicate runs above.
## the only difference in the following loop is that we run a different model (Pradlambda instead of Pradsen)
reruns.lambda=list()

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

    summary(cast.gen.cov$cov)
    
    
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("attached")
    )
    
    
    
    inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0))
    
    inp1 <- unite(as.data.frame(inp1), "ch", c(1:11), sep = "")
    
    summary(inp1)
    
    proc.pradlambda <- process.data(inp1, model= "Pradlambda")
    time.lambda <- mark(proc.pradlambda, model.parameters = list(Phi=Phi.time, p=p.time, Lambda=L.time))
    
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  reruns.lambda[[i]]=time.lambda$results$real
  
}





#######################################################
#######################################################

## now we estimate the mean for all model estimates after 100 runs
## Pradsen parametrization
## mean parameter estimates
# mean_results <- rowMeans(do.call(cbind, lapply(reruns, function(x) x$estimate)))

## collecting 100 replicate parameter estimates
estimates <- do.call(cbind, lapply(reruns, function(x) x$estimate))
lcl <- do.call(cbind, lapply(reruns, function(x) x$lcl))
ucl <- do.call(cbind, lapply(reruns, function(x) x$ucl))


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

##
##
estimates.lambda <- do.call(cbind, lapply(reruns.lambda, function(x) x$estimate))
lcl.lambda <- do.call(cbind, lapply(reruns.lambda, function(x) x$lcl))
ucl.lambda <- do.call(cbind, lapply(reruns.lambda, function(x) x$ucl))

# estimates.lambda[estimates.lambda<0.00001|estimates.lambda==1|estimates.lambda>=1] <- NA
# lcl.lambda[is.na(estimates.lambda)] <- NA
# ucl.lambda[is.na(estimates.lambda)] <- NA

estimates.lambda[ucl.lambda-lcl.lambda==0|ucl.lambda-lcl.lambda>=15]<-NA
lcl.lambda[is.na(estimates.lambda)] <- NA
ucl.lambda[is.na(estimates.lambda)] <- NA

estimates.lambda.mean <- rowMeans(estimates.lambda, na.rm = T)
lcl.lambda.mean <- rowMeans(lcl.lambda, na.rm = T)
ucl.lambda.mean <- rowMeans(ucl.lambda, na.rm = T)

estimates.lambda.median <- apply(estimates.lambda, 1, median, na.rm=TRUE)
lcl.lambda.median <- apply(lcl.lambda, 1, median, na.rm=TRUE)
ucl.lambda.median <- apply(ucl.lambda, 1, median, na.rm=TRUE)



###########################################################################################
## extracting parameter estimates from the results above
phi <- estimates[2:8,]
phiu <- ucl[2:8,]
phil <- lcl[2:8,]

p <- estimates[13:19,]
pu <- ucl[13:19,]
pl <- lcl[13:19,]

gam <- estimates[23:29,]
gamu <- ucl[23:29,]
gaml <- lcl[23:29,]


lambda <- estimates.lambda[23:29,]
lambdau <- ucl.lambda[23:29,]
lambdal <- lcl.lambda[23:29,]

## extracting parameter estimates medians from the results above
phi.median <- estimates.median[2:8]
phiu.median <- ucl.median[2:8]
phil.median <- lcl.median[2:8]

p.median <- estimates.median[13:19]
pu.median <- ucl.median[13:19]
pl.median <- lcl.median[13:19]

gam.median <- estimates.median[23:29]
gamu.median <- ucl.median[23:29]
gaml.median <- lcl.median[23:29]


lambda.median <- estimates.lambda.median[23:29]
lambdau.median <- ucl.lambda.median[23:29]
lambdal.median <- lcl.lambda.median[23:29]

t <- c(9.65,7.7,5.2,5.8,7.15,6.6,4.6)
# following vector is time per interval/stageslice
tp <- c(7.7,7.7,2.7,8.9,5.4,7.8,1.4)

origprob <- 1-gam
origCIl <- 1-gaml
origCIu <- 1-gamu
Orig_rate <- -log(1-origprob)/t
Orig_rate_CIl <- -log(1-origCIl)/t
Orig_rate_CIu <- -log(1-origCIu)/t

extinctprob <- 1-phi
extinctCIl <- 1-phil
extinctCIu <- 1-phiu
Ext_rate <- -log(1-extinctprob)/t
Ext_rate_CIl <- -log(1-extinctCIl)/t
Ext_rate_CIu <- -log(1-extinctCIu)/t

rate_p <- -log(1-p)/tp
ratel_p <- -log(1-pl)/tp
rateu_p <- -log(1-pu)/tp

### estimating median rates
origprob.median <- 1-gam.median
origCIl.median <- 1-gaml.median
origCIu.median <- 1-gamu.median
Orig_rate.median <- -log(1-origprob.median)/t
Orig_rate_CIl.median <- -log(1-origCIl.median)/t
Orig_rate_CIu.median <- -log(1-origCIu.median)/t

extinctprob.median <- 1-phi.median
extinctCIl.median <- 1-phil.median
extinctCIu.median <- 1-phiu.median
Ext_rate.median <- -log(1-extinctprob.median)/t
Ext_rate_CIl.median <- -log(1-extinctCIl.median)/t
Ext_rate_CIu.median <- -log(1-extinctCIu.median)/t

rate_p.median <- -log(1-p.median)/tp
ratel_p.median <- -log(1-pl.median)/tp
rateu_p.median <- -log(1-pu.median)/tp


#######################################################
#######################################################
## commands and steps are exactly as for the first loop of 100 replicate runs above.
## the only difference is that we extract all free living benthos taxa (n.at)

reruns.ob=list()

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
    
    summary(cast.gen.cov$cov)
    
    
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("n.at")
    )
    
    
    inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0))
    
    inp1 <- unite(as.data.frame(inp1), "ch", c(1:11), sep = "")
    
    summary(inp1)
    
    proc.pradsen <- process.data(inp1, model= "Pradsen")  #Pradel's survival and "growth"
    time.1.ob <- mark(proc.pradsen, model.parameters = list(Phi=Phi.time, p=p.time, Gamma=L.time))
    
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  reruns.ob[[i]]=time.1.ob$results$real
  
}


## commands and steps are exactly as for the loop of 100 replicate runs above.
## the only difference in the following loop is that we run a different model (Pradlambda instead of Pradsen)
reruns.lambda.ob=list()

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
    
    summary(cast.gen.cov$cov)
    
    
    
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("n.at")
    )
    
    
    inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0))
    
    inp1 <- unite(as.data.frame(inp1), "ch", c(1:11), sep = "")
    
    summary(inp1)
    
    proc.pradlambda <- process.data(inp1, model= "Pradlambda")
    time.lambda.ob <- mark(proc.pradlambda, model.parameters = list(Phi=Phi.time, p=p.time, Lambda=L.time))
    
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  reruns.lambda.ob[[i]]=time.lambda.ob$results$real
  
}





#######################################################
#######################################################

## now we estimate the mean for all model estimates after 100 runs
## Pradsen parametrization
## mean parameter estimates
# mean_results <- rowMeans(do.call(cbind, lapply(reruns, function(x) x$estimate)))
# save.image("//kant/nhm-sfs-u2/franzif/paper2/100.repruns.median.RData")
# save.image("//kant/nhm-sfs-u2/franzif/paper2/100.repruns.median.plusParacrinoids.RData")

## collecting 100 replicate parameter estimates
estimates.ob <- do.call(cbind, lapply(reruns.ob, function(x) x$estimate))
lcl.ob <- do.call(cbind, lapply(reruns.ob, function(x) x$lcl))
ucl.ob <- do.call(cbind, lapply(reruns.ob, function(x) x$ucl))

estimates.ob[estimates.ob<0.00001|estimates.ob==1|estimates.ob>=1] <- NA
lcl.ob[is.na(estimates.ob)] <- NA
ucl.ob[is.na(estimates.ob)] <- NA

estimates.ob[ucl.ob-lcl.ob==0|ucl.ob-lcl.ob==1|ucl.ob>1]<-NA
lcl.ob[is.na(estimates.ob)] <- NA
ucl.ob[is.na(estimates.ob)] <- NA


estimates.ob.mean <- rowMeans(estimates.ob, na.rm = T)
lcl.ob.mean <- rowMeans(lcl.ob, na.rm = T)
ucl.ob.mean <- rowMeans(ucl.ob, na.rm = T)

estimates.ob.median <- apply(estimates.ob, 1, median, na.rm=TRUE)
lcl.ob.median <- apply(lcl.ob, 1, median, na.rm=TRUE)
ucl.ob.median <- apply(ucl.ob, 1, median, na.rm=TRUE)


# 
# estimate.means.ob <- rowMeans(estimates.ob, na.rm = T)
##
##
##
estimates.lambda.ob <- do.call(cbind, lapply(reruns.lambda.ob, function(x) x$estimate))
lcl.lambda.ob <- do.call(cbind, lapply(reruns.lambda.ob, function(x) x$lcl))
ucl.lambda.ob <- do.call(cbind, lapply(reruns.lambda.ob, function(x) x$ucl))

# estimates.lambda[estimates.lambda<0.00001|estimates.lambda==1|estimates.lambda>=1] <- NA
# lcl.lambda[is.na(estimates.lambda)] <- NA
# ucl.lambda[is.na(estimates.lambda)] <- NA
# 
estimates.lambda.ob[ucl.lambda.ob-lcl.lambda.ob==0|ucl.lambda.ob-lcl.lambda.ob>=15]<-NA
lcl.lambda.ob[is.na(estimates.lambda.ob)] <- NA
ucl.lambda.ob[is.na(estimates.lambda.ob)] <- NA

# estimates.lambda[ucl.lambda-lcl.lambda==0|ucl.lambda-lcl.lambda==1|ucl>1]<-NA
# lcl.lambda[is.na(estimates.lambda)] <- NA
# ucl.lambda[is.na(estimates.lambda)] <- NA


estimates.lambda.ob.mean <- rowMeans(estimates.lambda.ob, na.rm = T)
lcl.lambda.ob.mean <- rowMeans(lcl.lambda.ob, na.rm = T)
ucl.lambda.ob.mean <- rowMeans(ucl.lambda.ob, na.rm = T)

estimates.lambda.ob.median <- apply(estimates.lambda.ob, 1, median, na.rm=TRUE)
lcl.lambda.ob.median <- apply(lcl.lambda.ob, 1, median, na.rm=TRUE)
ucl.lambda.ob.median <- apply(ucl.lambda.ob, 1, median, na.rm=TRUE)




###########################################################################################
## extracting parameter estimates from the results above
phi.ob <- estimates.ob[2:8,]
phiu.ob <- ucl.ob[2:8,]
phil.ob <- lcl.ob[2:8,]

p.ob <- estimates.ob[13:19,]
pu.ob <- ucl.ob[13:19,]
pl.ob <- lcl.ob[13:19,]

gam.ob <- estimates.ob[23:29,]
gamu.ob <- ucl.ob[23:29,]
gaml.ob <- lcl.ob[23:29,]


lambda.ob <- estimates.lambda.ob[23:29,]
lambdau.ob <- ucl.lambda.ob[23:29,]
lambdal.ob <- lcl.lambda.ob[23:29,]

## extracting median parameter estimates from the results above
phi.ob.median <- estimates.ob.median[2:8]
phiu.ob.median <- ucl.ob.median[2:8]
phil.ob.median <- lcl.ob.median[2:8]

p.ob.median <- estimates.ob.median[13:19]
pu.ob.median <- ucl.ob.median[13:19]
pl.ob.median <- lcl.ob.median[13:19]

gam.ob.median <- estimates.ob.median[23:29]
gamu.ob.median <- ucl.ob.median[23:29]
gaml.ob.median <- lcl.ob.median[23:29]


lambda.ob.median <- estimates.lambda.ob.median[23:29]
lambdau.ob.median <- ucl.lambda.ob.median[23:29]
lambdal.ob.median <- lcl.lambda.ob.median[23:29]


t <- c(9.65,7.7,5.2,5.8,7.15,6.6,4.6)
# following vector is time per interval/stageslice
tp <- c(7.7,7.7,2.7,8.9,5.4,7.8,1.4)

origprob.ob <- 1-gam.ob
origCIl.ob <- 1-gaml.ob
origCIu.ob <- 1-gamu.ob
Orig_rate.ob <- -log(1-origprob.ob)/t
Orig_rate_CIl.ob <- -log(1-origCIl.ob)/t
Orig_rate_CIu.ob <- -log(1-origCIu.ob)/t

extinctprob.ob <- 1-phi.ob
extinctCIl.ob <- 1-phil.ob
extinctCIu.ob <- 1-phiu.ob
Ext_rate.ob <- -log(1-extinctprob.ob)/t
Ext_rate_CIl.ob <- -log(1-extinctCIl.ob)/t
Ext_rate_CIu.ob <- -log(1-extinctCIu.ob)/t

rate_p.ob <- -log(1-p.ob)/tp
ratel_p.ob <- -log(1-pl.ob)/tp
rateu_p.ob <- -log(1-pu.ob)/tp


### Estimating median rates
origprob.ob.median <- 1-gam.ob.median
origCIl.ob.median <- 1-gaml.ob.median
origCIu.ob.median <- 1-gamu.ob.median
Orig_rate.ob.median <- -log(1-origprob.ob.median)/t
Orig_rate_CIl.ob.median <- -log(1-origCIl.ob.median)/t
Orig_rate_CIu.ob.median <- -log(1-origCIu.ob.median)/t

extinctprob.ob.median <- 1-phi.ob.median
extinctCIl.ob.median <- 1-phil.ob.median
extinctCIu.ob.median <- 1-phiu.ob.median
Ext_rate.ob.median <- -log(1-extinctprob.ob.median)/t
Ext_rate_CIl.ob.median <- -log(1-extinctCIl.ob.median)/t
Ext_rate_CIu.ob.median <- -log(1-extinctCIu.ob.median)/t

rate_p.ob.median <- -log(1-p.ob.median)/tp
ratel_p.ob.median <- -log(1-pl.ob.median)/tp
rateu_p.ob.median <- -log(1-pu.ob.median)/tp

save.image("//kant/nhm-sfs-u2/franzif/paper2/100.repruns.median.minusParacrinoids.RData")


load("//kant/nhm-sfs-u2/franzif/paper2/100.repruns.median.minusParacrinoids.RData")



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


tiff("median.rep100.minusParacrinoidea.tiff",
     res = 600,
     units = "mm",
     width = 166,
     height = 175,
     pointsize = 9)
par(mfrow=c(4,2), mar = c(0.5,2,0.1,0), oma = c(3,2,1,0))


plot(Stagebase, Orig_rate.median[1:7],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)


lines(Stagebase, Orig_rate.median[1:7], col = "black", type = "b")
  lines(Stagebase, Orig_rate_CIl.median[1:7], col = "grey65")
  lines(Stagebase, Orig_rate_CIu.median[1:7], col = "grey65")

# text(Stagemidpoints[2], 0.9, labels = "hard substrate taxa")

legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "hard substrate taxa")

# axis(1, col = 'grey75', line = 0.5, at = seq(445,485,10) )
axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))

# mtext("Age (Ma)", side = 1, line = 2.5)
mtext("Origination events per myrs", side = 2, line = 2)

#######################################################
#######################################################
plot(Stagebase, Orig_rate.ob.median[1:7],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)



lines(Stagebase, Orig_rate.ob.median[1:7], col = "black", type = "b")
  lines(Stagebase, Orig_rate_CIl.ob.median[1:7], col = "grey65")
  lines(Stagebase, Orig_rate_CIu.ob.median[1:7], col = "grey65")

# text(Stagemidpoints[2], 0.9, labels = "free living benthos")

legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "free-living benthic taxa")

# axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))
# mtext("Origination events per myrs", side = 2, line = 2)

#######################################################
#######################################################
plot(Stagebase, Ext_rate.median[1:7],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)

lines(Stagebase, Ext_rate.median[1:7], col = "black", type = "b")
  lines(Stagebase, Ext_rate_CIl.median[1:7], col = "grey65")
  lines(Stagebase, Ext_rate_CIu.median[1:7], col = "grey65")


# lines(Stagemidpoints,mean.N.B[3:9],type="b", lwd=1.5, pch=19)
# lines(Stagemidpoints,mean.N.L[3:9],type="b", lwd=1.5, pch=17, lty=2)


axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))

mtext("Extinction events per myrs", side = 2, line = 2)


#######################################################
#######################################################
plot(Stagebase, Ext_rate.ob.median[1:7],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)


lines(Stagebase, Ext_rate.ob.median[1:7], col = "black", type = "b")
  lines(Stagebase, Ext_rate_CIl.ob.median[1:7], col = "grey65")
  lines(Stagebase, Ext_rate_CIu.ob.median[1:7], col = "grey65")


# axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))
# 
# mtext("Extinction events per myrs", side = 2, line = 2)


#######################################################
#######################################################
plot(Stagebase, lambda.median[1:7]-1, type = "b", 
     # main = "Net Diversification", 
     ylim = c(-1.75,5),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(5, -1.2, -1.75)
abline(h = 0, col="darkgrey")

lines(Stagebase, lambda.median[1:7]-1, col = "black", type = "b")
  lines(Stagebase, lambdal.median[1:7]-1, col = "grey65")
  lines(Stagebase, lambdau.median[1:7]-1, col = "grey65")


# legend("topleft", legend="C", bty="n", cex = 1.25)

axis(2, col = 'grey75', line = -0.2, at = seq(-1, 5, 1))

mtext("Net diversification rate", side = 2, line = 2)


#######################################################
#######################################################
plot(Stagebase, lambda.ob.median[1:7]-1, type = "b", 
     # main = "Net Diversification", 
     ylim = c(-1.75,5),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(5, -1.2, -1.75)
abline(h = 0, col="darkgrey")

lines(Stagebase, lambda.ob.median[1:7]-1, col = "black", type = "b")
  lines(Stagebase, lambdal.ob.median[1:7]-1, col = "grey65")
  lines(Stagebase, lambdau.ob.median[1:7]-1, col = "grey65")


# axis(2, col = 'grey75', line = -0.2, at = seq(-1, 8, 1))
# 
# mtext("Net diversification rate", side = 2, line = 2)


#######################################################
#######################################################
plot(Stagemidpoints, rate_p.median[1:7],
     type = "b", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)


lines(Stagemidpoints, rate_p.median[1:7], col = "black", type = "b")
  lines(Stagemidpoints, ratel_p.median[1:7], col = "grey65")
  lines(Stagemidpoints, rateu_p.median[1:7], col = "grey65")


# lines(Stagemidpoints,mean.N.B[3:9],type="b", lwd=1.5, pch=19)
# lines(Stagemidpoints,mean.N.L[3:9],type="b", lwd=1.5, pch=17, lty=2)


axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.1)
mtext("Sampling events per myrs", side = 2, line = 2)

#######################################################
#######################################################
plot(Stagemidpoints, rate_p.ob.median[1:7],
     type = "b", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)

lines(Stagemidpoints, rate_p.ob.median[1:7], col = "black", type = "b")
  lines(Stagemidpoints, ratel_p.ob.median[1:7], col = "grey65")
  lines(Stagemidpoints, rateu_p.ob.median[1:7], col = "grey65")


axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
# axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.1)
# mtext("Sampling events per myrs", side = 2, line = 2)


dev.off()









 


### Supplement information
## Template for Fig. S1 and Fig. S3 (depending on Paracrinoids were in- or excluded)
tiff("att.fl.rep100.minusParacrinoidea.tiff",
     res = 600,
     units = "mm",
     width = 166,
     height = 175,
     pointsize = 9)

par(mfrow=c(4,2), mar = c(0.5,2,0.1,1), oma = c(3,2,0,0))


plot(Stagebase, Orig_rate[1:7,1],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)



for (i in 1:100){
  lines(Stagebase, Orig_rate_CIl[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate_CIu[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate[1:7,i], col = "black")}

legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend="hard substrate taxa")

# axis(1, col = 'grey75', line = 0.5, at = seq(445,485,10) )
axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))

# mtext("Age (Ma)", side = 1, line = 2.5)
mtext("Origination events per myrs", side = 2, line = 2)

#######################################################
#######################################################
plot(Stagebase, Orig_rate.ob[1:7,1],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)




for (i in 1:100){
  lines(Stagebase, Orig_rate_CIl.ob[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate_CIu.ob[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate.ob[1:7,i], col = "black")}

legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend="free-living benthic taxa")


# axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))
# 
# mtext("Origination events per myrs", side = 2, line = 2)

#######################################################
#######################################################
plot(Stagebase, Ext_rate[1:7,1],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)



for (i in 1:100){
  lines(Stagebase, Ext_rate_CIl[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate_CIu[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate[1:7,i], col = "black")}

axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))

mtext("Extinction events per myrs", side = 2, line = 2)


#######################################################
#######################################################
plot(Stagebase, Ext_rate.ob[1:7,1],
type = "b", 
ylim = c(-0.075,0.5),
xlim = rev(c(443.8,485)),
axes = F,
xlab = "",
ylab = "")
tscales.Ord(0.5, -0.025, -0.075)



for (i in 1:100){
  lines(Stagebase, Ext_rate_CIl.ob[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate_CIu.ob[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate.ob[1:7,i], col = "black")}

# axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))
# 
# mtext("Extinction events per myrs", side = 2, line = 2)


#######################################################
#######################################################
plot(Stagebase, lambda[1:7,1]-1, type = "b", 
     # main = "Net Diversification", 
     ylim = c(-2,8),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(8, -1.2, -2)
abline(h = 0, col="darkgrey")


for (i in 1:100){
  lines(Stagebase, lambdal[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambdau[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambda[1:7,i]-1, col = "black")}

axis(2, col = 'grey75', line = -0.2, at = seq(-1, 8, 1))

mtext("Net diversification rate", side = 2, line = 2)


#######################################################
#######################################################
plot(Stagebase, lambda.ob[1:7,1]-1, type = "b", 
     # main = "Net Diversification", 
     ylim = c(-2,8),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(8, -1.2, -2)
abline(h = 0, col="darkgrey")


for (i in 1:100){
  lines(Stagebase, lambdal.ob[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambdau.ob[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambda.ob[1:7,i]-1, col = "black")}

# axis(2, col = 'grey75', line = -0.2, at = seq(-1, 8, 1))
# 
# mtext("Net diversification rate", side = 2, line = 2)


#######################################################
#######################################################
plot(Stagemidpoints, rate_p[1:7,1],
     type = "b", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)



for (i in 1:100){
  lines(Stagemidpoints, ratel_p[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rateu_p[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rate_p[1:7,i], col = "black")}

axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.1)
mtext("Sampling events per myrs", side = 2, line = 2)

#######################################################
#######################################################
plot(Stagemidpoints, rate_p.ob[1:7,1],
     type = "b", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)



for (i in 1:100){
  lines(Stagemidpoints, ratel_p.ob[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rateu_p.ob[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rate_p.ob[1:7,i], col = "black")}

axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
# axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.1)
# mtext("Sampling events per myrs", side = 2, line = 2)


dev.off()










# tiff("median.newdata.rep10.att.tiff",
#      res = 600,
#      units = "mm",
#      width = 80,
#      height = 160,
#      pointsize = 9)
# par(mfrow=c(4,1), mar = c(0.5,2,0.1,1), oma = c(3,2,0,0))
# 
# 
# plot(Stagebase, Orig_rate.median[1:7],
#      type = "b", 
#      ylim = c(-0.075,0.5),
#      xlim = rev(c(443.8,485)),
#      axes = F,
#      xlab = "",
#      ylab = "")
# tscales.Ord(0.5, -0.025, -0.075)
# 
# 
# lines(Stagebase, Orig_rate.median[1:7], col = "black", type = "b")
# lines(Stagebase, Orig_rate_CIl.median[1:7], col = "grey65")
# lines(Stagebase, Orig_rate_CIu.median[1:7], col = "grey65")
# 
# # text(Stagemidpoints[2], 0.9, labels = "hard substrate taxa")
# 
# legend("topright",
#        inset=0.05,
#        box.lty=0,
#        text.font=2,
#        legend = "hard substrate taxa")
# 
# # axis(1, col = 'grey75', line = 0.5, at = seq(445,485,10) )
# axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))
# 
# # mtext("Age (Ma)", side = 1, line = 2.5)
# mtext("Origination events per myrs", side = 2, line = 2)
# 
# #######################################################
# #######################################################
# plot(Stagebase, Ext_rate.median[1:7],
#      type = "b", 
#      ylim = c(-0.075,0.5),
#      xlim = rev(c(443.8,485)),
#      axes = F,
#      xlab = "",
#      ylab = "")
# tscales.Ord(0.5, -0.025, -0.075)
# 
# lines(Stagebase, Ext_rate.median[1:7], col = "black", type = "b")
# lines(Stagebase, Ext_rate_CIl.median[1:7], col = "grey65")
# lines(Stagebase, Ext_rate_CIu.median[1:7], col = "grey65")
# 
# 
# # lines(Stagemidpoints,mean.N.B[3:9],type="b", lwd=1.5, pch=19)
# # lines(Stagemidpoints,mean.N.L[3:9],type="b", lwd=1.5, pch=17, lty=2)
# 
# 
# axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))
# 
# mtext("Extinction events per myrs", side = 2, line = 2)
# 
# 
# #######################################################
# #######################################################
# plot(Stagebase, lambda.median[1:7]-1, type = "b", 
#      # main = "Net Diversification", 
#      ylim = c(-1.75,5),
#      xlim = rev(c(444.18,485.4)),
#      axes = F,
#      xlab = "",
#      ylab = "")
# 
# tscales.Ord(5, -1.2, -1.75)
# abline(h = 0, col="darkgrey")
# 
# lines(Stagebase, lambda.median[1:7]-1, col = "black", type = "b")
# lines(Stagebase, lambdal.median[1:7]-1, col = "grey65")
# lines(Stagebase, lambdau.median[1:7]-1, col = "grey65")
# 
# 
# # legend("topleft", legend="C", bty="n", cex = 1.25)
# 
# axis(2, col = 'grey75', line = -0.2, at = seq(-1, 5, 1))
# 
# mtext("Net diversification rate", side = 2, line = 2)
# 
# 
# #######################################################
# #######################################################
# plot(Stagemidpoints, rate_p.median[1:7],
#      type = "b", 
#      ylim = c(-0.15,1),
#      xlim = rev(c(443.8,485)),
#      axes = F,
#      xlab = "",
#      ylab = "")
# tscales.Ord(1, -0.05, -0.15)
# 
# 
# lines(Stagemidpoints, rate_p.median[1:7], col = "black", type = "b")
# lines(Stagemidpoints, ratel_p.median[1:7], col = "grey65")
# lines(Stagemidpoints, rateu_p.median[1:7], col = "grey65")
# 
# 
# # lines(Stagemidpoints,mean.N.B[3:9],type="b", lwd=1.5, pch=19)
# # lines(Stagemidpoints,mean.N.L[3:9],type="b", lwd=1.5, pch=17, lty=2)
# 
# 
# axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
# axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))
# 
# mtext("Age (Ma)", side = 1, line = 2.1)
# mtext("Sampling events per myrs", side = 2, line = 2)
# 
# #######################################################
# dev.off()
# 
# 
# 
# 
# 
