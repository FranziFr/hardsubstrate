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
  Paracrinoidea,
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


## we will be creating four individual lists, where we store the results from 100 individual model runs
## the list is created by the command list() and filled in at the end of the loop (scroll down to the end of {})
reruns.Echinos=list()

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
    # melt_genus <- melt(genus.age, id=c("genus", "SS"), na.rm = TRUE)
    # melt_genus <- na.omit(melt_genus)
    # cast_genus <- cast(melt_genus, genus~SS, length)
    # cast_genus <- as.data.frame(cast_genus)
    # names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    genus.age <- genus.age[!is.na(genus.age$SS),]
    cast_genus <- dcast(genus.age, genus~SS, length)
    cast_genus <- as.data.frame(cast_genus)
    names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    
    ## in addition to our observations/non-observations, we need to know which groups the individual lines (genera) belong to
    ## possible groups are hard substrate taxa (attached) or free living benthos (n.at - abbr. for non attached)
    ## these assignments are added as an extra column at the end of the observation/non-observation matrix
    ## to assign a genus, we use the genus names that we extracted above, before the loop started
    # cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
    #   genus.name %in% attached.names ~ "attached",
    #   genus.name %in% Bryozoa.names ~ "attached",
    #   genus.name %in% Coral.names ~ "attached",
    #   genus.name %in% Porifera.names ~ "attached",
    #   genus.name %in% Brachiopoda.names ~ "attached",
    #   # genus.name %in% rest.Echinos ~ "attached",
    #   genus.name %in% Trilobita.names ~ "n.at",
    #   genus.name %in% Mollusca.names ~ "n.at",
    #   genus.name %in% unattached.names ~ "n.at"
    #   ))
    cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
      genus.name %in% attached.names ~ "Echino",
      genus.name %in% Bryozoa.names ~ "Bryo",
      genus.name %in% Coral.names ~ "Coral",
      genus.name %in% Porifera.names ~ "Pori",
      genus.name %in% Brachiopoda.names ~ "Brachio"
      # genus.name %in% rest.Echinos ~ "attached",
      # genus.name %in% Trilobita.names ~ "n.at",
      # genus.name %in% Mollusca.names ~ "n.at",
      # genus.name %in% unattached.names ~ "n.at"
    ))
    
    cast.gen.cov$cov <- as.factor(cast.gen.cov$cov)
    
    summary(cast.gen.cov$cov)
    
    ## in this first step, we are only interested in hard substrate taxa
    ## hence, we only include the "attached" into our input file
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("Echino")
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
  reruns.Echinos[[i]]=time.1$results$real
  
}


## commands and steps are exactly as for the loop of 100 replicate runs above.
## the only difference in the following loop is that we run a different model (Pradlambda instead of Pradsen)
reruns.lambda.Echinos=list()

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
    # cast_genus <- cast(melt_genus, genus~SS, length)
    # cast_genus <- as.data.frame(cast_genus)
    # names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    genus.age <- genus.age[!is.na(genus.age$SS),]
    cast_genus <- dcast(genus.age, genus~SS, length)
    cast_genus <- as.data.frame(cast_genus)
    names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    
    # cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
    #   genus.name %in% attached.names ~ "attached",
    #   genus.name %in% Bryozoa.names ~ "attached",
    #   genus.name %in% Coral.names ~ "attached",
    #   genus.name %in% Porifera.names ~ "attached",
    #   genus.name %in% Brachiopoda.names ~ "attached",
    #   # genus.name %in% rest.Echinos ~ "attached",
    #   genus.name %in% Trilobita.names ~ "n.at",
    #   genus.name %in% Mollusca.names ~ "n.at",
    #   genus.name %in% unattached.names ~ "n.at"
    # ))
    cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
      genus.name %in% attached.names ~ "Echino",
      genus.name %in% Bryozoa.names ~ "Bryo",
      genus.name %in% Coral.names ~ "Coral",
      genus.name %in% Porifera.names ~ "Pori",
      genus.name %in% Brachiopoda.names ~ "Brachio"
      # genus.name %in% rest.Echinos ~ "attached",
      # genus.name %in% Trilobita.names ~ "n.at",
      # genus.name %in% Mollusca.names ~ "n.at",
      # genus.name %in% unattached.names ~ "n.at"
    ))
    
    summary(cast.gen.cov$cov)
    
    
    
    # inp1 <- cast.gen.cov %>% filter(
    #   cov %in% c("attached")
    # )
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("Echino")
    )
    
    
    
    inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0))
    
    inp1 <- unite(as.data.frame(inp1), "ch", c(1:11), sep = "")
    
    summary(inp1)
    
    proc.pradlambda <- process.data(inp1, model= "Pradlambda")
    time.lambda <- mark(proc.pradlambda, model.parameters = list(Phi=Phi.time, p=p.time, Lambda=L.time))
    
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  reruns.lambda.Echinos[[i]]=time.lambda$results$real
  
}

reruns.Bryozoa=list()

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
    # melt_genus <- melt(genus.age, id=c("genus", "SS"), na.rm = TRUE)
    # melt_genus <- na.omit(melt_genus)
    # cast_genus <- cast(melt_genus, genus~SS, length)
    # cast_genus <- as.data.frame(cast_genus)
    # names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    genus.age <- genus.age[!is.na(genus.age$SS),]
    cast_genus <- dcast(genus.age, genus~SS, length)
    cast_genus <- as.data.frame(cast_genus)
    names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    ## in addition to our observations/non-observations, we need to know which groups the individual lines (genera) belong to
    ## possible groups are hard substrate taxa (attached) or free living benthos (n.at - abbr. for non attached)
    ## these assignments are added as an extra column at the end of the observation/non-observation matrix
    ## to assign a genus, we use the genus names that we extracted above, before the loop started
    # cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
    #   genus.name %in% attached.names ~ "attached",
    #   genus.name %in% Bryozoa.names ~ "attached",
    #   genus.name %in% Coral.names ~ "attached",
    #   genus.name %in% Porifera.names ~ "attached",
    #   genus.name %in% Brachiopoda.names ~ "attached",
    #   # genus.name %in% rest.Echinos ~ "attached",
    #   genus.name %in% Trilobita.names ~ "n.at",
    #   genus.name %in% Mollusca.names ~ "n.at",
    #   genus.name %in% unattached.names ~ "n.at"
    #   ))
    cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
      genus.name %in% attached.names ~ "Echino",
      genus.name %in% Bryozoa.names ~ "Bryo",
      genus.name %in% Coral.names ~ "Coral",
      genus.name %in% Porifera.names ~ "Pori",
      genus.name %in% Brachiopoda.names ~ "Brachio"
      # genus.name %in% rest.Echinos ~ "attached",
      # genus.name %in% Trilobita.names ~ "n.at",
      # genus.name %in% Mollusca.names ~ "n.at",
      # genus.name %in% unattached.names ~ "n.at"
    ))
    
    cast.gen.cov$cov <- as.factor(cast.gen.cov$cov)
    
    summary(cast.gen.cov$cov)
    
    ## in this first step, we are only interested in hard substrate taxa
    ## hence, we only include the "attached" into our input file
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("Bryo")
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
  reruns.Bryozoa[[i]]=time.1$results$real
  
}

reruns.lambda.Bryozoa=list()

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
    # cast_genus <- cast(melt_genus, genus~SS, length)
    # cast_genus <- as.data.frame(cast_genus)
    # names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    genus.age <- genus.age[!is.na(genus.age$SS),]
    cast_genus <- dcast(genus.age, genus~SS, length)
    cast_genus <- as.data.frame(cast_genus)
    names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    
    # cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
    #   genus.name %in% attached.names ~ "attached",
    #   genus.name %in% Bryozoa.names ~ "attached",
    #   genus.name %in% Coral.names ~ "attached",
    #   genus.name %in% Porifera.names ~ "attached",
    #   genus.name %in% Brachiopoda.names ~ "attached",
    #   # genus.name %in% rest.Echinos ~ "attached",
    #   genus.name %in% Trilobita.names ~ "n.at",
    #   genus.name %in% Mollusca.names ~ "n.at",
    #   genus.name %in% unattached.names ~ "n.at"
    # ))
    cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
      genus.name %in% attached.names ~ "Echino",
      genus.name %in% Bryozoa.names ~ "Bryo",
      genus.name %in% Coral.names ~ "Coral",
      genus.name %in% Porifera.names ~ "Pori",
      genus.name %in% Brachiopoda.names ~ "Brachio"
      # genus.name %in% rest.Echinos ~ "attached",
      # genus.name %in% Trilobita.names ~ "n.at",
      # genus.name %in% Mollusca.names ~ "n.at",
      # genus.name %in% unattached.names ~ "n.at"
    ))
    
    summary(cast.gen.cov$cov)
    
    
    
    # inp1 <- cast.gen.cov %>% filter(
    #   cov %in% c("attached")
    # )
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("Bryo")
    )
    
    
    
    inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0))
    
    inp1 <- unite(as.data.frame(inp1), "ch", c(1:11), sep = "")
    
    summary(inp1)
    
    proc.pradlambda <- process.data(inp1, model= "Pradlambda")
    time.lambda <- mark(proc.pradlambda, model.parameters = list(Phi=Phi.time, p=p.time, Lambda=L.time))
    
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  reruns.lambda.Bryozoa[[i]]=time.lambda$results$real
  
}


reruns.Porifera=list()

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
    # melt_genus <- melt(genus.age, id=c("genus", "SS"), na.rm = TRUE)
    # melt_genus <- na.omit(melt_genus)
    # cast_genus <- cast(melt_genus, genus~SS, length)
    # cast_genus <- as.data.frame(cast_genus)
    # names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    genus.age <- genus.age[!is.na(genus.age$SS),]
    cast_genus <- dcast(genus.age, genus~SS, length)
    cast_genus <- as.data.frame(cast_genus)
    names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    
    ## in addition to our observations/non-observations, we need to know which groups the individual lines (genera) belong to
    ## possible groups are hard substrate taxa (attached) or free living benthos (n.at - abbr. for non attached)
    ## these assignments are added as an extra column at the end of the observation/non-observation matrix
    ## to assign a genus, we use the genus names that we extracted above, before the loop started
    # cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
    #   genus.name %in% attached.names ~ "attached",
    #   genus.name %in% Bryozoa.names ~ "attached",
    #   genus.name %in% Coral.names ~ "attached",
    #   genus.name %in% Porifera.names ~ "attached",
    #   genus.name %in% Brachiopoda.names ~ "attached",
    #   # genus.name %in% rest.Echinos ~ "attached",
    #   genus.name %in% Trilobita.names ~ "n.at",
    #   genus.name %in% Mollusca.names ~ "n.at",
    #   genus.name %in% unattached.names ~ "n.at"
    #   ))
    cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
      genus.name %in% attached.names ~ "Echino",
      genus.name %in% Bryozoa.names ~ "Bryo",
      genus.name %in% Coral.names ~ "Coral",
      genus.name %in% Porifera.names ~ "Pori",
      genus.name %in% Brachiopoda.names ~ "Brachio"
      # genus.name %in% rest.Echinos ~ "attached",
      # genus.name %in% Trilobita.names ~ "n.at",
      # genus.name %in% Mollusca.names ~ "n.at",
      # genus.name %in% unattached.names ~ "n.at"
    ))
    
    cast.gen.cov$cov <- as.factor(cast.gen.cov$cov)
    
    summary(cast.gen.cov$cov)
    
    ## in this first step, we are only interested in hard substrate taxa
    ## hence, we only include the "attached" into our input file
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("Pori")
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
  reruns.Porifera[[i]]=time.1$results$real
  
}

reruns.lambda.Porifera=list()

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
    # cast_genus <- cast(melt_genus, genus~SS, length)
    # cast_genus <- as.data.frame(cast_genus)
    # names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    genus.age <- genus.age[!is.na(genus.age$SS),]
    cast_genus <- dcast(genus.age, genus~SS, length)
    cast_genus <- as.data.frame(cast_genus)
    names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    
    # cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
    #   genus.name %in% attached.names ~ "attached",
    #   genus.name %in% Bryozoa.names ~ "attached",
    #   genus.name %in% Coral.names ~ "attached",
    #   genus.name %in% Porifera.names ~ "attached",
    #   genus.name %in% Brachiopoda.names ~ "attached",
    #   # genus.name %in% rest.Echinos ~ "attached",
    #   genus.name %in% Trilobita.names ~ "n.at",
    #   genus.name %in% Mollusca.names ~ "n.at",
    #   genus.name %in% unattached.names ~ "n.at"
    # ))
    cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
      genus.name %in% attached.names ~ "Echino",
      genus.name %in% Bryozoa.names ~ "Bryo",
      genus.name %in% Coral.names ~ "Coral",
      genus.name %in% Porifera.names ~ "Pori",
      genus.name %in% Brachiopoda.names ~ "Brachio"
      # genus.name %in% rest.Echinos ~ "attached",
      # genus.name %in% Trilobita.names ~ "n.at",
      # genus.name %in% Mollusca.names ~ "n.at",
      # genus.name %in% unattached.names ~ "n.at"
    ))
    
    summary(cast.gen.cov$cov)
    
    
    
    # inp1 <- cast.gen.cov %>% filter(
    #   cov %in% c("attached")
    # )
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("Pori")
    )
    
    
    
    inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0))
    
    inp1 <- unite(as.data.frame(inp1), "ch", c(1:11), sep = "")
    
    summary(inp1)
    
    proc.pradlambda <- process.data(inp1, model= "Pradlambda")
    time.lambda <- mark(proc.pradlambda, model.parameters = list(Phi=Phi.time, p=p.time, Lambda=L.time))
    
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  reruns.lambda.Porifera[[i]]=time.lambda$results$real
  
}

reruns.Corals=list()

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
    # melt_genus <- melt(genus.age, id=c("genus", "SS"), na.rm = TRUE)
    # melt_genus <- na.omit(melt_genus)
    # cast_genus <- cast(melt_genus, genus~SS, length)
    # cast_genus <- as.data.frame(cast_genus)
    # names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    genus.age <- genus.age[!is.na(genus.age$SS),]
    cast_genus <- dcast(genus.age, genus~SS, length)
    cast_genus <- as.data.frame(cast_genus)
    names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    
    ## in addition to our observations/non-observations, we need to know which groups the individual lines (genera) belong to
    ## possible groups are hard substrate taxa (attached) or free living benthos (n.at - abbr. for non attached)
    ## these assignments are added as an extra column at the end of the observation/non-observation matrix
    ## to assign a genus, we use the genus names that we extracted above, before the loop started
    # cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
    #   genus.name %in% attached.names ~ "attached",
    #   genus.name %in% Bryozoa.names ~ "attached",
    #   genus.name %in% Coral.names ~ "attached",
    #   genus.name %in% Porifera.names ~ "attached",
    #   genus.name %in% Brachiopoda.names ~ "attached",
    #   # genus.name %in% rest.Echinos ~ "attached",
    #   genus.name %in% Trilobita.names ~ "n.at",
    #   genus.name %in% Mollusca.names ~ "n.at",
    #   genus.name %in% unattached.names ~ "n.at"
    #   ))
    cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
      genus.name %in% attached.names ~ "Echino",
      genus.name %in% Bryozoa.names ~ "Bryo",
      genus.name %in% Coral.names ~ "Coral",
      genus.name %in% Porifera.names ~ "Pori",
      genus.name %in% Brachiopoda.names ~ "Brachio"
      # genus.name %in% rest.Echinos ~ "attached",
      # genus.name %in% Trilobita.names ~ "n.at",
      # genus.name %in% Mollusca.names ~ "n.at",
      # genus.name %in% unattached.names ~ "n.at"
    ))
    
    cast.gen.cov$cov <- as.factor(cast.gen.cov$cov)
    
    summary(cast.gen.cov$cov)
    
    ## in this first step, we are only interested in hard substrate taxa
    ## hence, we only include the "attached" into our input file
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("Coral")
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
  reruns.Corals[[i]]=time.1$results$real
  
}

reruns.lambda.Corals=list()

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
    # cast_genus <- cast(melt_genus, genus~SS, length)
    # cast_genus <- as.data.frame(cast_genus)
    # names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    genus.age <- genus.age[!is.na(genus.age$SS),]
    cast_genus <- dcast(genus.age, genus~SS, length)
    cast_genus <- as.data.frame(cast_genus)
    names(cast_genus)[names(cast_genus) == "genus"] <- "genus.name"
    
    
    # cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
    #   genus.name %in% attached.names ~ "attached",
    #   genus.name %in% Bryozoa.names ~ "attached",
    #   genus.name %in% Coral.names ~ "attached",
    #   genus.name %in% Porifera.names ~ "attached",
    #   genus.name %in% Brachiopoda.names ~ "attached",
    #   # genus.name %in% rest.Echinos ~ "attached",
    #   genus.name %in% Trilobita.names ~ "n.at",
    #   genus.name %in% Mollusca.names ~ "n.at",
    #   genus.name %in% unattached.names ~ "n.at"
    # ))
    cast.gen.cov <- cast_genus %>% mutate(cov=case_when(
      genus.name %in% attached.names ~ "Echino",
      genus.name %in% Bryozoa.names ~ "Bryo",
      genus.name %in% Coral.names ~ "Coral",
      genus.name %in% Porifera.names ~ "Pori",
      genus.name %in% Brachiopoda.names ~ "Brachio"
      # genus.name %in% rest.Echinos ~ "attached",
      # genus.name %in% Trilobita.names ~ "n.at",
      # genus.name %in% Mollusca.names ~ "n.at",
      # genus.name %in% unattached.names ~ "n.at"
    ))
    
    summary(cast.gen.cov$cov)
    
    
    
    # inp1 <- cast.gen.cov %>% filter(
    #   cov %in% c("attached")
    # )
    inp1 <- cast.gen.cov %>% filter(
      cov %in% c("Coral")
    )
    
    
    
    inp1 <- cbind.data.frame(ifelse(inp1[,2:12] >=1, 1, 0))
    
    inp1 <- unite(as.data.frame(inp1), "ch", c(1:11), sep = "")
    
    summary(inp1)
    
    proc.pradlambda <- process.data(inp1, model= "Pradlambda")
    time.lambda <- mark(proc.pradlambda, model.parameters = list(Phi=Phi.time, p=p.time, Lambda=L.time))
    
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  reruns.lambda.Corals[[i]]=time.lambda$results$real
  
}


## collecting 100 replicate parameter estimates
estimates.Echinos <- do.call(cbind, lapply(reruns.Echinos, function(x) x$estimate))
lcl.Echinos <- do.call(cbind, lapply(reruns.Echinos, function(x) x$lcl))
ucl.Echinos <- do.call(cbind, lapply(reruns.Echinos, function(x) x$ucl))


estimates.Echinos[estimates.Echinos<0.00001|estimates.Echinos==1|estimates.Echinos>=1] <- NA
lcl.Echinos[is.na(estimates.Echinos)] <- NA
ucl.Echinos[is.na(estimates.Echinos)] <- NA

estimates.Echinos[ucl.Echinos-lcl.Echinos==0|ucl.Echinos-lcl.Echinos==1|ucl.Echinos>1]<-NA
lcl.Echinos[is.na(estimates.Echinos)] <- NA
ucl.Echinos[is.na(estimates.Echinos)] <- NA

##
estimates.mean.Echinos <- rowMeans(estimates.Echinos, na.rm = T)
lcl.mean.Echinos <- rowMeans(lcl.Echinos, na.rm = T)
ucl.mean.Echinos <- rowMeans(ucl.Echinos, na.rm = T)

estimates.median.Echinos <- apply(estimates.Echinos, 1, median, na.rm=TRUE)
lcl.median.Echinos <- apply(lcl.Echinos,1,median, na.rm=TRUE)
ucl.median.Echinos <- apply(ucl.Echinos,1,median, na.rm=TRUE)

##
##
estimates.lambda.Echinos <- do.call(cbind, lapply(reruns.lambda.Echinos, function(x) x$estimate))
lcl.lambda.Echinos <- do.call(cbind, lapply(reruns.lambda.Echinos, function(x) x$lcl))
ucl.lambda.Echinos <- do.call(cbind, lapply(reruns.lambda.Echinos, function(x) x$ucl))

# estimates.lambda[estimates.lambda<0.00001|estimates.lambda==1|estimates.lambda>=1] <- NA
# lcl.lambda[is.na(estimates.lambda)] <- NA
# ucl.lambda[is.na(estimates.lambda)] <- NA
# 
estimates.lambda.Echinos[ucl.lambda.Echinos-lcl.lambda.Echinos==0|ucl.lambda.Echinos-lcl.lambda.Echinos>=15]<-NA
lcl.lambda.Echinos[is.na(estimates.lambda.Echinos)] <- NA
ucl.lambda.Echinos[is.na(estimates.lambda.Echinos)] <- NA

estimates.lambda.mean.Echinos <- rowMeans(estimates.lambda.Echinos, na.rm = T)
lcl.lambda.mean.Echinos <- rowMeans(lcl.lambda.Echinos, na.rm = T)
ucl.lambda.mean.Echinos <- rowMeans(ucl.lambda.Echinos, na.rm = T)

estimates.lambda.median.Echinos <- apply(estimates.lambda.Echinos, 1, median, na.rm=TRUE)
lcl.lambda.median.Echinos <- apply(lcl.lambda.Echinos, 1, median, na.rm=TRUE)
ucl.lambda.median.Echinos <- apply(ucl.lambda.Echinos, 1, median, na.rm=TRUE)

###########################################################################################
## extracting parameter estimates from the results above
phi.Echinos <- estimates.Echinos[2:8,]
phiu.Echinos <- ucl.Echinos[2:8,]
phil.Echinos <- lcl.Echinos[2:8,]

p.Echinos <- estimates.Echinos[13:19,]
pu.Echinos <- ucl.Echinos[13:19,]
pl.Echinos <- lcl.Echinos[13:19,]

gam.Echinos <- estimates.Echinos[23:29,]
gamu.Echinos <- ucl.Echinos[23:29,]
gaml.Echinos <- lcl.Echinos[23:29,]


lambda.Echinos <- estimates.lambda.Echinos[23:29,]
lambdau.Echinos <- ucl.lambda.Echinos[23:29,]
lambdal.Echinos <- lcl.lambda.Echinos[23:29,]

## extracting parameter estimates medians from the results above
phi.median.Echinos <- estimates.median.Echinos[2:8]
phiu.median.Echinos <- ucl.median.Echinos[2:8]
phil.median.Echinos <- lcl.median.Echinos[2:8]

p.median.Echinos <- estimates.median.Echinos[13:19]
pu.median.Echinos <- ucl.median.Echinos[13:19]
pl.median.Echinos <- lcl.median.Echinos[13:19]

gam.median.Echinos <- estimates.median.Echinos[23:29]
gamu.median.Echinos <- ucl.median.Echinos[23:29]
gaml.median.Echinos <- lcl.median.Echinos[23:29]


lambda.median.Echinos <- estimates.lambda.median.Echinos[23:29]
lambdau.median.Echinos <- ucl.lambda.median.Echinos[23:29]
lambdal.median.Echinos <- lcl.lambda.median.Echinos[23:29]

t <- c(9.65,7.7,5.2,5.8,7.15,6.6,4.6)
# following vector is time per interval/stageslice
tp <- c(7.7,7.7,2.7,8.9,5.4,7.8,1.4)

origprob.Echinos <- 1-gam.Echinos
origCIl.Echinos <- 1-gaml.Echinos
origCIu.Echinos <- 1-gamu.Echinos
Orig_rate.Echinos <- -log(1-origprob.Echinos)/t
Orig_rate_CIl.Echinos <- -log(1-origCIl.Echinos)/t
Orig_rate_CIu.Echinos <- -log(1-origCIu.Echinos)/t

extinctprob.Echinos <- 1-phi.Echinos
extinctCIl.Echinos <- 1-phil.Echinos
extinctCIu.Echinos <- 1-phiu.Echinos
Ext_rate.Echinos <- -log(1-extinctprob.Echinos)/t
Ext_rate_CIl.Echinos <- -log(1-extinctCIl.Echinos)/t
Ext_rate_CIu.Echinos <- -log(1-extinctCIu.Echinos)/t

rate_p.Echinos <- -log(1-p.Echinos)/tp
ratel_p.Echinos <- -log(1-pl.Echinos)/tp
rateu_p.Echinos <- -log(1-pu.Echinos)/tp

### estimating median rates
origprob.median.Echinos <- 1-gam.median.Echinos
origCIl.median.Echinos <- 1-gaml.median.Echinos
origCIu.median.Echinos <- 1-gamu.median.Echinos
Orig_rate.median.Echinos <- -log(1-origprob.median.Echinos)/t
Orig_rate_CIl.median.Echinos <- -log(1-origCIl.median.Echinos)/t
Orig_rate_CIu.median.Echinos <- -log(1-origCIu.median.Echinos)/t

extinctprob.median.Echinos <- 1-phi.median.Echinos
extinctCIl.median.Echinos <- 1-phil.median.Echinos
extinctCIu.median.Echinos <- 1-phiu.median.Echinos
Ext_rate.median.Echinos <- -log(1-extinctprob.median.Echinos)/t
Ext_rate_CIl.median.Echinos <- -log(1-extinctCIl.median.Echinos)/t
Ext_rate_CIu.median.Echinos <- -log(1-extinctCIu.median.Echinos)/t

rate_p.median.Echinos <- -log(1-p.median.Echinos)/tp
ratel_p.median.Echinos <- -log(1-pl.median.Echinos)/tp
rateu_p.median.Echinos <- -log(1-pu.median.Echinos)/tp

###
###
###
## collecting 100 replicate parameter estimates
estimates.Bryozoa <- do.call(cbind, lapply(reruns.Bryozoa, function(x) x$estimate))
lcl.Bryozoa <- do.call(cbind, lapply(reruns.Bryozoa, function(x) x$lcl))
ucl.Bryozoa <- do.call(cbind, lapply(reruns.Bryozoa, function(x) x$ucl))


estimates.Bryozoa[estimates.Bryozoa<0.00001|estimates.Bryozoa==1|estimates.Bryozoa>=1] <- NA
lcl.Bryozoa[is.na(estimates.Bryozoa)] <- NA
ucl.Bryozoa[is.na(estimates.Bryozoa)] <- NA

estimates.Bryozoa[ucl.Bryozoa-lcl.Bryozoa==0|ucl.Bryozoa-lcl.Bryozoa==1|ucl.Bryozoa>1]<-NA
lcl.Bryozoa[is.na(estimates.Bryozoa)] <- NA
ucl.Bryozoa[is.na(estimates.Bryozoa)] <- NA

##
estimates.mean.Bryozoa <- rowMeans(estimates.Bryozoa, na.rm = T)
lcl.mean.Bryozoa <- rowMeans(lcl.Bryozoa, na.rm = T)
ucl.mean.Bryozoa <- rowMeans(ucl.Bryozoa, na.rm = T)

estimates.median.Bryozoa <- apply(estimates.Bryozoa, 1, median, na.rm=TRUE)
lcl.median.Bryozoa <- apply(lcl.Bryozoa,1,median, na.rm=TRUE)
ucl.median.Bryozoa <- apply(ucl.Bryozoa,1,median, na.rm=TRUE)

##
##
estimates.lambda.Bryozoa <- do.call(cbind, lapply(reruns.lambda.Bryozoa, function(x) x$estimate))
lcl.lambda.Bryozoa <- do.call(cbind, lapply(reruns.lambda.Bryozoa, function(x) x$lcl))
ucl.lambda.Bryozoa <- do.call(cbind, lapply(reruns.lambda.Bryozoa, function(x) x$ucl))

# estimates.lambda[estimates.lambda<0.00001|estimates.lambda==1|estimates.lambda>=1] <- NA
# lcl.lambda[is.na(estimates.lambda)] <- NA
# ucl.lambda[is.na(estimates.lambda)] <- NA
# 

estimates.lambda.Bryozoa[ucl.lambda.Bryozoa-lcl.lambda.Bryozoa==0|ucl.lambda.Bryozoa-lcl.lambda.Bryozoa>=15]<-NA
lcl.lambda.Bryozoa[is.na(estimates.lambda.Bryozoa)] <- NA
ucl.lambda.Bryozoa[is.na(estimates.lambda.Bryozoa)] <- NA

estimates.lambda.mean.Bryozoa <- rowMeans(estimates.lambda.Bryozoa, na.rm = T)
lcl.lambda.mean.Bryozoa <- rowMeans(lcl.lambda.Bryozoa, na.rm = T)
ucl.lambda.mean.Bryozoa <- rowMeans(ucl.lambda.Bryozoa, na.rm = T)

estimates.lambda.median.Bryozoa <- apply(estimates.lambda.Bryozoa, 1, median, na.rm=TRUE)
lcl.lambda.median.Bryozoa <- apply(lcl.lambda.Bryozoa, 1, median, na.rm=TRUE)
ucl.lambda.median.Bryozoa <- apply(ucl.lambda.Bryozoa, 1, median, na.rm=TRUE)

###########################################################################################
## extracting parameter estimates from the results above
phi.Bryozoa <- estimates.Bryozoa[2:8,]
phiu.Bryozoa <- ucl.Bryozoa[2:8,]
phil.Bryozoa <- lcl.Bryozoa[2:8,]

p.Bryozoa <- estimates.Bryozoa[13:19,]
pu.Bryozoa <- ucl.Bryozoa[13:19,]
pl.Bryozoa <- lcl.Bryozoa[13:19,]

gam.Bryozoa <- estimates.Bryozoa[23:29,]
gamu.Bryozoa <- ucl.Bryozoa[23:29,]
gaml.Bryozoa <- lcl.Bryozoa[23:29,]


lambda.Bryozoa <- estimates.lambda.Bryozoa[23:29,]
lambdau.Bryozoa <- ucl.lambda.Bryozoa[23:29,]
lambdal.Bryozoa <- lcl.lambda.Bryozoa[23:29,]

## extracting parameter estimates medians from the results above
phi.median.Bryozoa <- estimates.median.Bryozoa[2:8]
phiu.median.Bryozoa <- ucl.median.Bryozoa[2:8]
phil.median.Bryozoa <- lcl.median.Bryozoa[2:8]

p.median.Bryozoa <- estimates.median.Bryozoa[13:19]
pu.median.Bryozoa <- ucl.median.Bryozoa[13:19]
pl.median.Bryozoa <- lcl.median.Bryozoa[13:19]

gam.median.Bryozoa <- estimates.median.Bryozoa[23:29]
gamu.median.Bryozoa <- ucl.median.Bryozoa[23:29]
gaml.median.Bryozoa <- lcl.median.Bryozoa[23:29]


lambda.median.Bryozoa <- estimates.lambda.median.Bryozoa[23:29]
lambdau.median.Bryozoa <- ucl.lambda.median.Bryozoa[23:29]
lambdal.median.Bryozoa <- lcl.lambda.median.Bryozoa[23:29]

t <- c(9.65,7.7,5.2,5.8,7.15,6.6,4.6)
# following vector is time per interval/stageslice
tp <- c(7.7,7.7,2.7,8.9,5.4,7.8,1.4)

origprob.Bryozoa <- 1-gam.Bryozoa
origCIl.Bryozoa <- 1-gaml.Bryozoa
origCIu.Bryozoa <- 1-gamu.Bryozoa
Orig_rate.Bryozoa <- -log(1-origprob.Bryozoa)/t
Orig_rate_CIl.Bryozoa <- -log(1-origCIl.Bryozoa)/t
Orig_rate_CIu.Bryozoa <- -log(1-origCIu.Bryozoa)/t

extinctprob.Bryozoa <- 1-phi.Bryozoa
extinctCIl.Bryozoa <- 1-phil.Bryozoa
extinctCIu.Bryozoa <- 1-phiu.Bryozoa
Ext_rate.Bryozoa <- -log(1-extinctprob.Bryozoa)/t
Ext_rate_CIl.Bryozoa <- -log(1-extinctCIl.Bryozoa)/t
Ext_rate_CIu.Bryozoa <- -log(1-extinctCIu.Bryozoa)/t

rate_p.Bryozoa <- -log(1-p.Bryozoa)/tp
ratel_p.Bryozoa <- -log(1-pl.Bryozoa)/tp
rateu_p.Bryozoa <- -log(1-pu.Bryozoa)/tp

### estimating median rates
origprob.median.Bryozoa <- 1-gam.median.Bryozoa
origCIl.median.Bryozoa <- 1-gaml.median.Bryozoa
origCIu.median.Bryozoa <- 1-gamu.median.Bryozoa
Orig_rate.median.Bryozoa <- -log(1-origprob.median.Bryozoa)/t
Orig_rate_CIl.median.Bryozoa <- -log(1-origCIl.median.Bryozoa)/t
Orig_rate_CIu.median.Bryozoa <- -log(1-origCIu.median.Bryozoa)/t

extinctprob.median.Bryozoa <- 1-phi.median.Bryozoa
extinctCIl.median.Bryozoa <- 1-phil.median.Bryozoa
extinctCIu.median.Bryozoa <- 1-phiu.median.Bryozoa
Ext_rate.median.Bryozoa <- -log(1-extinctprob.median.Bryozoa)/t
Ext_rate_CIl.median.Bryozoa <- -log(1-extinctCIl.median.Bryozoa)/t
Ext_rate_CIu.median.Bryozoa <- -log(1-extinctCIu.median.Bryozoa)/t

rate_p.median.Bryozoa <- -log(1-p.median.Bryozoa)/tp
ratel_p.median.Bryozoa <- -log(1-pl.median.Bryozoa)/tp
rateu_p.median.Bryozoa <- -log(1-pu.median.Bryozoa)/tp
###
###
###
## collecting 100 replicate parameter estimates
estimates.Porifera <- do.call(cbind, lapply(reruns.Porifera, function(x) x$estimate))
lcl.Porifera <- do.call(cbind, lapply(reruns.Porifera, function(x) x$lcl))
ucl.Porifera <- do.call(cbind, lapply(reruns.Porifera, function(x) x$ucl))


estimates.Porifera[estimates.Porifera<0.00001|estimates.Porifera==1|estimates.Porifera>=1] <- NA
lcl.Porifera[is.na(estimates.Porifera)] <- NA
ucl.Porifera[is.na(estimates.Porifera)] <- NA

estimates.Porifera[ucl.Porifera-lcl.Porifera==0|ucl.Porifera-lcl.Porifera==1|ucl.Porifera>1]<-NA
lcl.Porifera[is.na(estimates.Porifera)] <- NA
ucl.Porifera[is.na(estimates.Porifera)] <- NA

##
estimates.mean.Porifera <- rowMeans(estimates.Porifera, na.rm = T)
lcl.mean.Porifera <- rowMeans(lcl.Porifera, na.rm = T)
ucl.mean.Porifera <- rowMeans(ucl.Porifera, na.rm = T)

estimates.median.Porifera <- apply(estimates.Porifera, 1, median, na.rm=TRUE)
lcl.median.Porifera <- apply(lcl.Porifera,1,median, na.rm=TRUE)
ucl.median.Porifera <- apply(ucl.Porifera,1,median, na.rm=TRUE)

##
##
estimates.lambda.Porifera <- do.call(cbind, lapply(reruns.lambda.Porifera, function(x) x$estimate))
lcl.lambda.Porifera <- do.call(cbind, lapply(reruns.lambda.Porifera, function(x) x$lcl))
ucl.lambda.Porifera <- do.call(cbind, lapply(reruns.lambda.Porifera, function(x) x$ucl))

# estimates.lambda[estimates.lambda<0.00001|estimates.lambda==1|estimates.lambda>=1] <- NA
# lcl.lambda[is.na(estimates.lambda)] <- NA
# ucl.lambda[is.na(estimates.lambda)] <- NA
# 
estimates.lambda.Porifera[ucl.lambda.Porifera-lcl.lambda.Porifera==0|ucl.lambda.Porifera-lcl.lambda.Porifera>=15]<-NA
lcl.lambda.Porifera[is.na(estimates.lambda.Porifera)] <- NA
ucl.lambda.Porifera[is.na(estimates.lambda.Porifera)] <- NA

estimates.lambda.mean.Porifera <- rowMeans(estimates.lambda.Porifera, na.rm = T)
lcl.lambda.mean.Porifera <- rowMeans(lcl.lambda.Porifera, na.rm = T)
ucl.lambda.mean.Porifera <- rowMeans(ucl.lambda.Porifera, na.rm = T)

estimates.lambda.median.Porifera <- apply(estimates.lambda.Porifera, 1, median, na.rm=TRUE)
lcl.lambda.median.Porifera <- apply(lcl.lambda.Porifera, 1, median, na.rm=TRUE)
ucl.lambda.median.Porifera <- apply(ucl.lambda.Porifera, 1, median, na.rm=TRUE)

###########################################################################################
## extracting parameter estimates from the results above
phi.Porifera <- estimates.Porifera[2:8,]
phiu.Porifera <- ucl.Porifera[2:8,]
phil.Porifera <- lcl.Porifera[2:8,]

p.Porifera <- estimates.Porifera[13:19,]
pu.Porifera <- ucl.Porifera[13:19,]
pl.Porifera <- lcl.Porifera[13:19,]

gam.Porifera <- estimates.Porifera[23:29,]
gamu.Porifera <- ucl.Porifera[23:29,]
gaml.Porifera <- lcl.Porifera[23:29,]


lambda.Porifera <- estimates.lambda.Porifera[23:29,]
lambdau.Porifera <- ucl.lambda.Porifera[23:29,]
lambdal.Porifera <- lcl.lambda.Porifera[23:29,]

## extracting parameter estimates medians from the results above
phi.median.Porifera <- estimates.median.Porifera[2:8]
phiu.median.Porifera <- ucl.median.Porifera[2:8]
phil.median.Porifera <- lcl.median.Porifera[2:8]

p.median.Porifera <- estimates.median.Porifera[13:19]
pu.median.Porifera <- ucl.median.Porifera[13:19]
pl.median.Porifera <- lcl.median.Porifera[13:19]

gam.median.Porifera <- estimates.median.Porifera[23:29]
gamu.median.Porifera <- ucl.median.Porifera[23:29]
gaml.median.Porifera <- lcl.median.Porifera[23:29]


lambda.median.Porifera <- estimates.lambda.median.Porifera[23:29]
lambdau.median.Porifera <- ucl.lambda.median.Porifera[23:29]
lambdal.median.Porifera <- lcl.lambda.median.Porifera[23:29]

t <- c(9.65,7.7,5.2,5.8,7.15,6.6,4.6)
# following vector is time per interval/stageslice
tp <- c(7.7,7.7,2.7,8.9,5.4,7.8,1.4)

origprob.Porifera <- 1-gam.Porifera
origCIl.Porifera <- 1-gaml.Porifera
origCIu.Porifera <- 1-gamu.Porifera
Orig_rate.Porifera <- -log(1-origprob.Porifera)/t
Orig_rate_CIl.Porifera <- -log(1-origCIl.Porifera)/t
Orig_rate_CIu.Porifera <- -log(1-origCIu.Porifera)/t

extinctprob.Porifera <- 1-phi.Porifera
extinctCIl.Porifera <- 1-phil.Porifera
extinctCIu.Porifera <- 1-phiu.Porifera
Ext_rate.Porifera <- -log(1-extinctprob.Porifera)/t
Ext_rate_CIl.Porifera <- -log(1-extinctCIl.Porifera)/t
Ext_rate_CIu.Porifera <- -log(1-extinctCIu.Porifera)/t

rate_p.Porifera <- -log(1-p.Porifera)/tp
ratel_p.Porifera <- -log(1-pl.Porifera)/tp
rateu_p.Porifera <- -log(1-pu.Porifera)/tp

### estimating median rates
origprob.median.Porifera <- 1-gam.median.Porifera
origCIl.median.Porifera <- 1-gaml.median.Porifera
origCIu.median.Porifera <- 1-gamu.median.Porifera
Orig_rate.median.Porifera <- -log(1-origprob.median.Porifera)/t
Orig_rate_CIl.median.Porifera <- -log(1-origCIl.median.Porifera)/t
Orig_rate_CIu.median.Porifera <- -log(1-origCIu.median.Porifera)/t

extinctprob.median.Porifera <- 1-phi.median.Porifera
extinctCIl.median.Porifera <- 1-phil.median.Porifera
extinctCIu.median.Porifera <- 1-phiu.median.Porifera
Ext_rate.median.Porifera <- -log(1-extinctprob.median.Porifera)/t
Ext_rate_CIl.median.Porifera <- -log(1-extinctCIl.median.Porifera)/t
Ext_rate_CIu.median.Porifera <- -log(1-extinctCIu.median.Porifera)/t

rate_p.median.Porifera <- -log(1-p.median.Porifera)/tp
ratel_p.median.Porifera <- -log(1-pl.median.Porifera)/tp
rateu_p.median.Porifera <- -log(1-pu.median.Porifera)/tp
###
###
###
## collecting 100 replicate parameter estimates
estimates.Corals <- do.call(cbind, lapply(reruns.Corals, function(x) x$estimate))
lcl.Corals <- do.call(cbind, lapply(reruns.Corals, function(x) x$lcl))
ucl.Corals <- do.call(cbind, lapply(reruns.Corals, function(x) x$ucl))


estimates.Corals[estimates.Corals<0.00001|estimates.Corals==1|estimates.Corals>=1] <- NA
lcl.Corals[is.na(estimates.Corals)] <- NA
ucl.Corals[is.na(estimates.Corals)] <- NA

estimates.Corals[ucl.Corals-lcl.Corals==0|ucl.Corals-lcl.Corals==1|ucl.Corals>1]<-NA
lcl.Corals[is.na(estimates.Corals)] <- NA
ucl.Corals[is.na(estimates.Corals)] <- NA

##
estimates.mean.Corals <- rowMeans(estimates.Corals, na.rm = T)
lcl.mean.Corals <- rowMeans(lcl.Corals, na.rm = T)
ucl.mean.Corals <- rowMeans(ucl.Corals, na.rm = T)

estimates.median.Corals <- apply(estimates.Corals, 1, median, na.rm=TRUE)
lcl.median.Corals <- apply(lcl.Corals,1,median, na.rm=TRUE)
ucl.median.Corals <- apply(ucl.Corals,1,median, na.rm=TRUE)

##
##
estimates.lambda.Corals <- do.call(cbind, lapply(reruns.lambda.Corals, function(x) x$estimate))
lcl.lambda.Corals <- do.call(cbind, lapply(reruns.lambda.Corals, function(x) x$lcl))
ucl.lambda.Corals <- do.call(cbind, lapply(reruns.lambda.Corals, function(x) x$ucl))

# estimates.lambda[estimates.lambda<0.00001|estimates.lambda==1|estimates.lambda>=1] <- NA
# lcl.lambda[is.na(estimates.lambda)] <- NA
# ucl.lambda[is.na(estimates.lambda)] <- NA
# 
estimates.lambda.Corals[ucl.lambda.Corals-lcl.lambda.Corals==0|ucl.lambda.Corals-lcl.lambda.Corals>=15]<-NA
lcl.lambda.Corals[is.na(estimates.lambda.Corals)] <- NA
ucl.lambda.Corals[is.na(estimates.lambda.Corals)] <- NA

estimates.lambda.mean.Corals <- rowMeans(estimates.lambda.Corals, na.rm = T)
lcl.lambda.mean.Corals <- rowMeans(lcl.lambda.Corals, na.rm = T)
ucl.lambda.mean.Corals <- rowMeans(ucl.lambda.Corals, na.rm = T)

estimates.lambda.median.Corals <- apply(estimates.lambda.Corals, 1, median, na.rm=TRUE)
lcl.lambda.median.Corals <- apply(lcl.lambda.Corals, 1, median, na.rm=TRUE)
ucl.lambda.median.Corals <- apply(ucl.lambda.Corals, 1, median, na.rm=TRUE)

###########################################################################################
## extracting parameter estimates from the results above
phi.Corals <- estimates.Corals[2:8,]
phiu.Corals <- ucl.Corals[2:8,]
phil.Corals <- lcl.Corals[2:8,]

p.Corals <- estimates.Corals[13:19,]
pu.Corals <- ucl.Corals[13:19,]
pl.Corals <- lcl.Corals[13:19,]

gam.Corals <- estimates.Corals[23:29,]
gamu.Corals <- ucl.Corals[23:29,]
gaml.Corals <- lcl.Corals[23:29,]


lambda.Corals <- estimates.lambda.Corals[23:29,]
lambdau.Corals <- ucl.lambda.Corals[23:29,]
lambdal.Corals <- lcl.lambda.Corals[23:29,]

## extracting parameter estimates medians from the results above
phi.median.Corals <- estimates.median.Corals[2:8]
phiu.median.Corals <- ucl.median.Corals[2:8]
phil.median.Corals <- lcl.median.Corals[2:8]

p.median.Corals <- estimates.median.Corals[13:19]
pu.median.Corals <- ucl.median.Corals[13:19]
pl.median.Corals <- lcl.median.Corals[13:19]

gam.median.Corals <- estimates.median.Corals[23:29]
gamu.median.Corals <- ucl.median.Corals[23:29]
gaml.median.Corals <- lcl.median.Corals[23:29]


lambda.median.Corals <- estimates.lambda.median.Corals[23:29]
lambdau.median.Corals <- ucl.lambda.median.Corals[23:29]
lambdal.median.Corals <- lcl.lambda.median.Corals[23:29]

t <- c(9.65,7.7,5.2,5.8,7.15,6.6,4.6)
# following vector is time per interval/stageslice
tp <- c(7.7,7.7,2.7,8.9,5.4,7.8,1.4)

origprob.Corals <- 1-gam.Corals
origCIl.Corals <- 1-gaml.Corals
origCIu.Corals <- 1-gamu.Corals
Orig_rate.Corals <- -log(1-origprob.Corals)/t
Orig_rate_CIl.Corals <- -log(1-origCIl.Corals)/t
Orig_rate_CIu.Corals <- -log(1-origCIu.Corals)/t

extinctprob.Corals <- 1-phi.Corals
extinctCIl.Corals <- 1-phil.Corals
extinctCIu.Corals <- 1-phiu.Corals
Ext_rate.Corals <- -log(1-extinctprob.Corals)/t
Ext_rate_CIl.Corals <- -log(1-extinctCIl.Corals)/t
Ext_rate_CIu.Corals <- -log(1-extinctCIu.Corals)/t

rate_p.Corals <- -log(1-p.Corals)/tp
ratel_p.Corals <- -log(1-pl.Corals)/tp
rateu_p.Corals <- -log(1-pu.Corals)/tp

### estimating median rates
origprob.median.Corals <- 1-gam.median.Corals
origCIl.median.Corals <- 1-gaml.median.Corals
origCIu.median.Corals <- 1-gamu.median.Corals
Orig_rate.median.Corals <- -log(1-origprob.median.Corals)/t
Orig_rate_CIl.median.Corals <- -log(1-origCIl.median.Corals)/t
Orig_rate_CIu.median.Corals <- -log(1-origCIu.median.Corals)/t

extinctprob.median.Corals <- 1-phi.median.Corals
extinctCIl.median.Corals <- 1-phil.median.Corals
extinctCIu.median.Corals <- 1-phiu.median.Corals
Ext_rate.median.Corals <- -log(1-extinctprob.median.Corals)/t
Ext_rate_CIl.median.Corals <- -log(1-extinctCIl.median.Corals)/t
Ext_rate_CIu.median.Corals <- -log(1-extinctCIu.median.Corals)/t

rate_p.median.Corals <- -log(1-p.median.Corals)/tp
ratel_p.median.Corals <- -log(1-pl.median.Corals)/tp
rateu_p.median.Corals <- -log(1-pu.median.Corals)/tp


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
###
###
save.image("//kant/nhm-sfs-u2/franzif/paper2/100.repruns.median.ind.plusParacrinoids.RData")
load("//kant/nhm-sfs-u2/franzif/paper2/100.repruns.median.ind.plusParacrinoids.RData")

###
###
###
tiff("ind.groups.3col.100rep.median.tiff",
     res = 600,
     units = "mm",
     width = 166,
     height = 170,
     pointsize = 9)
par(mfrow=c(4,3), mar = c(0.5,2,0.1,0), oma = c(3,2,1,0))

plot(Stagebase, Orig_rate.median.Echinos[1:7],
     type = "b", 
     ylim = c(-0.12,0.8),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.8, -0.025, -0.12)



lines(Stagebase, Orig_rate.median.Echinos[1:7], col = "black", type = "b")
lines(Stagebase, Orig_rate_CIl.median.Echinos[1:7], col = "grey65")
lines(Stagebase, Orig_rate_CIu.median.Echinos[1:7], col = "grey65")

legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "Echinodermata")

# axis(1, col = 'grey75', line = 0.5, at = seq(445,485,10) )
axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.8, 0.1))

# mtext("Age (Ma)", side = 1, line = 2.5)
mtext("Origination events per myrs", side = 2, line = 2)

#######################################################
plot(Stagebase, Orig_rate.median.Bryozoa[1:7],
     type = "b", 
     ylim = c(-0.12,0.8),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.8, -0.025, -0.12)



lines(Stagebase, Orig_rate.median.Bryozoa[1:7], col = "black", type = "b")
lines(Stagebase, Orig_rate_CIl.median.Bryozoa[1:7], col = "grey65")
lines(Stagebase, Orig_rate_CIu.median.Bryozoa[1:7], col = "grey65")


legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "Bryozoa")
#######################################################
plot(Stagebase, Orig_rate.median.Porifera[1:7],
     type = "b", 
     ylim = c(-0.12,0.8),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.8, -0.025, -0.12)



lines(Stagebase, Orig_rate.median.Porifera[1:7], col = "black", type = "b")
lines(Stagebase, Orig_rate_CIl.median.Porifera[1:7], col = "grey65")
lines(Stagebase, Orig_rate_CIu.median.Porifera[1:7], col = "grey65")


legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "Porifera")

#######################################################
#######################################################
plot(Stagebase, Ext_rate.median.Echinos[1:7],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)

lines(Stagebase, Ext_rate.median.Echinos[1:7], col = "black", type = "b")
lines(Stagebase, Ext_rate_CIl.median.Echinos[1:7], col = "grey65")
lines(Stagebase, Ext_rate_CIu.median.Echinos[1:7], col = "grey65")

axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))

mtext("Extinction events per myrs", side = 2, line = 2)

#######################################################
plot(Stagebase, Ext_rate.median.Bryozoa[1:7],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)


lines(Stagebase, Ext_rate.median.Bryozoa[1:7], col = "black", type = "b")
lines(Stagebase, Ext_rate_CIl.median.Bryozoa[1:7], col = "grey65")
lines(Stagebase, Ext_rate_CIu.median.Bryozoa[1:7], col = "grey65")
#######################################################
plot(Stagebase, Ext_rate.median.Porifera[1:7],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)


lines(Stagebase, Ext_rate.median.Porifera[1:7], col = "black", type = "b")
lines(Stagebase, Ext_rate_CIl.median.Porifera[1:7], col = "grey65")
lines(Stagebase, Ext_rate_CIu.median.Porifera[1:7], col = "grey65")


#######################################################
#######################################################
plot(Stagebase, lambda.median.Echinos[1:7]-1, type = "b", 
     # main = "Net Diversification", 
     ylim = c(-2.64,15),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(15, -1.2, -2.64)
abline(h = 0, col="darkgrey")

lines(Stagebase, lambda.median.Echinos[1:7]-1, col = "black", type = "b")
lines(Stagebase, lambdal.median.Echinos[1:7]-1, col = "grey65")
lines(Stagebase, lambdau.median.Echinos[1:7]-1, col = "grey65")


# legend("topleft", legend="C", bty="n", cex = 1.25)

axis(2, col = 'grey75', line = -0.2, at = c(-1, seq(0, 15, 3)))

mtext("Net diversification rate", side = 2, line = 2)

#######################################################
plot(Stagebase, lambda.median.Bryozoa[1:7]-1, type = "b", 
     # main = "Net Diversification", 
     ylim = c(-1.75,5),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(5, -1.2, -1.75)
abline(h = 0, col="darkgrey")

lines(Stagebase, lambda.median.Bryozoa[1:7]-1, col = "black", type = "b")
lines(Stagebase, lambdal.median.Bryozoa[1:7]-1, col = "grey65")
lines(Stagebase, lambdau.median.Bryozoa[1:7]-1, col = "grey65")

axis(2, col = 'grey75', line = -0.2, at = c(-1, seq(0, 5, 1)))

#######################################################
plot(Stagebase, lambda.median.Porifera[1:7]-1, type = "b", 
     # main = "Net Diversification", 
     ylim = c(-1.75,5),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(5, -1.2, -1.75)
abline(h = 0, col="darkgrey")

lines(Stagebase, lambda.median.Porifera[1:7]-1, col = "black", type = "b")
lines(Stagebase, lambdal.median.Porifera[1:7]-1, col = "grey65")
lines(Stagebase, lambdau.median.Porifera[1:7]-1, col = "grey65")

#######################################################
#######################################################
plot(Stagemidpoints, rate_p.median.Echinos[1:7],
     type = "b", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)


lines(Stagemidpoints, rate_p.median.Echinos[1:7], col = "black", type = "b")
lines(Stagemidpoints, ratel_p.median.Echinos[1:7], col = "grey65")
lines(Stagemidpoints, rateu_p.median.Echinos[1:7], col = "grey65")


axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.1)
mtext("Sampling events per myrs", side = 2, line = 2)

#######################################################
plot(Stagemidpoints, rate_p.median.Bryozoa[1:7],
     type = "b", 
     ylim = c(-0.3,2),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(2, -0.1, -0.3)

lines(Stagemidpoints, rate_p.median.Bryozoa[1:7], col = "black", type = "b")
lines(Stagemidpoints, ratel_p.median.Bryozoa[1:7], col = "grey65")
lines(Stagemidpoints, rateu_p.median.Bryozoa[1:7], col = "grey65")


axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
axis(2, col = 'grey75', line = -0.2, at = seq(0, 2, 0.4))

# axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.1)
#######################################################
plot(Stagemidpoints, rate_p.median.Porifera[1:7],
     type = "b", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)

lines(Stagemidpoints, rate_p.median.Porifera[1:7], col = "black", type = "b")
lines(Stagemidpoints, ratel_p.median.Porifera[1:7], col = "grey65")
lines(Stagemidpoints, rateu_p.median.Porifera[1:7], col = "grey65")


axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.1)

dev.off()

###
###
###
## SI figure - 100 replicate runs -
###
###
###

tiff("ind.groups.3col.100rep.tiff",
     res = 600,
     units = "mm",
     width = 166,
     height = 170,
     pointsize = 9)
par(mfrow=c(4,3), mar = c(0.5,2,0.1,0), oma = c(3,2,1,0))

plot(Stagebase, Orig_rate.median.Echinos[1:7],
     type = "b", 
     ylim = c(-0.12,0.8),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.8, -0.025, -0.12)



for (i in 1:100){
  lines(Stagebase, Orig_rate_CIl.Echinos[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate_CIu.Echinos[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate.Echinos[1:7,i], col = "black")}


legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "Echinodermata")

# axis(1, col = 'grey75', line = 0.5, at = seq(445,485,10) )
axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.8, 0.2))

# mtext("Age (Ma)", side = 1, line = 2.5)
mtext("Origination events per myrs", side = 2, line = 2)

#######################################################
plot(Stagebase, Orig_rate.median.Bryozoa[1:7],
     type = "l", 
     ylim = c(-0.12,0.8),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.8, -0.025, -0.12)



for (i in 1:100){
  lines(Stagebase, Orig_rate_CIl.Bryozoa[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate_CIu.Bryozoa[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate.Bryozoa[1:7,i], col = "black")}


legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "Bryozoa")
#######################################################
plot(Stagebase, Orig_rate.median.Porifera[1:7],
     type = "l", 
     ylim = c(-0.12,0.8),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.8, -0.025, -0.12)


for (i in 1:100){
  lines(Stagebase, Orig_rate_CIl.Porifera[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate_CIu.Porifera[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate.Porifera[1:7,i], col = "black")}


legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "Porifera")

#######################################################
#######################################################
plot(Stagebase, Ext_rate.median.Echinos[1:7],
     type = "l", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)

for (i in 1:100){
  lines(Stagebase, Ext_rate_CIl.Echinos[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate_CIu.Echinos[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate.Echinos[1:7,i], col = "black")}

axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))

mtext("Extinction events per myrs", side = 2, line = 2)

#######################################################
plot(Stagebase, Ext_rate.median.Bryozoa[1:7],
     type = "l", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)


for (i in 1:100){
  lines(Stagebase, Ext_rate_CIl.Bryozoa[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate_CIu.Bryozoa[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate.Bryozoa[1:7,i], col = "black")}
#######################################################
plot(Stagebase, Ext_rate.median.Porifera[1:7],
     type = "l", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)


for (i in 1:100){
  lines(Stagebase, Ext_rate_CIl.Porifera[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate_CIu.Porifera[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate.Porifera[1:7,i], col = "black")}


#######################################################
#######################################################
plot(Stagebase, lambda.median.Echinos[1:7]-1, type = "l", 
     # main = "Net Diversification", 
     ylim = c(-1.75,5),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(5, -1.2, -1.75)
abline(h = 0, col="darkgrey")

for (i in 1:100){
  lines(Stagebase, lambdal.Echinos[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambdau.Echinos[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambda.Echinos[1:7,i]-1, col = "black")}


# legend("topleft", legend="C", bty="n", cex = 1.25)

axis(2, col = 'grey75', line = -0.2, at = seq(-1, 5, 1))

mtext("Net diversification rate", side = 2, line = 2)

#######################################################
plot(Stagebase, lambda.median.Bryozoa[1:7]-1, type = "l", 
     # main = "Net Diversification", 
     ylim = c(-1.75,5),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(5, -1.2, -1.75)
abline(h = 0, col="darkgrey")

for (i in 1:100){
  lines(Stagebase, lambdal.Bryozoa[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambdau.Bryozoa[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambda.Bryozoa[1:7,i]-1, col = "black")}

#######################################################
plot(Stagebase, lambda.median.Porifera[1:7]-1, type = "l", 
     # main = "Net Diversification", 
     ylim = c(-1.75,5),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(5, -1.2, -1.75)
abline(h = 0, col="darkgrey")

for (i in 1:100){
  lines(Stagebase, lambdal.Porifera[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambdau.Porifera[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambda.Porifera[1:7,i]-1, col = "black")}

#######################################################
#######################################################
plot(Stagemidpoints, rate_p.median.Echinos[1:7],
     type = "l", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)


for (i in 1:100){
  lines(Stagemidpoints, ratel_p.Echinos[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rateu_p.Echinos[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rate_p.Echinos[1:7,i], col = "black")}


axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.1)
mtext("Sampling events per myrs", side = 2, line = 2)

#######################################################
plot(Stagemidpoints, rate_p.median.Bryozoa[1:7],
     type = "l", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)
for (i in 1:100){
  lines(Stagemidpoints, ratel_p.Bryozoa[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rateu_p.Bryozoa[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rate_p.Bryozoa[1:7,i], col = "black")}


axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
# axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))

mtext("Age (Ma)", side = 1, line = 2.1)
#######################################################
plot(Stagemidpoints, rate_p.Porifera[1:7,1],
     type = "l", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)


for (i in 1:100){
  lines(Stagemidpoints, ratel_p.Porifera[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rateu_p.Porifera[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rate_p.Porifera[1:7,i], col = "black")}

##

axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))

mtext("Age (Ma)", side = 1, line = 2.1)

dev.off()

###
###
###
## Corals median
###
###
###
tiff("ind.groups.1col.100rep.median.tiff",
     res = 600,
     units = "mm",
     width = 80,
     height = 170,
     pointsize = 9)
par(mfrow=c(4,1), mar = c(0.5,2,0.1,0), oma = c(3,2,1,0))
#######################################################
plot(Stagebase, Orig_rate.median.Corals[1:7],
     type = "b", 
     ylim = c(-0.12,0.8),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.8, -0.025, -0.12)



lines(Stagebase, Orig_rate.median.Corals[1:7], col = "black", type = "b")
lines(Stagebase, Orig_rate_CIl.median.Corals[1:7], col = "grey65")
lines(Stagebase, Orig_rate_CIu.median.Corals[1:7], col = "grey65")


legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "Corals")

axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.8, 0.1))
mtext("Origination events per myrs", side = 2, line = 2)

#######################################################
plot(Stagebase, Ext_rate.median.Corals[1:7],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)


lines(Stagebase, Ext_rate.median.Corals[1:7], col = "black", type = "b")
lines(Stagebase, Ext_rate_CIl.median.Corals[1:7], col = "grey65")
lines(Stagebase, Ext_rate_CIu.median.Corals[1:7], col = "grey65")

axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))
mtext("Extinction events per myrs", side = 2, line = 2)

#######################################################
plot(Stagebase, lambda.median.Corals[1:7]-1, type = "b", 
     # main = "Net Diversification", 
     ylim = c(-2.64,15),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(15, -1.2, -2.64)
abline(h = 0, col="darkgrey")

lines(Stagebase, lambda.median.Corals[1:7]-1, col = "black", type = "b")
lines(Stagebase, lambdal.median.Corals[1:7]-1, col = "grey65")
lines(Stagebase, lambdau.median.Corals[1:7]-1, col = "grey65")

axis(2, col = 'grey75', line = -0.2, at = c(-1, seq(0, 15, 3)))
mtext("Net diversification rate", side = 2, line = 2)

#######################################################
plot(Stagemidpoints, rate_p.median.Corals[1:7],
     type = "b", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)

lines(Stagemidpoints, rate_p.median.Corals[1:7], col = "black", type = "b")
lines(Stagemidpoints, ratel_p.median.Corals[1:7], col = "grey65")
lines(Stagemidpoints, rateu_p.median.Corals[1:7], col = "grey65")


axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
mtext("Age (Ma)", side = 1, line = 2.1)

axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))
mtext("Sampling events per myrs", side = 2, line = 2)


dev.off()

###
###
###
## Corals - replicate runs
###
###
###
tiff("ind.groups.1col.100rep.tiff",
     res = 600,
     units = "mm",
     width = 80,
     height = 170,
     pointsize = 9)
par(mfrow=c(4,1), mar = c(0.5,2,0.1,0), oma = c(3,2,1,0))
#######################################################
plot(Stagebase, Orig_rate.median.Corals[1:7],
     type = "b", 
     ylim = c(-0.12,0.8),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.8, -0.025, -0.12)

for (i in 1:100){
  lines(Stagebase, Orig_rate_CIl.Corals[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate_CIu.Corals[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Orig_rate.Corals[1:7,i], col = "black")}



legend("topright",
       inset=0.05,
       box.lty=0,
       text.font=2,
       legend = "Corals")

axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.8, 0.1))
mtext("Origination events per myrs", side = 2, line = 2)

#######################################################
plot(Stagebase, Ext_rate.median.Corals[1:7],
     type = "b", 
     ylim = c(-0.075,0.5),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(0.5, -0.025, -0.075)


for (i in 1:100){
  lines(Stagebase, Ext_rate_CIl.Corals[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate_CIu.Corals[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagebase, Ext_rate.Corals[1:7,i], col = "black")}


axis(2, col = 'grey75', line = -0.2, at = seq(0, 0.5, 0.1))
mtext("Extinction events per myrs", side = 2, line = 2)

#######################################################
plot(Stagebase, lambda.median.Corals[1:7]-1, type = "b", 
     # main = "Net Diversification", 
     ylim = c(-2.64,15),
     xlim = rev(c(444.18,485.4)),
     axes = F,
     xlab = "",
     ylab = "")

tscales.Ord(15, -1.2, -2.64)
abline(h = 0, col="darkgrey")

for (i in 1:100){
  lines(Stagebase, lambdal.Corals[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambdau.Corals[1:7,i]-1, col = "grey65")}

for (i in 1:100){
  lines(Stagebase, lambda.Corals[1:7,i]-1, col = "black")}

axis(2, col = 'grey75', line = -0.2, at = c(-1, seq(0, 15, 3)))
mtext("Net diversification rate", side = 2, line = 2)

#######################################################
plot(Stagemidpoints, rate_p.median.Corals[1:7],
     type = "b", 
     ylim = c(-0.15,1),
     xlim = rev(c(443.8,485)),
     axes = F,
     xlab = "",
     ylab = "")
tscales.Ord(1, -0.05, -0.15)

for (i in 1:100){
  lines(Stagemidpoints, ratel_p.Corals[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rateu_p.Corals[1:7,i], col = "grey65")}

for (i in 1:100){
  lines(Stagemidpoints, rate_p.Corals[1:7,i], col = "black")}


axis(1, col = 'grey75', line = 0.1, at = seq(445,485,10))
mtext("Age (Ma)", side = 1, line = 2.1)

axis(2, col = 'grey75', line = -0.2, at = seq(0, 1, 0.2))
mtext("Sampling events per myrs", side = 2, line = 2)


dev.off()