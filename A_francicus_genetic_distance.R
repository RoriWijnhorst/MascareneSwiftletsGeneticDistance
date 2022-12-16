install.packages("dplyr")
install.packages("tidyverse")
library(tidyverse)
library(dplyr)

setwd("Your_Directory")
Distance <- read.csv("Large_matrix_HKY_NJ4.csv")
Distance[Distance == 0] <- NA

#Between Mauritius and Reunion A. francicus
Between <- Distance[grep("Mauritius", Distance[,1]), ]
Between <- Between %>% select(contains("Reunion")) 
Between_mean <- mean(as.matrix(Between), na.rm = TRUE)
print(Between_mean)
Between_min <- min(as.matrix(Between), na.rm = TRUE)
print(Between_min)
Between_max <- max(as.matrix(Between), na.rm = TRUE)
print(Between_max)
print(ncol(Between)*nrow(Between))

#Within Reunion A. francicus
Reunion <- Distance[grep("Reunion", Distance[,1]), ]
Reunion <- Reunion %>% select(contains("Reunion")) 
Reunion_mean <- mean(as.matrix(Reunion), na.rm = TRUE)
print(Reunion_mean)
Reunion_min <- min(as.matrix(Reunion), na.rm = TRUE)
print(Reunion_min)
Reunion_max <- max(as.matrix(Reunion), na.rm = TRUE)
print(Reunion_max)
print(((ncol(Reunion)*nrow(Reunion))-nrow(Reunion))/2)

#Within Mauritius A. francicus
Mauritius <- Distance[grep("Mauritius", Distance[,1]), ]
Mauritius <- Mauritius %>% select(contains("Mauritius")) 
Mauritius_mean <- mean(as.matrix(Mauritius), na.rm = TRUE)
print(Mauritius_mean)
Mauritius_min <- min(as.matrix(Mauritius), na.rm = TRUE)
print(Mauritius_min)
Mauritius_max <- max(as.matrix(Mauritius), na.rm = TRUE)
print(Mauritius_max)
print(((ncol(Mauritius)*nrow(Mauritius))-nrow(Mauritius))/2)

#Within A. francicus
Francicus <- Distance[grep("francicus", Distance[,1]), ]
Francicus <- Francicus %>% select(contains("francicus")) 
Francicus_mean <- mean(as.matrix(Francicus), na.rm = TRUE)
print(Francicus_mean)
Francicus_min <- min(as.matrix(Francicus), na.rm = TRUE)
print(Francicus_min)
Francicus_max <- max(as.matrix(Francicus), na.rm = TRUE)
print(Francicus_max)
print(((ncol(Francicus)*nrow(Francicus))-nrow(Francicus))/2)

#Within A. elaphrus
elaphrus <- Distance[grep("elaphrus", Distance[,1]), ]
elaphrus <- elaphrus %>% select(ends_with("elaphrus")) 
elaphrus_mean <- mean(as.matrix(elaphrus), na.rm = TRUE)
print(elaphrus_mean)
elaphrus_min <- min(as.matrix(elaphrus), na.rm = TRUE)
print(elaphrus_min)
elaphrus_max <- max(as.matrix(elaphrus), na.rm = TRUE)
print(elaphrus_max)
print(((ncol(elaphrus)*nrow(elaphrus))-nrow(elaphrus))/2)

#Within A. maximus
maximus <- Distance[grep("maximus", Distance[,1]), ]
maximus <- maximus %>% select(ends_with("maximus")) 
maximus_mean <- mean(as.matrix(maximus), na.rm = TRUE)
print(maximus_mean)
maximus_min <- min(as.matrix(maximus), na.rm = TRUE)
print(maximus_min)
maximus_max <- max(as.matrix(maximus), na.rm = TRUE)
print(maximus_max)
print(((ncol(maximus)*nrow(maximus))-nrow(maximus))/2)

#Within A. terraereginae
terraereginae <- Distance[grep("terraereginae", Distance[,1]), ]
terraereginae <- terraereginae %>% select(ends_with("terraereginae")) 
terraereginae_mean <- mean(as.matrix(terraereginae), na.rm = TRUE)
print(terraereginae_mean)
terraereginae_min <- min(as.matrix(terraereginae), na.rm = TRUE)
print(terraereginae_min)
terraereginae_max <- max(as.matrix(terraereginae), na.rm = TRUE)
print(terraereginae_max)
print(((ncol(terraereginae)*nrow(terraereginae))-nrow(terraereginae))/2)

#Within A. ocistus
ocistus <- Distance[grep("ocistus", Distance[,1]), ]
ocistus <- ocistus %>% select(ends_with("ocistus")) 
ocistus_mean <- mean(as.matrix(ocistus), na.rm = TRUE)
print(ocistus_mean)
ocistus_min <- min(as.matrix(ocistus), na.rm = TRUE)
print(ocistus_min)
ocistus_max <- max(as.matrix(ocistus), na.rm = TRUE)
print(ocistus_max)
print(((ncol(ocistus)*nrow(ocistus))-nrow(ocistus))/2)

#Between sawtelli and bartschi
sawtelli_bartschi <- Distance[grep("sawtelli", Distance[,1]), ]
sawtelli_bartschi <- sawtelli_bartschi %>% select(contains("bartschi")) 
sawtelli_bartschi_mean <- mean(as.matrix(sawtelli_bartschi), na.rm = TRUE)
print(sawtelli_bartschi_mean)
sawtelli_bartschi_min <- min(as.matrix(sawtelli_bartschi), na.rm = TRUE)
print(sawtelli_bartschi_min)
sawtelli_bartschi_max <- max(as.matrix(sawtelli_bartschi), na.rm = TRUE)
print(sawtelli_bartschi_max)
print(ncol(sawtelli_bartschi)*nrow(sawtelli_bartschi))

#Between A. francicus and elaphrus
francicus_elaphrus <- Distance[grep("francicus", Distance[,1]), ]
francicus_elaphrus <- francicus_elaphrus %>% select(contains("elaphrus")) 
francicus_elaphrus_mean <- mean(as.matrix(francicus_elaphrus), na.rm = TRUE)
print(francicus_elaphrus_mean)
francicus_elaphrus_min <- min(as.matrix(francicus_elaphrus), na.rm = TRUE)
print(francicus_elaphrus_min)
francicus_elaphrus_max <- max(as.matrix(francicus_elaphrus), na.rm = TRUE)
print(francicus_elaphrus_max)
print(ncol(francicus_elaphrus)*nrow(francicus_elaphrus))

#Between ocistus and leucophaeus
ocistus_leucophaeus <- Distance[grep("leucophaeus", Distance[,1]), ]
ocistus_leucophaeus <- ocistus_leucophaeus %>% select(contains("ocistus")) 
ocistus_leucophaeus_mean <- mean(as.matrix(ocistus_leucophaeus), na.rm = TRUE)
print(ocistus_leucophaeus_mean)
ocistus_leucophaeus_min <- min(as.matrix(ocistus_leucophaeus), na.rm = TRUE)
print(ocistus_leucophaeus_min)
ocistus_leucophaeus_max <- max(as.matrix(ocistus_leucophaeus), na.rm = TRUE)
print(ocistus_leucophaeus_max)
print(ncol(ocistus_leucophaeus)*nrow(ocistus_leucophaeus))


#Between fuciphagus and sororum
fuciphagus_sororum <- Distance[grep("sororum", Distance[,1]), ]
fuciphagus_sororum <- fuciphagus_sororum %>% select(contains("fuciphagus")) 
fuciphagus_sororum_mean <- mean(as.matrix(fuciphagus_sororum), na.rm = TRUE)
print(fuciphagus_sororum_mean)
fuciphagus_sororum_min <- min(as.matrix(fuciphagus_sororum), na.rm = TRUE)
print(fuciphagus_sororum_min)
fuciphagus_sororum_max <- max(as.matrix(fuciphagus_sororum), na.rm = TRUE)
print(fuciphagus_sororum_max)
print(ncol(fuciphagus_sororum)*nrow(fuciphagus_sororum))

view(fuciphagus_sororum)
