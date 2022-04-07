####R script exploration####

#import the data
Groundfish <- read.csv("ai2014_2018.csv")
names(Groundfish)

#load some packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(dplyr)

#filter some data
unique(Groundfish$COMMON)
Sablefish <- Groundfish %>% filter(COMMON == "sablefish")
names(Sablefish)
#I'm interested in NUMCPUE as my covariate

#let's load that geostatistical package we used for scallops
##and use that to make a geodataframe
##and plot just like we did for scallops, with the covariate as sablefish number
??as.geodata
library(geoR)
##seems to have loaded, got some weird warnings tho
data("Ksat") #just looking at this data structure


#the challenge of creating a geodata object so I can plot it
length(Sablefish$LATITUDE)
names(Sablefish)
Sablefish_relevant <- Sablefish[c(2,1,8)] #got the relevant data frame here
###you may want to add more
#Sable.matrix <- as.matrix(data=c(Sablefish$LONGITUDE, Sablefish$LATITUDE, ),nrow=162)
#make a matric
Sable_matrix <- as.matrix(Sablefish_relevant) #that worked!!
Sable_geo <- as.geodata(Sable_matrix) #now create the geodata object

##WARNING!! I pulled out all examples of sablefish. There are surveys with 0 sablefish that I did not
##acknoledge at this point. Those 0's are probably important


##which surveys do we have?
names(Sablefish)
Sablefish_survey_df <- Sablefish[c(16,17)]
Sablefish
#surveys total
Groundfish$HAUL


#unite
SableID1 <- unite(Sablefish_survey_df, ID_new, CRUISE:HAUL)
names(Sablefish_survey_df)
head(Sablefish_survey_df)

Groundfish_survey_ID <- Groundfish[c(16,17)]

GroundID1<- unite(data=Groundfish_survey_ID, col=ID_new, CRUISE:HAUL)

GroundID <-as.factor(GroundID1$ID_new)
SableID <- as.factor(SableID1$ID_new)
nosable <- unique(setdiff(GroundID, SableID))
length(unique(GroundID))
length(nosable)
length(unique(SableID)) #not the same as sable ID. I do not know why


#seemed to work. not let's find the coords for nosable
Groundfish$ID_new <- GroundID #a factor
names(Groundfish)
head(Groundfish)
#does intersect work here
#nosable.df <- Groundfish %>% intersect(GroundID, nosable)
nosable.df <- nosable %in% Groundfish$ID_new #HELP

Groundfish_locations_nosable <- Groundfish %>% filter(ID_new %in% c(nosable))
#THANK YOU MATT!!
names(Groundfish_locations_nosable)
Groundfish_locations_nosable_coords <- unique(Groundfish_locations_nosable[c(2,1,18)])
Groundfish_locations_nosable_coords2 <- unique(Groundfish_locations_nosable[c(2,1)])
length(Groundfish$LATITUDE)
length(Groundfish_locations_nosable_coords$LATITUDE)
length(unique(Groundfish_locations_nosable_coords$ID_new))
length(Groundfish_locations_nosable_coords2$LATITUDE)

head(Groundfish_locations_nosable_coords)
names(Groundfish_locations_nosable_coords)
head(Sable_matrix)
Groundfish_locations_nosable_coords$NUMCPUE <- rep(0, 993)
Nosable_matrix_prep <- Groundfish_locations_nosable_coords[-3]

#so theres 162 sablefish hauls and
##993 hauls without sablefish if we use the Groundfish_locations_nosable_coords dataset
##let's combine the dataframe so that we have sablefish coords and our zeros!!

head(Sable_matrix)
Sable_0_matrix <- as.matrix(Nosable_matrix_prep)
#Sable_matrix[,1] <- abs(Sable_matrix[,1]) #this was not the right step
#Sable_0_matrix[,1] <- abs(Sable_0_matrix[,1] ) #tis was not the right step

Sable_matrix_combined <- rbind(Sable_matrix, Sable_0_matrix)

Sable_geo <- as.geodata(Sable_matrix_combined)
Sable_geo
plot(Sable_geo)

#how to center my coordiantes
library(spBayes)

#try ggplot
Sable_geo_df <- as.data.frame(Sable_geo)
#ggplot(Sable_geo_df) + aes(x=LONGITUDE, y=LATITUTE,)

#CENTER THE COORDINATES,THEN PLOT AGAIN
#scallops$clons <- scallops$long - mean(scallops$long)
#scallops$clats <- scallops$lat - mean(scallops$lat)
Sable_matrix_combined_centered <- na.omit(Sable_matrix_combined)
Sable_matrix_combined_centered[,1] <- Sable_matrix_combined_centered[,1]- mean(Sable_matrix_combined_centered[,1])
Sable_matrix_combined_centered[,2] <- Sable_matrix_combined_centered[,2]- mean(Sable_matrix_combined_centered[,2])

Sable_centered <- as.geodata(Sable_matrix_combined_centered) #does not like that I have some duplicate locations
plot(Sable_centered)
plot(Sable_geo)


#margaret suggestion to fix coordinates
lons <- Sable_geo$coords[,1]
lons <- ifelse(lons<0, lons+180, lons-180)
Sable_geo$coords[,1] <- lons
plot(Sable_geo)



