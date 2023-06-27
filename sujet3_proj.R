# Lecture du fichier requirements.txt
requirements <- readLines("requirements_R.txt")

# Installation des packages
for (package in requirements) {
  install.packages(package)
}
library(aws.s3)
library(dplyr)
library(ggformula)
library(ggplot2)
library(tidyverse)
library(readr)
library(haven)
library(sf)
library(mapsf)

bucket <- "projet-funathon"
path_data <- "2023/sujet3/diffusion"
description_indiv <- s3read_using(read_delim, object = paste(path_data, "description-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
habitudes_indiv <- s3read_using(read_delim, object = paste(path_data, "habitudes-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
actphys_sedent <- s3read_using(read_delim, object = paste(path_data, "actphys-sedent.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
fpq <- s3read_using(read_delim, object = paste(path_data, "fpq.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)

#ce que les enfants aiment manger
aime<-habitudes_indiv[,c(2,86:95)]

infos_indiv<-description_indiv%>%
  select(NOIND, sex_PS,tage_PS,region_adm_12cl,agglo_5cl)

habitudes<-aime%>%
  left_join(infos_indiv)

## Partie 1 : Analyse exploratoire des données et visualisations
summary(habitudes)

mineur<- habitudes%>%
  filter(tage_PS<"7")

majeur<- habitudes%>%
  filter(tage_PS>"6")

# Mineur 
barplot <- function(variable){
  ggplot(mineur) +
    aes(x = factor(eval(parse(text = variable))), fill = factor(sex_PS),by=factor(sex_PS)) +
    geom_bar(position=position_dodge(.9)) +
    xlab(variable) +
    ylab("Proportion") +
    labs(fill = "Sexe") +
    theme_classic() +
    scale_fill_brewer()
} 

barplot("aime_legumes")
barplot("aime_viande")
barplot("aime_lait")
barplot("aime_poisson")

# Cartographie
url <- "https://minio.lab.sspcloud.fr/projet-cartiflette/diffusion/shapefiles-test1/year%3D2022/administrative_level%3DREGION/crs%3D4326/FRANCE_ENTIERE%3Dmetropole/vectorfile_format%3D%27geojson%27/provider%3D%27IGN%27/source%3D%27EXPRESS-COG-CARTO-TERRITOIRE%27/raw.geojson"
region <- sf::st_read(url)
region <- region %>% st_transform(2154)
mineur <- mineur %>% mutate(NOM_M=case_when(region_adm_12cl==1 ~ "ILE-DE-FRANCE",
                                            region_adm_12cl==2 ~ "NORMANDIE",
                                            region_adm_12cl==3 ~ "CENTRE-VAL DE LOIRE",
                                            region_adm_12cl==4 ~ "PAYS DE LA LOIRE",
                                            region_adm_12cl==5 ~ "BRETAGNE",
                                            region_adm_12cl==6 ~ "HAUTS-DE-FRANCE",
                                            region_adm_12cl==7 ~ "GRAND EST",
                                            region_adm_12cl==8 ~ "BOURGOGNE-FRANCHE-COMTE",
                                            region_adm_12cl==9 ~ "AUVERGNE-RHONE-ALPES",
                                            region_adm_12cl==10 ~ "PROVENCE-ALPES-COTE D'AZUR",
                                            region_adm_12cl==11 ~ "OCCITANIE",
                                            region_adm_12cl==12 ~ "NOUVELLE-AQUITAINE",))
carte<-right_join(region,mineur)
  
mf_init(x=region)
mf_shadow(x=region)
plot(st_geometry(region),lwd=1,border = "black",col="white",add = T)
legume <- carte %>% group_by(NOM_M) %>% summarise(freq_conso_biere_moyenne=mean(BA_biere_freq_M,na.rm=TRUE))

mf_map(x=carte,
       type=)


# Afficher deux cartes en vis-à-vis
par(mfrow = c(1,2))

mf_init(x=region)
mf_shadow(x=region)
plot(st_geometry(region),lwd=1,border = "black",col="white",add = T)# Population active occupée dans l'industrie, les hommes

mf_map(
  x = carte, 
  var = "IND_H", 
  type = "prop", 
  inches = .2, 
  val_max = 600, 
  leg_title = "Hommes", 
  leg_val_cex = .5,
)
# ajout d'un titre
mf_title("Population active occupée dans l'industrie")

mf_init(x=region)
mf_shadow(x=region)
plot(st_geometry(region),lwd=1,border = "black",col="white",add = T)
# Population active occupée dans l'industrie, les femmes
mf_map(
  x = com, 
  var = "IND_F", 
  type = "prop",
  inches = .2, 
  val_max = 600, 
  leg_title ="Femmes", 
  leg_val_cex = .5
)

# ajout d'un titre
mf_title("Population active occupée dans l'industrie")