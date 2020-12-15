library(dplyr)
library(rgdal)
library(leaflet)
library(quantmod)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)

#######################
# LECTURE DES DONNEES #
#######################

# Je définit le path du wd comme étant le sous dossier data pour lire les couches sig
setwd("./data/")

# Je lis les données brutes en csv de l'export de la base prométhée
df_feux <- read.csv2("liste_incendies_ du_07_12_2020_formatte.csv")

# Tableaux croisés dynamiques nombre, surface totale, surface max sur :
# Année (graphe) commenter
# mois et heure (surface) commenter
# départements (graphe) commenter

# Je lis la couche vecteur des régions, départements, communes, epci
df_reg <- readOGR(dsn=getwd(), layer="REGION")
df_dep <- readOGR(dsn=getwd(), layer="DEPARTEMENT")
df_com <- readOGR(dsn=getwd(), layer="COMMUNE")
df_epci <- readOGR(dsn=getwd(), layer="EPCI")

# Je lis les couches vecteurs des carroyages DFCI
df_dfci2 <- readOGR(dsn=getwd(), layer="DFCI2")
df_dfci20 <- readOGR(dsn=getwd(), layer="DFCI20")

# Je redéfinit le path du wd un dossier au dessus dans la racine pour retourner dans le dossier
# principal du projet.
setwd("..")

############################
# PREPARATION DE LA DONNEE #
############################

# Communes

# Je fais un group by pour regrouper les données selon le champ code_INSEE puis un summarise pour y associer
# les données de surface brulée ainsi que de nombre de feux
df_feux_gb_com <- df_feux %>% group_by(code_INSEE) %>% summarize(surface_ha=sum(surface_ha), nbr_feux=n())

# Je fait une jointure du shapefile des communes et des donnees promethees de feux que j'ai regroupé
# juste au dessus en utilisant les champs du code insee
df_feux_com = df_com %>%
  merge(
    x=df_com,
    y=df_feux_gb_com,
    by.x="INSEE_COM",
    by.y="code_INSEE"
  )

# Je supprime le dataframe df_feux_gb_com car je n'en ai plus besoin (les données sont intégrées dans le
# dataframe df_feux_com)
rm(df_feux_gb_com)

# Je supprime les communes qui ne comportent pas de feux (celles qui n'ont pas de valeur dans le champ nbr_feux)
#df_feux_com <- subset(df_feux_com, df_feux_com$surf_parcourue_m2 != 0)
df_feux_com = subset(df_feux_com, df_feux_com@data$nbr_feux != "")

# DFCI2

# Je fais un group by pour regrouper les données selon le champ code_DFCI puis un summarise pour y associer
# les données de surface brulée ainsi que de nombre de feux
df_feux_gb_dfci2 <- df_feux %>% group_by(code_DFCI) %>% summarize(surface_ha=sum(surface_ha), nbr_feux=n())

# Je fait une jointure du shapefile du carroyage DFCI à 2km et des donnees promethees de feux que j'ai
# regroupé juste au dessus en utilisant les champs du code DFCI
df_feux_dfci2 = df_dfci2 %>%
  merge(
    x=df_dfci2,
    y=df_feux_gb_dfci2,
    by.x="NOM",
    by.y="code_DFCI"
  )

# Je supprime le dataframe df_feux_gb_dfci2 car je n'en ai plus besoin (les données sont intégrées dans le
# dataframe df_feux_dfci2)
rm(df_feux_gb_dfci2)

# Je supprime les carreaux DFCI qui ne comportent pas de feux (ceux qui n'ont pas de valeur dans le champ nbr_feux)
df_feux_dfci2 = subset(df_feux_dfci2, df_feux_dfci2@data$nbr_feux != "")

# DFCI20

# Je fais un group by pour regrouper les données selon le champ code_DFCI puis un summarise pour y associer
# les données de surface brulée ainsi que de nombre de feux
df_feux_gb_dfci20 <- df_feux %>% group_by(code_DFCI) %>% summarize(surface_ha=sum(surface_ha), nbr_feux=n())

# Je fait une jointure du shapefile du carroyage DFCI à 20km et des donnees promethees de feux que j'ai
# regroupé juste au dessus en utilisant les champs du code DFCI
df_feux_dfci20 = ddf_dfci20 %>%
  merge(
    x=df_dfci20,
    y=df_feux_gb_dfci20,
    by.x="NOM",
    by.y="code_DFCI"
  )

# Je supprime le dataframe df_feux_gb_dfci20 car je n'en ai plus besoin (les données sont intégrées dans le
# dataframe df_feux_dfci20)
rm(df_feux_gb_dfci20)

# Je supprime les carreaux DFCI qui ne comportent pas de feux (ceux qui n'ont pas de valeur dans le champ nbr_feux)
df_feux_dfci20 = subset(df_feux_dfci20, df_feux_dfci20@data$nbr_feux != "")

#spplot(y, "surface_ha", main = "Area of Different Ecoregions", sub = "Average Area", col = "transparent")
#my.palette <- brewer.pal(n = 8, name = "Blues") #color selection no.8 #of blues
#spplot(l, "AREA", col.regions = my.palette, cuts = 6, col = "transparent") #6 shades of blue
#library(classInt)
#my.palette2 <- brewer.pal(n = 8, name = "YlOrRd")
#breaks.qt <- classIntervals(l$AREA, n = 6, style = "quantile", intervalClosure = "right")
#spplot(l, "AREA", col = "transparent", col.regions = my.palette2, 
#       at = breaks.qt$brks)
#world=readOGR(dsn=getwd(), layer="countries")
#world$UNREG1 <- as.factor(iconv(as.character(world$UNREG1), "latin1", "UTF-8"))  # avoid the problems with 'tildes' 
#spplot(world, "UNREG1",main="World Poltical Boundaries", col.regions = colorRampPalette(brewer.pal(12, "Set3"))(18), 
#       col = "white")  # Plot the 'unreg1' form the 'world' object.

#########
# CARTE #
#########

# Définition de la symbologie graphique selon un champ
palette_feux_com <- colorQuantile("YlOrRd",
                             domain=df_feux_com@data$surface_ha)

palette_feux_dfci2 <- colorQuantile("YlOrRd",
                             domain=df_feux_dfci2@data$surface_ha)

# Définition du format des popups
popup_feux_com <- paste("Commune: ", df_feux_com@data$NOM_COM_M, "<br/>",
                    "Surface brulée: ", round(df_feux_com, 2), "ha",
                    sep="") %>%
  lapply(htmltools::HTML)

popup_feux_dfci2 <- paste("Carreau DFCI: ", df_feux_dfci2@data$NOM, "<br/>",
                        "Surface brulée: ", round(df_feux_dfci2@data$surface_ha, 2), "ha",
                        sep="") %>%
  lapply(htmltools::HTML)

# Je créé ma carte leaflet de base avec
map <- leaflet() %>%
  # Localisation de base de la carte lorsqu'elle est initialisée
  setView(5, 45, 6) %>%
  
  # Ajout des fonds de cartes: group correspond au nom que l'on veut donner au fond de carte
  # (C'est ce nom que l'on va encapsuler dans un groupe du LayersControl et qui apparaîtra dans
  # la légende).
  addTiles(group="OSM (default)") %>%
  
  # Ajout des polygones des régions, départements, epci, communes et définition pour chacun
  # de leur style graphique (sert de fond de carte donc n'utilise pas les données de feux
  # pour faire des graduations)
  addPolygons(data=df_reg, fill=FALSE, weight=2, color="#000", group="Régions") %>%
  addPolygons(data=df_dep, fill=FALSE, weight=1, color="#000", group="Départements") %>%
  addPolygons(data=df_epci, fill=FALSE, weight=0.5, color="#000", group="EPCI") %>%
  
  # AJout des données de feux

  # Ajout des données de feux selon les communes
  addPolygons(
    data=df_feux_com,
    fillColor=palette_feux_com(df_feux_com@data$surface_ha),
    fillOpacity = 0.9,
    color="black",
    group="COM",
    weight=0.3,
    label=popup_feux_com) %>%

  # Ajout des données DFCI à 2km
  addPolygons(
    data=df_feux_dfci2,
    fillColor=palette_feux_dfci2(df_feux_dfci2@data$surface_ha),
    fillOpacity = 0.9,
    color="black",
    group="DFCI2",
    weight=0.3,
    label=popup_feux_dfci2) %>%

  # Ajout du menu de control des couches et regroupement des couches par groupes de control.
  addLayersControl(
    baseGroups=c("COM", "DFCI2"),
    overlayGroups=c("Régions", "Départements", "EPCI"),
    options=layersControlOptions(collapsed=TRUE)) %>%
  # Je définit quelles couches sont cachées par défaut
  # Ca aide à ce que la carte charge plus vite.
  hideGroup("EPCI") %>%
  hideGroup("COM") %>%
  hideGroup("DFCI2") %>%
  
  # Ajout tout simple de la barre d'échelle
  addScaleBar(position="bottomleft")
  
# Créé litéralement la carte en executant la fonction leaflet derrière
# C'est là que je génère le rendu de la carte dans le viewer en appelant la fonction map
# (je préfère la sauvegarder en html directement à la fin du script)
map

# Ajout de la légende
# raise Error in get(".xts_chob", .plotxtsEnv) : objet '.xts_chob' introuvable
# écrire leaflet::addLegend au lieu de %>% addLegend() à l'air de régler le problème

# n'est pas executé
leaflet:addLegend(map,
                  values=round(df_feux_com@data$surface_ha),
                  group="Surface brulée par communes",
                  pal=palette_feux_com,
                  labFormat = labelFormat(suffix="ha"),
                  position="bottomleft")
leaflet::addLegend(map,
                   values=round(df_feux_dfci2@data$surface_ha),
                   group="Surface brulée par carreau DFCI de 2km",
                   pal=palette_feux_dfci2,
                   labFormat = labelFormat(suffix="ha"),
                   position="bottomleft")

# Sauvegarde map (la cartographie) vers le fichier map.html dans le wd par défaut
# TODO ça crash...
#saveWidget(map, file="map.html")
saveWidget(widget=map,
           file="map.html",
           selfcontained = FALSE)

# Maintenant que c'est enregistré je peux supprimer toutes les variables qui m'ont permise de générer le graph
#rm(map)