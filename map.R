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
df_dfci2 <- readOGR(dsn=getwd(), layer="CARRO_DFCI_2X2_L93")
df_dfci20 <- readOGR(dsn=getwd(), layer="CARRO_DFCI_20X20_L93")

# Je redéfinit le path du wd un dossier au dessus dans la racine pour retourner dans le dossier
# principal du projet.
setwd("..")

############################
# PREPARATION DE LA DONNEE #
############################

# Communes

# je fait une jointure du shapefile et des donnees promethees sur les champs du code insee
df_feux_com = df_com %>%
  merge(
    x=df_com,
    y=df_feux,
    by.x="INSEE_COM",
    by.y="code_INSEE",
    duplicateGeoms=TRUE
  )

#df_feux_com <- subset(df_feux_com, df_feux_com$surf_parcourue_m2 != 0)
#part = df_feux_com@data[df_feux_com@data$dep == "13",]
df_feux_com = subset(df_feux_com, df_feux_com@data$type_feu == "0")

# DFCI2

# je fait une jointure du shapefile et des donnees promethees sur les champs du code insee
df_feux_dfci2 = df_dfci2 %>%
  merge(
    x=df_dfci2,
    y=df_feux,
    by.x="NOM",
    by.y="code_DFCI",
    duplicateGeoms=TRUE
  )

#df_feux_com <- subset(df_feux_com, df_feux_com$surf_parcourue_m2 != 0)
#part = df_feux_com@data[df_feux_com@data$dep == "13",]
df_feux_dfci2 = subset(df_feux_dfci2, df_feux_dfci2@data$type_feu == "0")

# DFCI20

# je fait une jointure du shapefile et des donnees promethees sur les champs du code insee
df_feux_dfci20 = df_dfci20 %>%
  merge(
    x=df_dfci20,
    y=df_feux,
    by.x="NOM",
    by.y="code_DFCI",
    duplicateGeoms=TRUE
  )

#df_feux_com <- subset(df_feux_com, df_feux_com$surf_parcourue_m2 != 0)
#part = df_feux_com@data[df_feux_com@data$dep == "13",]
df_feux_dfci20 = subset(df_feux_dfci20, df_feux_dfci20@data$type_feu == "0")

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

# Définition des écarts de valeurs dans la symologie
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)

# Définition de la symbologie graphique selon un champ
palette_feux <- colorBin("YlOrRd",
                    domain=df_feux_com$surface_ha,
                    bins=bins)

# Définition du format des popups
popup_feux <- paste("Commune:", df_feux_com$NOM_COM, "<br/>",
                    "Aire brulée: ", round(df_feux_com$surface_ha, 2),
                    sep="") %>%
  lapply(htmltools::HTML)

# Définition du format de la légende
# De l'aide sur le formattage di sprintf
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sprintf
legend_feux <- paste("Région: ", df_reg$NOM_REG,"<br/>",
                    "Département: ", df_dep$NOM_DEP, "<br/>",
                    "EPCI: ", df_epci$NOM_EPCI, "<br/>",
                    "Commune:", df_com$NOM_COM,
                    sep="") %>%
  lapply(htmltools::HTML)

# Je créé ma carte leaflet de base avec
map <- leaflet() %>%
  # Localisation de base de la carte lorsqu'elle est initialisée
  setView(5, 50, 6) %>%
  
  # Ajout des fonds de cartes: group correspond au nom que l'on veut donner au fond de carte
  # (C'est ce nom que l'on va encapsuler dans un groupe du LayersControl et qui apparaîtra dans
  # la légende).
  addTiles(group="OSM (default)") %>%
  addProviderTiles(providers$CartoDB.Positron, group="CartoDB") %>%
  
  # Ajout des polygones des régions, départements, epci, communes et définition pour chacun
  # de leur style graphique (sert de fond de carte donc n'utilise pas les données de feux
  # pour faire des graduations)
  addPolygons(data=df_reg, fill=FALSE, weight=2, color="#000", group="Régions") %>%
  addPolygons(data=df_dep, fill=FALSE, weight=1, color="#000", group="Départements") %>%
  addPolygons(data=df_epci, fill=FALSE, weight=0.5, color="#000", group="EPCI") %>%
  addPolygons(data=df_com, fill=FALSE, weight=0.25, color="#000", group="Communes") %>%
  
  # Ajout des couches de données
  addPolygons(
    data=df_feux_com,
    fillColor=palette_feux(df_feux_com$surface_ha),
    stroke=TRUE,
    fillOpacity = 0.9,
    color="white",
    group="FEUXCOM",
    weight=0.3,
    label=popup_feux,
    labelOptions=labelOptions( 
      style=list("font-weight"="normal", padding="3px 8px"), 
      textsize="13px", 
      direction="auto"
    )) %>%
  addPolygons(
    data=df_feux_dfci2,
    fillColor=palette_feux(df_feux_dfci2$surface_ha),
    stroke=TRUE,
    fillOpacity = 0.9,
    color="white",
    group="FEUXDFCI2",
    weight=0.3,
    label=popup_feux,
    labelOptions=labelOptions( 
      style=list("font-weight"="normal", padding="3px 8px"), 
      textsize="13px", 
      direction="auto"
    )) %>%
  
  # Ajout du menu de control des couches et regroupement des couches par groupes de control.
  addLayersControl(
    baseGroups=c("OSM (default)", "CartoDB"),
    overlayGroups=c("Régions", "Départements", "EPCI", "Communes", "Feux"),
    options=layersControlOptions(collapsed=TRUE)) %>%
  # Je définit quelles couches sont cachées par défaut
  # Ca aide à ce que la carte charge plus vite.
  hideGroup("FEUXCOM") %>%
  hideGroup("FEUXDFCI2") %>%
  hideGroup("EPCI") %>%
  hideGroup("Communes") %>%
  hideGroup("Feux") %>%
  
  # Ajout tout simple de la barre d'échelle
  addScaleBar(position="bottomleft")
  
  # Ajout de la légende
  # TODO raise Error in get(".xts_chob", .plotxtsEnv) : objet '.xts_chob' introuvable
  # écrire leaflet::addLegend au lieu de %>% addLegend() à l'air de régler le problème
  leaflet::addLegend(map, values="a", pal=palette_feux, position="bottomright")
  #addLegend(pal=palette_feux, values=df_feux_communes$surface_ha, opacity=0.9, title="Surface brulée (ha)", position="bottomleft")

# Créé litéralement la carte en executant la fonction leaflet derrière
# C'est là que je génère le rendu de la carte dans le viewer en appelant la fonction map
# (je préfère la sauvegarder en html directement à la fin du script)
#map

# Sauvegarde map (la cartographie) vers le fichier map.html dans le wd par défaut
# (car je ne l'ai pas redéterminé)
# TODO ça crash...
#saveWidget(map, file="map.html")

# Maintenant que c'est enregistré je peux supprimer toutes les variables qui m'ont permise de générer le graph
rm(map)