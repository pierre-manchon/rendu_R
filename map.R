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

#########
# CARTE #
#########

# je fait une jointure du shapefile et des donnees promethees sur les champs du code insee
df_feux_communes = df_com %>%
  merge(
    x=df_com,
    y=df_feux,
    by.x="INSEE_COM",
    by.y="code_INSEE",
    duplicateGeoms=TRUE
    )

# Définition des écarts de valeurs dans la symologie
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)

# Définition de la symbologie graphique selon un champ
palette_feux <- colorBin("YlOrRd",
                    domain=df_feux_communes$surface_ha,
                    bins=bins)

# Définition du format des popups
popup_feux <- paste("Commune:", df_feux_communes$NOM_COM, "<br/>",
                    "Aire brulée: ", round(df_feux_communes$surface_ha, 2),
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
  
  addPolygons(
    data=df_feux_communes,
    fillColor=palette_feux(df_feux_communes$surface_ha),
    stroke=TRUE,
    fillOpacity = 0.9,
    color="white",
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
  hideGroup("EPCI") %>%
  hideGroup("Communes") %>%
  hideGroup("Feux") %>%
  
  # Ajout tout simple de la barre d'échelle
  addScaleBar(position="bottomleft") %>%
  
  # Ajout de la légende
  # TODO raise Error in get(".xts_chob", .plotxtsEnv) : objet '.xts_chob' introuvable
  # écrire leaflet::addLegend au lieu de %>% addLegend() à l'air de régler le problème
  leaflet::addLegend(map, values="a", pal=palette_feux, position="bottomright")
  addLegend(pal=palette_feux, values=df_feux_communes$surface_ha, opacity=0.9, title="Surface brulée (ha)", position="bottomleft")

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