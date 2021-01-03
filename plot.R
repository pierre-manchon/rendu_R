library(dplyr)
library(plotly)
library(htmlwidgets)

#######################
# LECTURE DES DONNEES #
#######################

# Je definit le path du wd comme etant le sous dossier data pour lire les couches sig
setwd("./data/")

# Je lis les donnees brutes en csv de l'export de la base promethee
df_feux <- read.csv2("liste_incendies_ du_07_12_2020_formatte.csv")

# Je redefinit le path du wd un dossier au dessus dans la racine pour retourner dans le dossier
# principal du projet.
setwd("..")

############
# GRAPH 2D #
###########

# Je fais un group by pour regrouper les donnees selon un champ puis un summarise pour y associer les donnees
df_feux_gb_annee_surfha_nbr <- df_feux %>% group_by(annee) %>% summarize(surface_ha_max=round(max(surface_ha)), sum_surface_ha=round(sum(surface_ha)), nbr=n())

# C'est la que je met en forme le graph
# https://plotly.com/r/line-charts/
feu_par_ans <- plot_ly(
  data=df_feux_gb_annee_surfha_nbr,
  x=df_feux_gb_annee_surfha_nbr$annee,
  y=df_feux_gb_annee_surfha_nbr$sum_surface_ha,
  name="Surface totale brulee",
  type="scatter",
  mode="lines+markers",
  color=I('black')
) %>%
  add_trace(y=df_feux_gb_annee_surfha_nbr$surface_ha_max,
            name="Surface maximum brulee",
            type="scatter",
            mode="lines+markers",
            color=I("red")) %>%
  add_trace(y=df_feux_gb_annee_surfha_nbr$nbr,
            name="Nombre d'incendies",
            type="scatter",
            mode="lines+markers",
            color=I("blue"))

# Je met en forme le titre du graph ainsi que les titres des axes
feu_par_ans <- feu_par_ans %>% layout(title = "Surface totale et maximum brulee (ha) et
                                    nombre d'incendies selon les annees",
                                      # Met le mode "Compare data on hover" actif par defaut.
                                      hovermode='compare',
                                      legend=list(x=0.75,
                                                  y=0.9),
                                      xaxis=list(title="Annees",
                                                 rangeslider=list(
                                                   type="date")),
                                      yaxis=list(visible=FALSE)) %>% 
  config(displayModeBar = FALSE)  # Cache les commandes du graph par defaut.

# C'est la que je genere le rendu du graph dans le viewer en appelant la fonction graph
# (je prefere le sauvegarder en html directement)
#feu_par_ans
# Sauvegarde graph (le graphique 2D) vers le fichier feu_par_an.html dans le wd par defaut
# (car je ne l'ai pas redetermine)
saveWidget(feu_par_ans, file="feu_par_an.html")

# Maintenant que c'est enregistre je peux supprimer toutes les variables qui m'ont permise de generer le graph
rm(df_feux_gb_annee_surfha_nbr)
rm(feu_par_ans)

# Je fais un group by pour regrouper les donnees selon un champ puis un summarise pour y associer les donnees
df_feux_gb_dep_surfha_nbr <- df_feux %>% group_by(dep) %>% summarize(surface_ha_max=round(max(surface_ha)), sum_surface_ha=round(sum(surface_ha)), nbr=n())

# C'est la que je met en forme le graph
# https://plotly.com/r/line-charts/
feu_par_dep <- plot_ly(
  data=df_feux_gb_dep_surfha_nbr,
  x=df_feux_gb_dep_surfha_nbr$dep,
  y=df_feux_gb_dep_surfha_nbr$nbr,
  name="Nombre d'incendies",
  type="scatter",
  color=I("blue")
) %>%
  add_trace(y=df_feux_gb_dep_surfha_nbr$sum_surface_ha,
            name="Surface totale brulee",
            type="bar",
            color=I('black')) %>%
  add_trace(y=df_feux_gb_dep_surfha_nbr$surface_ha_max,
            name="Surface maximum brulee",
            type="bar",
            color=I("red"))

# Je met en forme le titre du graph ainsi que les titres des axes
feu_par_dep <- feu_par_dep %>% layout(title = "Surface totale et maximum brulee (ha) et
                                    nombre d'incendies selon les departements",
                                      # Met le mode "Compare data on hover" actif par defaut.
                                      legend=list(x=0,
                                                  y=0.9),
                                      xaxis=list(title="Departements"),
                                      yaxis=list(visible=FALSE),
                                      barmode='overlay',
                                      bargap=0.15) %>% 
  config(displayModeBar = FALSE)  # Cache les commandes du graph par defaut.

# C'est la que je genere le rendu du graph dans le viewer en appelant la fonction graph
# (je prefere le sauvegarder en html directement)
#feu_par_dep

# Sauvegarde graph (le graphique 2D) vers le fichier feu_par_an.html dans le wd par defaut
# (car je ne l'ai pas redetermine)
saveWidget(feu_par_dep, file="feu_par_dep.html")

# Maintenant que c'est enregistre je peux supprimer toutes les variables qui m'ont permise de generer le graph
rm(df_feux_gb_dep_surfha_nbr)
rm(feu_par_dep)

##########
# TESTS #
#########

# GRAPH 2D

# TODO Je cree une fonction pour pouvoir generer des graphs basiques plus facilement
# df_plot <- df_feux %>% group_by(annee) %>% summarize(nbr=n())
# plot(df_plot, type="o", main="", xlab="Annees", ylab="Nbr incendies")
# ça marche mais ça n'utilise que n() en y (passer n() ou sum() en argument de la fonction
# ne marche pas)
to_plot<- function(df, colname="", sum="", title="title", xlab="xlab", ylab="ylab") {
  df_plot <- df %>% group_by(df[colname]) %>% summarize(n())
  return(plot(df_plot, type="o", main=title, xlab=xlab, ylab=ylab))
}

to_plot(df=df_feux,
        colname="annee",
        title="Nombre d'incendies selon les annees",
        xlab="Annees",
        ylab="Nombre d'incendies")

to_plot(df=df_feux,
        colname="dep",
        title="Nombre d'incendies selon les departements",
        xlab="Departements",
        ylab="Nombre d'incendies")

# SURFACE PLOT

# j'extrait les donnees que je veux mettre en forme dans le surface plot grâce a un group_by
# puis je continue de les mettre en forme en faisant la somme des surfaces puis je change l'ordre
# des colonnes en recreant un data frame et enfin je le converti en matrice numerique pour creer
# le surface plot
#df_feux_prematrix <- df_feux %>% group_by(mois, heure)
#df_feux_prematrix <- df_feux_prematrix %>% summarise(sum_surface_ha = sum(surface_ha))

#df_feux_prematrix <- data.frame(
#  mois=df_feux_prematrix$mois,
#  heure=df_feux_prematrix$heure,
#  surface_ha=df_feux_prematrix$sum_surface_ha)

# Je cree une matrice numerique a partir du data frame pour creer le surfaceplot
#df_feux_matrix <- matrix(as.numeric(unlist(df_feux_prematrix)),nrow=nrow(df_feux_prematrix))

# Je supprime le data.frame utilise pour creer la matrice car il ne nous sert plus
#rm(df_feux_prematrix)

# Je cree le surface plot
#feu_mois_heure <- plot_ly(z=df_feux_matrix) %>%
#  add_surface(contours=list(z=list(show=TRUE,
#                                   usecolormap=TRUE,
#                                   highlightcolor="#ff0000",
#                                   project=list(z=TRUE))))
#feu_mois_heure <- feu_mois_heure %>%
#  config(displayModeBar = FALSE)

# Je genere la visualisation du graph (je l'enregistre en html plus bas)
#feu_mois_heure

# Sauvegarde le graph de surface vers feu_mois_heure.html
#saveWidget(feu_mois_heure, file="feu_mois_heure.html")

# Maintenant que c'est enregistre je peux supprimer toutes les variables qui m'ont permise de generer le graph
#rm(df_feux_matrix)
#rm(feu_mois_heure)