install.packages("leaflet")
install.packages("maps")
install.packages("maptools")
install.packages("htmltools")

library(leaflet)
library(maps)
library(maptools)
library(htmltools)

bdd <- read.csv("centres-vaccination_1.csv", sep = ";")

vacicon <- makeIcon(iconUrl = "vac.png",
                    iconWidth = 35, iconHeight = 35,
                    iconAnchorX = 35, iconAnchorY = 35)

mapfr <- map("france", fill = FALSE, plot = FALSE)

drawmap <-leaflet(data = mapfr) %>% addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
            addMarkers(bdd$long_coor1, bdd$lat_coor1, 
                       popup = paste("<b>", bdd$nom, "</b>", "<br>", 
                                     "<em>", "Adresse: ", "</em>", bdd$adr_num, " ", bdd$adr_voie, ", ", bdd$com_cp, " ", bdd$com_nom, "<br>",
                                     ifelse(bdd$rdv == "t", 
                                            paste("<b>", "Rdv", "<br>",
                                                  "Lundi: ", bdd$rdv_lundi, "<br>",
                                                  "Mardi: ", bdd$rdv_mardi, "<br>",
                                                  "Mercredi: ", bdd$rdv_mercredi, "<br>",
                                                  "Jeudi: ", bdd$rdv_jeudi, "<br>",
                                                  "Vendredi: ", bdd$rdv_vendredi, "<br>",
                                                  "Samedi: ", bdd$rdv_samedi, "<br>",
                                                  "Dimanche: ", bdd$rdv_dimanche, "</b>", "<br>"), 
                                            paste("<b>", "Rdv indisponible", "</b>", "<br>")),
                                     ifelse(bdd$rdv_site_web == "",
                                            paste("<b>", "Website indisponible", "</b>", "<br>"),
                                            paste("<em>", "Rdv website: ", "</em>", 
                                                  "<a href=", bdd$rdv_site_web, ">", bdd$rdv_site_web, "</a>", "<br>")
                                            ),
                                     ifelse(bdd$rdv_tel == "",
                                            paste("<b>", "Téléphone indisponible", "</b>", "<br>"),
                                            paste("<em>", "Tel: ", "</em>", bdd$rdv_tel, "<br>")
                                     ),
                                     ifelse(bdd$rdv_tel2 == "",
                                            "",
                                            paste("<em>", "Tel2: ", "</em>", bdd$rdv_tel2, "<br>")
                                     )),
                       label = bdd$nom,
                       labelOptions = labelOptions(direction = "top",style = list(
                                                     "color" = "#6495ED",
                                                     "font-family" = "times",
                                                     "font-weight" = "bold",
                                                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                     "font-size" = "18px",
                                                     "border-color" = "rgba(0,0,0,0.5)"
                                                   )),
                       clusterOptions = markerClusterOptions(), icon = vacicon)

browsable(
  tagList(list(
    tags$head(
      tags$style(
        ".leaflet-popup-content-wrapper {
            font-size = 14px,
            padding: 2px;
            border-radius: 0px;
          }"
      )
    ),
    drawmap
  ))
)

