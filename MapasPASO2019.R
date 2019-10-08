### ANALISIS ELECTORAL PASO 2019 SALTA - CATEGORÍA GOBERNADOR

#paquetes
library(tidyverse)
library(readr)
library(readxl)
library(dbplyr)
library(sf)
library(leaflet)


#CARGO BASES


shape <- read_sf("MapaMunicipiosSalta.kml") %>%
  select(municipio = Name, geometry) %>% 
  st_cast(geometry, to = "MULTIPOLYGON") %>%
  print()
  

#CARGO BASE DE DATOS 
paso_gober <- read_csv("pasoSaltaGob_2019.csv") %>%
  select(-X1) %>% print()

paso_gober <- paso_gober %>%
  select(municipio = descripcion_ubicacion,
         LEAVY = `Celeste Y Blanca`,
         ISA = `El Futuro Es Con Todos`,
         OLMEDO = `Olmedo Gobernador 2019`,
         SAENZ = `Saenz Gobernador`,
         BLANCOS = `Voto en BLANCO`,
         POSITIVOS =  cant_votos_positivos) %>% 
  mutate(ganador = ifelse(SAENZ > ISA & SAENZ > LEAVY & SAENZ > OLMEDO, "Saenz",
                          ifelse(LEAVY > ISA & LEAVY > OLMEDO, "Leavy",
                                 ifelse(ISA> OLMEDO, "Isa", "Olmedo")))) %>% print()

# JOIN CON BASE GEOGRAFIA
gober_geo <- paso_gober %>%
  mutate(color = if_else(paso_gober$ganador == "Leavy", "#00aeef",
                        if_else(paso_gober$ganador == "Saenz", "#960000", "#FFFF33" ))) %>%
  left_join(shape, by = "municipio") %>%
  st_as_sf() %>%
  print()

mapa_municipios <- gober_geo %>% select(geometry, color) %>% st_zm()

#MAPA CON GANADORES X MUNICIPIO
leaflet(mapa_municipios) %>% 
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(fillColor = gober_geo$color,
              fillOpacity = 0.75, color = "white", dashArray = "4", weight = 3)


# VOTOS A SAENZ POR MUNICIPIO
saenz <- gober_geo %>% mutate(porcentaje = SAENZ / POSITIVOS) %>%
  select(geometry, porcentaje) %>% print()

ggplot(data=saenz) +
  geom_sf(aes(fill=porcentaje))  +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(title = "VOTO A SAENZ POR MUNICIPIO (en porcentaje)",
       x = "",
       y = "") +
  scale_fill_viridis_c(direction = -1)


# VOTOS A LEAVY POR MUNICIPIO

leavy <- gober_geo %>% mutate(porcentaje = LEAVY / POSITIVOS) %>%
  select(geometry, porcentaje) %>% print()

ggplot(data=leavy) +
  geom_sf(aes(fill=porcentaje))  +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(title = "VOTO A LEAVY POR MUNICIPIO (en porcentaje)",
       x = "",
       y = "") +
  scale_fill_viridis_c(direction = -1)

# VOTOS A ISA POR MUNICIPIO
isa <- gober_geo %>% mutate(porcentaje = ISA / POSITIVOS) %>%
  select(geometry, porcentaje) %>% print()

ggplot(data=isa) +
  geom_sf(aes(fill=porcentaje))  +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(title = "VOTO A MIGUEL ISA POR MUNICIPIO (en porcentaje)",
       x = "",
       y = "") +
  scale_fill_viridis_c(direction = -1)


# VOTOS A OLMEDO POR MUNICIPIO
olmedo <- gober_geo %>% mutate(porcentaje = OLMEDO / POSITIVOS) %>%
  select(geometry, porcentaje) %>% print()

ggplot(data=olmedo) +
  geom_sf(aes(fill=porcentaje))  +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(title = "VOTO A OLMEDO POR MUNICIPIO (en porcentaje)",
       x = "",
       y = "") +
  scale_fill_viridis_c(direction = -1)




#TREEMAPS
library(ggplot2)
library(treemapify)
library(treemap)
library(dplyr)

library(scales)
library(viridis)
library(extrafont)

arbol_saenz <- gober_geo %>%
  mutate(diferencia = SAENZ - LEAVY) %>%
  filter(ganador == "Saenz") %>%
  as_data_frame() %>% 
  select(municipio, diferencia, color) %>% 
  print()


ggplot(arbol_saenz, aes(area = diferencia, fill=municipio, label=diferencia)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", fontface = "italic")
