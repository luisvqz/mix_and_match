
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("ggsankey", quietly = TRUE)) remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(tidyverse)
library(gtExtras)
library(readxl)
library(gt)
library(webshot2)
library(circlize)

setwd("H:/.shortcut-targets-by-id/1aGoDMf1J-PODyaVsn5786Z-ugWxfoOpG/R01 2021 project implementation/2026 Manuscripts R01/Perils working documents/Frontiers/Scripts")

river_fam <- read.csv("Full_crossings_2026.csv")
connections_bat_fam<-river_fam %>%
  group_by(
    family,
    viral_family) %>%
  count(sort=TRUE) %>%
  ungroup()

virus_sd<- WOS_long %>% 
  group_by(
    Virus) %>% 
  count(sort = TRUE) %>% 
  ungroup()  

View(virus_sd)


fam.col = c(
  "Molossidae"= "#2062ae",
  "Phyllostomidae"= "#49b864",
  "Pteropodidae"= "#522e8c",
  "Miniopteridae"= "#D63227",
  "vespertilionidae"= "#f9a143",
  "Rhinolophidae"= "#d5539e",  
  "Hipposideridae"= "#a7a7bd",
  "Adenoviridae" = "#d39eb1",
  "Arenaviridae" = "#d39eb1",
  "Coronaviridae" = "#d39eb1",
  "Filoviridae" = "#946f1f",
  "Flaviviridae" = "#70b844",
  "Hepeviridae" = "#d39eb1",
  "Orthoherpesviridae" = "#d39eb1",
  "Orthomyxoviridae" = "#66be80",
  "Paramyxoviridae" = "#d39eb1",
  "Peribunyaviridae" = "#d39eb1",
  "Picornaviridae" = "#d39eb1",
  "Reoviridae" = "#d39eb1",
  "Retroviridae" = "#771113",
  "Rhabdoviridae" = "#ca4a9b",
  "Spinareoviridae" = "#d39eb1",
  "Togaviridae" = "#d39eb1")



Chords_full<-chordDiagram(connections_bat_fam, grid.col = fam.col, annotationTrack = c("grid", "panel"),link.sort = TRUE, link.decreasing = TRUE)
Chords_full

webby<-as.tibble(Chords_full)
View(webby)
sum(webby$value1)


webby %>%
  group_by(cn) %>% 
  summarise(total = across(value1, sum))


connections_bat_fam_bdcl <- WOS_long %>% 
  filter(subject=="cells") %>%  
  group_by(
    bat_species,
    Virus) %>%
  count(sort=TRUE) %>%
  ungroup()


Chords_bdcl<-chordDiagram(connections_bat_fam_bdcl, grid.col = grid.col, annotationTrack = c("grid", "panel"))
Chords_bdcl

connections_bat_fam_bat <- WOS_long %>% 
  filter(subject=="bat") %>%  
  group_by(
    bat_species,
    Virus) %>%
  count(sort=TRUE) %>%
  ungroup()


Chords_bats<-chordDiagram(connections_bat_fam_bat, grid.col = grid.col, annotationTrack = c("grid", "panel"))
Chords_bats
