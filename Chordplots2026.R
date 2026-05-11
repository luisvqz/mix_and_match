library("tidyverse")
library(gtExtras)
library(readxl)
library(gt)
library( webshot2)
library(circlize)

connections<-Full_crossings_2026%>%
  group_by(
    Bat_names,
    Virus) %>%
  count(sort=TRUE) %>%
  ungroup()

bats_sd<- Full_crossings_2026 %>%
  group_by(
    Bat_names) %>% 
      count(sort=TRUE) %>% 
      ungroup()
View(bats_sd)

virus_sd<- Full_crossings_2026 %>% 
  group_by(
    Virus) %>% 
    count(sort = TRUE) %>% 
    ungroup()  

View(virus_sd)

grid.col = c(
  "Rousettus aegyptiacus" = "#D63227",
  "Tadarida brasiliensis" = "#2062ae",
  "Eptesicus fuscus" = "#f9a143",
  "Artibeus jamaicensis" = "#49b864",
  "Pteropus alecto" = "#d5539e",
  "Eidolon helvum" = "#a7a7bd",
  "Hypsignathus monstrosus" = "#a7a7bd",
  "Myotis lucifugus" = "#a7a7bd",
  "Rousettus leschenaultii" = "#a7a7bd",
  "Carollia perspicillata" = "#a7a7bd",
  "Epomops buettikoferi" = "#a7a7bd",
  "Miniopterus fuliginosus" = "#a7a7bd",
  "Myotis daubentonii" = "#a7a7bd",
  "Eonycteris spelaea" = "#a7a7bd",
  "Desmodus rotundus" = "#a7a7bd",
  "Pteropus vampyrus" = "#a7a7bd",
  "Pteropus dasymallus" = "#a7a7bd",
  "Rhinolophus ferrumequinum" = "#a7a7bd",
  "Sturnira parvidens" = "#a7a7bd",
  "Miniopterus schreibersii" = "#a7a7bd",
  "Mops condylurus" = "#a7a7bd",
  "Pipistrellus pipistrellus" = "#a7a7bd",
  "Cnephaeus nilssonii" = "#a7a7bd",
  "Epomophorus crypturus" = "#a7a7bd",
  "Myotis brandtii" = "#a7a7bd",
  "Perimyotis subflavus" = "#a7a7bd",
  "Pteropus poliocephalus" = "#a7a7bd",
  "Rhinolophus landeri" = "#a7a7bd",
  "Artibeus intermedius" = "#a7a7bd",
  "Artibeus lituratus" = "#a7a7bd",
  "Carollia subrufa" = "#a7a7bd",
  "Myotis davidii" = "#a7a7bd",
  "Myotis myotis" = "#a7a7bd",
  "Phyllostomus discolor" = "#a7a7bd",
  "Antrozous pallidus" = "#a7a7bd",
  "Cnephaeus serotinus" = "#a7a7bd",
  "Epomophorus wahlbergi" = "#a7a7bd",
  "Hipposideros caffer/ruber" = "#a7a7bd",
  "Molossus sinaloe" = "#a7a7bd",
  "Mops plicatus" = "#a7a7bd",
  "Mops pumilus" = "#a7a7bd",
  "Nyctalus noctula" = "#a7a7bd",
  "Pipistrellus sp." = "#a7a7bd",
  "Rhinolophus lepidus" = "#a7a7bd",
  "Rhinolophus sinicus" = "#a7a7bd",
  "IAV" = "#70b844",
  "RABV" = "#ca4a9b",
  "EBOV" = "#946f1f",
  "MARV" = "#b8c3e4",
  "JEV" = "#66be80",
  "HIV-1" = "#771113",
  "MuLV" = "#d39eb1",
  "RVFV" = "#d39eb1",
  "SARS-CoV-2" = "#d39eb1",
  "SIVmac" = "#d39eb1",
  "NiV" = "#d39eb1",
  "VEEV" = "#d39eb1",
  "WNV" = "#d39eb1",
  "SLEV" = "#d39eb1",
  "BDBV" = "#d39eb1",
  "RESTV" = "#d39eb1",
  "SUDV" = "#d39eb1",
  "TAFV" = "#d39eb1",
  "HeV" = "#d39eb1",
  "EMCV" = "#d39eb1",
  "DENV-1" = "#d39eb1",
  "ZIKV" = "#d39eb1",
  "EBLV-1" = "#d39eb1",
  "LBV" = "#d39eb1",
  "MELV" = "#d39eb1",
  "MERS" = "#d39eb1",
  "NDV" = "#d39eb1",
  "PRV (Melaka)" = "#d39eb1",
  "SeV" = "#d39eb1",
  "African HeV (strain M74)" = "#d39eb1",
  "CHIKV" = "#d39eb1",
  "HSV-1" = "#d39eb1",
  "Henipavirus" = "#d39eb1",
  "IDV" = "#d39eb1",
  "Lyssavirus-1150" = "#d39eb1",
  "Lyssaviruses Yuli" = "#d39eb1",
  "Lyssaviruses-Stade" = "#d39eb1",
  "SOSV" = "#d39eb1",
  "TCRV" = "#d39eb1",
  "ARAV" = "#d39eb1",
  "Bat-CoV" = "#d39eb1",
  "Bunyavirus (Tahyna Virus)" = "#d39eb1",
  "CVB3 (Coxsackie B-3)" = "#d39eb1",
  "Cedar" = "#d39eb1",
  "DENV-2" = "#d39eb1",
  "DENV-4" = "#d39eb1",
  "EBLV-2" = "#d39eb1",
  "Kaeng Khoi virus" = "#d39eb1",
  "LLOV" = "#d39eb1",
  "Lyssavirus serotype 4" = "#d39eb1",
  "Lyssavirus serotypes 1 and 4" = "#d39eb1",
  "RABV Aravan" = "#d39eb1",
  "RABV Irkut" = "#d39eb1",
  "RABV Khujand" = "#d39eb1",
  "WCBV" = "#d39eb1",
  "YOKV" = "#d39eb1",
  "ZEBOV" = "#d39eb1",
  "bat adenovirus-TJM (BtAdV-TJM)" = "#d39eb1",
  "feline foamy virus" = "#d39eb1",
  "reovirus serotype 3" = "#d39eb1"
)

Chords_full<-chordDiagram(connections, grid.col = grid.col, annotationTrack = c("grid", "panel"),link.sort = TRUE, link.decreasing = TRUE)
Chords_full

connections_bdcl <- Full_crossings_2026%>% 
  filter(subject=="cells") %>%  
  group_by(
    Bat_names,
    Virus) %>%
  count(sort=TRUE) %>%
  ungroup()

Chords_bdcl<-chordDiagram(connections_bdcl, grid.col = grid.col, annotationTrack = c("grid", "panel"))
Chords_bdcl

connections_bat <- Full_crossings_2026 %>% 
  filter(subject=="bat") %>%  
  group_by(
    Bat_names,
    Virus) %>%
  count(sort=TRUE) %>%
  ungroup()

Chords_bats<-chordDiagram(connections_bat, grid.col = grid.col, annotationTrack = c("grid", "panel"))
Chords_bats

Chords_bats<-chordDiagram(connections_bat_fam_bat, grid.col = grid.col, annotationTrack = c("grid", "panel"))
Chords_bats
