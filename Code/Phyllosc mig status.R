## Luke Ozsanlav-Harris

## Find out how mnay species of Phyllosc are migraotyr according to BirdLife

## Packages required
pacman::p_load(tidyverse, data.table)


## Read in the data
BL <- read_csv("Data/species-filter-results.csv")

## Seperate out the species name into the two seperate parts. 
BL <- separate(BL, `Scientific name`, into = c("Genus", "SpeciesName"), sep = " ")

## Now filter for Phylooscpus
Ph <- filter(BL, Genus == "Phylloscopus")

## Get table of migraotyer status 
table(Ph$`Migratory status`)
