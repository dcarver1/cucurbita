library(dplyr)

setwd("H:\\cucurbita")
Cucurbita <- read.csv("analysisData\\Cucurbita_total_2018_8_31 - Cucurbita.csv")

#List of existing cwr species 
CWR <- c("Cucurbita_argyrosperma_subsp._sororia",
         "Cucurbita_cordata",
         "Cucurbita_digitata", 
         "Cucurbita_ecuadorensis",
         "Cucurbita_foetidissima",
         "Cucurbita_lundelliana",
         "Cucurbita_maxima_subsp._andreana",
         "Cucurbita_okeechobeensis",
         "Cucurbita_okeechobeensis_subsp._martinezii",
         "Cucurbita_okeechobeensis_subsp._okeechobeensis",
         "Cucurbita_palmata",
         "Cucurbita_pedatifolia",
         "Cucurbita_pepo_subsp._fraterna",
         "Cucurbita_pepo_subsp._ovifera_var._ozarkana", 
         "Cucurbita_pepo_subsp._ovifera_var._texana",
         "Cucurbita_radicans",
         "Cucurbita_xscabridifolia")


#total number of occurances 
startRow <- nrow(Cucurbita)
#remove features that do not have a lat long and are know as one of the 17 species of CWR 
### before I was gonig off of the wild/not wild catergory in improvements. This methods provides 1000 more entiries. 
cucurbita <- Cucurbita %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  group_by(Taxon_final) %>%
  filter(Taxon_final %in% CWR)
#number of occurances that are spatial and CWR 
endRow <- nrow(cucurbita)
print(paste("A total of ", startRow, " occurances were collected. Of which ", endRow, " contained the above assumptions" ))

# write out all the data into a single file
write.csv(cucurbita, file = "cucurbitaAllWild.csv")

#generate a conut of all the species. 
counts <- cucurbita %>% 
  group_by(Taxon_final) %>%
  summarise(total = n()) %>%
  arrange(desc(total))
View(counts)

# loop through the list of names and generate csv based on the species 
for(i in CWR){
  tbl <- filter(cucurbita, Taxon_final %in% i)
  glimpse(tbl)
  file <- write.csv(tbl, file = paste("analysisData\\",as.character(i),".csv", sep = ""))
  print(file)
} 

#because mymaps can only handle 2000 occurances at once I'm spliting up the Cucurbita_foetidissima data into two csvs 
foetidissima <- filter(cucurbita, Taxon_final == "Cucurbita_foetidissima")
write.csv(slice(foetidissima, 1:2000), file = paste("analysisData\\Cucurbita_foetidissima1_2000.csv"))
write.csv(slice(foetidissima, 2001:2269), file = paste("analysisData\\Cucurbita_foetidissima2001_2269.csv"))





# cnrt+shirt+c to commit out large sections of code.
# # this worked for the three sets but we need a specific csv for each species 
# cucurbitaSet1 <- filter(cucurbita, Taxon_final %in% c("Cucurbita_foetidissima","Cucurbita_lundelliana"))
# cucurbitaSet2 <- filter(cucurbita, Taxon_final %in% c("Cucurbita_palmata","Cucurbita_argyrosperma_subsp._sororia", "Cucurbita_okeechobeensis_subsp._martinezii"))
# cucurbitaSet3 <- filter(cucurbita, !Taxon_final %in% c("Cucurbita_foetidissima","Cucurbita_lundelliana","Cucurbita_palmata","Cucurbita_argyrosperma_subsp._sororia",
#                                                        "Cucurbita_okeechobeensis_subsp._martinezii", "Cucurbita_maxima","Cucurbita_ficifolia", "Cucurbita_moschata",
#                                                        "Cucurbita_pepo", "Cucurbita_pepo_subsp._ovifera", "Cucurbita_pepo_subsp._ovifera_var._ovifera", 
#                                                        "Cucurbita_pepo_subsp._pepo", "Cucurbita_argyrosperma", "Cucurbita_argyrosperma_subsp._argyrosperma"))
# 
# glimpse(cucurbitaSet3)
# write.csv(cucurbitaSet1, file = "cucurbitaWildSet1.csv")
# write.csv(cucurbitaSet2, file = "cucurbitaWildSet2.csv")
# write.csv(cucurbitaSet3, file = "cucurbitaWildSet3.csv")


