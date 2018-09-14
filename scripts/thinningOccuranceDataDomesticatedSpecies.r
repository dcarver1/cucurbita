###
# Sample script for the domesitcated species 
###

library(devtools)
library(dbplyr)
install_github("DFJL/SamplingUtil")



setwd("H:\\cucurbita")
Cucurbita <- read.csv("analysisData\\Cucurbita_total_2018_8_31 - Cucurbita.csv")
View(Cucurbita)
#List of existing cwr species 
domestics <- c("Cucurbita_argyrosperma", 
         "Cucurbita_argyrosperma_subsp._argyrosperma",
         "Cucurbita_ficifolia",
         "Cucurbita_maxima",
         "Cucurbita_maxima_subsp._maxima",
         "Cucurbita_moschata",
         "Cucurbita_pepo",
         "Cucurbita_pepo_subsp._ovifera",
         "Cucurbita_pepo_subsp._ovifera_var._ovifera",
         "Cucurbita_pepo_subsp._pepo")


#total number of occurances 
startRow <- nrow(Cucurbita)
#remove features that do not have a lat long and are know as one of the 17 species of CWR 
### before I was gonig off of the wild/not wild catergory in improvements. This methods provides 1000 more entiries. 
cucurbita <- Cucurbita %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  group_by(Taxon_final) %>%
  filter(Taxon_final %in% domestics)
#number of occurances that are spatial and CWR 
endRow <- nrow(cucurbita)
print(paste("A total of ", startRow, " occurances were collected. Of which ", endRow, " contained the above assumptions" ))

# write out all the data into a single file
write.csv(cucurbita, file = "cucurbitaAllDomesticated.csv")

#generate a conut of all the species. 
counts <- cucurbita %>% 
  group_by(Taxon_final) %>%
  summarise(total = n()) %>%
  arrange(desc(total))
View(counts)


# loop through the list of names and generate csv based on the species 
for(species in domestics){
  tbl <- filter(cucurbita, Taxon_final %in% species)
  #Incoperating the filter by country code 
  count_occ<-nrow(tbl)
  
  countries<- unique(na.omit(tbl$country))
  p<-c()
  n<-c()
  x<-data.frame()
  y<-c()
  muestra<-list()
  
  
  for(i in 1:length(countries)){
    
    n[i]<-nrow(tbl[which(tbl$country==countries[i]),])
    p[i]<-n[i]/count_occ
    
  }
  if(count_occ>=2000){
    
    nsizeProp<-nstrata(n=2000,wh=p,method="proportional")
    smple<-list()
    for(i in 1:length(countries)){
      smple[[i]]<-sample(rownames(tbl[which(tbl$country==countries[i]),]), size=nsizeProp[i], replace=F)
      muestra[[i]]<-tbl[smple[[i]],]
      
    }
    
    muestra<- do.call(rbind, muestra)
    
    
  }else{
    
    muestra<-tbl
  }
  
  # leave this out for now for troubleshooting purposes 
  write.csv(muestra, file = paste("analysisData\\",as.character(species),".csv", sep = ""))
  print(paste("the original occurance count for", species, " was ",count_occ , " and the final count is ", nrow(muestra)))
} 
