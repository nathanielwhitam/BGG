#install.packages("xlsx")                                         # Install xlsx R package
#install.packages("stringi")
library("xlsx")
library("stringi")
BGGData<-xlsx::read.xlsx("BGG_Data_Set.xlsx",sheetIndex = 1)

Genres<-BGGData[["Domains"]]
Genres
GenresStr<-gsub(" ","",paste(Genres,collapse= ","))
GenresStr<-gsub("Games","",GenresStr)
GenresStr
Seperated<-unlist(strsplit(GenresStr,","))
Seperated
GenresDF<-as.data.frame(table(Seperated))
GenresDF
GenreNames<- unique(Seperated)
GenreColNames<- sapply(GenreNames,function(x)paste0("Genre_",x))
GenreColNames
MyData <-BGGData
MyData[,GenreColNames]<-FALSE
MyData$Domains[is.na(MyData$Domains)] <- "NA"

for(i in 1:length(GenreNames)){
  MyData[,GenreColNames[i]]<-ifelse(stri_detect(MyData$Domains,fixed =GenreNames[i]),TRUE,FALSE)
}



Mechanics<-BGGData[["Mechanics"]]
Mechanics
MechanicsStr<-gsub(" ","",paste(Mechanics,collapse= ","))
MechanicsStr
Seperated<-unlist(strsplit(MechanicsStr,","))
Seperated
table(Seperated)
MechanicsDF<-as.data.frame(table(Seperated)) ##ignores NA values
summary(MechanicsDF$Freq)
par(mfrow = c(2, 2))
hist(MechanicsDF$Freq)
hist(MechanicsDF$Freq[MechanicsDF$Freq<1000])
hist(MechanicsDF$Freq[MechanicsDF$Freq<100])
MechanicNames<- unique(Seperated)
MechanicColNames<- sapply(MechanicNames,function(x)paste0("Mechanic_",x))
MechanicColNames


MyData[,MechanicColNames]<-FALSE
MyData$Mechanics[is.na(MyData$Mechanics)] <- "NA"
for(i in 1:length(MechanicNames)){
  MyData[,MechanicColNames[i]]<-ifelse(stri_detect(gsub(" ","",MyData$Mechanics),fixed =MechanicNames[i]),TRUE,FALSE)
}

CountDf<-MyData[,MechanicColNames]
MyData[,"MechanicsSum"]<-rowSums(CountDf==TRUE)

saveRDS(MyData,file="MyBGG.rds")




