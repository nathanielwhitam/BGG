BGGDF <- readRDS("MyBGG.rds")
str(BGGDF)
BGGDF$Owned.Users[is.na(BGGDF$Owned.Users)]<-0
summary(BGGDF[,4:12])
for (i in 4:12){
  tempdf<-as.data.frame(unclass(summary(BGGDF[,i])))
  tempdf[nrow(tempdf) + 1,]<-var(BGGDF[,i])
  tempdf[nrow(tempdf) + 1,]<-sd(BGGDF[,i])
  if(i==4){
    summarydf <- data.frame(Min.Players=tempdf[,1], row.names=c('min','1st Qu.','Median','Mean','3rd Qu.','Max.','var','sd'))
  }
  else{
    summarydf[colnames(BGGDF)[i]] <- tempdf[,1]
  }
}


Rating <- BGGDF[["Rating.Average"]]
n= length(Rating)
par(mfrow = c(1, 1))
hist(Rating)

NumRatings <- BGGDF[["Users.Rated"]]
par(mfrow = c(1, 2))
hist(NumRatings)
hist(NumRatings[NumRatings<200])
length(NumRatings[NumRatings>200])

plot(NumRatings,Rating)
plot(NumRatings[NumRatings>10000],Rating[NumRatings>10000])

cormatrix<-as.data.frame(cor(BGGDF[,4:12]))


GenreCor<-t(cor(BGGDF[,15:23],  # Calculate correlations
    BGGDF$Rating.Average))
MechanicCor<-t(cor(BGGDF[,24:ncol(BGGDF)],  # Calculate correlations
                 BGGDF$Rating.Average))



CutDF<-BGGDF[BGGDF$Users.Rated>10000,]
CutDFcormatrix<-as.data.frame(cor(CutDF[,4:12]))
CutDFGenreCor<-t(cor(CutDF[,15:23],  # Calculate correlations
                     CutDF$Rating.Average))
CutDFMechanicCor<-t(cor(CutDF[,24:ncol(CutDF)],  # Calculate correlations
                        CutDF$Rating.Average))

tempdf<-as.data.frame(unclass(summary(CutDF$Rating.Average)))
tempdf[nrow(tempdf) + 1,]<-var(CutDF$Rating.Average)
tempdf[nrow(tempdf) + 1,]<-sd(CutDF$Rating.Average)
summarydf['10KRatings_Rating.Average'] <- tempdf[,1]


top500<-BGGDF[BGGDF$BGG.Rank<=500,]
top500cormatrix<-as.data.frame(cor(top500[,4:12]))
top500GenreCor<-t(cor(top500[,15:23],  # Calculate correlations
                      top500$Rating.Average))
top500MechanicCor<-t(cor(top500[,24:ncol(top500)],  # Calculate correlations
                         top500$Rating.Average))

tempdf<-as.data.frame(unclass(summary(top500$Rating.Average)))
tempdf[nrow(tempdf) + 1,]<-var(top500$Rating.Average)
tempdf[nrow(tempdf) + 1,]<-sd(top500$Rating.Average)
summarydf['top500_Rating.Average'] <- tempdf[,1]




summarydf

summary(MechanicCor[1,])
summary(CutDFMechanicCor[1,])
summary(top500MechanicCor[1,])
