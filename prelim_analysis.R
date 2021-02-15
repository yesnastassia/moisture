library(vegan)
library(dplyr)

field_data=read.csv("field_moisture_data.csv")
imagery_data=import_moist("imagery_data.csv")
field_data=field_data[1:294, ]

##formatting:
##put them into the same data frame and match up AM/PM data with correct sample
##period, make only one row for each plot
df<-make_df_complete(imagery_data,field_data)


##a little look at variation in moisture
morning <- df %>% subset(SamplePeriod==1,
                  select=c(MoistureTop,MoistureBottom,WeightTop,WeightBottom)) %>%
                  mutate(MoistureTotal=(MoistureTop*WeightTop+MoistureBottom*WeightBottom)/(WeightTop+WeightBottom))
afternoon <- df %>% subset(SamplePeriod==2,
                    select=c(MoistureTop,MoistureBottom,WeightTop,WeightBottom)) %>%
                    mutate(MoistureTotal=(MoistureTop*WeightTop+MoistureBottom*WeightBottom)/(WeightTop+WeightBottom))

##plot total moisture by sample period
transparentBlue <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
transparentRed <- rgb(255, 0, 0, max = 255, alpha = 125, names = "blue50")
hist1<-hist(morning$MoistureTotal,breaks=20)
hist2<-hist(afternoon$MoistureTotal, breaks=20)
plot(hist2,col=transparentRed)
plot(hist1,col=transparentBlue,add=TRUE)


tall <- df %>% subset(GrassType==1,
              select=c(MoistureTop,MoistureBottom,WeightTop,WeightBottom)) %>%
              mutate(MoistureTotal=(MoistureTop*WeightTop+MoistureBottom*WeightBottom)/(WeightTop+WeightBottom))
short <- df %>% subset(GrassType==2,
              select=c(MoistureTop,MoistureBottom,WeightTop,WeightBottom)) %>%
              mutate(MoistureTotal=(MoistureTop*WeightTop+MoistureBottom*WeightBottom)/(WeightTop+WeightBottom))

##plot total moisture by grass type
hist1<-hist(tall$MoistureTotal,breaks=20)
hist2<-hist(short$MoistureTotal, breaks=20)
plot(hist1,col=transparentRed, xlim=c(.07,.48))
plot(hist2,col=transparentBlue,add=TRUE)


##plot top for tall versus total for tall
hist1<-hist(tall$MoistureTotal,breaks=20)
hist2<-hist(tall$MoistureTop, breaks=20)
plot(hist2,col=transparentRed)
plot(hist1,col=transparentBlue,add=TRUE)


##calculate distance matrices for field moisture and x,y coordinates
distxy<-dist(df[,c("X","Y")], method = "euclidean")
distmoist<-dist(df[,c("MoistureTop","MoistureBottom","MoistureLive")], method = "euclidean")

##check for spatial effects- r=.01 so no
mantel(xdis=distxy, ydis=distmoist, method="pearson", permutations=99999, na.rm=TRUE)

##check for normality, both top and live moisture fail this test
shapiro.test(df$MoistureBottom)
shapiro.test(df$MoistureTop)
shapiro.test(df$MoistureLive)

##Do I want to eliminate this outlier?
##df$MoistureTop[36]=NA
hist(df$MoistureTop, breaks=20)
shapiro.test(log10(df$MoistureTop))

hist(df$MoistureLive, breaks=20)
shapiro.test(log10(df$MoistureLive))


##check total moisture for normality -- it passes
Total <- df %>% subset(select=c(MoistureTop,MoistureBottom,WeightTop,WeightBottom)) %>%
                mutate(MoistureTotal=(MoistureTop*WeightTop+MoistureBottom*WeightBottom)/(WeightTop+WeightBottom))
shapiro.test(Total$MoistureTotal)

##next make some groups
