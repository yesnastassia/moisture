library(vegan)
library(dplyr)

field_data=read.csv("field_moisture_data.csv")
imagery_data=import_moist("imagery_data_all.csv")
texture_data=read.csv("Texture Calculation/texture_vals.csv")
field_data=field_data[1:294, ]

##formatting:
##put them into the same data frame and match up AM/PM data with correct sample
##period, make only one row for each plot
df<-make_df_complete(imagery_data,field_data,texture_data)



##a look at the distribution
tall <- df %>% subset(GrassType==1,
                      select=c(MoistureTop,MoistureBottom,WeightTop,WeightBottom)) %>%
  mutate(MoistureTotal=MoistureTop)
short <- df %>% subset(GrassType==2,
                       select=c(MoistureTop,MoistureBottom,WeightTop,WeightBottom)) %>%
  mutate(MoistureTotal=(MoistureTop*WeightTop+MoistureBottom*WeightBottom)/(WeightTop+WeightBottom))


##plot total moisture by grass type
transparentBlue <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
transparentRed <- rgb(255, 0, 0, max = 255, alpha = 125, names = "blue50")
hist1<-hist(tall$MoistureTotal,breaks=20)
hist2<-hist(short$MoistureTotal, breaks=20)
plot(hist1,col=transparentRed, xlim=c(.07,.48))
plot(hist2,col=transparentBlue,add=TRUE)



##calculate distance matrices for field moisture and x,y coordinates
distxy<-dist(df[,c("X","Y")], method = "euclidean")
distmoist<-dist(df[,c("MoistureTop","MoistureBottom","MoistureLive")], method = "euclidean")

##check for spatial effects- r=.01 so no
mantel(ydis=distxy, xdis=distmoist, method="pearson", permutations=99999, na.rm=TRUE)



##next PCA
df_wavelengths<-subset(df, select=c(R,G,B,RE,NIR))
scaled_wavelengths <-scale(df_wavelengths)
PCA<-princomp(scaled_wavelengths, cor = FALSE, scores = TRUE)
layout(matrix(1))
plot(PCA)
biplot(PCA)
PCA_coeff=PCA$loadings[,1:4]


##PCA again, with SWIR
df_wavelengths_SWIR<- df %>% subset(select=c(R,G,B,RE,NIR,SWIR)) %>%
  filter(SWIR>0)
scaled_wavelengths_SWIR <-scale(df_wavelengths_SWIR)
PCA_swir<-princomp(scaled_wavelengths_SWIR, cor = FALSE, scores = TRUE)
PCA_coeff_swir=PCA_swir$loadings[,1:4]
