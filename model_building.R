library(dplyr)
library(ggplot2)
##import
##note: plot 54 collected incorrectly
##plot 115 is total moisture and not separate, should not affect analysis
imagery_data<-import_moist("imagery_data_all.csv")
field_data=read.csv("field_moisture_data.csv")
field_data=field_data[1:294, ]
texture_data=read.csv("Texture Calculation/texture_vals.csv")

#get the field data and the imagery data matched up in the same dataframe
df<-make_df_complete(imagery_data,field_data,texture_data)


##check for spatial autocorrelation
##calculate distance matrices for field moisture and x,y coordinates
distxy<-dist(df[,c("X","Y")], method = "euclidean")
distmoist<-dist(df[,c("MoistureTop","MoistureBottom","MoistureLive")], method = "euclidean")

mantel(ydis=distxy, xdis=distmoist, method="pearson", permutations=99999, na.rm=TRUE)



##absent top or live bags cause a problem with NA values when calculating totals
##this needs to be done after any actual use of the separate moistures!!
df<-mutate(df, MoistureLiveNoZeroes = case_when(
          is.na(MoistureLive) == TRUE ~ 0,
          is.na(MoistureLive) == FALSE ~ MoistureLive
          )
  )
df<-mutate(df, WeightLiveNoZeroes = case_when(
          is.na(WeightLive) == TRUE ~ 0,
          is.na(WeightLive) == FALSE ~ WeightLive
          )
)
df<-mutate(df, MoistureTopNoZeroes = case_when(
          is.na(MoistureTop) == TRUE ~ 0,
          is.na(MoistureTop) == FALSE ~ MoistureTop
          )
)
df<-mutate(df, WeightTopNoZeroes = case_when(
          is.na(WeightTop) == TRUE ~ 0,
          is.na(WeightTop) == FALSE ~ WeightTop
          )
)


##calculate total moisture
df<-mutate(df,TotalMoisture=(MoistureTopNoZeroes*WeightTopNoZeroes+MoistureBottom*WeightBottom+MoistureLiveNoZeroes*WeightLiveNoZeroes)/(WeightTopNoZeroes+WeightBottom+WeightLiveNoZeroes))


##make a column for moisture that is total for short grass and top for tall grass
df<-mutate(df,typeMoisture=TotalMoisture)
for (n in 1:60){
  df[n,"typeMoisture"]=df[n,"MoistureTop"]
  #df[n,"typeMoisture"]=df[n,"MoistureTop"]*df[n,"WeightTop"]+df[n,"MoistureLive"]*df[n,"WeightLive"]
}

##remove bad data
df$typeMoisture[54]=NA


##try some ratios
df<-mutate(df,NDWI=(NIR-SWIR)/(NIR+SWIR))
df<-mutate(df,NDVI=(NIR-R)/(NIR+R))
df<-mutate(df,VARI=(G-R)/(G+R-B))
df<-mutate(df,MoistureTransformed=log10(TotalMoisture))


##some plots of moisture distribution
##a little look at variation in moisture
morning <- df %>% subset(SamplePeriod==1)
afternoon <- df %>% subset(SamplePeriod==2)

                          
##plot total moisture by sample period
layout(matrix(c(1),1,1))
transparentBlue <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
transparentRed <- rgb(255, 0, 0, max = 255, alpha = 125, names = "blue50")
hist1<-hist(morning$TotalMoisture,breaks=20)
hist2<-hist(afternoon$TotalMoisture, breaks=30)
plot(hist2,col=transparentRed)
plot(hist1,col=transparentBlue,add=TRUE)


tall <- df %>% subset(GrassType==1)
short <- df %>% subset(GrassType==2)

##plot total moisture by grass type
layout(matrix(c(1),1,1))
hist1<-hist(tall$TotalMoisture,breaks=30)
hist2<-hist(short$TotalMoisture, breaks=30)
plot(hist2,col=transparentRed, xlim=c(.08,.85))
plot(hist1,col=transparentBlue,add=TRUE)


##plot top for tall versus total for tall
layout(matrix(c(1),1,1))
hist1<-hist(tall$TotalMoisture,breaks=30)
hist2<-hist(tall$MoistureTop, breaks=20)
plot(hist2,col=transparentRed, xlim=c(.08,.85))
plot(hist1,col=transparentBlue,add=TRUE)


##exploratory plotting
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3))
plot(TotalMoisture~NDWI, data=df)
plot(TotalMoisture~VARI, data=df)
plot(TotalMoisture~NDVI, data=df)
plot(TotalMoisture~R, data=df)
plot(TotalMoisture~G, data=df)
plot(TotalMoisture~B, data=df)
plot(TotalMoisture~NIR, data=df)
plot(TotalMoisture~SWIR, data=df)


##check for outliers >3SD in reflectance data


##check for normality
shapiro.test(df$typeMoisture)
df<-mutate(df,typeMoisture_transformed=log10(df$typeMoisture))

shapiro.test(df$TotalMoisture)
df<-mutate(df,totalMoisture_transformed=log10(df$TotalMoisture))
shapiro.test(df$totalMoisture_transformed)

##modeling with indices
fit <- lm(totalMoisture_transformed ~  VARI+NDVI+GrassType, data=df)
summary(fit)
plot(fit)


##collinearity of predictors
cor_matrix <- cor(df[11:16])


##next try PCA
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


##classification tree
library(mvpart)
df<-mutate(df,GrassType=as.factor(GrassType))
tree<-mvpart(GrassType~R+G+B+RE+NIR+SWIR, df, method="class", xv="pick", xvmult=100)
##could be summarized as "tall if B<.1627 or (G>=.2137 and B<.1784)"


##try vars from PCA
##make sure to use standardized values
df<-mutate(df,PC1=PCA_coeff[1,1]*R_r+PCA_coeff[2,1]*G_r+PCA_coeff[3,1]*B_r+PCA_coeff[4,1]*RE_r+PCA_coeff[5,1]*NIR_r)
df<-mutate(df,PC2=PCA_coeff[1,2]*R_r+PCA_coeff[2,2]*G_r+PCA_coeff[3,2]*B_r+PCA_coeff[4,2]*RE_r+PCA_coeff[5,2]*NIR_r)
df<-mutate(df,PC3=PCA_coeff[1,3]*R_r+PCA_coeff[2,3]*G_r+PCA_coeff[3,3]*B_r+PCA_coeff[4,3]*RE_r+PCA_coeff[5,3]*NIR_r)
df<-mutate(df,PC4=PCA_coeff[1,4]*R_r+PCA_coeff[2,4]*G_r+PCA_coeff[3,4]*B_r+PCA_coeff[4,4]*RE_r+PCA_coeff[5,4]*NIR_r)
shapiro.test(df$PC1)
shapiro.test(df$PC3)


fit_PCA<-lm(totalMoisture_transformed~PC1+PC2, data=df)
summary(fit_PCA)



##try PCA including SWIR
##first get rid of null value plots
##not as good
df_SWIR<- df %>% subset(select=c(R,G,B,RE,NIR,SWIR)) %>%
  filter(abs(SWIR_r)>0)
cor(df_SWIR)


##fit just each type separately? why is the top so bad??
tallfit <- lm(totalMoisture_transformed~PC1+PC2, data=df[1:60,])
summary(tallfit)


shortfit <- lm(totalMoisture_transformed~PC1+PC2,data=df[61:120,])
summary(shortfit)



##can we predict grass type?
df<-mutate(df,GrassType=GrassType-1) ##now tall is 0 and short is 1
typefit <- glm(GrassType ~ PC1+PC2+PC3, data = df, family = "binomial")
summary(typefit)
with(typefit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



