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
mantel(ydis=distxy, xdis=distmoist, method="pearson", permutations=99999, na.rm=TRUE)

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


# ##decision trees? experiment
# library(rpart)
# fit1=rpart(Moisture~R+G+RE+NIR+SWIR+TimeOfDay, data=df, method="anova") 
# printcp(fit1) # display the resultsin
# plotcp(fit1) # visualize cross-validation results
# summary(fit1) # detailed summary of splits
# 
# # plot tree
# plot(fit1, uniform=TRUE,
#      main="Classification Tree for Moisture")
# text(fit1, use.n=TRUE, all=TRUE, cex=.8)
# 
# ##other library
# library(randomForest)
# n1=c(1,3,17,22,34,40,61,67,88,91,92,110)
# n2=c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,41,42,43,44,45,
#      46,47,48,49,50,51,52,53,54,55,56,57,68,59,60,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,
#      85,86,87,89,90,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,111,112,113,114,115,116,117,118,119,120)
# df1=df[n1, ]
# df2=df[n2, ]
# 
# fit2 <- randomForest(
#   Moisture ~ R+G+RE+NIR+TimeOfDay,
#   data=df2
# )
# 
# mtry <- tuneRF(df2[4:9],df2$Moisture, ntreeTry=500,
#                stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
# best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
# 
# 
# fit2 <- randomForest(
#   Moisture ~ R+G+RE+NIR+TimeOfDay,
#   data=df2, mtry=best.m
# )
# 
# print(fit2) # view results
# plot(fit2)


##try PCA including SWIR
##first get rid of null value plots
##not as good
# df_SWIR<- df %>% subset(select=c(R_r,G_r,B_r,RE_r,NIR_r,SWIR_r,TimeSinceFlightTop,typeMoisture,GrassType)) %>%
#   filter(abs(SWIR_r)>0)
# df<-mutate(df,PC1_s=PCA_coeff_swir[1,1]*R_r+PCA_coeff_swir[2,1]*G_r+PCA_coeff_swir[3,1]*B_r+PCA_coeff_swir[4,1]*RE_r+PCA_coeff_swir[5,1]*NIR_r+PCA_coeff_swir[6,1]*SWIR_r)
# df<-mutate(df,PC2_s=PCA_coeff_swir[1,2]*R_r+PCA_coeff_swir[2,2]*G_r+PCA_coeff_swir[3,2]*B_r+PCA_coeff_swir[4,2]*RE_r+PCA_coeff_swir[5,2]*NIR_r+PCA_coeff_swir[6,2]*SWIR_r)
# df<-mutate(df,PC3_s=PCA_coeff_swir[1,3]*R_r+PCA_coeff_swir[2,3]*G_r+PCA_coeff_swir[3,3]*B_r+PCA_coeff_swir[4,3]*RE_r+PCA_coeff_swir[5,3]*NIR_r+PCA_coeff_swir[6,3]*SWIR_r)
# df<-mutate(df,PC4_s=PCA_coeff_swir[1,4]*R_r+PCA_coeff_swir[2,4]*G_r+PCA_coeff_swir[3,4]*B_r+PCA_coeff_swir[4,4]*RE_r+PCA_coeff_swir[5,4]*NIR_r+PCA_coeff_swir[6,4]*SWIR_r)
# 
# 
# fit_PCA_swir<-lm(typeMoisture_transformed~PC1_s +PC2_s, data=df)
# summary(fit_PCA_swir)