#under construction extremely messy file to revisit

#import, trim file, convert 0-255 reflectance VALUES TO 0-1
imagery_data<-import_moist("imagery_data.csv")
field_data=read.csv("field_moisture_data.csv")
field_data=field_data[1:294, ]

#get the field data and the imagery data matched up in the same dataframe
##note! 115 actually has 1+2 types in same bag, labeled 2
    ##  54 is mixed by mistake
##remove 54 unless using total moisture, 115 using total no matter what
df<-make_df_complete(imagery_data,field_data)

df<-mutate(df,TotalMoisture=(MoistureTop*WeightTop+MoistureBottom*WeightBottom)/(WeightTop+WeightBottom))

##some don't have a top bag, and the total gets listed as NA so populate it with the bottom moisture
df$TotalMoisture[88]=df$MoistureBottom[88]
df$TotalMoisture[115]=df$MoistureBottom[115]

##make a column for moisture that is total for short grass and top for tall grass
df<-mutate(df,typeMoisture=(MoistureTop*WeightTop+MoistureBottom*WeightBottom)/(WeightTop+WeightBottom))
for (n in 1:60){
  n
  df[n,"typeMoisture"]=df[n,"MoistureTop"]
}
df$typeMoisture[54]=NA
df$typeMoisture[88]=df$MoistureBottom[88]
df$typeMoisture[115]=df$MoistureBottom[115]

##try some ratios
df<-mutate(df,ratioRG=R/G)
df<-mutate(df,ratioNIRG=NIR/G)
df<-mutate(df,ratioSWIRG=SWIR/G)
df<-mutate(df,NDVI=(NIR-R)/(NIR+R))
df<-mutate(df,MoistureTransformed=log10(TotalMoisture))


##exploratory plotting
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3))
plot(typeMoisture~ratioRG, data=df)
plot(typeMoisture~ratioNIRG, data=df)
plot(typeMoisture~ratioSWIRG, data=df)
plot(typeMoisture~NDVI, data=df)
plot(typeMoisture~R, data=df)
plot(typeMoisture~G, data=df)
plot(typeMoisture~B, data=df)
plot(typeMoisture~NIR, data=df)
plot(typeMoisture~SWIR, data=df)

##find best model
library(MASS)
fit <- lm(typeMoisture~ratioRG+NDVI+B+ratioNIRG+R+NIR+ratioSWIRG+G+SWIR,data=df)
step <- stepAIC(fit, direction="both")
step$anova


fit <- lm(typeMoisture ~ B + ratioNIRG, data=df)
summary(fit)
plot(fit)



##fit just each type separately? not particularly fruitful

fit <- lm(TopTransformed~ratioRG+NDVI+B+ratioNIRG+R+NIR+ratioSWIRG+G+SWIR,data=df[1:60,])
step <- stepAIC(fit, direction="both")
step$anova

tallfit <- lm(TopTransformed ~ R + ratioNIRG, data=df[1:60,])
summary(tallfit)
sqplot(tallfit)

fit <- lm(TotalMoisture~ratioRG+NDVI+B+ratioNIRG+R+NIR+ratioSWIRG+G+SWIR,data=df[61:120,])
step <- stepAIC(fit, direction="both")
step$anova

shortfit <- lm(TotalMoisture ~ NDVI + B + NIR + G, data=df[61:120,])
summary(shortfit)
sqplot(shortfit)

##what if we do the two times of day separately? not particularly fruitful
df_morning<-filter(df, SamplePeriod==1)
df_afternoon<-filter(df, SamplePeriod==2)

layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3))
plot(typeMoisture~ratioRG, data=df_morning)
plot(typeMoisture~ratioNIRG, data=df_morning)
plot(typeMoisture~ratioSWIRG, data=df_morning)
plot(typeMoisture~NDVI, data=df_morning)
plot(typeMoisture~R, data=df_morning)
plot(typeMoisture~G, data=df_morning)
plot(typeMoisture~B, data=df_morning)
plot(typeMoisture~NIR, data=df_morning)
plot(typeMoisture~SWIR, data=df_morning)


layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3))
plot(typeMoisture~ratioRG, data=df_afternoon)
plot(typeMoisture~ratioNIRG, data=df_afternoon)
plot(typeMoisture~ratioSWIRG, data=df_afternoon)
plot(typeMoisture~NDVI, data=df_afternoon)
plot(typeMoisture~R, data=df_afternoon)
plot(typeMoisture~G, data=df_afternoon)
plot(typeMoisture~B, data=df_afternoon)
plot(typeMoisture~NIR, data=df_afternoon)
plot(typeMoisture~SWIR, data=df_afternoon)

fit <- lm(typeMoisture ~ B+ratioSWIRG, data=df_morning)
summary(fit)



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