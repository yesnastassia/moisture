#under construction extremely messy file to revisit

#import, trim file, convert 0-255 reflectance VALUES TO 0-1
imagery_data<-import_moist("imagery_data.csv")
field_data=read.csv("field_moisture_data.csv")
field_data=field_data[1:294, ]

#get the field data and the imagery data matched up in the same dataframe

##note! 115 actually has 1+2 types in same bag, labeled 2
    ##  54 is mixed by mistake

field_moisture=c()
type=c()
grass_type=c()
weight=replicate(120,0)
count=0;
range=c(1:294)
for (n in range){
  if (as.numeric(field_data[n,1])<61){ #it's tall
    if (field_data[n,"Sample.Type"]==1) { #use tops in this case
      field_moisture[as.numeric(field_data[n,1])]=field_data[n,"Fuel.Moisture"]
      type[as.numeric(field_data[n,1])]=field_data[n,"Sample.Period"]
      weight[as.numeric(field_data[n,1])]=field_data[n,"Wet.Gross.Weight"]-field_data[n,"Wet.Container.Weight"]
      grass_type[as.numeric(field_data[n,1])]=1
    }
  }
  if ((as.numeric(field_data[n,1])>60)&field_data[n,"Sample.Type"]!=3){ #it's short (and not a live bag)
    if (weight[as.numeric(field_data[n,1])]==0) { #use tops+bottoms in this case, if this is the first one
      field_moisture[as.numeric(field_data[n,1])]=field_data[n,"Fuel.Moisture"]
      type[as.numeric(field_data[n,1])]=field_data[n,"Sample.Period"]
      weight[as.numeric(field_data[n,1])]=field_data[n,"Wet.Gross.Weight"]-field_data[n,"Wet.Container.Weight"]
      grass_type[as.numeric(field_data[n,1])]=2
    }
    if (weight[as.numeric(field_data[n,1])]!=0) { #if a second bag exists
      count=count+1
      field_moisture[as.numeric(field_data[n,1])]=(weight[as.numeric(field_data[n,1])]*field_moisture[as.numeric(field_data[n,1])]+field_data[n,'Fuel.Moisture']*(field_data[n,"Wet.Gross.Weight"]-field_data[n,"Wet.Container.Weight"]))/(field_data[n,"Wet.Gross.Weight"]-field_data[n,"Wet.Container.Weight"]+weight[as.numeric(field_data[n,1])])
    }
  }
}


##then add it to a dataframe with just the imagery values from the same time of day the fields data was collected
df <- data.frame(Index=integer(),
                 Moisture=double(),
                 TimeOfDay=integer(),
                 GrassType=integer(),
                 R=double(),
                 G=double(),
                 B=double(),
                 RE=double(),
                 NIR=double(),
                 SWIR=double(),
                 ratioRG=double(),
                 ratioNIRG=double(),
                 ratioSWIRG=double(),
                 NDVI=double()
                 )
for (n in 1:120){
  if (type[n]==1){
    df[n,"Index"]=n
    df[n,"Moisture"]=field_moisture[n]
    df[n,"TimeOfDay"]=type[n]
    df[n,"GrassType"]=grass_type[n]
    df[n,"R"]=imagery_data[n,"AM_R_M"]
    df[n,"G"]=imagery_data[n,"AM_G_M"]
    df[n,"B"]=imagery_data[n,"AM_B_M"]
    df[n,"RE"]=imagery_data[n,"AM_RE_M"]
    df[n,"NIR"]=imagery_data[n,"AM_NIR_M"]
    df[n,"SWIR"]=imagery_data[n,"AM_SWIR_M"]
  }
  if (type[n]==2){
    df[n,"Index"]=n
    df[n,"Moisture"]=field_moisture[n]
    df[n,"TimeOfDay"]=type[n]
    df[n,"GrassType"]=grass_type[n]
    df[n,"R"]=imagery_data[n,"PM_R_M"]
    df[n,"G"]=imagery_data[n,"PM_G_M"]
    df[n,"B"]=imagery_data[n,"PM_B_M"]
    df[n,"RE"]=imagery_data[n,"PM_RE_M"]
    df[n,"NIR"]=imagery_data[n,"PM_NIR_M"]
    df[n,"SWIR"]=imagery_data[n,"PM_SWIR_M"]
  }
}

df["ratioRG"]=df["R"]/df["G"]
df["ratioNIRG"]=df["NIR"]/df["G"]
df["ratioSWIRG"]=df["SWIR"]/df["G"]
df["NDVI"]=(df["NIR"]-df["R"])/(df["NIR"]+df["R"])


##exploratory plotting
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3))
plot(Moisture~ratioRG, data=df)
plot(Moisture~ratioNIRG, data=df)
plot(Moisture~ratioSWIRG, data=df)
plot(Moisture~NDVI, data=df)
plot(Moisture~R, data=df)
plot(Moisture~G, data=df)
plot(Moisture~B, data=df)
plot(Moisture~NIR, data=df)
plot(Moisture~SWIR, data=df)

layout(matrix(c(1,2,3,4),2,2))
fit <- lm(Moisture ~ R + B + GrassType, data=df)
summary(fit)
plot(fit)

df_nooutliers<-df[-c(36,54,84),]
fitno <- lm(Moisture ~ R + B + GrassType, data=df_nooutliers)
summary(fitno)
plot(fitno)

##fit just each type separately?
tallfit <- lm(Moisture ~ B + TimeOfDay, data=df[1:60,])
summary(tallfit)
sqplot(tallfit)

shortfit <- lm(Moisture ~ B + TimeOfDay, data=df[61:120,])
summary(shortfit)
plot(shortfit)



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