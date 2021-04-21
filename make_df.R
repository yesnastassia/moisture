make_df<-function(imagery_data,field_data,texture_data){
  ##need to make a df with the top, bottom, and live moisture and weight for each
  ##plot as well as the relevant imagery information, calculate time since flight,
  ##also location
  
  #sample period 1 is AM, 2 is PM
  #sample type 1 is top, 2 is bottom, 3 is live
  #1-60 are tall and 61-120 are short
  
  library(vegan)
  
  df <- data.frame(Index=integer(),
                   MoistureTop=double(),
                   WeightTop=double(),
                   WeightBottom=double(),
                   MoistureBottom=double(),
                   MoistureLive=double(),
                   TimeSinceFlightTop=integer(),
                   TimeSinceFlightBottom=integer(),
                   GrassType=integer(),
                   SamplePeriod=integer(),
                   R=double(),
                   G=double(),
                   B=double(),
                   RE=double(),
                   NIR=double(),
                   SWIR=double(),
                   R_r=double(),
                   G_r=double(),
                   B_r=double(),
                   RE_r=double(),
                   NIR_r=double(),
                   SWIR_r=double(),
                   X=integer(),
                   Y=integer(),
                   TopTransformed=double(),
                   WeightLive=double(),
                   Texture=double(),
                   Texture_r=double()
  )
  
  ##turn field moisture into series of things with one entry/plot
  ##rewrite this to not use for loops, it's ugly
  top_moisture=c()
  bottom_moisture=c()
  live_moisture=c()
  sample_period=c()
  X=c()
  Y=c()
  top_time_collected=c()
  bottom_time_collected=c()
  top_weight=replicate(120,0)
  bottom_weight=replicate(120,0)
  live_weight=replicate(120,0)
  range=c(1:294)
  for (n in range){
    if (field_data$Sample.Type[n]==2){
      bottom_moisture[as.numeric(field_data[n,1])]=field_data$Fuel.Moisture[n]
      bottom_weight[as.numeric(field_data[n,1])]=field_data$Dry.Gross.Weight[n]-field_data$Dry.Container.Weight[n]
      sample_period[as.numeric(field_data[n,1])]=field_data$Sample.Period[n]
      X[as.numeric(field_data[n,1])]=field_data$X[n]
      Y[as.numeric(field_data[n,1])]=field_data$Y[n]
      bottom_time_collected[as.numeric(field_data[n,1])]=field_data$Time.Collected[n]
    }
    if (field_data$Sample.Type[n]==1){
      top_moisture[as.numeric(field_data[n,1])]=field_data$Fuel.Moisture[n]
      top_weight[as.numeric(field_data[n,1])]=field_data$Dry.Gross.Weight[n]-field_data$Dry.Container.Weight[n]
      top_time_collected[as.numeric(field_data[n,1])]=field_data$Time.Collected[n]
    }
    if (field_data$Sample.Type[n]==3){
      live_moisture[as.numeric(field_data[n,1])]=field_data$Fuel.Moisture[n]
      live_weight[as.numeric(field_data[n,1])]=field_data$Dry.Gross.Weight[n]-field_data$Dry.Container.Weight[n]
    }
  }
  
  
  
  
  
  ##put field moisture and corresponding imagery data in dataframe
  ##time for AM flight was 939
  ##time for PM flight was 1506
  for (n in 1:120){
    df[n,"Index"]=n
    df[n,"MoistureTop"]=top_moisture[n]
    df[n,"MoistureBottom"]=bottom_moisture[n]
    df[n,"MoistureLive"]=live_moisture[n]
    df[n,"WeightTop"]=top_weight[n]
    df[n,"WeightBottom"]=bottom_weight[n]
    df[n,"WeightLive"]=live_weight[n]
    df[n,"X"]=X[n]
    df[n,"Y"]=Y[n]
    df[n,"SamplePeriod"]=sample_period[n]
    if (sample_period[n]==1){
      df[n,"TimeSinceFlightTop"]=top_time_collected[n]-939
      df[n,"TimeSinceFlightBottom"]=bottom_time_collected[n]-939
      df[n,"R"]=imagery_data[n,"AM_R_M"]
      df[n,"G"]=imagery_data[n,"AM_G_M"]
      df[n,"B"]=imagery_data[n,"AM_B_M"]
      df[n,"RE"]=imagery_data[n,"AM_RE_M"]
      df[n,"NIR"]=imagery_data[n,"AM_NIR_M"]
      df[n,"SWIR"]=imagery_data[n,"AM_SWIR_M"]
      df[n,"R_SD"]=imagery_data[n,"AM_R_SD"]
      df[n,"G_SD"]=imagery_data[n,"AM_G_SD"]
      df[n,"B_SD"]=imagery_data[n,"AM_B_SD"]
      df[n,"RE_SD"]=imagery_data[n,"AM_RE_SD"]
      df[n,"NIR_SD"]=imagery_data[n,"AM_NIR_SD"]
      df[n,"SWIR_SD"]=imagery_data[n,"AM_SWIR_SD"]
      df[n,"Texture"]=texture_data[n,"texture_AM"]
      
      
    }
    if (sample_period[n]==2){
      df[n,"TimeSinceFlightTop"]=top_time_collected[n]-1506
      df[n,"TimeSinceFlightBottom"]=bottom_time_collected[n]-1506
      df[n,"R"]=imagery_data[n,"PM_R_M"]
      df[n,"G"]=imagery_data[n,"PM_G_M"]
      df[n,"B"]=imagery_data[n,"PM_B_M"]
      df[n,"RE"]=imagery_data[n,"PM_RE_M"]
      df[n,"NIR"]=imagery_data[n,"PM_NIR_M"]
      df[n,"SWIR"]=imagery_data[n,"PM_SWIR_M"]
      df[n,"R_SD"]=imagery_data[n,"PM_R_SD"]
      df[n,"G_SD"]=imagery_data[n,"PM_G_SD"]
      df[n,"B_SD"]=imagery_data[n,"PM_B_SD"]
      df[n,"RE_SD"]=imagery_data[n,"PM_RE_SD"]
      df[n,"NIR_SD"]=imagery_data[n,"PM_NIR_SD"]
      df[n,"SWIR_SD"]=imagery_data[n,"PM_SWIR_SD"]
      df[n,"Texture"]=texture_data[n,"texture_PM"]
      
    }
    if (n<61){
      df[n,"GrassType"]=1 #tall
    }
    if (n>60){
      df[n,"GrassType"]=2 #short
    }
  }
  
  ##need to scale for PCA
  df[17:22] <-scale(df[,11:16])
  df[28] <-scale(df[,27])
  ##df[,17:22]<-decostand(x=df[,16:21], method="total", MARGIN=2, na.rm = FALSE)
  ##df[,26:31]<-decostand(x=df[,17:22], method="total", MARGIN=2, na.rm = TRUE)
  
  return(df)
}


