make_df_complete<-function(imagery_data,field_data){
  ##need to make a df with the top, bottom, and live moisture and weight for each
  ##plot as well as the relevant imagery information, calculate time since flight,
  ##also location
  
  #sample period 1 is AM, 2 is PM
  #sample type 1 is top, 2 is bottom, 3 is live
  #1-60 are tall and 61-120 are short
  
  df <- data.frame(Index=integer(),
                   MoistureTop=double(),
                   WeightTop=double(),
                   WeightBottom=double(),
                   MoistureBottom=double(),
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
                   X=integer(),
                   Y=integer()
  )
  
  ##turn field moisture into series of things with one entry/plot
  top_moisture=c()
  bottom_moisture=c()
  sample_period=c()
  X=c()
  Y=c()
  top_time_collected=c()
  bottom_time_collected=c()
  top_weight=replicate(120,0)
  bottom_weight=replicate(120,0)
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
  }
  
  
  ##put field moisture and corresponding imagery data in dataframe
  ##time for AM flight was 939
  ##time for PM flight was 1506
  for (n in 1:120){
    if (sample_period[n]==1){
      df[n,"Index"]=n
      df[n,"MoistureTop"]=top_moisture[n]
      df[n,"MoistureBottom"]=bottom_moisture[n]
      df[n,"WeightTop"]=top_weight[n]
      df[n,"WeightBottom"]=bottom_weight[n]
      df[n,"TimeSinceFlightTop"]=top_time_collected[n]-939
      df[n,"TimeSinceFlightBottom"]=bottom_time_collected[n]-939
      df[n,"SamplePeriod"]=sample_period[n]
      df[n,"X"]=X[n]
      df[n,"Y"]=Y[n]
      df[n,"R"]=imagery_data[n,"AM_R_M"]
      df[n,"G"]=imagery_data[n,"AM_G_M"]
      df[n,"B"]=imagery_data[n,"AM_B_M"]
      df[n,"RE"]=imagery_data[n,"AM_RE_M"]
      df[n,"NIR"]=imagery_data[n,"AM_NIR_M"]
      df[n,"SWIR"]=imagery_data[n,"AM_SWIR_M"]
    }
    if (sample_period[n]==2){
      df[n,"Index"]=n
      df[n,"MoistureTop"]=top_moisture[n]
      df[n,"MoistureBottom"]=bottom_moisture[n]
      df[n,"WeightTop"]=top_weight[n]
      df[n,"WeightBottom"]=bottom_weight[n]
      df[n,"TimeSinceFlightTop"]=top_time_collected[n]-1506
      df[n,"TimeSinceFlightBottom"]=bottom_time_collected[n]-1506
      df[n,"SamplePeriod"]=sample_period[n]
      df[n,"X"]=X[n]
      df[n,"Y"]=Y[n]
      df[n,"R"]=imagery_data[n,"PM_R_M"]
      df[n,"G"]=imagery_data[n,"PM_G_M"]
      df[n,"B"]=imagery_data[n,"PM_B_M"]
      df[n,"RE"]=imagery_data[n,"PM_RE_M"]
      df[n,"NIR"]=imagery_data[n,"PM_NIR_M"]
      df[n,"SWIR"]=imagery_data[n,"PM_SWIR_M"]
    }
    if (n<61){
      df[n,"GrassType"]=1 #tall
    }
    if (n>60){
      df[n,"GrassType"]=2 #short
    }
  }
  return(df)
}