import_moist<-function(imagery_data_path){
  imagery_data=read.csv(imagery_data_path)
  imagery_data["PM_RE_M"] = imagery_data["PM_RE_M"] / 255
  imagery_data["PM_RE_SD"] = imagery_data["PM_RE_SD"] / 255
  imagery_data["PM_NIR_M"] = imagery_data["PM_NIR_M"] / 255
  imagery_data["PM_NIR_SD"] = imagery_data["PM_NIR_SD"] / 255
  imagery_data["PM_R_M"] = imagery_data["PM_R_M"] / 255
  imagery_data["PM_R_SD"] = imagery_data["PM_R_SD"] / 255
  imagery_data["PM_G_M"] = imagery_data["PM_G_M"] / 255
  imagery_data["PM_G_SD"] = imagery_data["PM_G_SD"] / 255
  imagery_data["PM_B_M"] = imagery_data["PM_B_M"] / 255
  imagery_data["PM_B_SD"] = imagery_data["PM_B_SD"] / 255
  imagery_data["PM_SWIR_M"] = imagery_data["PM_SWIR_M"] / 255
  imagery_data["PM_SWIR_SD"] = imagery_data["PM_SWIR_SD"] / 255
  imagery_data["AM_RE_M"] = imagery_data["AM_RE_M"] / 255
  imagery_data["AM_RE_SD"] = imagery_data["AM_RE_SD"] / 255
  imagery_data["AM_NIR_M"] = imagery_data["AM_NIR_M"] / 255
  imagery_data["AM_NIR_SD"] = imagery_data["AM_NIR_SD"] / 255
  imagery_data["AM_R_M"] = imagery_data["AM_R_M"] / 255
  imagery_data["AM_R_SD"] = imagery_data["AM_R_SD"] / 255
  imagery_data["AM_G_M"] = imagery_data["AM_G_M"] / 255
  imagery_data["AM_G_SD"] = imagery_data["AM_G_SD"] / 255
  imagery_data["AM_B_M"] = imagery_data["AM_B_M"] / 255
  imagery_data["AM_B_SD"] = imagery_data["AM_B_SD"] / 255
  imagery_data["AM_SWIR_M"] = imagery_data["AM_SWIR_M"] / 255
  imagery_data["AM_SWIR_SD"] = imagery_data["AM_SWIR_SD"] / 255
  return(imagery_data)
}
