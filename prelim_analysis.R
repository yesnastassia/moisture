field_data=read.csv("field_moisture_data.csv")
imagery_data=import_moist("imagery_data.csv")
field_data=field_data[1:294, ]

##formatting:
##put them into the same data frame and match up AM/PM data with correct sample
##period, make only one row for each plot
df<-make_df_complete(imagery_data,field_data)


##calculate distance matrices for field moisture and x,y coordinates
distxy<-dist(df[,c("X","Y")], method = "euclidean")
distmoist<-dist(df[,c("MoistureTop","MoistureBottom")], method = "euclidean")

##mantel test
mantel(xdis=distxy, ydis=distmoist, method="pearson", permutations=999, na.rm=TRUE)
