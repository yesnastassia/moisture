library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggbiplot)
library(mvpart)



imagery_data = import_moist("imagery_data_all.csv")
field_data = read.csv("field_moisture_data.csv")
field_data = field_data[1:294, ]
texture_data = read.csv("Texture Calculation/texture_vals.csv")

# get the field data and the imagery data matched up in the same dataframe
df = make_df(imagery_data, field_data,texture_data)



## check for spatial autocorrelation
## calculate distance matrices for field moisture and x,y coordinates
distxy = dist(df[,c("X", "Y")], method = "euclidean")
distmoist = dist(df[,c("MoistureTop", "MoistureBottom", "MoistureLive")],
                 method = "euclidean")

mantel(ydis = distxy, xdis = distmoist, method = "pearson", permutations=99999,
       na.rm=TRUE)



## absent top or live bags cause a problem with NA values when calculating totals
df = mutate(df, MoistureLiveNoNA = case_when(
          is.na(MoistureLive) == TRUE ~ 0,
          is.na(MoistureLive) == FALSE ~ MoistureLive
          )
  )
df = mutate(df, WeightLiveNoNA = case_when(
          is.na(WeightLive) == TRUE ~ 0,
          is.na(WeightLive) == FALSE ~ WeightLive
          )
)
df = mutate(df, MoistureTopNoNA = case_when(
          is.na(MoistureTop) == TRUE ~ 0,
          is.na(MoistureTop) == FALSE ~ MoistureTop
          )
)
df = mutate(df, WeightTopNoNA = case_when(
          is.na(WeightTop) == TRUE ~ 0,
          is.na(WeightTop) == FALSE ~ WeightTop
          )
)

## calculate total moisture
df = mutate(df,TotalMoisture=(MoistureTopNoNA * WeightTopNoNA + MoistureBottom
                             * WeightBottom + MoistureLiveNoNA * WeightLiveNoNA)/
                            (WeightTopNoNA + WeightBottom + WeightLiveNoNA))

## calculate total weight
df = mutate(df, TotalWeight = WeightTopNoNA + WeightBottom + WeightLiveNoNA)

## make a column for moisture that is total for short grass and top for tall grass
df = mutate(df, typeMoisture = TotalMoisture)
for (n in 1:60){
  df[n,"typeMoisture"]=df[n,"MoistureTop"]
}

## make a column for moisture that is total-live
df = mutate(df, TotalMoisture_nolive = (MoistureTopNoNA * WeightTopNoNA + 
                                          MoistureBottom * WeightBottom) /
                                        (WeightTopNoNA + WeightBottom))

## make a column for moisture that is top+live
df = mutate(df, typeMoisture_live = (MoistureTopNoNA *WeightTopNoNA + 
                                       MoistureLiveNoNA * WeightLiveNoNA) /
                                    (WeightTopNoNA + WeightLiveNoNA))



## remove bad data
## note: plot 54 collected incorrectly
## plot 115 is collected incorrectly and not separated
df$typeMoisture[54] = NA
df$TotalMoisture[54] = NA
df$TotalMoisture_nolive[54] = NA
df$typeMoisture_live[54] = NA

## this one is entirely a bush
df$typeMoisture[8] = NA
df$TotalMoisture[8] = NA
df$TotalMoisture_nolive[8] = NA
df$typeMoisture_live[8] = NA



## try some ratios
df = mutate(df, NDWI = (NIR - SWIR) / (NIR + SWIR))
df = mutate(df, NDVI = (NIR - R) / (NIR + R))
df = mutate(df, VARI = (G - R) / (G + R - B))
df = mutate(df, NDGR = (G - R) / (G + R))



## some plots of moisture distribution
## a little look at variation in moisture
morning = df %>% subset(SamplePeriod == 1)
afternoon = df %>% subset(SamplePeriod == 2)

        
                 
## plot total moisture by sample period
layout(matrix(c(1), 1, 1))
transparentBlue = rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
transparentRed = rgb(255, 0, 0, max = 255, alpha = 125, names = "blue50")
hist1 = hist(morning$TotalMoisture, breaks = 20)
hist2 = hist(afternoon$TotalMoisture, breaks = 30)
plot(hist2, col = transparentRed)
plot(hist1, col = transparentBlue, add = TRUE)

## plot total moisture by grass type
df = mutate(df, GrassType = as.factor(GrassType))
ggplot(df, aes(x = TotalMoisture, fill = GrassType)) + 
  geom_histogram(position = "identity", alpha = .2, bins = 50) +
  xlab("Total Moisture (as proportion of dry weight)") + ylab("Count") +
  theme(text = element_text(size = 16))




## check for normality
shapiro.test(df$typeMoisture)
df = mutate(df, typeMoisture_transformed = log10(df$typeMoisture))

shapiro.test(df$TotalMoisture)
df = mutate(df, totalMoisture_transformed = log10(df$TotalMoisture))
shapiro.test(df$totalMoisture_transformed)

shapiro.test(df$TotalMoisture_nolive)
df = mutate(df, totalMoisture_nolive_transformed = log10(df$TotalMoisture_nolive))

shapiro.test(df$typeMoisture_live)
df = mutate(df, typeMoisture_live_transformed = log10(df$typeMoisture_live))

## exploratory plotting
p1 = ggplot(df, aes(x = NDWI, y = TotalMoisture)) + geom_point() + 
  theme(text = element_text(size=14))
p2 = ggplot(df, aes(x = VARI, y = TotalMoisture)) + geom_point()
p3 = ggplot(df, aes(x = NDVI, y = TotalMoisture)) + geom_point()
p4 = ggplot(df, aes(x = NDGR, y = TotalMoisture)) + geom_point()
p5 = ggplot(df, aes(x = B, y = TotalMoisture)) + geom_point()
p6 = ggplot(df, aes(x = G, y = TotalMoisture)) + geom_point()
p7 = ggplot(df, aes(x = R, y = TotalMoisture)) + geom_point()
p8 = ggplot(df, aes(x = NIR, y = TotalMoisture)) + geom_point()
p9 = ggplot(df, aes(x = SWIR, y = TotalMoisture)) + geom_point()

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3)



## plot the different moistures v NDVI for tall grass
p1 = ggplot(df[1:60,], aes(x = NDVI, y = TotalMoisture)) + geom_point() +
  theme(text = element_text(size = 16))
p2 = ggplot(df[1:60,], aes(x = NDVI, y = TotalMoisture_nolive)) + geom_point() +
  theme(text = element_text(size = 16))
p3 = ggplot(df[1:60,], aes(x = NDVI, y = typeMoisture)) + geom_point() +
  theme(text = element_text(size = 16))
p4 = ggplot(df[1:60,], aes(x = NDVI, y = typeMoisture_live)) + geom_point() +
  theme(text = element_text(size = 16))
grid.arrange(p1, p2, p3, p4, nrow = 2)

## correlations for top
cor(df[1:60,]$TotalMoisture, df[1:60,]$NDVI, use = "complete.obs")
cor(df[1:60,]$TotalMoisture_nolive, df[1:60,]$NDVI, use = "complete.obs")
cor(df[1:60,]$typeMoisture, df[1:60,]$NDVI, use = "complete.obs")
cor(df[1:60,]$typeMoisture_live, df[1:60,]$NDVI, use = "complete.obs")

## plot moisture with and without live for short grass
p1 = ggplot(df[61:120,], aes(x = NDVI, y = TotalMoisture)) + geom_point() +
  theme(text = element_text(size = 14))
p2 = ggplot(df[61:120,], aes(x = NDVI, y = TotalMoisture_nolive)) + geom_point() + 
  theme(text = element_text(size = 14))
grid.arrange(p1, p2, nrow = 1)

## correlations for bottom
cor(df[61:120,]$TotalMoisture, df[61:120,]$NDVI, use = "complete.obs")
cor(df[61:120,]$TotalMoisture_nolive, df[61:120,]$NDVI, use = "complete.obs")



## modeling with indices
fit1 = lm(TotalMoisture ~  NDVI + TotalWeight, data = df)
fit2 = lm(TotalMoisture ~  NDWI, data = df)
fit3 = lm(TotalMoisture ~  VARI, data = df)
fit4 = lm(TotalMoisture ~  NDGR, data = df)
fit5 = lm(TotalMoisture ~  NDVI + TotalWeight + GrassType, data = df)
AIC(fit1,fit2,fit3,fit4,fit5)
summary(fit1)
plot(fit1)

## next try PCA
df_wavelengths = subset(df, select = c(R, G, B, RE, NIR))
scaled_wavelengths = scale(df_wavelengths)
PCA = princomp(scaled_wavelengths, cor = FALSE, scores = TRUE)
PCA_coeff = PCA$loadings[,1:4]
ggbiplot(PCA, choices=c(2,4)) + geom_point() +theme(text = element_text(size=14))


## PCA again, with SWIR
df_wavelengths_SWIR = df %>% subset(select = c(R, G, B, RE, NIR, SWIR)) %>%
  filter(SWIR>0)
scaled_wavelengths_SWIR = scale(df_wavelengths_SWIR)
PCA_swir = princomp(scaled_wavelengths_SWIR, cor = FALSE, scores = TRUE)
PCA_coeff_swir=PCA_swir$loadings[,1:4]




## make PCs
## make sure to use standardized values
df = mutate(df, PC1 = PCA_coeff[1,1] * R_r + PCA_coeff[2,1] * G_r +
              PCA_coeff[3,1] * B_r + PCA_coeff[4,1] * RE_r + PCA_coeff[5,1] * NIR_r)
df = mutate(df, PC2 = PCA_coeff[1,2] * R_r + PCA_coeff[2,2] * G_r +
              PCA_coeff[3,2] * B_r + PCA_coeff[4,2] * RE_r + PCA_coeff[5,2] * NIR_r)
df = mutate(df, PC3 = PCA_coeff[1,3] * R_r + PCA_coeff[2,3] * G_r + PCA_coeff[3,3] *
              B_r + PCA_coeff[4,3] * RE_r + PCA_coeff[5,3] * NIR_r)
df = mutate(df, PC4 = PCA_coeff[1,4] * R_r + PCA_coeff[2,4] * G_r +
              PCA_coeff[3,4] * B_r + PCA_coeff[4,4] * RE_r+PCA_coeff[5,4] * NIR_r)



## classification tree (nope)
## try making classes for 0-.1, .1-.2 .2-.3, .3-.4, .4-.5, .6-.7 and .71+
df = df %>% mutate(class = case_when(TotalMoisture <= .1 ~ 1,
                                TotalMoisture > .1&TotalMoisture<.2 ~ 2,
                                TotalMoisture > .2&TotalMoisture<.3 ~ 3,
                                TotalMoisture > .3&TotalMoisture<.4 ~ 4,
                                TotalMoisture > .4&TotalMoisture<.5 ~ 5,
                                TotalMoisture > .5&TotalMoisture<.6 ~ 6,
                                TotalMoisture > .6&TotalMoisture<.7 ~ 7,
                                TotalMoisture > .7 ~ 8))
tree = mvpart(class ~ PC1 + PC2 + PC3 + PC4, method = "class",
              df, xv = "pick", xvmult = 100)




## best model and plot
fit_PCA<-lm(TotalMoisture ~ PC2 + PC4 + TotalWeight, data = df)
summary(fit_PCA)
layout(matrix(c(1,2,3,4),2,2))
plot(fit_PCA)

predicted = predict(fit_PCA)
actual = df %>% subset(is.na(TotalMoisture) == FALSE)
layout(matrix(c(1), 1, 1))
ggplot(actual, aes(predicted, TotalMoisture, color = GrassType)) + geom_point() +
  geom_abline() + xlab("Predicted Moisture") + ylab("Observed Moisture") + 
  theme(text = element_text(size = 14))
## collinearity of predictors
cor_matrix = cor(df[11:16])



## fit just each type separately? no
tallfit = lm(TotalMoisture ~ PC2 + PC4 + TotalWeight, data = df[1:60,])
summary(tallfit)

shortfit <- lm(TotalMoisture ~ PC2 + PC4 + TotalWeight, data = df[61:120,])
summary(shortfit)




