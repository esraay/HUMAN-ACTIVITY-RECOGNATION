library(ggplot2)
tablo <- data.frame()
tabloo <- data.frame()
a = read.csv("sensor_data.csv")

ggplot(a, aes(x = a$Activity)) +
  geom_bar(fill = "steelblue") +
  scale_x_discrete(limits = c("STANDING", "SITTING", "LAYING", 
                              "WALKING", "DOWNSTAIRS", "UPSTAIRS"),
                   labels = c("STANDING", "SITTING", 
                              "LAYING", "WALKING", "DOWNSTAIRS", 
                              "UPSTAIRS"))
ggplot(a, aes(x = Activity, y = a$tBodyAccmaxX)) +
  geom_jitter(color="darkgreen") +
  scale_x_discrete(limits = c("STANDING", "SITTING", "LAYING", 
                              "WALKING", "DOWNSTAIRS", "UPSTAIRS"),
                   labels = c("STANDING", "SITTING", 
                              "LAYING", "WALKING", "DOWNSTAIRS", 
                              "UPSTAIRS"))
project <- function(){
  acc = acc[,3:5]
  gravity = gravity[,3:5]
  tablo = data.frame()
  normalizedgra= (gravity - min(gravity)) / (max(gravity) - min(gravity)) * (1-(-1)) - 1
  normalizedacc= (acc - min(acc))/ (max(acc) - min(acc)) * 2 - 1
  tBodyAccmaxX <- max(normalizedacc$X)
  tBodyAccCorXY <- cor(normalizedacc$X,normalizedacc$Y)
  tGravityAccmeanX <- mean(normalizedgra$X)
  tGravityAccmeanY <- mean(normalizedgra$Y)
  tGravityAccstdX <- sd(normalizedgra$X)
  tGravityAccmaxX <- max(normalizedgra$X)
  tGravityAccmaxY <- max(normalizedgra$Y)
  tGravityAccmaxZ <- max(normalizedgra$Z)
  tGravityAccminX <- min(normalizedgra$X)
  tGravityAccminY <- min(normalizedgra$Y)
  tablo <- cbind(tBodyAccmaxX,tBodyAccCorXY,tGravityAccmeanX,tGravityAccmeanY,tGravityAccstdX,tGravityAccmaxX,tGravityAccmaxY,tGravityAccmaxZ,tGravityAccminX,tGravityAccminY)
  return(tablo)
}

i = 1
while(i < 21){
  acc = read.csv(paste('Standing/Sensor Record data(',i,')/AccelerometerLinear.csv',sep=""))
  gravity = read.csv(paste('Standing/Sensor Record data(',i,')/Gravity.csv',sep=""))
  acc <- acc[1:128,]
  gravity <- gravity[1:128,]
  tablo = rbind(project(),tablo)
  i = i + 1
}
  tablo$Activity = "STANDING"
tablo2 = rbind(tablo2,tablo)

write.csv(tablo2, file = "Standing.csv",row.names = FALSE)

