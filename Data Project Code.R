#Import Dataset I will be working with
library(readr)
battedBallData <- read_csv("Career Stuff/Rays R&D Project/battedBallData.csv")
View(battedBallData)
attach(battedBallData)

#Clean Data
delete.na <- function(DF, n=0) {
  battedBallData[rowSums(is.na(battedBallData)) <= n,]
}
battedBallData <- delete.na(battedBallData, 2)

#Predicting System A using System B
summary(lm(speed_A ~ speed_B + vangle_B))
summary(lm(vangle_A ~ speed_B + vangle_B))
#Test to see if r-squared improves if we find vangle_A first
summary(lm(speed_A ~ speed_B + vangle_B + vangle_A))
#Test to see if r-squared improves if we find speed_A first
summary(lm(vangle_A ~ speed_B + vangle_B + speed_A))

#Predicting System B using System A
summary(lm(speed_B ~ speed_A + vangle_A))
summary(lm(vangle_B ~ speed_A + vangle_A))
#Test to see if r-squared improves if we find vangle_B first
summary(lm(speed_B ~ speed_A + vangle_A + vangle_B))
#Test to see if r-squared improves if we find speed_B first
summary(lm(vangle_B ~ speed_A + vangle_A + speed_B))

#Models are better at predicting the missing vangle values first, so we will find vangle then speed with our found vangle
#Predicting vangle_B and replacing the NA values with the predictions
vangle_B_predictions <- predict(lm(vangle_B ~ speed_A + vangle_A,data = battedBallData, na.action = na.exclude), newdata = battedBallData)
battedBallData3 <- battedBallData
for (i in 1:length(battedBallData3$vangle_B)) if (is.na(battedBallData3$vangle_B[i])) battedBallData3$vangle_B[i]=vangle_B_predictions[i]
View(battedBallData3)
sum(is.na(battedBallData3$vangle_B)) #check to make sure there are no na values left

#Predicting speed_B and replacing the NA values
speed_B_predictions <- predict(lm(speed_B ~ speed_A + vangle_A + vangle_B,data = battedBallData3, na.action = na.exclude), newdata = battedBallData3)
for (i in 1:length(battedBallData3$speed_B)) if (is.na(battedBallData3$speed_B[i])) battedBallData3$speed_B[i]=speed_B_predictions[i]
View(battedBallData3)
sum(is.na(battedBallData3$speed_B))

#Predicting vangle_A and replacing the NA values
vangle_A_predictions <- predict(lm(vangle_A ~ speed_B + vangle_B,data = battedBallData3, na.action = na.exclude), newdata = battedBallData3)
for (i in 1:length(battedBallData3$vangle_A)) if (is.na(battedBallData3$vangle_A[i])) battedBallData3$vangle_A[i]=vangle_A_predictions[i]
View(battedBallData3)
sum(is.na(battedBallData3$vangle_A))

#Predicting speed_A and replacing the NA values
speed_A_predictions <- predict(lm(speed_A ~ speed_B + vangle_B + vangle_A,data = battedBallData3, na.action = na.exclude), newdata = battedBallData3)
for (i in 1:length(battedBallData3$speed_A)) if (is.na(battedBallData3$speed_A[i])) battedBallData3$speed_A[i]=speed_A_predictions[i]
View(battedBallData3)
sum(is.na(battedBallData3$speed_A))

#Calculation of the averages by batter and hit type
#See how many hits were recorded for each batter to see if some may be underrepresented
View(table(battedBallData3$batter))

#View avg speed-off-bat by batter and hit type
batter_hit <- paste(battedBallData3$batter,battedBallData3$hittype)
avg_bat_hittype <- aggregate(battedBallData3$speed_A, list(batter_hit), FUN = mean)
View(avg_bat_hittype)


#Create matrices of avg speed-off-bat by batter and hit type for both systems A and B
avg_speed_A <- tapply(battedBallData3$speed_A, list(battedBallData3$batter,battedBallData3$hittype), mean)
View(avg_speed_A)
avg_speed_B <- tapply(battedBallData3$speed_B, list(battedBallData3$batter,battedBallData3$hittype), mean)
View(avg_speed_B)

#Create matrix of the avg of each batters avg speed-off-bat by hit type
avg_hitSpeed <- (avg_speed_A+avg_speed_B)/2
View(avg_hitSpeed)

#Create final matrix listing each batters true avg speed-off-bat using a combination of systems A and B
avgHitSpeed <- rowMeans(avg_hitSpeed,na.rm=TRUE)
matrix1<-matrix(avgHitSpeed, ncol = 1)
colnames(matrix1)<- c('Avg True Speed-Off-Bat')
View(matrix1)
