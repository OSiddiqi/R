str(CO2)
#Plant is an ordinal factor
mean_uptake <- mean(CO2$uptake)
#mean is 27.213
boxplot(CO2$uptake ~ CO2$Type)
quebec_CO2 <-subset(CO2, CO2$Type == "Quebec")
mississippi_CO2 <- subset(CO2, CO2$Type == "Mississippi")
mean_checker <-function(vec1a, vec2a) {
#  mean1 <- mean(vec1a)
#  return(vec1a)
#  mean2 <- mean(vec2a)
#  return(vec2a)
  if ((mean1 <- mean(vec1a)) < (mean2 <- mean(vec2a))) {
    print("Quebec is higher")
  } else {
    print("Mississippi is higher")
  }
}
vec1a <- colMeans(mississippi_CO2[c("uptake")])
vec2a <- colMeans(quebec_CO2[c("uptake")])