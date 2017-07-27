day=read.csv("day.csv")
nrow(day)
ncol(day)
dim(day)
names(day)
day$raw.temp <- (day$temp*41)
head(day)

spring <- subset(day, season == 1)$raw.temp
sp.mean <- mean(spring) 
sp.median <- median(spring) 
sp.sd <- sd(spring) 

summer <- subset(day, season == 2)$raw.temp
su.mean <- mean(summer) 
su.median <- median(summer) 
su.sd <- sd(summer) 

fall <- subset(day, season == 3)$raw.temp
fa.mean <- mean(fall)
fa.median <-median(fall)
fa.sd <- sd(fall)

winter <- subset(day, season == 4)$raw.temp
wi.mean <- mean(winter)
wi.median <- median(winter)
wi.sd <- sd(winter)


hist(x = spring, 
     main = "Temperatures in Spring", 
     xlab = "Temperature in Celcius", 
     ylab = "Number of Days",
     xlim = c(0, 25),
     ylim = c(0, 45))

abline(v = sp.mean, lwd = 2, lty = 1, col = "red")  
text(x = 17, y = 35, 
     labels = paste("Mean = ", round(mean(spring),2), sep = ""), col="red" )

abline(v = sp.median, lwd = 2, lty = 3, col = "blue") 
text(x = 6, y = 35, 
     labels = paste("Median = ", round(median(spring),2), sep = ""), col="blue" )

hist(x = summer, 
     main = "Temperatures in Summer", 
     xlab = "Temperature in Celcius", 
     ylab = "Number of Days", 
     xlim = c(0, 35), ylim = c(0, 40)
)
abline(v = su.mean, lwd = 2, lty = 1, col = "red") 
text( x = 15, y = 40, 
      labels = paste("Mean = ", round(mean(summer),2), sep = ""),
      col = "red")

abline(v = su.median, lwd = 2, lty = 3, col = "blue") 
text(x = 31, y = 40, 
     labels = paste("Median = ", round(median(summer),2), sep = ""), col = "blue" )

hist(x = fall, 
     main = "Temperatures in Fall", 
     xlab = "Temperature in Celcius", 
     ylab = "Number of Days", 
     xlim = c(15, 40), ylim = c(0, 70)
)
abline(v = fa.mean, lwd = 2, lty = 1, col = "red") 
text(x = 24, y = 60, 
     labels = paste("Mean = ", round(mean(fall),3), sep = ""), col = "red" )

abline(v = fa.median, lwd = 2, lty = 3, col ="blue") 
text(x = 35, y = 60, 
     labels = paste("Median = ", round(median(fall),3), sep = ""), col ="blue" )

hist(x = winter, 
     main = "Temperatures in Winter", 
     xlab = "Temperature in Celcius", 
     ylab = "Number of Days", 
     xlim = c(0, 30), ylim = c(0, 40)
)

abline(v = wi.mean, lwd = 2, lty = 1, col = "red")  
text(x = 23, y = 40, 
     labels = paste("Mean = ", round(mean(winter),2), sep = ""), col = "red" )

abline(v = wi.median, lwd = 2, lty = 3, col ="blue") 
text(x = 10, y = 40, 
     labels = paste("Median = ", round(median(winter),2), sep = ""), col ="blue" )

is.integer(day)
is.null(day)

day$raw.atemp <-(day$atemp * 50)
day$raw.mean.temp.atemp <- (day$raw.temp + day$raw.atemp)/2
head(day)

cor.temp <- cor.test(x = day$raw.temp, y = day$cnt)

cor.temp

Temperature <- day$raw.temp
Amount.Rentals <- day$cnt

cor.atemp <- cor.test(x = day$raw.atemp,y = day$cnt)
cor.atemp

Feeled.Temperature <- day$raw.atemp
Amount.Rentals <- day$cnt

day$raw.mean.temp.atemp <-(day$raw.temp + day$raw.atemp)/2
cor.mean.temp.atemp <- cor.test(x = day$raw.mean.temp.atemp,y = day$cnt)
cor.mean.temp.atemp

Feeled.Raw.Temperature <- day$raw.mean.temp.atemp
Amount.Rentals <- day$cnt

par(mfrow=c(2,2))

plot(x = Temperature, y = Amount.Rentals, main = "Correlation", col = "red")
abline(lm(Amount.Rentals ~ Temperature), col = "blue")
legend("topleft", legend = paste("cor = ", round(cor(Temperature, Amount.Rentals), 2), sep = ""),lty = 1, col = "blue")

plot(x = Feeled.Temperature, y = Amount.Rentals, main = "Correlation", col = "blue")
abline(lm(Amount.Rentals ~ Feeled.Temperature), col = "red")
legend("topleft", legend = paste("cor = ", round(cor(Feeled.Temperature, Amount.Rentals), 2), sep = ""),lty = 1, col = "red")

plot(x = Feeled.Raw.Temperature, y = Amount.Rentals, main = "Correlation", col = "green")
abline(lm(Amount.Rentals ~ Feeled.Raw.Temperature), col = "orange")
legend("topleft", legend = paste("cor = ", round(cor(Temperature, Amount.Rentals), 2), sep = ""),lty = 1, col = "orange")


plot(x = 1, y = 1, xlab = "Temperature", ylab = "Amount of rentals", xlim = c(0, 40), ylim = c(0, 10000), main = "Three correlations combined")

points(Feeled.Raw.Temperature, Amount.Rentals, pch = 8, col = "green")
points(Temperature, Amount.Rentals, pch = 8, col = "red")
points(Feeled.Temperature, Amount.Rentals, pch = 8, col = "blue")

par(mfrow=c(1,1))


test.result.1 <- t.test(x = day$raw.temp, y = day$raw.atemp, alternative = "two.sided")
test.result.1

hist(day$raw.temp, yaxt = "n", xaxt = "n", xlab = "",
     ylab = "", main = "Two Sample t-test", xlim = c(5, 40), col = rgb(0, 0, 1, alpha = .1))
text(x = 13, y = 140, paste("Mean real Temp.\n", round(mean(day$raw.temp), 2), sep = ""), col = "blue")
abline(v = mean(day$raw.temp), lty = 1,
       col = rgb(0, 0, 1, alpha = 1), lwd = 4)

par(new = T)
hist(day$raw.atemp, yaxt = "n", xaxt = "n", xlab = "",
     ylab = "", main = "", xlim = c(5, 40), col = rgb(1, 0, 0, alpha = .1))

abline(v = mean(day$raw.atemp), lty = 1,
       col = rgb(1, 0, 0, alpha = 1), lwd = 4)
text(x= 32, y = 131, paste("Mean feeled Temp.\n", round(mean(day$raw.atemp), 2), sep = ""),  col = "red")
mtext(text = "Alternative Hypothesis is confirmed true difference in means is not equal to 0", line = 0, side = 3)

temp.spring <- subset(day, subset = season == "1")$raw.temp
atemp.spring <- subset(day, subset = season == "1")$raw.atemp
test.result.spring <- t.test(x = temp.spring, y = atemp.spring, alternative = "two.sided")
test.result.spring

temp.summer <- subset(day, subset = season == "2")$raw.temp
atemp.summer <- subset(day, subset = season == "2")$raw.atemp
test.result.summer <- t.test(x = temp.summer, y = atemp.summer, alternative = "two.sided")
test.result.summer

temp.fall <- subset(day, subset = season == "3")$raw.temp
atemp.fall <- subset(day, subset = season == "3")$raw.atemp
test.result.fall <- t.test(x = temp.fall, y = atemp.fall, alternative = "two.sided")
test.result.fall

temp.winter <- subset(day, subset = season == "4")$raw.temp
atemp.winter <- subset(day, subset = season == "4")$raw.atemp
test.result.winter <- t.test(x = temp.winter, y = atemp.winter, alternative = "two.sided")
test.result.winter

plot(x = 1, y = 1, xlab = "Temperature in Celcius", ylab = "Bike rentals", type = "n", main = "Association between temperature and bike rentals",
     xlim = c(0, 40), ylim = c(0, 7000))
min(day$raw.temp)
max(day$raw.temp)
min(day$casual)
min(day$registered)
max(day$casual)
max(day$registered)

day$raw.temp <- (day$temp*41)
points(day$raw.temp, day$casual, pch = 16, col = "red")
points(day$raw.temp, day$registered, pch = 16, col = "skyblue")

legend("topleft",legend = c("casual", "registered"), col = c("red","skyblue"), pch = c(16, 16), bg = "white")

cor.reg <- cor.test(x = day$raw.temp, y = day$registered)
cor.reg

cor.cas <- cor.test(x = day$raw.temp,
                    y = day$casual)
cor.cas

abline(lm(day$registered ~ day$raw.temp), lty = 6, col = "blue")

abline(lm(day$casual ~ day$raw.temp), lty = 6, col = "orange")

reg <- paste("cor = ", round(cor(day$registered, day$raw.temp), 2), sep = "")
cas <- paste("cor = ", round(cor(day$casual, day$raw.temp), 2), sep = "")

legend("left",legend = c(cas, reg) , col = c('orange', 'blue'),pch = c(16, 16), bg = "white")

max(day$casual)
max(day$registered)

lookup <- data.frame("numbers"=c("1","2","3","4"),
                     "weather"=c("nice","cloudy", "wet", "lousy")
)

day <- merge(x= day,
             y= lookup,
             by.x="weathersit",
             by.y="numbers",
)

head(day)

total.rentals.lm <- lm(cnt ~ holiday + weather, data = day)
summary (total.rentals.lm)

anv.weather <- anova (total.rentals.lm)
anv.weather

anv.weather$`F value`
anv.weather$`Pr(>F)`
weather.aov <- aov(cnt ~ weather, data = day)
summary(weather.aov)
TukeyHSD(weather.aov)
lookup.month<- data.frame("mnth" = c(1:12),
                          "mnth.name" = c("01Jan", "02Feb", "03March", "04April", "05May", "06June", "07July", "08Aug", "09Sept", "10Oct", "11Nov", "12Dec"), stringsAsFactors = FALSE)

day <- merge(x=day, y= lookup.month, by = 'mnth')


# Convert the nomalized windspeed and humidity
day$raw.windspeed <- (day$windspeed*67)
day$raw.hum <- (day$hum * 100)
head(day)

install.packages("dplyr")
library(dplyr)
require(dplyr)
month.agg <- day %>% group_by(mnth.name) %>% summarise(
  mean.temp = mean(raw.temp),
  mean.hum = mean(raw.hum),
  mean.windspeed = mean(raw.windspeed),
  mean.rentals = mean(cnt))

month.agg
par(mfrow=c(2,2))
barplot(height = month.agg$mean.rentals,
        names.arg = month.agg$mnth.name ,col = "red", main = "Mean rentals" )

barplot(height = month.agg$mean.windspeed,
        names.arg = month.agg$mnth.name,col = "blue", main = "Mean Windspeed (km/h)" )

barplot(height = month.agg$mean.hum,
        names.arg = month.agg$mnth.name,col = "green", main = "Mean Humidity" )


barplot(height = month.agg$mean.temp,
        names.arg = month.agg$mnth.name,col = "skyblue", main = "Mean Temperature" )

par(mfrow=c(1,1))
max(day$raw.temp)
biking.day <- function (temp.thresh, windspeed.thresh, weathersit.thresh)
{result <- with (day, raw.temp > temp.thresh & 
                   raw.windspeed < windspeed.thresh & 
                   weathersit < weathersit.thresh)

return(result)} 

mean(biking.day(5, 40, 3))

mean(biking.day(10, 20, 2))



