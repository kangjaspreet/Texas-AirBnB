library(geoR)
library(sp)
library(gstat)
library(maps)
library(ggplot2)
library(lattice)

### This geospatial project is based on a AirBnb dataset that looks at average booking costs per night
### on AirBnb in Texas



air <- read.csv("C:/Users/Jaspo/Documents/geostats/Air.csv", header = T)

### First thing we want to check are amount of NA values within each column

colSums(is.na(air))

### Looking at structure of data to see which variables are categorical, and which are numeric

str(air)

# Need to do the following conversions:
# average_rate(Factor) -> numeric
# bedrooms_count(Factor) -> numeric


levels(air$bedrooms_count)
table(air$bedrooms_count)
air$bedrooms_count <- as.character(air$bedrooms_count)
air$bedrooms_count[air$bedrooms_count == "Studio"] <- "0"
air$bedrooms_count[air$bedrooms_count == ""] <- "NA"
air$bedrooms_count <- as.factor(air$bedrooms_count)
table(air$bedrooms_count)

levels(air$bedrooms_count) <- c("0", "1", "10", "11", "13", "2", "3", "4", "5", "6", "7", "8", "9", "NA")
air$bedrooms_count <- as.numeric(levels(air$bedrooms_count)[air$bedrooms_count])
# Bedroom_count has successfully been converted to numeric
table(air$bedrooms_count)

# Now we focus on changing average_rate from factor to numeriv. The problem here are the dollar signs.
# We use gsub in conjuction with as.numeric to get rid of those dollar signs and convert to numeric.

air$average_rate_per_night <- as.numeric(gsub("\\$", "", air$average_rate_per_night))
str(air)

colSums(is.na(air))
index <- which(is.na(air$latitude)) # Checking which observations for latitude are NA
air <- air[-index,] # Getting rid of NA rows for latitude
colSums(is.na(air)) # No more NA values for latitude, longtude, and average.. Still 3 NA values for bedrooms_count. 
# Should I get rid of these observations as well since we have so many, or try to predict them? 
index <- which(is.na(air$bedrooms_count))
air <- air[-index, ]
colSums(is.na(air))
### Data cleansing done!

### Part 2: Spatial Statistics


air_geo <- as.geodata(air[,c(8,7,2)])

### Addressing duplicate coordinates. Will take mean of the duplicate coordinates

d <- data.frame(dup.coords(air_geo))
ind <- as.integer(row.names(d))
d <- data.frame(d, "index" = ind)

length(unique(d$longitude)) # There are 4702 duplicates
length(unique(d$latitude))

length(unique(air$longitude))
length(unique(air$latitude))


test <- data.frame("x" = c(1,2,3,4,5,4,3,2,1))
length(unique(test$x)) # Comes out to 5. So what's happening is it's getting rid of duplicate,
# but not original
duplicated(test$x)
# We can find mean of duplicate coordinates. Or we can get rid of duplicate points, and keep
# original. So limiting AirBnB booking to one intance of one house instead of multiple instances
# of one house
dup_ind <- which(duplicated(air$latitude) == TRUE)
air_dup_removed <- air[-dup_ind, ] # duplicates removed in original dataset
hist(log(air_dup_removed$average_rate_per_night))


### Creating a linear model to see which predictors are significant

model <- lm(log(average_rate_per_night)~bedrooms_count+date_of_listing, data=air_dup_removed)
summary(model)

### Bedrooms is significant


plot(exp(model$fitted.values), air_dup_removed$average_rate_per_night, xlim=c(0,5000), ylim=c(0,5000))

air_dup_removed2 <- air_dup_removed

### Our h-scatterplot

coordinates(air_dup_removed2) <- ~longitude+latitude #Convert the data into spatial data 
hscat(average_rate_per_night~1, air_dup_removed2[1:1000,], c(0,1)) #This will produce the z(s) against z(s+1) plot. 
hscat(average_rate_per_night~1, air_dup_removed2[1:1000,], c(0,1,1.5, 2,3)) #This will produce 4 different #h-scatterplots with h=1, 2^0.5, 2, 3.



air_geo_removed <- as.geodata(air_dup_removed[,c(8,7,2)])
dup.coords(air_geo_removed) # successfully removed duplicate coordinates

new <- data.frame("x" = air_dup_removed[,8], "y" = air_dup_removed[,7], "bedrooms" = air_dup_removed$bedrooms_count, "date" = air_dup_removed$date_of_listing, "data" = air_dup_removed[,2])
# air_geo_removed1 <- as.geodata(air_dup_removed1)
summary(new)
index <- which(new$data >= 1000) # Remmoving outliers (airbnb prices >= 1000)
new1 <- new[-index,]

summary(new)
set.seed(1)
split <- sample(1:11162, 2500) # My computer kept freezing up with this large of a dataset when performing
# kriging. I tried reducing it to 7500, then 5000, then 3500.. Still froze up. Reduced to 2500

new2 <- new1[split,]
# Perform universal and ordinary kriging, and compare the two. Do cross vaildation (too many points), so
# instead split the data into training and testing. Compare by looking at PRESS of both

# There are 2500 observations. We split the data 70:30
2500*0.70 # 1750 observations will be in training
2500-1750 # 750 will be in testing (cross-validation)

set.seed(1)
train <- sample(1:2500, 1750)
model_train <- new2[train,]
model_test <- new2[-train,]

q <- map("county", "texas")
map(q)
points(model_train$x, model_train$y, cex=log(model_train$data)/mean(log(model_train$data)), col="red")



### Variogram Modeling

# omnidirectional variogram
g <- gstat(id="log_data", formula = log(data)~model_train$bedrooms, locations = ~x+y, data = model_train)
q <- variogram(g)
plot(q)

# variogram in all directions
v <- variogram(g, alpha=c(0,45,90,135)) 
plot(v)

# Fitting variogram model 
fit_var <- vgm(0.125,"Sph",1.0,0.30) 
plot(q, fit_var)

# Using Gstat, plot the variogram and fit a variogram model using: 
#1) Default weights
#2) Cressie's weights
#3) npairs
#4) OLS

v.fit1 <- fit.variogram(q, vgm(0.125,"Sph",1.0,0.30), fit.method=1) # npairs
v.fit2 <- fit.variogram(q, vgm(0.125,"Sph",1.0,0.30), fit.method=2) # Cressie's
v.fit6 <- fit.variogram(q, vgm(0.125,"Sph",1.0,0.30), fit.method=6) # OLS
v.fit7 <- fit.variogram(q, vgm(0.125,"Sph",1.0,0.30), fit.method=7) # default

# par(mfrow=c(1,1))
plot(q, v.fit1)
plot(q, v.fit2)
plot(q, v.fit6)
plot(q, v.fit7)

# Ordinary Kriging using Cressie's weights
pred <- krige(id="log_data", log(data)~1, locations=~x+y, model=v.fit2, data=model_train, newdata=model_test)

difference <- model_test$data - exp(pred$log_data.pred)
summary(difference)
cbind(model_test$data[1:100], exp(pred$log_data.pred[1:100]))

plot(pred$log_data.pred,log(model_test$data), xlab="Predicted Values", ylab="Observed Values")
cor(pred$log_data.pred, log(model_test$data)) 

# difference
press <- sum((log(model_test$data) - pred$log_data.pred)^2)
press # PRESS for ordinary kriging is 414.2102

# Universal kriging
pred_uk <- krige(id="log_data", log(data)~bedrooms, locations=~x+y, model=v.fit2, data=model_train, newdata=model_test)

difference <- model_test$data - exp(pred_uk$log_data.pred)
summary(difference)
cbind(model_test$data[1:100], exp(pred_uk$log_data.pred[1:100]))

plot(pred_uk$log_data.pred,log(model_test$data), xlab="Predicted Values", ylab="Observed Values")
cor(pred_uk$log_data.pred, log(model_test$data)) 

# difference
press <- sum((log(model_test$data) - pred_uk$log_data.pred)^2)
press # PRESS for universal kriging is 238.9227

### Universal kriging performs much better, and the correlation jumps 20% to 0.77.

### Producing a raster map of predicted values
x.range <- as.integer(range(model_test[,1]))
y.range <- as.integer(range(model_test[,2]))

cbind(pred_uk$log_data.pred[1:10], pred_uk$log_data.var[1:10])

# Raster map of predicted values
qqq <- matrix(pred_uk$log_data.pred, length(seq(from=x.range[1], to=x.range[2], by=1)), length(seq(from=y.range[1], to=y.range[2], by=1)))
image(seq(from=x.range[1], to=x.range[2], by=1), seq(from=y.range[1],to=y.range[2], by=1), qqq, xlab="West to East",ylab="South to North", main="Raster map of the predicted values")
contour(seq(from=x.range[1], to=x.range[2], by=1), 
        seq(from=y.range[1],to=y.range[2], by=1), qqq, add=TRUE, col="black", labcex=1)

# Variances raster map
qqq1 <- matrix(pred_uk$log_data.var, length(seq(from=x.range[1], to=x.range[2], by=1)), length(seq(from=y.range[1], to=y.range[2], by=1)))
image(seq(from=x.range[1], to=x.range[2], by=1), seq(from=y.range[1],to=y.range[2], by=1), qqq1, xlab="West to East",ylab="South to North", main="Raster map of the variances")
contour(seq(from=x.range[1], to=x.range[2], by=1), 
        seq(from=y.range[1],to=y.range[2], by=1), qqq1, add=TRUE, col="black", labcex=1)



# Using Lattice package
# A raster map using the kriged values: 
levelplot(pred_uk$log_data.pred~x+y, pred_uk, aspect ="iso", main="Universal kriging predictions")
# A raster map using the variances of the kriged values: 
levelplot(pred_uk$log_data.var~x+y, pred_uk, aspect ="iso", main="Universal kriging variances")

