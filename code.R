require(datasets)
require(dplyr)

# 1. Loading Data
data('mtcars')

# Discovering the dataset
str(mtcars)     # No. of Observations & No. of Variables & Values of each column
names(mtcars)   # Dataset Keys
head(mtcars)    # Head of the dataset
tail(mtcars)    # Tail of the dataset
summary(mtcars) # Statistics of each column.


# 2. Extracting Information
# Display the head of each type of transmission -manual and automatic- separately.
print(head(dplyr::filter(mtcars, am == 1)))   # Manual Transmission
print(head(dplyr::filter(mtcars, am == 0)))   # Automatic Transmission

# Display the top 10 cars according to: Displacement, hp & drat.
# Method 1: Using order from base
sorted1 = head(mtcars[with(mtcars, order(-disp, -hp, -drat)),], 10)

# Method 2: By using arrange from dplyr
sorted2 = head(arrange(mtcars, desc(disp), desc(hp), desc(drat)), 10)

# Display cars whose mpg is above average only.
average_mpg = sum(mtcars['mpg']) / nrow(mtcars['mpg'])
print(filter(mtcars, mpg > average_mpg))

# Chart Representations
# mpg   -> Histogram
hist(mtcars$mpg, main='Distribution of Cars by MPG', xlab='MPG', ylab='No. of Cars',
     col=rainbow(5))
# cyl   -> Bar Plot
barplot(table(mtcars$cyl), xlab='Cylinders', ylab='No. of cars', 
        main='Distribution of Cars by Cylinders', col=rainbow(5))
# disp  -> Histogram
hist(mtcars$disp, main='Distribution of Cars by Displacement', xlab='Displacement',
     ylab='No. of Cars', col=rainbow(5))
# hp    -> Histogram
hist(mtcars$hp, main='Distribution of Cars by HP', xlab='HP',
     ylab='No. of Cars', col=rainbow(5))
# drat  -> Histogram
hist(mtcars$drat, main='Distribution of Cars by Rear Axle Ratio', xlab='Drat',
     ylab='No. of Cars', col=rainbow(5))
# wt    -> Histogram
hist(mtcars$wt, main='Distribution of Cars by Weight', xlab='Wt',
     ylab='No. of Cars', col=rainbow(5))
# qsec  -> Histogram
hist(mtcars$qsec, main='Distribution of Cars by 1/4 mile time', xlab='qsec',
     ylab='No. of Cars', col=rainbow(5))
# vs    -> Pie Chart
pie(table(mtcars$vs), main='Engine Type', labels=c('V-shaped (0)', 'Straight (1)'), col=rainbow(7))
# am    -> Pie Chart
pie(table(mtcars$am), main='Transmission', labels=c('Automatic (0)', 'Manual (1)'), col=rainbow(7))
# gear  -> Pie Chart
pie(table(mtcars$gear), main='Distribution of Cars by # of gears', col=rainbow(7))
# carb  -> Bar Plot
barplot(table(mtcars$carb), xlab='Number of Carburetors', ylab='No. of cars', 
        main='Distribution of Cars by Carburetors', col=rainbow(5))


# boxplot(x, data, notch, varwidth, names, main)
# Plot the boxplots for the following features: disp, hp and qsec.
boxplot(mtcars$disp, data=mtcars, main='Boxplot of Displacement')
boxplot(mtcars$hp, data=mtcars, main='Boxplot of Horse Power')
boxplot(mtcars$qsec, data=mtcars, main='Boxplot of 1/4 mile time (qsec)')
# Extract the 3 main percentiles.
# What can you deduce?




# 3. Distributions
# A.
mean = mean(mtcars$wt)
std_dev = sd(mtcars$wt)
Probability = 1 - pnorm(3.4, mean=mean, sd=std_dev)
print(Probability * 100)



# 5. Bonus
# A. 
for (i in 1:ncol(mtcars)) {
  qqnorm(mtcars[,i])
}
