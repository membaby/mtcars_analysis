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
print(head(filter(mtcars, am == 1)))   # Manual Transmission
print(head(filter(mtcars, am == 0)))   # Automatic Transmission

# Display the top 10 cars according to: Displacement, hp & drat.
# Method 1: Using order from base
print(head(mtcars[with(mtcars, order(-disp, -hp, -drat)),], 10))  # Sorting by the three columns at same time
print(head(mtcars[with(mtcars, order(-disp)),], 10))              # Sorting by the displacement
print(head(mtcars[with(mtcars, order(-hp)),], 10))                # Sorting by the Horse Power (HP)
print(head(mtcars[with(mtcars, order(-drat)),], 10))              # Sorting by the DRAT


# Method 2: By using arrange from dplyr
print(head(arrange(mtcars, desc(disp), desc(hp), desc(drat)), 10))   # Sorting by the three columns at same time
print(head(arrange(mtcars, desc(disp)), 10))                         # Sorting by the displacement
print(head(arrange(mtcars, desc(hp)), 10))                           # Sorting by the Horse Power (HP)
print(head(arrange(mtcars, desc(drat)), 10))                         # Sorting by the DRAT

# Display cars whose mpg is above average only.
average_mpg = sum(mtcars['mpg']) / nrow(mtcars['mpg'])
print(filter(mtcars, mpg > average_mpg))

# Chart Representations
# mpg   -> Histogram
hist(mtcars$mpg, main='Distribution of Cars by MPG', xlab='Miles Per Galon', ylab='No. of Cars',
     col=rainbow(5))
# cyl   -> Bar Plot
barplot(table(mtcars$cyl), xlab='Cylinders', ylab='No. of cars', 
        main='Distribution of Cars by Cylinders', col=rainbow(5))
# disp  -> Histogram
hist(mtcars$disp, main='Distribution of Cars by Disp.', xlab='Displacement',
     ylab='No. of Cars', col=rainbow(5), breaks=5)
# hp    -> Histogram
hist(mtcars$hp, main='Distribution of Cars by HP', xlab='Horsepower',
     ylab='No. of Cars', col=rainbow(5), breaks=5)
# drat  -> Histogram
hist(mtcars$drat, main='Distribution of Cars by DRAT', xlab='Rear Axle Ratio',
     ylab='No. of Cars', col=rainbow(5), breaks=5)
# wt    -> Histogram
hist(mtcars$wt, main='Distribution of Cars by Weight', xlab='Weight',
     ylab='No. of Cars', col=rainbow(5), breaks=5)
# qsec  -> Histogram
hist(mtcars$qsec, main='Distribution of Cars by QSEC', xlab='1/4 mile time',
     ylab='No. of Cars', col=rainbow(5), breaks=5)
# vs    -> Pie Chart
pie(table(mtcars$vs), main='Engine Type', labels=c('V-shaped (0)', 'Straight (1)'), col=rainbow(7))
# am    -> Pie Chart
pie(table(mtcars$am), main='Transmission', labels=c('Automatic (0)', 'Manual (1)'), col=rainbow(7))
# gear  -> Pie Chart
barplot(table(mtcars$gear), xlab='# of gears', ylab='No. of cars', 
        main='Distribution of Cars by # of gears', col=rainbow(5))

# carb  -> Bar Plot
barplot(table(mtcars$carb), xlab='Number of Carburetors', ylab='No. of cars', 
        main='Distribution of Cars by Carburetors', col=rainbow(5))


# Plot the boxplots for the following features: disp, hp and qsec.
boxplot(mtcars$disp, data=mtcars, main='Boxplot of Displacement')
boxplot(mtcars$hp, data=mtcars, main='Boxplot of Horse Power')
boxplot(mtcars$qsec, data=mtcars, main='Boxplot of 1/4 mile time (qsec)')
# Extract the 3 main percentiles.
print(quantile(mtcars$disp, c(0.25, .5, .75)))        # Percentiles of displacement
print(quantile(mtcars$hp, c(0.25, .5, .75)))          # Percentiles of hp
print(quantile(mtcars$qsec, c(0.25, .5, .75)))        # Percentiles of qsec




# 2. Distribution
# A.
weight = mtcars$wt
weight_sorted = weight[order(weight)]
cumulative = pnorm(3.4, mean(weight_sorted), sd(weight_sorted))
prob = 1 - cumulative
paste("Percentage of cars having weight of 3.4 lb or more is ", prob*100, "%", sep = "")

# B.
manual_count = 0
for (i in mtcars$vs)
{
  if (i == 1) 
  {
    manual_count = manual_count + 1
  }
}
success_prob = manual_count / length(mtcars$vs) #Probability of a car being manual
cumulative = pbinom(seq(0,32), 32, success_prob)
paste("The probability of 18 or less cars being manuals out of the 32 cars is", cumulative[19])

# C.
sample_space_count = 5^12
favourable_outcomes = 0
for (i in seq(0,4))
{
  favourable_outcomes = favourable_outcomes + (factorial(12)/(factorial(12-i)*factorial(i))) * 4^(12-i)
}
paste("The probability of 4 or less parking spaces being filled with the correct car type is", as.character(favourable_outcomes/sample_space_count))



# 4. Permutations and Combinations
# A.
#Method 1:
perms = permutations(3,3,seq(0,2), repeats.allowed = TRUE)
perms <- perms[-1:-9,]
perms

#Method 2:
perms = c()
for (i in 1:2)
{
  for (j in 0:2)
  {
    for (k in 0:2)
    {
      perms = append(perms, c(i,j,k))
    }
  }
}
perms = matrix(perms, 3^3 - 3^2, 3, byrow = TRUE)
perms

#B)
#Method 1:
sample_space_count = (factorial(9)/(factorial(3)*factorial(9-3)))
comb_count = 1 * 1 * (factorial(2)/factorial(2-1)*factorial(1))
prob = comb_count / sample_space_count

#Method 2:
sample_space = combinations(9, 3, seq(9))
combs = combinations(2, 1, seq(3,4))
prob = nrow(combs) / nrow(sample_space)
paste("Probability of getting 3 numbers where the max is 5 and the min is 2 is ", prob, sep = "")

  

# 5. Bonus
# A. Plot Q-Q plot for the mtcars dataset.
library("car")

qqPlot(mtcars$mpg,   "unif")
qqPlot(mtcars$cyl,   "unif")
qqPlot(mtcars$disp,  "unif")
qqPlot(mtcars$hp,    "unif")
qqPlot(mtcars$drat,  "unif")
qqPlot(mtcars$wt,    "norm")
qqPlot(mtcars$qsec,  "norm")
qqPlot(mtcars$vs,    "unif")
qqPlot(mtcars$am,    "pois", lambda=32*0.56)
qqPlot(mtcars$gear,  "unif")
qqPlot(mtcars$carb,  "unif")
