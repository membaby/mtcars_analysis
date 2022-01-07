

#Distribution Questions.
library(datasets)
data("mtcars")
#A)
weight = mtcars$wt
weight_sorted = weight[order(weight)]
cumulative = pnorm(3.4, mean(weight_sorted), sd(weight_sorted))
plot(weight_sorted, cumulative)
prob = 1- cumulative
paste("Probability of a car having weight of 3.4 lb or more is", as.character(prob))
#B)
manual_count = 0
for (i in mtcars$vs)
{
  if (i == 1) 
  {
    manual_count = manual_count + 1
  }
}
success_prob = manual_count / length(mtcars$vs)
#probability of 18 cars or less being manual
cumulative = pbinom(seq(0,32), 32, success_prob)
paste("The probability of 18 or less cars being manuals out of the 32 cars is", as.character(cumulative[19]))
#C)


sample_space_count = 5^12
favourable_outcomes = 0
for (i in seq(0,4))
{
  favourable_outcomes = favourable_outcomes + (factorial(12)/(factorial(12-i)*factorial(i))) * 4^(12-i)
}
paste("The probability of 4 or less parking spaces being filled with the correct car type is", as.character(favourable_outcomes/sample_space_count))


#Permutations and Combinations Questions
library(gtools)
#A)
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












