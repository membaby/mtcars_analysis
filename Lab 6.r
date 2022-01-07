

data("mtcars")

#Distribution Questions.
#A)
weight = mtcars$wt
average = sum(weight) / length(weight)
variance = 0
for (i in weight)
{
  variance = variance + (i - average)^2
}

variance = variance / length(weight)
plot(weight, dnorm(weight, average, variance^0.5))

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
#A)
#Method 1:
perm_count = 3^3 - 3^2

#Method 2:
all_perms = permutations(3,3,seq(0,2), repeats.allowed = TRUE)
two_digit_nums = permutations(3,2,seq(0,2), repeats.allowed = TRUE)
perm_count = nrow(all_perms) - nrow(two_digit_nums)

paste("Number of possible ternary numbers with 3 digits =", perm_count)

#B)
library(gtools)

#Method 1:
sample_space_count = (factorial(9)/(factorial(3)*factorial(9-3)))
comb_count = 1 * 1 * (factorial(2)/factorial(2-1)*factorial(1))
prob = comb_count / sample_space_count

#Method 2:
sample_space = combinations(9, 3, seq(9))
combs = combinations(2, 1, seq(3,4))
prob = nrow(combs) / nrow(sample_space)












