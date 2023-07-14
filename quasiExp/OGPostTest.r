# The following script is a programiticall implicated description 
# of the mathematics and methodlogy for chapter 5 of
# Charles reichardt quasi experiments textbook.

# One group post-test Design example
# 15 students were recruited to investigate polic rrepsone to black 
# panthers. the student were given black panther bumper stickers.
# across students the average was 33 citations across the 17 days of study
n <- 15
m <- 33
x <- rpois(n, m)
print(x)

# Fit poisson linear model to estimate intercept term
sum <- summary(glm(x ~ 1, family = poisson))
print(sum)
# Exponentiate the intercept parameter to get back the expected value of the single group
print(exp(coef(sum)[1]))

