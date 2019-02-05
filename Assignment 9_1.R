
# 1. If Z is norm (mean = 0, sd = 1)
#  Find P(Z > 2.64)

pnorm(2.64, mean = 0, sd = 1, lower.tail = FALSE)

#------------------------

#  Find P(|Z| > 1.39)
#  = 1 - P(-1.39 < X < 1.39)
1 - (pnorm(1.39, mean = 0, sd=1) - pnorm(-1.39, mean = 0, sd=1))



# Suppose p = the proportion of students who are admitted to the graduate school 
# of the University of California at Berkeley, and suppose that a public relation 
# officer boasts that UCB has historically had a 40% acceptance rate for its graduate
# school. Consider the data stored in the table UCBAdmissions from 1973. Assuming 
# these observations constituted a simple random sample, are they consistent with 
# the officer's claim, or do they provide evidence that the acceptance rate was 
# significantly less than 40%? Use an alpha = 0.01 significance level.



View(UCBAdmissions)
class(UCBAdmissions)

# Our null hypothesis, H0 is p= 0.40
# Alternative Hypothesis , Ha is p < 0.4

-qnorm(0.99)   # to find z alpha

A <- as.data.frame(UCBAdmissions)
head(A)

xtabs(Freq ~ Admit, data = A)

# calculate the value of the test statistic.
phat <- 1755/(1755 + 2771)
(phat - 0.4)/sqrt(0.4 * 0.6/(1755 + 2771))


prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less",
          conf.level = 0.99, correct = FALSE)


library(IPSUR)
library(HH)
library(ggplot2)
temp <- prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less",conf.level = 0.99, correct = FALSE)
plot(temp, "Hypoth")
