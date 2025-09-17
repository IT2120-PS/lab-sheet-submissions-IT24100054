setwd("C:\\Users\\saths\\OneDrive\\Desktop\\it24100054")

##Question 01
#random variable X = a uniform distribution with a=0 and b=40
# P(10<X<25)= P(X<=25)- P(X<=10)
punif(25, min=0, max=40, lower.tail=TRUE)- punif(10, min=0, max=40, lower.tail = TRUE)

##Question 02
#random variable X = exponential distribution with lambda=1/3
#P(X<= 2)
pexp (2,rate = 1/3, lower.tail = TRUE)

##Question 03
#random variable X = normal distribution with mean=100 and standard deviation=15
#Part i  P(X>130) 
pnorm(130, mean=100, sd=15, lower.tail=FALSE)

#Part ii P(x>q)= 0.95.
qnorm(0.95, mean = 100, sd = 15, lower.tail = TRUE)