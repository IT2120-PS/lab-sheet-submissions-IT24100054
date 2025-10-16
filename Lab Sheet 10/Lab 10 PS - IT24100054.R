setwd("C:\\Users\\saths\\OneDrive\\Desktop\\IT24100054")

#1
#i
observed <- c(55, 62, 43, 46, 50)
prob <- c(.2, .2, .2, .2, .2)
chisq.test(x=observed, p=prob)

#2
#i
file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"

housetasks <- read.delim(file_path, row.names = 1)
housetasks

#ii
chisq <- chisq.test(housetasks)
chisq

#EXERCISE
#1
#ii
observed_counts <- c(120, 95, 85, 100)
probabilities <- c(0.25, 0.25, 0.25, 0.25)
chi_test_result <- chisq.test(x = observed_counts, p = probabilities)
print(chi_test_result)
