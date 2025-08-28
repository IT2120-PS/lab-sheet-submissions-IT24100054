setwd("C:\\Users\\it24100054\\Desktop\\IT24100054")
Delivery_Times<-read.table("Exercise - Lab 05.txt",header=TRUE)
print(Delivery_Times)


hist(Delivery_Times$Delivery,breaks=seq(20,70,by=5),right = TRUE,
     main="Histogram Of Delivery Times", xlab="Delivery Times", ylab="Frequency")


delivery_times_freq <- hist(Delivery_Times$Delivery,
                            breaks = seq(20,70,by=5),
                            right = TRUE,
                            plot = FALSE)
cumulative_freq <- cumsum(delivery_times_freq$counts)
plot(delivery_times_freq$mids, cumulative_freq, type = "o",
     main = "Cumulative Frequency Polygon (Ogive) of Delivery Times",
     xlab = "Delivery Times",
     ylab = "Cumulative Frequency",
     pch = 16)