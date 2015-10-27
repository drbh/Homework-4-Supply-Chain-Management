library(ggplot2)
library(reshape2)

data <- read.csv("/Users/drbh/Desktop/P10_08.csv", stringsAsFactors = FALSE)

data$Earnings <- as.numeric(gsub("\\,", "", data$Earnings))

data$EPR <- data$Earnings/data$Rounds

interest <- c('Yards.Drive', 'Driving.Accuracy', 'Greens.in.Regulation', 'Putting.Average', 'Sand.Save.Pct', 'EPR')
d <- data[interest]

d <- melt(d, id.vars="EPR")

ggplot(d, aes(value,EPR,fill=variable)) + geom_point() + facet_grid(~ variable, scales = "free") + stat_smooth(method = "lm")

YD <- summary(lm(data$EPR ~ data$Yards.Drive))$r.squared
DA <- summary(lm(data$EPR ~ data$Driving.Accuracy))$r.squared
GIR <- summary(lm(data$EPR ~ data$Greens.in.Regulation))$r.squared
PA <- summary(lm(data$EPR ~ data$Putting.Average))$r.squared
SSP <- summary(lm(data$EPR ~ data$Sand.Save.Pct))$r.squared
cat('YD: ',YD,'DA: ',DA,'GIR: ',GIR,'PA: ',PA,'SSP: ',SSP)
 