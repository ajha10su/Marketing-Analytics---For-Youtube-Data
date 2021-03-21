library(dplyr)
library(ggplot2)

#Is there a relationship between publishing time and the number of views? 
#Is it better to publish a video during the weekend?
data_us <- read.csv('USvideos.csv')
library(lubridate)
data_us$hour <- substr(data_us$publish_time,12,13)
data_us$hour <- as.numeric(data_us$hour)

q1 <- tapply(data_us$views,list(data_us$hour),FUN = sum)
q1 <- as.data.frame(q1)
q1$hours <- rownames(q1)
rownames(q1) <- NULL
total_view <- q1$q1
hours <- q1$hours
hours <- as.numeric(hours)
df2 <- q1
df2$hours <- as.numeric(df2$hours)

#First plot
plot(hours, total_view, main = "Total number of views in terms of different hours",
     xlab = "Hours", ylab = "Total number of views",
     pch = 19,col = "darkgreen",las=1)
axis(side=1, at=0:23, labels=hours[0:24],cex.axis=1)
lines(lowess(hours, total_view), col = "red")

#Second plot
data_us$hour <- substr(data_us$publish_time,12,13)
data_us$hour <- as.numeric(data_us$hour)

data_us$day_of_week <- weekdays(as.Date(data_us$publish_time))
day_week <- tapply(data_us$likes,list(data_us$day_of_week),FUN = sum)
day_week <- as.data.frame(day_week)
day_week <- cbind(the_day = rownames(day_week), day_week)
rownames(day_week) <- 1:nrow(day_week)
rownames(day_week) <- NULL
names(day_week) <- c("day_of_week", "totalViews")
day_week$share = day_week$totalViews/sum(day_week$totalViews)

plot2 = ggplot(day_week, aes(x="", y=share, fill=day_of_week)) + geom_bar(stat="identity", width=1)
plot2 = plot2 + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(share*100), "%")), position = position_stack(vjust = 0.5))
plot2 = plot2 + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#FF4500")) 
plot2 = plot2 + labs(x = NULL, y = NULL, fill = NULL, title = "Number of views in terms of different days")
plot2 = plot2 + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(color = "#666666"))

plot2