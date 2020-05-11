require(ggplot2)
data <- read.csv("assignment-02-data-formated.csv",header = TRUE);


#set up factors
data$coralType <- as.factor(data$coralType);
data$location<-factor(data$location, levels = unique(data$location[order(data$latitude, decreasing = TRUE)]))
data$location <- as.factor(data$location);
#convert values column to decimal format
data$value <- as.numeric(sub("%", "",data$value,fixed=TRUE))/100

#Static PLot 
ggplot(data,aes(year,value))+facet_grid(coralType~location)+labs(x="Year",y="Bleaching %")+theme_bw()+geom_point()+geom_smooth(aes(color=..y..),method = lm)



