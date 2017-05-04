setwd("/Users/test/Downloads/want")
getwd()
mydata <- read.csv("Data Challenge Numbers - Fall 2017 - Sheet1.csv")
summary(mydata)
##################################################
#extract shop78&42                               #
#mydata1 <- subset(mydata, shop_id==78)          #
#not useful here, I just use to to valide dataset#
##################################################
#trasform into datafram
df <- data.frame(mydata)
#delete conditional rows
df <- df[!(df$shop_id==78),]
df <- df[!(df$shop_id==42),]
write.csv(df, file="df.csv")
summary(df)
# after below command,I can use attribute names directly latter
attach(df)
summary(order-amount)
#calculate stdev
sd(order_amount)
[1] 155.9411
###################################
#draw nomial distribution, mean line, media line
#not useful for this data statistic, but can be used for special dataset which be assumed nominal distribution
#curve(dnorm(x,mean(order_amount),sd(order_amount)),xlim=c(50,1100),col="black",lwd=3,main = "Nomal Distribution",ylab="f(x)")
#abline(v=mean(order_amount),col="red",lwd=2)
#text(380,0,"VOA=300.2",col="red")
#abline(v=median(order_amount),col="blue",lwd=2)
#text(200,0,"Median=284",col="blue")
#no line for mod()
#####################################
#draw histogram
#prob = TRUE, # show densities instead of frequencies
break1 <- seq(50,1100,50)
hist(order_amount,breaks=break1,col = "antiquewhite4",border = "black", prob = FALSE,xlim = c(50,1100),xlab = "Order_amount",main = "Distribution of Order_amount")
#xaxt="n"  will not show x-axia
#change the value in axia
axis= axis(1,at=seq(0,1100,50))
axis= axis(2,at=seq(0,1000,100))
#I want to statistic how many orders value in [150,200]
highfle <- subset(df, order_amount>=150 & order_amount <=200)
write.csv(highfle,file="highfle.csv")
nrow(highfle)#947
#draw a horizen line to show 947
abline(h=947,col="lightgreen",lwd=2)
#draw density plot
d <- density(order_amount)
plot(d, main="Density plot to show distribution",xlab="Order_amount")
polygon(d, col="lavenderblush")
abline(v=mean(order_amount),col="red",lwd=3)
text(380,0.0001,"AOV=300.2",col="red")
abline(v=median(order_amount),col="blue",lwd=3)
text(200,0.0001,"Median=284",col="blue")