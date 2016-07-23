library(swirl)
#Some principles of data analysis:
#1. Show comparison
#2. Show causality mechanism
#3. Multivariate analysis
#4. Integrate multiple components to show a whole picture
#5. Content is the king


library("ggplot2")
iris
qplot(Sepal.Length,Sepal.Width,data=iris,color=Species,shape=Species, geom=c("point","smooth"))
?loess
loess(Sepal.Length~Sepal.Width,iris,model=TRUE)
qplot(Sepal.Length,Sepal.Width,data=iris, geom=c("point","smooth"),facets=.~Species)
qplot(Sepal.Length,data=iris,facets=Species~.,binwidth=3)

#make plot process
plot(airquality$Wind,airquality$Ozone,type="n")
title(main="Wind and Ozone in NYC")
may<- subset(airquality,Month==5)
points(may$Wind,may$Ozone,pch=2,col="blue")
points(may$Wind,may$Ozone,pch=17,col="blue")
notmay<-subset(airquality,Month!=5)
points(notmay$Wind,notmay$Ozone, pch=8, col="red")
legend("topright",pch=c(17,8),col=c("blue","red"),legend=c("May","Other Months"))
abline(v=median(airquality$Wind),lty=2,lwd=2)

#plotting side by side
par(mfrow=c(1,2))
plot(airquality$Wind,airquality$Ozone, main="Ozone and Wind")
plot(airquality$Ozone,airquality$Solar.R,main="Ozone and Solar Radiation")

#3 plots 
par(mfrow=c(1,3),mar=c(4,4,2,1),oma=c(0,0,2,0))
plot(airquality$Wind,airquality$Ozone, main="Ozone and Wind")
plot(airquality$Solar.R, airquality$Ozone, main = "Ozone and Solar Radiation")
plot(airquality$Temp, airquality$Ozone, main = "Ozone and Temperature")
mtext("Ozone and Weather in New York City",outer=TRUE)

#Lattice 
xyplot(Ozone~Wind,data=airquality)
xyplot(Ozone ~ Wind, data = airquality, pch=8, col="red", main="Big Apple Data")
xyplot(Ozone~Wind|as.factor(Month), data=airquality,layout=c(5,1))
p<-xyplot(Ozone~Wind, data=airquality)

xyplot(y~x|f,layout=c(2,1))

source(pathtofile("myLabels.R"),local=TRUE)# read r code from source file
xyplot(price~carat|color*cut, data=diamonds,strip=FALSE,pch=20, xlab=myxlab,ylab=myylab,main=mymain)












#Extra

G<-factor(c('f','m','m','m','f','m','f','m','f','f'))
A<-factor(c('adult', 'juvenile','juvenile', 'juvenile','adult', 'adult', 'juvenile', 'juvenile','adult', 'juvenile'))
t<-table(G,A)

#margin.table--> For a contingency table in array form, compute the sum of table 
#entries for a given index.
margin.table(t,1)
margin.table(t,2)

prop.table(t,1)
prop.table(t,2)
prop.table(t)


#Generating Sequence
 
seq(-4,1,0.5)
[1] -4.0 -3.5 -3.0 -2.5 -2.0 -1.5 -1.0 -0.5  0.0  0.5  1.0

seq(from=3,to=1,length=4)
[1] 3.000000 2.333333 1.666667 1.000000
seq(length=5,from=3,by=1)
[1] 3 4 5 6 7
rep(x,times,each=**)
rep(5,10)
[1] 5 5 5 5 5 5 5 5 5 5
rep(c(3,5),4,each=3)
[1] 3 3 3 5 5 5 3 3 3 5 5 5 3 3 3 5 5 5 3 3 3 5 5 5
rep(c(3,5),4)
[1] 3 5 3 5 3 5 3 5

#gl Generate factors by specifying the pattern of their levels.
#gl(k,n) k--> # of evels of a factor, n--> # of repetitions for each level
gl(3,4,labels=c("Promoters","Neutral","Detractors"))
[1] Promoters  Promoters  Promoters  Promoters  Neutral    Neutral    Neutral    Neutral    Detractors
[10] Detractors Detractors Detractors
Levels: Promoters Neutral Detractors

gl(4,2,3) -->level of 4, repeat 2 times for each level, total length of output =3
gl(4,2,3)
[1] 1 1 2
Levels: 1 2 3 4


#colors:
sample(colors(),10)

#The first, colorRamp, takes a palette of colors (the arguments) and returns a function that takes values
# between 0 and 1 as arguments. The 0 and 1 correspond to the extremes of the color palette. Arguments
# between 0 and 1 return blends of these extremes.
pal <- colorRamp(c("red","blue"))
pal(0.5)
pal(seq(0,1,len=6))

     [,1] [,2] [,3]
[1,]  255    0    0
[2,]  204    0   51
[3,]  153    0  102
[4,]  102    0  153
[5,]   51    0  204
[6,]    0    0  255


p1<-colorRampPalette(c("red","blue"))
p1(2)

p2<-colorRampPalette(c("red","yellow"))
p2(10)

plot(x,y,pch=19,col=rgb(0,0.5,0.5,0.3))

##################GGPLOT2###############################
#qplot

qplot(displ,hwy, data=mpg)

qplot(displ,hwy, data=mpg, color=drv)

qplot(displ,hwy, data=mpg, color=drv, geom=c("point","smooth"))

qplot(y=hwy,data=mpg, color=drv)

qplot(drv,hwy,data=mpg,geom="boxplot") #boxplot with qplot

qplot(drv,hwy,data=mpg,geom="boxplot",color=manufacturer)

qplot(hwy,data=mpg,fill=drv)

qplot(displ,hwy, data=mpg,facets=.~drv) #facets for scatteredplot

qplot(hwy, data=mpg, facets= drv~.,binwidth=2)

# ggplot2
qplot(displ,hwy, data=mpg,geom=c("point","smooth"), facets=.~drv)

g<-ggplot(data=mpg,aes(displ,hwy));

g+geom_point()+geom_smooth();

g+geom_point()+geom_smooth(method="lm")

g+geom_point()+geom_smooth(method="lm")+facet_grid(.~drv)

g+geom_point()+geom_smooth(method="lm")+facet_grid(.~drv)+ggtitle("Swirl Rules!")

#customize ggplot
g+geom_point(color="pink",size=4,alpha=0.5)
#setting colors dependent on variables
g+geom_point(size=4,alpha=0.5,aes(color=drv))

#setting colors dependent on variables and setting labels
g+geom_point(aes(color=drv))+labs(title="Swirl Rules!",x="Displacement",y="Hwy Mileage")

g+geom_point(aes(color=drv),size=2,alpha=0.5)+geom_smooth(size=4, linetype=3, method="lm",se=FALSE)

#simple plot with theme_bw -->black and white theme
g+geom_point(aes(color=drv))+theme_bw(base_family="Times")

#base plot
plot(myx,myy,type="l",ylim=c(-3,3));

g <- ggplot(testdat, aes(x = myx, y = myy)) + geom_line() 

g+geom_line()+ylim(-3,3) # by specifying y limit, the outlier is ignored.

g+geom_line()+coord_cartesian(ylim=c(-3,3))

#example to showcase power of ggplot()
g<-ggplot(mpg,aes(displ,hwy, color=factor(year)))
g+geom_point()+facet_grid(drv~cyl,margins=TRUE)

g + geom_point() + facet_grid(drv~cyl,margins=TRUE)+geom_smooth(method="lm",size=2,se=FALSE,color="black")
g + geom_point() + facet_grid(drv~cyl,margins=TRUE)+geom_smooth(method="lm",size=2,se=FALSE,color="black")+labs(title="Swirl Rules!",x="Displacement",y="Highway Mileage")

#histogram 
qplot(price, data=diamonds)
qplot(price,data=diamonds,binwidth=18497/30)

qplot(price, data=diamonds,geom="density")
qplot(price, data=diamonds,geom="density", color=cut)
qplot(carat,price,data=diamonds,shape=cut)
qplot(carat,price,data=diamonds,color=cut)

qplot(carat,price,data=diamonds,color=cut)+geom_smooth(method="lm")
qplot(carat,price,data=diamonds,color=cut,facets=.~cut)+geom_smooth(method="lm")  #the number of panels in plotting

####ggplot#####
g<-ggplot(diamonds, aes(depth,price))

g+geom_point(alpha=1/3)
g+geom_point(alpha=1/3)+facet_grid(cut~car2)+geom_smooth(method="lm",size=3,color="pink")

ggplot(diamonds,aes(carat,price))+geom_boxplot()+facet_grid(.~cut)










cutpoints <- quantile(diamonds$carat,seq(0,1,length=4),na.rm=TRUE) 

g<-ggplot(diamonds,aes(depth,price))
g+geom_point(alpha=1/3)+facet_grid(cut~car2)


#########hierarchical clustering###########################
hc<-hclust(distxy) # hclust gets to cluster the dist() object
plot(hc)
plot(as.dendrogram(hc))

abline(h=1.5,col="blue")
abline(h=0.4,col="red")

#=============complete linkage===============#
dist(dFsm)

#########heatmap###########################
heatmap(mt)

#########  KMeans Clustering   ###########################








