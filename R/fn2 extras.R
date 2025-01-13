cor(xdata$Degree, xdata$N.avg.male.bs)
cor(xdata$Degree, xdata$Spatial.assoc)

plot(xdata$Num.fledglings, xdata$N.avg.male.bs)

summary(lm(xdata$Num.fledglings ~ xdata$N.avg.male.bs + xdata$Largeoaks + xdata$Age_cat + xdata$Year.s))

summary(lm(xdata$Num.fledglings ~ xdata$Degree + xdata$Largeoaks + xdata$Age_cat + xdata$Year.s))


summary(glm(xdata$Num.fledglings ~ xdata$Degree + xdata$N.avg.male.bs + xdata$Largeoaks + xdata$Age_cat + xdata$Year.s))


#vif 

library(car)

laydate <- glm(xdata$April.lay.date ~ xdata$Age_num + xdata$Age_cat + 
                        xdata$Year.w + xdata$Largeoaks + xdata$Bondstrength + 
                        xdata$Degree + xdata$AnnualDensity)
            
vif(laydate)  

binarysucc <- glm(xdata$Binary.succ ~ xdata$Age_num + $Age_cat + xdata$Year.w + xdata$Largeoaks)
vif(binarysucc)

clutch <- glm(xdata$Clutch.size ~ xdata$Age_num + xdata$Age_cat + xdata$Year.w + xdata$Largeoaks + xdata$Spatial.assoc)
vif(clutch)

meanweight <- glm(xdata$Mean.chick.weight ~ xdata$Age_num + xdata$Age_cat + xdata$Year.w + xdata$Largeoaks)
vif(meanweight)


numfledge <- glm(xdata$Num.fledglings ~ xdata$Age_num + xdata$Age_cat + 
                 xdata$Year.w + xdata$Largeoaks + xdata$N.avg.male.bs + 
                 xdata$Degree)
vif(numfledge)


vif(laydate)
vif(binarysucc)
vif(clutch)
vif(meanweight)
vif(numfledge)
