# Linear regression
# info <- read.csv('test.csv',header = TRUE)
# names = colnames(info)
# print(names)
# lm_yx = lm(y~x,info)
# summary(lm_yx)
# plot(info$x,info$y)
# plot(lm_yx)

# main
rm(list = ls())



# raw data processing
Data.raw <- read.csv('data.csv',header = TRUE)
Names <- c('ROA','RPT','ASSET','GROW','LEV')
N <- length(Data.raw$code)
m <- 5;


# RPT.S <- matrix(rep(0,m*N),N,m)
# RPT.B <- matrix(rep(0,m*N),N,m)
# RPT.P <- matrix(rep(0,m*N),N,m)
# RPT.R <- matrix(rep(0,m*N),N,m)
# for (i in 1:m){
#   RPT.S[,i] <- Data.raw[[paste('RPT.S','.',2014+i,'.',sep = '')]]
#   RPT.B[,i] <- Data.raw[[paste('RPT.B','.',2014+i,'.',sep = '')]]
#   RPT.P[,i] <- Data.raw[[paste('RPT.P','.',2014+i,'.',sep = '')]]
#   RPT.R[,i] <- Data.raw[[paste('RPT.R','.',2014+i,'.',sep = '')]]
# }
# for (i in 1:N){
#   if(sum(RPT.S[i,]!=0) > 3){
#     RPT.S[i,c(RPT.S[i,]==0)] <- mean(RPT.S[i,c(RPT.S[i,]!=0)])
#   }
#   if(sum(RPT.B[i,]!=0) > 3){
#     RPT.B[i,c(RPT.B[i,]==0)] <- mean(RPT.B[i,c(RPT.B[i,]!=0)])
#   }
# }
# 
# RPT <- abs(RPT.S - RPT.B - RPT.P + RPT.R)

# data
Data <- data.frame(ROA=rep(0,m*N),
                   RPT=rep(0,m*N),
                   ASSET=rep(0,m*N),
                   GROW=rep(0,m*N),
                   LEV=rep(0,m*N))
for (i in 1:m){
  Data$ROA[(i-1)*N+(1:N)] <- Data.raw[[paste('ROA','.',2014+i,'.',sep = '')]]
  # Data$RPT[(i-1)*N+(1:N)] <- RPT[,i]
  Data$RPT[(i-1)*N+(1:N)] <- Data.raw[[paste('RPT6','.',2014+i,'.',sep = '')]]
  Data$ASSET[(i-1)*N+(1:N)] <- Data.raw[[paste('ASSET','.',2014+i,'.',sep = '')]]
  Data$GROW[(i-1)*N+(1:N)] <- Data.raw[[paste('GROW','.',2014+i,'.',sep = '')]]
  Data$LEV[(i-1)*N+(1:N)] <- Data.raw[[paste('LEV','.',2014+i,'.',sep = '')]]
}

Data <- Data[c(Data[['RPT']]>0),]
Data <- Data[c(Data[['ASSET']]>0),]
eps <- 10^0
# Data <- Data[c(!is.na(Data[['RPT']])),]
Data$RPT <- log(eps+Data$RPT)
Data$ASSET <- log(eps+Data$ASSET)
# Data <- Data[c(abs(Data[['GROW']])<500),]
# Data <- Data[c(abs(Data[['ROE']])<40),]
# Data$RPT2 <- Data$RPT^2
# rm(i,N,m,RPT.B,RPT.P,RPT.R,RPT.S,RPT)

# eliminating the 4 max and 4 min
for (i in 1:4){
  Data <- Data[-c(Data[['ROA']] == max(Data[['ROA']])),]
  Data <- Data[-c(Data[['ROA']] == min(Data[['ROA']])),]
}
rm(i,eps)





for (loop in 1){
# descriptive results
descriptive.results <- data.frame(name = Names,
                                  N = rep(0,length(Names)),
                                  min = rep(0,length(Names)),
                                  max = rep(0,length(Names)),
                                  mean = rep(0,length(Names)),
                                  sd = rep(0,length(Names)),
                                  skewness = rep(0,length(Names)),
                                  kurtosis = rep(0,length(Names)))
for (i in 1:length(Names)){
  descriptive.results$N[i] <- length(Data[[Names[i]]])
  descriptive.results$min[i] <- min(Data[[Names[i]]])
  descriptive.results$max[i] <- max(Data[[Names[i]]])
  descriptive.results$mean[i] <- mean(Data[[Names[i]]])
  descriptive.results$sd[i] <- sd(Data[[Names[i]]])
  descriptive.results$skewness[i] <- 
    mean(((Data[[Names[i]]]-mean(Data[[Names[i]]]))/sd(Data[[Names[i]]]))^3)
  descriptive.results$kurtosis[i] <- 
    mean(((Data[[Names[i]]]-mean(Data[[Names[i]]]))/sd(Data[[Names[i]]]))^4)-3
}
rm(i)


# Pearson correlation test
Pearson.test.cor <- matrix(rep(0,length(Names)*length(Names)),length(Names),length(Names))
Pearson.test.p.value <- matrix(rep(0,length(Names)*length(Names)),length(Names),length(Names))
for (i in 1:length(Names)){
  for (j in 1:i){
    temp <- cor.test(Data[[Names[i]]],Data[[Names[j]]],
                     alternative='two.side',method='pearson')
    Pearson.test.cor[i,j] <- temp$estimate
    Pearson.test.p.value[i,j] <- temp$p.value
  }
}
Pearson.test.cor <- data.frame(Pearson.test.cor)
colnames(Pearson.test.cor) <- Names
Pearson.test.cor <- cbind(Names,Pearson.test.cor)
Pearson.test.p.value <- data.frame(Pearson.test.p.value)
colnames(Pearson.test.p.value) <- Names
Pearson.test.p.value <- cbind(Names,Pearson.test.p.value)
rm(temp)
rm(i,j)


library(car)
library(lmtest)
# model.1: ROE=b0+b1*RPT+b2*ASSET+b3*GROW+b4*LEV+b5*DIR+err
model.1.lm <- lm(ROA~.,Data[c(1:5)])
model.1.summary <- summary(model.1.lm)
model.1.vif <- vif(model.1.lm)
model.1.dw <- dwtest(model.1.lm)
model.1.F.test <- data.frame(name = c('SSR','SSE','SST'),
                              sq = rep(NaN,3),
                              df = rep(NaN,3),
                              sq.ave = rep(NaN,3),
                              F = rep(NaN,3),
                              sig = rep(NaN,3))
y <- Data[['ROA']]
x <- Data[c(2:5)]
y.predict <- predict(model.1.lm,x)
model.1.F.test$sq[1] <- sum((y.predict - mean(y))^2)
model.1.F.test$sq[2] <- sum((y.predict - y)^2)
model.1.F.test$sq[3] <- model.1.F.test$sq[1] + model.1.F.test$sq[2]
model.1.F.test$df[1] <- model.1.summary$fstatistic[2]
model.1.F.test$df[2] <- model.1.summary$fstatistic[3]
model.1.F.test$sq.ave[1] <- model.1.F.test$sq[1] / model.1.F.test$df[1]
model.1.F.test$sq.ave[2] <- model.1.F.test$sq[2] / model.1.F.test$df[2]
model.1.F.test$F[1] <- model.1.F.test$sq.ave[1] / model.1.F.test$sq.ave[2]
model.1.F.test$sig[1] <- 1-pf(model.1.F.test$F[1],
                             model.1.summary$fstatistic[2],
                             model.1.summary$fstatistic[3])
model.1 <- list(lm = model.1.lm,summary = model.1.summary,
                vif = model.1.vif,dw = model.1.dw,F.test = model.1.F.test)
rm(model.1.lm,model.1.summary,model.1.vif,model.1.dw,model.1.F.test)
rm(y,x,y.predict)



# model 2: RPT=b0+b1*DIR+b2*ASSET+b3*GROW+b4*LEV+err
model.2.lm <- lm(RPT~.,Data[c(2:5)])
model.2.summary <- summary(model.2.lm)
model.2.vif <- vif(model.2.lm)
model.2.dw <- dwtest(model.2.lm)
model.2.F.test <- data.frame(name = c('SSR','SSE','SST'),
                             sq = rep(NaN,3),
                             df = rep(NaN,3),
                             sq.ave = rep(NaN,3),
                             F = rep(NaN,3),
                             sig = rep(NaN,3))
y <- Data[['RPT']]
x <- Data[c(3:5)]
y.predict <- predict(model.2.lm,x)
model.2.F.test$sq[1] <- sum((y.predict - mean(y))^2)
model.2.F.test$sq[2] <- sum((y.predict - y)^2)
model.2.F.test$sq[3] <- model.2.F.test$sq[1] + model.2.F.test$sq[2]
model.2.F.test$df[1] <- model.2.summary$fstatistic[2]
model.2.F.test$df[2] <- model.2.summary$fstatistic[3]
model.2.F.test$sq.ave[1] <- model.2.F.test$sq[1] / model.2.F.test$df[1]
model.2.F.test$sq.ave[2] <- model.2.F.test$sq[2] / model.2.F.test$df[2]
model.2.F.test$F[1] <- model.2.F.test$sq.ave[1] / model.2.F.test$sq.ave[2]
model.2.F.test$sig[1] <- 1-pf(model.2.F.test$F[1],
                              model.2.summary$fstatistic[2],
                              model.2.summary$fstatistic[3])
model.2 <- list(lm = model.2.lm,summary = model.2.summary,
                vif = model.2.vif,dw = model.2.dw,F.test = model.2.F.test)
rm(model.2.lm,model.2.summary,model.2.vif,model.2.dw,model.2.F.test)
rm(y,x,y.predict)


# residuals analysis
rs <- residuals(model.1$lm)
rss <- rs/sd(rs)
# Data <- Data[c(abs(rss)<2),]

}
rm(loop)


plot(Data$RPT,Data$ROE)
# plot(Data$ASSET,Data$ROE)
# plot(Data$GROW,Data$ROE)
# plot(Data$LEV,Data$ROE)
# 
# plot(Data$ASSET,Data$RPT)
# plot(Data$GROW,Data$RPT)
# plot(Data$LEV,Data$RPT)
# plot(model.1$lm,4)
summary(model.1$lm)
summary(model.2$lm)



write.table(descriptive.results,
            file = "table.1.descriptive.results.csv",
            sep = ',',
            row.names = FALSE)
write.table(Pearson.test.cor,
            file = "table.2.Pearson.test.cor.csv",
            sep = ',',
            row.names = FALSE)
write.table(Pearson.test.p.value,
            file = "table.2.Pearson.test.p.value.csv",
            sep = ',',
            row.names = FALSE)
write.table(data.frame(name = names(model.1$vif),vif = model.1$vif),
            file = "table.3.model.1.vif.csv",
            sep = ',',
            row.names = FALSE)
write.table(data.frame(model = 1,
                       R = sqrt(model.1$summary$r.squared),
                       R.squared = model.1$summary$r.squared,
                       adj.R.squared = model.1$summary$adj.r.squared,
                       sigma = model.1$summary$sigma,
                       DW = model.1$dw$statistic),
            file = "table.3.model.1.total.csv",
            sep = ',',
            row.names = FALSE)
write.table(model.1$F.test,
            file = "table.3.model.1.F.test.csv",
            sep = ',',
            row.names = FALSE)
model <- rownames(data.frame(model.1$summary$coefficients))
write.table(cbind(model,data.frame(model.1$summary$coefficients)),
            file = "table.3.model.1.summary.csv",
            sep = ',',
            row.names = FALSE)
rm(model)
write.table(data.frame(name = names(model.2$vif),vif = model.2$vif),
            file = "table.4.model.2.vif.csv",
            sep = ',',
            row.names = FALSE)
write.table(data.frame(model = 2,
                       R = sqrt(model.2$summary$r.squared),
                       R.squared = model.2$summary$r.squared,
                       adj.R.squared = model.2$summary$adj.r.squared,
                       sigma = model.2$summary$sigma,
                       DW = model.2$dw$statistic),
            file = "table.4.model.2.total.csv",
            sep = ',',
            row.names = FALSE)
write.table(model.2$F.test,
            file = "table.4.model.2.F.test.csv",
            sep = ',',
            row.names = FALSE)
model <- rownames(data.frame(model.2$summary$coefficients))
write.table(cbind(model,data.frame(model.2$summary$coefficients)),
            file = "table.4.model.2.summary.csv",
            sep = ',',
            row.names = FALSE)
rm(model)
