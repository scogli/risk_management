rm(list=ls())
install.packages("sgt")
library(sgt)
portfolio_fit <- rsgt(n = 3065,mu = 0.000432212,lambda = 0.005829033,p = 2.073333775,q = 2.05337848)



mapped_portfolio <-  rsgt(n = 3065,mu = 0.000142554
,lambda = -0.067094176
,p = 1.866428829,q = 1.576477759
)

residual <-  rsgt(n = 3065,mu = 0.000260451,
                  lambda = 0,032732787,
                  p =2.149483719,
                  q = 1.793870016)


install.packages("ggpubr")
library("ggpubr")


ggdensity(c(portfolio_fit,mapped_portfolio),combine = TRUE,alpha =1)+
  theme(plot.title = element_text(hjust = 0.5))+ggtitle("Portfolio SGT FIT")

df <- as.data.frame(portfolio_fit)


df$mapped_portfolio <- mapped_portfolio

df$residual <- residual
attach(df)

View(df)

options(scipen = 999)
ggplot(data = df,aes(x = df$portfolio_fit))+
  geom_density(y ="..density.." ,alpha=.2, fill="#FF6666")


plt_portfolio <- ggplot(data = df,aes(x=df$portfolio_fit))+
  geom_density(y ="..density..",alpha=1, fill="#f68463")+
  ggtitle("Portfolio Fit")+
  labs(x="")+
  geom_text(x=4, y=0.44, label="mu = 0.000432212")+
  geom_text(x=4, y=0.38, label="lambda = 0.005829033")+
  geom_text(x=4, y=0.32, label="p = 2.073333775")+
  geom_text(x=4, y=0.26, label="q = 2.05337848")
  

plt_portfolio


plt_mapped_portfolio <- ggplot(data = df,aes(x=df$mapped_portfolio))+
  geom_density(y ="..density..",alpha=1, fill="#872fc5")+
  ggtitle("Mapped Portfolio Fit
          ")+
  labs(x="")+
  geom_text(x=7, y=0.6, label="mu = 0.000142554")+
  geom_text(x=7, y=0.54, label="lambda = - 0.067094176")+
  geom_text(x=7, y=0.48, label="p = 1.866428829")+
  geom_text(x=7, y=0-42, label="q = 1.576477759")

plt_mapped_portfolio


plt_residual <- ggplot(data = df,aes(x=df$residual))+
  geom_density(y ="..density.." ,alpha=1, fill="#acd01b")+
  ggtitle("Residuals Fit")+
  labs(x="")+geom_text(x=200000000, y=0.000000016, label="mu = 0.000432212")+
  geom_text(x=200000000, y=0.000000014, label="lambda = 0.005829033")+
  geom_text(x=200000000, y=.000000012, label="p = 2.073333775")+
  geom_text(x=200000000, y=.000000010, label="q = 2.05337848")
plt_residual



par(mfrow=c(2,2))



plt_mapped_portfolio

attach(risk_metrics_var)



ggplot(data = risk_metrics_var,mapping = aes(x= Date))+
  geom_line(mapping = aes(y = `var_1%_eur`),alpha = 0.94, colour = "#3874e7")+
  labs(y= "Value in EUR")+
  ggtitle("1-day 1% VaR ???")

summary(risk_metrics_var$`var_1%_eur`)
