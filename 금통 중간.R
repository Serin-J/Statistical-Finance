rm(list = ls())
gc()

library(stringr)
library(ggplot2)
brand = c("하이트","제일기획", "농심",  "NHN", "한국제지", "모나미")
p = length(brand)
kospi_five = list()
for(i in 1:p){
  dat = NULL
  for(j in 1:3){
    link = paste0("./finance/", brand[i], j, ".csv")
    file = read.csv(link, stringsAsFactors = F, 
                    col.names = c("date", "price","대비", "거래량", "거래대금", "시가",
                                  "고가", "저가", "시가총액", "상장주식수"))
    dat = rbind(dat, file)
  }
  dat$date = as.Date(dat$date)
  dat[,-1] = apply(dat[,-1], 2, function(X) str_replace_all(X, ",", ""))
  dat[,-1] = apply(dat[,-1], 2, as.numeric)
  kospi_five[[i]] = dat
}
names(kospi_five) = brand


kospi_price = matrix(kospi_five[[1]][,2], ncol = 1)
for(i in 2:p){
  kospi_price = cbind(kospi_price, matrix(kospi_five[[i]][,2], ncol = 1))
}

#

library(reshape2)
price = cbind(date = kospi_five[[1]]$date, kospi_price)
price = melt(price, value.name = "price", variable.name = "brand", id.vars = "date")

library(ggthemes)
library(scales)
ggplot(data = price) + geom_line(aes(x = date, y = price, col = brand)) + 
  facet_wrap(~brand, scale = "free_y", ncol = 3) + scale_y_continuous(labels = comma) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          linetype = "dotted"),
        panel.grid.major.y = element_line(color = "lightgrey",
                                          linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank()) +
  scale_color_economist()

#

N = matrix(round(1/tail(kospi_price, 1) * 10^6), ncol = 1)
str(kospi_price)
head(kospi_price)
r = kospi_price %*% N

portfolio = data.frame(date = kospi_five[[1]]$date, price = r)
head(portfolio)

library(dplyr)
portfolio = portfolio %>% arrange(date) %>% mutate(log_return = c(NA, diff(log(price))))
portfolio = portfolio[-1,]
portfolio$squared_logreturn = portfolio$log_return^2
head(portfolio)
summary(portfolio)

plot(price~date, data = portfolio, type = "l")
plot(log_return^2~date, data = portfolio, type = "l", ylim = c(0, 0.01))
ggplot(data = portfolio) +
  geom_line(aes(x = date, y = price, col = "price")) +
  geom_line(aes(x = date, y = squared_logreturn*6*10^8+3.5*10^6, col = "squared_logreturn"), alpha = 0.7) +
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          linetype = "dotted"),
        panel.grid.major.y = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(margin = margin(0, 0, 0, 10))) +
  scale_y_continuous(sec.axis = sec_axis(~./(6*10^8)-(3.5*10^6)/(6*10^8), 
                                         name = "squared log return")) +
  scale_color_manual(values = c("price" = "darkblue", "squared_logreturn" = "orange")) +
  coord_cartesian(ylim = c(3.5*10^6, 6.5*10^6))

par(mfrow = c(1, 2))
acf(portfolio$log_return)
acf(portfolio$log_return^2)




volatility_model = function(data, model = "sGARCH",q = 1, p= 1, dist = "norm"){
  library(rugarch)
  spec = ugarchspec(variance.model = list(model = model, 
                                          garchOrder = c(q, p)),
                    mean.model = list(armaOrder = c(0, 0),include.mean = F),
                    distribution.model = dist)
  fit = ugarchfit(spec, data)
  coef = fit@fit$coef
  sigma = fit@fit$sigma
  criteria = infocriteria(fit)[1:2]
  MSE = mean((data^2 - sigma^2)^2)
  QLIKE = sum(data^2/sigma^2 - 1 - log(data^2/sigma^2))
  criteria = c(criteria, MSE, QLIKE)
  names(criteria) = c("AIC", "BIC", "MSE", "QLIKE")
  return(list(sigma = sigma, residuals = data/sigma,
              coef = coef, criteria = criteria, model = fit))
}
plot_return_sigma = function(data, coef, sigma){
  library(reshape2)
  library(ggplot2)
  portfolio_garch = data
  portfolio_garch$sigma = sigma
  portfolio_garch = melt(portfolio_garch, id.vars = "date", variable.name = "group", value.name = "value")
  
  coef[2:3] = round(coef[2:3], 3)
  coef[1] = round(coef[1], 9)
  if(length(coef) == 3){
    subtitle = bquote(paste(alpha[0] == .(coef[1]), ", ",
                            alpha[1] == .(coef[2]), ", ",
                            beta[0] == .(coef[3])))
  }
  
  
  ggplot(data = portfolio_garch) +
    geom_line(aes(x = date, y = value^2, group = group, col = group),
              size = 0.7) + 
    theme(panel.background = element_rect(fill = "white",
                                          color = "white"),
          panel.grid.major.x = element_line(color = "lightgrey",
                                            linetype = "dotted"),
          panel.grid.major.y = element_line(color = "lightgrey", linetype = "dotted"),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") +
    ylab(NULL) + scale_color_manual(values = c("orange", "darkblue"), label = c("Squared Log return", "squared sigma"))
                            
}



#

fit = volatility_model(portfolio$log_return, model = "sGARCH",
                       1, 1, "std")
plot_return_sigma(portfolio[,-c(2, 4)], fit$coef, fit$sigma)+ coord_cartesian(ylim = c(0, 0.004))

fit$coef


library(nortest)
library(tseries)

fit = volatility_model(portfolio$log_return[701:nrow(portfolio)], model = "gjrGARCH",
                       1, 1, "sstd")

par(mfrow = c(2, 2))
plot(fit$model)
8
9
11
0
par(mfrow = c(1, 1))


fit = volatility_model(portfolio$log_return, model = "eGARCH",
                       1, 1, "std")
fit$coef
lillie.test(fit$residuals)
shapiro.test(fit$residuals)
jarque.bera.test(fit$residuals)
ad.test(fit$residuals)

for(i in seq(5, 30, length = 6)){
  print(Box.test(fit$residuals^2, lag = i, type = "Ljung-Box"))
}
# AIC, BIC
fit$criteria
criteria = matrix(0, nrow = 3, ncol = 4)
model  = c("sGARCH", "gjrGARCH", "eGARCH")
for(i in 1:3){
  fit = volatility_model(portfolio$log_return, model = model[i],
                         1, 1, "std")
  criteria[i,] = fit$criteria
}
rownames(criteria) = model
colnames(criteria) = names(fit$criteria)
fit$criteria
criteria

# 
a = 0
d = matrix(0, nrow = 75, ncol = 4)
name = c()
for(q in 1:5){
  for(p in 1:5){
    for(i in 1:3){
      a = a + 1
      fit = volatility_model(portfolio$log_return, model = model[i], q, p, "std")
      d[a, ] = fit$criteria
      name[a] = paste0(model[i], "(", q, ",", p, ")")
    }
  }
}
colnames(d) = names(fit$criteria)
rownames(d) = name
head(d)
head(d[order(d[,1]),])
fit_1 = volatility_model(portfolio$log_return, model = "sGARCH", 1, 1, "std")
fit_2 = volatility_model(portfolio$log_return, model = "gjrGARCH", 1, 1, "std")
fit_3= volatility_model(portfolio$log_return, model = "eGARCH", 1, 1, "std")
#fit_4 = volatility_model(portfolio$log_return, model = "eGARCH", 5, 3, "std")
criteria
portfolio_garch = portfolio[,-c(2, 4)]
portfolio_garch$garch_1_1 = fit_1$sigma
portfolio_garch$gjrgarch_1_1 = fit_2$sigma
portfolio_garch$egarch_1_1 = fit_3$sigma
#portfolio_garch$egarch_5_3 = fit_4$sigma
portfolio_garch
library(reshape2)
portfolio_garch = melt(portfolio_garch, id.vars = c("date", "log_return"), variable.name = "group", value.name = "value")
str(portfolio_garch)
levels(portfolio_garch$group) = c("Garch(1,1)", "GJR-Garch(1,1)", "E-Garch(1,1)")#, "E-Garch(5, 3)")

ggplot(data = portfolio_garch) +
  geom_line(aes(x = date, y = log_return^2), col = "orange") +
  geom_line(aes(x = date, y = value^2, group = group, col = group),
            size = 0.7) + 
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          linetype = "dotted"),
        panel.grid.major.y = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top") +
  scale_color_manual(values = c("darkgreen","darkblue","darkred")) + ylab(NULL) +
  facet_wrap(~group, ncol = 2) + 
  coord_cartesian(ylim = c(0, 0.004))
  

