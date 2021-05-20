
library(stringr)
library(ggplot2)
library(dplyr)
brand = c("하이트","제일기획", "농심",  "NHN", "한국제지", "모나미")
p = length(brand)
kospi_five = list()
for(i in 1:p){
  dat = NULL
  for(j in 1:6){
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
head(portfolio)
portfolio[701,]
tail(portfolio)
N = c(42, 52, 3, 14, 34, 253)
r = kospi_price %*% N

portfolio = data.frame(date = kospi_five[[1]]$date, price = r)

portfolio = portfolio %>% arrange(date) %>% mutate(log_return = c(NA, diff(log(price))))
portfolio = portfolio[-1,]
portfolio$squared_logreturn = portfolio$log_return^2

ggplot(data = portfolio[701:nrow(portfolio),]) +
  geom_line(aes(x = date, y = price, col = "price")) +
  geom_line(aes(x = date, y = log_return*2*10^7+5*10^6, col = "log return"), alpha = 0.5) +
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
  scale_y_continuous(sec.axis = sec_axis(~./(2*10^7)-(5*10^6)/(2*10^7), 
                                         name = "log return")) +
  scale_color_manual(values = c("price" = "darkblue", "log return" = "orange")) +
  coord_cartesian(ylim = c(3.5*10^6, 6.5*10^6))
summary(portfolio)

ggplot(data = portfolio[701:nrow(portfolio),]) +
  geom_hline(yintercept = 0, col = "grey") + 
  geom_line(aes(x = date, y = log_return), col = "orange",
            size = 0.7) + 
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          linetype = "dotted"),
        panel.grid.major.y = element_line(color = "lightgrey", linetype = "dotted"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")

# normal, std-t, sstd-t, historical, evt
library(rugarch)

nrow(portfolio)
portfolio[701, ]

sigma*qnorm(0.05)
plot(log_return~date, portfolio[701:nrow(portfolio),], type = "l")
lines(portfolio$date[701:nrow(portfolio)], negative_VaR, col = "red")
lines(portfolio$date[701:nrow(portfolio)], ES, col = "blue")
out = !(VaR < portfolio$log_return)
points(log_return~date, portfolio[out,], col = "blue")

data = portfolio$log_return
alpha = 0.05
model = "gjrGARCH"
dist = "norm"
fit_VaR = function(data, alpha, model = "sGARCH", q = 1, p = 1, dist = "norm",
                   FHS = F, EVT = F){
  library(rugarch)
  library(fGarch)
  library(fExtremes)
  
  spec = ugarchspec(variance.model = list(model = model, 
                                          garchOrder = c(q, p)),
                    mean.model = list(armaOrder = c(0, 0),include.mean = F),
                    distribution.model = dist)
  fit = ugarchfit(spec, data)
  coef = fit@fit$coef
  sigma = fit@fit$sigma
  if(FHS){
    
    residual = data/sigma
    negative_VaR = c()
    ES = c()
    for(i in 701:length(data)){
      quan = quantile(residual[(i-700):(i-1)], alpha)
      negative_VaR[i-700] = sigma[i]*quan
      ES[i-700] = sigma[i]*sum(residual[(i-700):(i-1)] * (residual[(i-700):(i-1)] < quan))/(alpha * 700)
    }
    
    sigma = sigma[701:length(data)]
    
    }else if(EVT){
     
    library(fExtremes)
    gpd_fit = gpdFit(-data/sigma, u = quantile(-data/sigma, 0.95))
    coef = gpd_fit@fit$par.ests
    xi = coef["xi"]
    beta = coef["beta"]
    u = gpd_fit@fit$threshold
    sigma = sigma[701:length(data)]
    negative_VaR = -sigma*(u + (beta/xi)*((0.05/alpha)^xi - 1))
    
  f = function(x){return(u + (beta/xi)*((0.05/x)^xi - 1))}
    ES = -sigma*integrate(f, 0, alpha)$value/alpha
  }else{
    if(dist == "norm"){
      
      sigma = sigma[701:length(data)]
      negative_VaR = sigma*qnorm(alpha)
      f = function(x){return(qnorm(x))}
      ES = -sigma*integrate(f, 1-alpha, 1)$value/alpha
    
      }else if(dist == "std"){
        
      d = coef["shape"]
      sigma = sigma[701:length(data)]
      negative_VaR = sigma*qstd(alpha, nu = d)
      f = function(x){return(qstd(x, nu = d))}
      ES = -sigma*integrate(f, 1-alpha, 1)$value/alpha
      
      }else if(dist == "sstd"){
      
      sigma = sigma[701:length(data)]  
      d = coef["shape"]
      xi = coef["skew"]
      negative_VaR = sigma*qsstd(alpha, nu = d, xi = xi)
      f = function(x){return(qsstd(x, nu= d, xi = xi))}
      ES = -sigma*integrate(f, 1-alpha, 1)$value/alpha
    }
  }
  out = !(negative_VaR < data[701:nrow(portfolio)])
  return(data.frame(negative_VaR = negative_VaR,
                    out = out,
                    ES = ES))
}
plot_make1 = function(fit, alpha){
  library(reshape2)
  library(ggplot2)
  
  portfolio_V_E = data.frame(portfolio[701:nrow(portfolio),c(1, 3)], fit)
  long_p = melt(portfolio_V_E, id.vars = c("date", "log_return", "out"))
  
  p = ggplot() +
    geom_line(data = long_p, aes(x = date, y = log_return), col = "orange") +
    geom_line(data = long_p, aes(x = date, y = value, group = variable, col = variable, linetype = variable)) +
    scale_color_manual(values = c("steelblue", "tomato"), label = c(paste0("Negative ", 100 * alpha, "% VaR"), "ES")) +
    theme(panel.background = element_rect(fill = "white",
                                          color = "white"),
          panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") +
    ylab(paste0("Negative ", 100 * alpha, "% VaR")) +
    guides(linetype = F) +
    coord_cartesian(ylim = c(-0.15, 0.08))
  return(p)
}

plot_make2 = function(fit, alpha){
  library(reshape2)
  library(ggplot2)
  
  portfolio_V_E = data.frame(portfolio[701:nrow(portfolio),c(1, 3)], fit)
  long_p = melt(portfolio_V_E, id.vars = c("date", "log_return", "out"))
  
  p = ggplot() +
    geom_point(data = long_p, aes(x = date, y = log_return), col = "lightgrey") +
    geom_line(data = long_p[long_p$variable == "negative_VaR",], 
              aes(x = date, y = value), col = "steelblue") +
    geom_point(data = long_p[long_p$out,], aes(x = date, y = log_return), col = "tomato", size = 2) +
    theme(panel.background = element_rect(fill = "white",
                                          color = "white"),
          panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") +
    labs(caption = paste0("Number of Violations : ", sum(portfolio_V_E$out))) +
    ylab(paste0("Negative ", 100 * alpha, "% VaR")) +
    coord_cartesian(ylim = c(-0.12, 0.08))
  return(p)
}
library(patchwork)
length(portfolio$log_return[701:nrow(portfolio)])
fit = fit_VaR(portfolio$log_return, alpha = 0.01, model = "gjrGARCH", q = 1, p = 1, dist = "std", EVT = F)


p1 = plot_make2(fit_VaR(portfolio$log_return, alpha = 0.05, model = "sGARCH", q = 1, p = 1, dist = "norm", FHS = F), 
                0.05)  + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  ggtitle(label = NULL, subtitle = "GARCH(1, 1)")

p2 = plot_make2(fit_VaR(portfolio$log_return, alpha = 0.01, model = "sGARCH", q = 1, p = 1, dist = "norm", FHS = F), 
                0.01) + theme(axis.title.x = element_blank(),
                              axis.text.x = element_blank())

p3 = plot_make2(fit_VaR(portfolio$log_return, alpha = 0.005, model = "sGARCH", q = 1, p = 1, dist = "norm", FHS = T), 
                0.005) + theme(axis.title.x = element_blank(),
                              axis.text.x = element_blank())

p4 = plot_make2(fit_VaR(portfolio$log_return, alpha = 0.05, model = "gjrGARCH", q = 1, p = 1, dist = "norm", FHS = T), 
                0.05) + ggtitle(label = NULL, subtitle = "GJR-GARCH(1, 1)")

p5 = plot_make2(fit_VaR(portfolio$log_return, alpha = 0.01, model = "gjrGARCH", q = 1, p = 1, dist = "norm", FHS = T), 
                0.01)

p6 = plot_make2(fit_VaR(portfolio$log_return, alpha = 0.005, model = "gjrGARCH", q = 1, p = 1, dist = "norm", FHS = T), 
                0.005)
(p1 + p2 + p3)/(p4 + p5 + p6)

library(GAS)
model = c("sGARCH", "gjrGARCH")
dist = c("norm", "std", "sstd", "std", "std")
FHS = c(F, F, F, F, T)
EVT = c(F, F, F, T, F)
p = c(0.05, 0.01, 0.005)

LR = matrix(0, nrow = 40, ncol = 3)
pvalue = matrix(0, nrow = 40, ncol = 3)
loss = matrix(0, nrow = 10, ncol = 3)
numv = matrix(0, nrow = 10, ncol = 3)
a=0
for(i in 1:2){
  for(j in 1:5){
    a = a + 1
    for(k in 1:3){
      fit = fit_VaR(portfolio$log_return, alpha = p[k], model = model[i], q = 1, p = 1, dist = dist[j],
                    FHS = FHS[j], EVT = EVT[j])
      back_test = BacktestVaR(data = portfolio$log_return[701:nrow(portfolio)], VaR = fit$negative_VaR, alpha = p[k])
      pvalue[a*4-(3:0), k] = c(back_test$LRuc[2], 1-pchisq(back_test$LRcc[1] - back_test$LRuc[1], 1),
                               back_test$LRcc[2], unlist(back_test$DQ)[2])
      LR[a*4-(3:0), k] = c(back_test$LRuc[1], back_test$LRcc[1] - back_test$LRuc[1],
                               back_test$LRcc[1], unlist(back_test$DQ)[1])
      loss[a, k] = sum(VaRloss(alpha = p[k], actual = portfolio$log_return[701:nrow(portfolio)], VaR = fit$negative_VaR))
      numv[a, k] = sum(fit$out)
    }
  }
}
BacktestVaR(portfolio$log_return[701:nrow(portfolio)], fit$negative_VaR, 0.01)
fit = 
r = paste(rep(model, each = 20), rep(rep(c("norm", "std", "sstd", "EVT", "FHS"), 2), each = 4), rep(c("uc", "ind", "cc", "DQ"), 10))
rownames(LR) = rownames(pvalue) = r
round(pvalue[c(5:16, 25:36),], 3)
r2 = paste(rep(model, each = 5), rep(c("norm", "std", "sstd", "EVT", "FHS"), 2))
rownames(loss) = rownames(numv) = r2
loss
colnames(numv) = 772 * p
numv


