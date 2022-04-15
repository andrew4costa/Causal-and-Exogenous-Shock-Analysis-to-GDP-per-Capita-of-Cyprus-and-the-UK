# environment settings
rm(list = ls())
graphics.off()

# packages
library(vars)
library(ggplot2)
library(tseries)
library(dynlm)
library(knitr)
library(broom)
library(readxl)
library(janitor)
library(zoo)

data <- read_excel("Desktop/Milan/Adv.Macro/WID_Data_Metadata/uk_cy.xlsx")
View(data)

data <- row_to_names(data, 1)
View(data)

#data
data$Year <- as.Date(data$Year, format = "%Y")
data$`UK Average National Income` <- as.numeric(as.character(data$`UK Average National Income`))
data$`CY Average National Income` <- as.numeric(as.character(data$`CY Average National Income`))
data$`UK GDP per Capita` <- as.numeric(as.character(data$`UK GDP per Capita`))
data$`CY GDP per Capita` <- as.numeric(as.character(data$`CY GDP per Capita`))
View(data)

# configure data and plot as time series
cy_percap <- ts(data$`CY GDP per Capita`, start=c(1970,1))
uk_percap <- ts(data$`UK GDP per Capita`, start=c(1970,1))
View(cy_percap)

ts.plot(cy_percap, uk_percap, type="l", main = 'Cyprus and'
        lty=c(1,2), col=c(1,2))
legend("topleft", border=NULL, legend=c("CY","UK"), 
       lty=c(1,2), col=c(1,2))

# test for stationarity
adf.test(cy_percap)
adf.test(uk_percap)

# the two series are not of I(1)
adf.test(diff(diff(cy_percap)))
adf.test(diff(diff(uk_percap)))

# test for cointegration
cint1.dyn <- dynlm(cy_percap~uk_percap, data=data)
kable(tidy(cint1.dyn), digits=3,
      caption="The results of the Cointegration")

ehat <- resid(cint1.dyn)
adf.test(ehat)

vec_cy<- dynlm(d(cy_percap)~L(ehat), data=data)
vec_uk <- dynlm(d(uk_percap)~L(ehat), data=data)
tidy(vec_cy)
tidy(vec_uk)

Dcy <- diff(cy_percap)
Duk <- diff(uk_percap)
varmat <- as.matrix(cbind(Dcy,Duk))
varfit <- VAR(varmat)
summary(varfit)
impresp <- irf(varfit)
plot(impresp)
plot(fevd(varfit))
