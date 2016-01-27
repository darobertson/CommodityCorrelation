library(Quandl)
library(xts)
library(PerformanceAnalytics)
library(RColorBrewer)
library(gplots)
library(corrplot)
setwd("~/Desktop/CommodityCorrelation")

# Example of reading in a Quandl dataset to xts.
# Name columns so we know what each holds after joining them together

# ag
ag.tickers <- c("CHRIS/CME_C1", "CHRIS/CME_W1", "CHRIS/CME_S1",
                "CHRIS/CME_EH1", "CHRIS/CME_LB1", "CHRIS/CME_SM1",
                "CHRIS/CME_BO1", "CHRIS/CME_CPO1")
ag.raw <- Quandl(ag.tickers[1], type="xts")[,"Settle"]
for (i in 2:length(ag.tickers)) {
  ag.tmp <- Quandl(ag.tickers[i], type="xts")[,"Settle"]
  ag.raw <- cbind(ag.raw, ag.tmp)
}
colnames(ag.raw) <- c("corn", "wheat", "soybeans", "ethanol", "lumber",
                      "soymeal", "soyoil", "palmoil")

# metals
metals.tickers <- c("CHRIS/CME_GC1", "CHRIS/CME_SI1", "CHRIS/CME_HG1",
                    "CHRIS/SHFE_ZN1", "OFDP/TIN_36", "CHRIS/SHFE_PB1",
                    "CHRIS/SGX_FEF1", "CHRIS/SHFE_RB1")
metals.raw <- Quandl(metals.tickers[1], type="xts")[,"Settle"]
for (i in 2:length(metals.tickers)) {
  if (metals.tickers[i] == "OFDP/TIN_36") {
    metals.tmp <- Quandl(metals.tickers[i], type="xts")[,"Mid"]
  }
  else {
    metals.tmp <- Quandl(metals.tickers[i], type="xts")[,"Settle"]
  }
  metals.raw <- cbind(metals.raw, metals.tmp)
}
colnames(metals.raw) <- c("gold", "silver", "copper", "zinc", "tin", "lead",
                      "iron ore", "steel rebar")

# petro
petro.tickers <- c("CHRIS/CME_CL1", "CHRIS/ICE_B1", "CHRIS/CME_RB1",
                   "CHRIS/CME_HO1", "CHRIS/CME_SE1")
petro.raw <- Quandl(petro.tickers[1], type="xts")[,"Settle"]
for (i in 2:length(petro.tickers)) {
  petro.tmp <- Quandl(petro.tickers[i], type="xts")[,"Settle"]
  petro.raw <- cbind(petro.raw, petro.tmp)
}
colnames(petro.raw) <- c("wti crude", "brent crude", "ny harb rbob gasoline",
                         "ny harbor ulsd", "sing 380cst bunker")

# natgas
natgas.tickers <- c("CHRIS/CME_NG1", "CHRIS/ICE_M1", "CHRIS/CME_B01",
                    "CHRIS/CME_C01", "CHRIS/CME_D01")
natgas.raw <- Quandl(natgas.tickers[1], type="xts")[,"Settle"]
for (i in 2:length(natgas.tickers)) {
  natgas.tmp <- Quandl(natgas.tickers[i], type="xts")[,"Settle"]
  natgas.raw <- cbind(natgas.raw, natgas.tmp)
}
colnames(natgas.raw) <- c("henry hb natgas", "uk natgas", "mt bel propane",
                          "mt bel ethane", "mt bel butane")

# power
power.tickers <- c("CHRIS/CME_N91", "CHRIS/CME_B61")
power.raw <- Quandl(power.tickers[1], type="xts")[,"Settle"]
for (i in 2:length(power.tickers)) {
  power.tmp <- Quandl(power.tickers[i], type="xts")[,"Settle"]
  power.raw <- cbind(power.raw, power.tmp)
}
colnames(power.raw) <- c("pjm wb oprtma 5mw", "pjm n il oprtma 5mw")

# coal
coal.tickers <- c("CHRIS/CME_QL1", "CHRIS/CME_QP1", "CHRIS/ICE_ATW1",
                  "CHRIS/ICE_NCF1", "CHRIS/ICE_AFR1")
coal.raw <- Quandl(coal.tickers[1], type="xts")[,"Settle"]
for (i in 2:length(coal.tickers)) {
  coal.tmp <- Quandl(coal.tickers[i], type="xts")[,"Settle"]
  coal.raw <- cbind(coal.raw, coal.tmp)
}
colnames(coal.raw) <- c("app coal", "powrivbas coal", "ara coal", "newcastle coal",
                        "rich bay coal")

# Now join all of the price datasets together. Then trim them down.
prcdata.full <- cbind(ag.raw, metals.raw, petro.raw, natgas.raw,
                      power.raw, coal.raw)
prcdata.full[prcdata.full == 0] <- NA
retdata.full <- diff(log(prcdata.full))
prcdata <- prcdata.full["20120301/20150301"]
retdata <- retdata.full["20120301/20150301"]


# average daily log returns
avg.daily.ret <- colMeans(retdata, na.rm = TRUE)
#avg.daily.ret <- cbind(avg.daily.ret)

# annual volatilities
annual.vol <- apply(retdata, 2, sd, na.rm=TRUE)*sqrt(250)

# semideviation
semi.dev <- t(SemiDeviation(retdata)*sqrt(250))

# skewness and kurtosis
ret.skewness <- t(skewness(retdata, method="moment"))

ret.kurtosis <- t(kurtosis(retdata, method="moment"))


qthree <- cbind(avg.daily.ret, annual.vol, semi.dev, 
                ret.skewness,ret.kurtosis)

write.csv(qthree, file = "qthreeanswers.csv")

# correlation matrix
cor.matrix <- cor(retdata, use="complete")
rounded <- round(cor.matrix, digits = 3)
col<- colorRampPalette(c("red", "azure3", "azure3", "green"))(4)

corrplot(rounded, type = "upper",
         col = col,
         method = "color")

