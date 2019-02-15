library(tidyverse)
library(readxl)
library(tidyquant)
library(formattable)
options(scipen = 9999)

getwd()

#symbols <- c("GSF.OL", "OSEBX.OL")

#xts
#getSymbols(symbols, src = "yahoo")

#xts <- merge(GSF.OL, OSEBX.OL, join = "fill")


#gsf_adj <- GSF.OL$GSF.OL.Adjusted["2014/"] 


#### read from excel (requires files in working directory)

gsf <- read_excel("C:/Users/Martin/Documents/Økad 6.sem/BED-2032/GSFkursdata.xlsx") %>%
  rename(date = "GSF", GSF = "Siste", Volume = "Totalt omsatt (NOK)", Shares_traded = "Totalt antall aksjer omsatt") %>%
  arrange(date)

str(gsf)
summary(gsf)

gsf %>%
  filter(is.na(GSF)) 
#22. mai 2015 er NA, legger inn en pris på 28 
#for beregne returns og matche osebx for beta
#if newer dfset is downloaded indexing [335] will replace the wrong value.
gsf$GSF[330] <- 28.0 
summary(gsf)

ggplot(gsf, aes(x=date, y=GSF)) +
  geom_line()

#returns_log <- gsf %>%
# tq_mutate(mutate_fun = periodReturn, period = "daily", type = "log")



df <- gsf %>%
  select(date, GSF) %>%
  tq_mutate(mutate_fun = periodReturn, period = "daily", type = "arithmetic")

osebx <- read_excel("C:/Users/Martin/Documents/Økad 6.sem/BED-2032/OSEBXkursdata.xlsx") %>%
  rename(date = "OSEBX", OSEBX = "Siste") %>% 
  arrange(date)

summary(osebx)

df <- osebx %>% 
  tq_transmute(mutate_fun = periodReturn, period = "daily", type = "arithmetic") %>%
  full_join(df, by = "date") %>%
  rename(market.returns = "daily.returns.x", gsf.returns = "daily.returns.y")

df <- df[c(1,3,4,2)]

ggplot(df, aes(x=date, y=gsf.returns)) +
  ggtitle("Arithmetic daily returns") + geom_line()


#choose 1 of the histograms, but which one?
hist(df$gsf.returns, breaks = 30)

b <- ggplot(df, aes(x=gsf.returns))
b + geom_histogram(bins = 30)
b + geom_density()

#Histogram with proportions calculated
r.hist <- hist(df$gsf.returns, breaks = 20, plot=FALSE)
r.hist$counts <- r.hist$counts/sum(r.hist$counts)
plot(r.hist, xlab = "Daily arithmetic returns", ylab = "Proportion")


gsf_sum <- df %>% 
  summarise(ann_mean = mean(gsf.returns)*252,
            ann_sd = sd(gsf.returns) * sqrt(252),
            skewness = skewness(gsf.returns),
            kurtosis = kurtosis(gsf.returns),
            sharpe = (ann_mean - 0.02)/ann_sd, #risk-free in CAPM set to 2%
            beta = cov(gsf.returns, market.returns)/var(market.returns)) 

gsf_sum$ann_mean  <- percent(gsf_sum$ann_mean)
gsf_sum$ann_sd <- percent(gsf_sum$ann_sd)
gsf_sum
#alternative: built-in function for CAPM beta.
tq_performance(df, gsf.returns, market.returns, performance_fun = CAPM.beta)

#todo: discuss summary statistics
#       analyse market position

### CAPM     
#CAPM = Rf + beta * (Rm - Rf)
# Rf = 2%, finn nøyaktig med kilde (mitt anslag)
# Rm = 8% finn nøyaktig med kilde (mitt anslag)
# beta = 0.753

expected.return <- 0.02 + 0.748*(0.08-0.02)
percent(expected.return)

### WACC
#   WACC = EKandel * cost of equity + gjeldsandel * cost of debt *(1-tax)
#   Cost of equity = CAPM
#   cost of debt = vanlig gjeldsrente = 5% (mitt anslag)
#   Equity weight = 0.47 (beregnet)
#   Debt weight = 0.53 (beregnet)

equity <- 0.47
debt <- 0.53
cost_of_debt <- 0.05
cost_of_equity <- expected.return #(CAPM)

WACC <- cost_of_equity*equity + cost_of_debt*debt 
percent(WACC)


#### Comment on liquidity
head(gsf)
ggplot(gsf, aes(x=date, y=Shares_traded)) +
  geom_line()

###Svært likvid, ekstremtilfelle 18.mai hvor Marine Harvest solge omtrent 25% av aksjene.
### Grieg Seafood er en av de 25 mest omsatte aksjene på Oslo Børs. Dermed en del av OBX-indeksen.

