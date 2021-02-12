library(TTR)
library(tidyverse)
data(ttrc)
ttrc %>% as.tibble
Sys.setlocale("LC_ALL", "cht")
stochOSC <- stoch(ttrc[, c("High", "Low", "Close")])
stochWPR <- WPR(ttrc[, c("High", "Low", "Close")])
plot(tail(stochOSC[, "fastK"], 100), type = "l",
main = "Fast %K and Williams %R", ylab = "",
ylim = range(cbind(stochOSC, stochWPR), na.rm = TRUE))
lines(tail(stochWPR, 100), col = "blue")
lines(tail(1 - stochWPR, 100), col = "red", lty = "dashed")
ttrc %>% class
#%>% .[(nrow(.) - 8):nrow(.),])

KDold <- (Price %>% arrange(ら戳) %>% select(程蔼基, 程基, Μ絃基) %>% rename(High = 程蔼基, Low = 程基, Close = Μ絃基)) %>% stoch(., nFastK = 9)
KDNew <- (Price %>% select(-K,-FD,-D) %>%  rbind(June) %>% CleanPrice %>% arrange(ら戳) %>% select(程蔼基, 程基, Μ絃基) %>% rename(High = 程蔼基, Low = 程基, Close = Μ絃基)) %>% stoch(., nFastK = 9)
Price$K <- KD[, 1]
Price$FD <- KD[, 2]
Price$D <- KD[, 3]
KD[1,]