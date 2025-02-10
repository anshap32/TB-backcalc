library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)
library(pander)
library(stargazer)
library(readr)
library(lubridate)
library(dplyr)
library(RColorBrewer)


setwd("/usr3/graduate/anshap/Thesis")


load("project1_mcmc/graphing/output/cam_my.rdata")
load("project1_mcmc/graphing/output/cam_comp.rdata")

load("project1_mcmc/graphing/output/phl_my.rdata")
load("project1_mcmc/graphing/output/phl_comp.rdata")

load("project1_mcmc/graphing/output/viet_my.rdata")
load("project1_mcmc/graphing/output/viet_comp.rdata")

library(scales)


a <- a  + xlab("") + 
  theme(axis.text.x=element_blank())  + 
  xlim(1996, 2017) + #ylim(0, 150000) + 
  scale_y_continuous(labels = label_comma())
a

b <- b  + xlab("")+ 
  theme(axis.text.x=element_blank())  + 
  xlim(1996, 2017) +# ylim(0, 1250000) + 
  scale_y_continuous(labels = label_comma())

c <- c + 
  xlim(c(1996, 2017)) +# ylim(0, 500000) + 
  scale_y_continuous(labels = label_comma()) 
c

d <- d  + ylab("") + xlab("") + ggtitle("(a)")+
  theme(axis.text.x=element_blank())+
  xlim(1999, 2018) #+ ylim(0, 150000)
d
e <- e  + ylab("") + xlab("")+ ggtitle("(b)")+
  theme(axis.text.x=element_blank()) + 
  xlim(1999, 2018) #+ ylim(0, 1250000)

f <- f + ylab("") + ggtitle("(c)")+
  # theme(axis.text.y=element_blank())+
  xlim(1999, 2018)

ggarrange(a, b, c,
          ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom", align = "hv")

ggarrange(d, e, f,
          ncol = 1, nrow = 3, common.legend = TRUE, legend="bottom", align = "hv")

