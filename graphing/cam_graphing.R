library(nimble)
library(dplyr)
library(readxl)
library(ggplot2)
library(basicMCMCplots)
set.seed(1)

setwd("/projectnb/as-thesis/project1")

dat <- read_excel("updated_country_dat.xlsx") %>%
  filter(Country == "Cambodia",
         Year < 2020)

pops <- read_excel("country_pops.xlsx") %>% 
  filter(Country == "Cambodia", year < 2020)

load("project1_mcmc/cure_models/remove_extra_notifs/by_sex/output/cam_all.RData")
L <- nrow(dat)
k <- 5.85
r <- 2.11
#=====================================
# FEMALES
#=====================================

graph_dat1_f <- as.data.frame(cbind(dat$Year, dat$Notifs_f, "Notifications", rep(NA, L), rep(NA, L)))
graph_dat2_f <- as.data.frame(cbind(dat$Year, mcmc.out$summary$all.chains[1:22, 2], 
                                    "Estimated Incidence", 
                                    mcmc.out$summary$all.chains[1:22, 4], 
                                    mcmc.out$summary$all.chains[1:22, 5]))
names(graph_dat1_f) <- c("year", "count", "type", "low", "high")
names(graph_dat2_f) <- c("year", "count", "type", "low", "high")

graph_dat1_f <- graph_dat1_f %>%
  mutate(year = as.character(year),
         year = as.numeric(year),
         count = as.character(count),
         count = as.numeric(count),
         low = as.character(low),
         low = as.numeric(low),    
         high = as.character(high),
         high = as.numeric(high))

#augment later lambdas and reduce earlier lambdas

graph_dat3_f <- graph_dat2_f %>%
  mutate(year = as.character(year),
         year = as.numeric(year),
         count = as.character(count),
         count = as.numeric(count),    
         low = as.character(low),
         low = as.numeric(low),    
         high = as.character(high),
         high = as.numeric(high),
         pr_year_hi = L-row_number() + 1,
         pr_hi = 1-pgamma(pr_year_hi, k, rate = r),
         aug_count = as.numeric(count)*(1+pr_hi), 
         high = as.numeric(high)*(1+pr_hi), 
         low = as.numeric(low)*(1+pr_hi),
         type = "Estimated Incidence") %>%
  select(-c(count)) %>%
  rename(count = aug_count) %>%
  select(year, count, type, low, high)

graph_dat_f <- rbind(graph_dat1_f, graph_dat3_f) %>%
  mutate(type = as.factor(type)) %>% 
  filter(year < 2018)

graph_dat_f$type <- relevel(graph_dat_f$type, "Estimated Incidence")

#=====================================
# MALES
#=====================================
graph_dat1_m <- as.data.frame(cbind(dat$Year, dat$Notifs_m, "Notifications", rep(NA, L), rep(NA, L)))
graph_dat2_m <- as.data.frame(cbind(dat$Year, mcmc.out$summary$all.chains[23:44, 2], 
                                    "Estimated Incidence", 
                                    mcmc.out$summary$all.chains[23:44, 4], 
                                    mcmc.out$summary$all.chains[23:44, 5]))
names(graph_dat1_m) <- c("year", "count", "type", "low", "high")
names(graph_dat2_m) <- c("year", "count", "type", "low", "high")

graph_dat1_m <- graph_dat1_m %>%
  mutate(year = as.character(year),
         year = as.numeric(year),
         count = as.character(count),
         count = as.numeric(count),
         low = as.character(low),
         low = as.numeric(low),    
         high = as.character(high),
         high = as.numeric(high))

#augment later lambdas and reduce earlier lambdas

graph_dat3_m <- graph_dat2_m %>%
  mutate(year = as.character(year),
         year = as.numeric(year),
         count = as.character(count),
         count = as.numeric(count),    
         low = as.character(low),
         low = as.numeric(low),    
         high = as.character(high),
         high = as.numeric(high),
         pr_year_hi = L-row_number() + 1,
         pr_hi = 1-pgamma(pr_year_hi, k, rate = r),
         aug_count = as.numeric(count)*(1+pr_hi), 
         high = as.numeric(high)*(1+pr_hi), 
         low = as.numeric(low)*(1+pr_hi),
         type = "Estimated Incidence") %>%
  select(-c(count)) %>%
  rename(count = aug_count) %>%
  select(year, count, type, low, high)

graph_dat_m <- rbind(graph_dat1_m, graph_dat3_m) %>%
  mutate(type = as.factor(type)) %>% 
  filter(year < 2018)

graph_dat_m$type <- relevel(graph_dat_m$type, "Estimated Incidence")

#=====================================
# get CI bounds for total incidence
#=====================================
graph_dat1_all <- as.data.frame(cbind(dat$Year, dat$Notifications, "Notifications", rep(NA, L), rep(NA, L)))
graph_dat2_all <- as.data.frame(cbind(dat$Year, mcmc.out$summary$all.chains[45:66, 2], 
                                    "Estimated Incidence", 
                                    mcmc.out$summary$all.chains[45:66, 4], 
                                    mcmc.out$summary$all.chains[45:66, 5]))
names(graph_dat1_all) <- c("year", "count", "type", "low", "high")
names(graph_dat2_all) <- c("year", "count", "type", "low", "high")

graph_dat1_all <- graph_dat1_all %>%
  mutate(year = as.character(year),
         year = as.numeric(year),
         count = as.character(count),
         count = as.numeric(count),
         low = as.character(low),
         low = as.numeric(low),    
         high = as.character(high),
         high = as.numeric(high))

#augment later lambdas and reduce earlier lambdas

graph_dat3_all <- graph_dat2_all %>%
  mutate(year = as.character(year),
         year = as.numeric(year),
         count = as.character(count),
         count = as.numeric(count),    
         low = as.character(low),
         low = as.numeric(low),    
         high = as.character(high),
         high = as.numeric(high),
         pr_year_hi = L-row_number() + 1,
         pr_hi = 1-pgamma(pr_year_hi, k, rate = r),
         aug_count = as.numeric(count)*(1+pr_hi), 
         high = as.numeric(high)*(1+pr_hi), 
         low = as.numeric(low)*(1+pr_hi),
         type = "Estimated Incidence") %>%
  select(-c(count)) %>%
  rename(count = aug_count) %>%
  select(year, count, type, low, high)

graph_dat_all <- rbind(graph_dat1_all, graph_dat3_all) %>%
  mutate(type = as.factor(type)) %>% 
  filter(year < 2018)


#================================================================
# MAKE OUTPUT
#================================================================
graph_dat_m2 <- graph_dat_m %>%
  mutate(type = as.character(type), 
         cat = "Males") 

graph_dat_f2 <- graph_dat_f %>%
  mutate(type = as.character(type), 
         cat = "Females") 

graph_dat_all2 <- graph_dat_all %>%
  mutate(type = as.character(type), 
         cat = "Total") 

graph_all <- rbind(graph_dat_all2, graph_dat_m2, graph_dat_f2)


# add on pops
library(tidyr)

pops1 <- pops %>% select(year, pop_m, pop_f, pop_t) %>% 
  pivot_longer(
    cols = pop_m:pop_t) %>%
  mutate(cat = ifelse(name == "pop_f", "Females", 
                      ifelse(name == "pop_m", "Males", "Total"))) %>%
  select(-c(name)) %>%
  rename(pop = value) %>%
  mutate(year = as.numeric(year))

graph_all <- graph_all %>% 
  left_join(pops1) %>%
  mutate(count_per100k = (count/pop)*100000, 
         low_per100k = (low/pop)*100000,
         high_per100k = (high/pop)*100000)



library(scales)
graph_all$cat <- as.factor(graph_all$cat)
graph_all$cat <- relevel(graph_all$cat, ref = "Total")
graph_all$year <- as.numeric(graph_all$year)


a <- ggplot(graph_all) + 
  geom_line(aes(x = year, y = count_per100k, color = cat, linetype = type, 
                group=interaction(cat, type)), linewidth = 1.2) + 
  geom_errorbar(aes(x = year, ymin=low_per100k, ymax=high_per100k, width=0.2, 
                    group=cat, color = cat), linewidth = 1.2)+
  labs(title = "(a)", x = "Year", y = "Count per 100,000 population") + 
  theme_bw()+ 
  theme(axis.text.x = element_text( color = "black", size = 16), 
        axis.text.y = element_text(color = "black", size = 16), 
        axis.title.x = element_text(color = "black", size = 16), 
        axis.title.y = element_text(color = "black", size = 16), 
        plot.title = element_text(color = "black", size = 16), 
        legend.text = element_text(color = "black", size = 16)) + 
  theme(legend.position="bottom", legend.title=element_blank()) + 
  scale_y_continuous(label=comma) +   scale_color_brewer(palette = "Dark2")
a

save(a, file = "project1_mcmc/graphing/output/cam_my.rdata")

## make table

tab_all <- graph_all %>%
  filter(type == "Estimated Incidence", cat == "Total") %>%
  mutate_if(is.numeric, round) %>%
  mutate(final_all = paste0(count, " (", low, ", ", high, ")"), 
         final_all_100k = paste0(count_per100k, " (", low_per100k, ", ", high_per100k, ")")) %>%
   # select(year, final_all, final_all_100k)
  select(year, count, count_per100k)

tab_m <- graph_all %>%
  filter(type == "Estimated Incidence", cat == "Males") %>%
  mutate_if(is.numeric, round) %>%
  mutate(final_m = paste0(count, " (", low, ", ", high, ")"), 
         final_m_100k = paste0(count_per100k, " (", low_per100k, ", ", high_per100k, ")")) %>%
  select(year, final_m, final_m_100k)

tab_w <- graph_all %>%
  filter(type == "Estimated Incidence", cat == "Females") %>%
  mutate_if(is.numeric, round) %>%
  mutate(final_w = paste0(count, " (", low, ", ", high, ")"), 
         final_w_100k = paste0(count_per100k, " (", low_per100k, ", ", high_per100k, ")")) %>%
  select(year, final_w, final_w_100k)

ratio <- as.data.frame(cbind(dat$Year, mcmc.out$summary$all.chains[67:88, 2], 
                                       mcmc.out$summary$all.chains[67:88, 4], 
                                       mcmc.out$summary$all.chains[67:88, 5]))
names(ratio) <- c("year", "count", "low", "high")
tab_r <- ratio %>%
  filter(year <= 2017) %>%
  mutate(count = round(count, 2), 
         low = round(low,2), 
         high = round(high, 2),
         final_r = paste0(count, " (", low, ", ", high, ")")) %>%
  # select(year, final_r)
  select(year, count)

tab <- left_join(tab_all, tab_m, by = "year") %>% left_join(tab_w, by = "year") %>% left_join(tab_r, by = "year")

# View(tab_r)
# View(tab_all)
comb_dat <- graph_all %>%
  filter(type == "Estimated Incidence", cat == "Total") %>%
  mutate(type = "Backcalculation Estimated Incidence")%>%
  select(year, count_per100k, type, low_per100k, high_per100k)


who <- read_excel("who_inc_estimates.xlsx") %>% 
  mutate(year = as.character(year)) %>%
  filter(country == "Cambodia", 
         year < 2018) %>%
  rename(count_per100k=e_inc_100k, 
         low_per100k = e_inc_100k_lo, 
         high_per100k = e_inc_100k_hi) %>%
  mutate(type = "WHO Estimated Incidence") %>%
  select(year, count_per100k, type, low_per100k, high_per100k)

who_bounds <- rbind(who, comb_dat)  %>%
  filter(year < 2018, year >1999)

who_bounds$type <- as.factor(who_bounds$type)
who_bounds$type <- relevel(who_bounds$type, ref = "Backcalculation Estimated Incidence")
who_bounds$year <- as.numeric(who_bounds$year)

d <- ggplot(who_bounds) + 
  geom_line(aes(x = year, y = count_per100k, group = type, color = type), linewidth = 1.2) + 
  geom_errorbar(aes(x = year, ymin=low_per100k, ymax=high_per100k,width=0.2, 
                    group = type, color = type), linewidth = 1.2)+
  labs(title = "(d)", x = "Year", y = "Count") + 
  theme_bw() + 
  theme(legend.position="bottom")+ 
  theme(axis.text.x = element_text( color = "black", size = 16), 
        axis.text.y = element_text(color = "black", size = 16), 
        axis.title.x = element_text(color = "black", size = 16), 
        axis.title.y = element_text(color = "black", size = 16), 
        plot.title = element_text(color = "black", size = 16), 
        legend.text = element_text(color = "black", size = 16)) + 
  theme(legend.position="bottom", legend.title=element_blank()) + 
  theme(legend.position="bottom", legend.title = element_blank())+ 
  scale_color_manual(values = c("#1B9E77", "navy")) +
  scale_y_continuous(label=comma)

d
save(d, file = "project1_mcmc/graphing/output/cam_comp.rdata")
