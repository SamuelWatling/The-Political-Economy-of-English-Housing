setwd("C:/Users/samue/Documents/Processed European Data")

library(tidyverse)
library(readxl)
library(lubridate)
library(rlang)
library(zoo)
library(xts)
library(ggpattern)

Data <- read_excel("Replication Data.xlsx") %>% 
        select(-c("...1")) %>% 
  mutate(across(Population:`Private Housing Ratio`, ~ as.numeric(.x))) %>%
  mutate(PerCapHouse = `Housing Stock (Estimate)`/ Population, PerCapHouseAlt = `Housing Stock (Alternative)`/ Population) %>%
  mutate(Total = `Gross Building` * 100 / `Housing Stock (Estimate)`) %>% 
  mutate(Publicbuild = `Public Housing Ratio` * `Gross Building`, Privatebuild = `Private Housing Ratio` * `Gross Building`) %>%
  mutate(Public = Publicbuild*100 / `Housing Stock (Estimate)`, Private = Privatebuild*100 / `Housing Stock (Estimate)`) %>%
  mutate(Date = ymd(Date)) %>% 
  group_by(Country) %>%
  mutate(PerCapPct = (PerCapHouse - lag(PerCapHouse))*100/ PerCapHouse) %>%
  ungroup() %>%
  mutate(across(Population:`Private Housing Ratio`, ~ signif(.x, digits = 5)))

view(Data)

IrelandGraph <- Data %>% 
  filter(Country %in% c("Ireland", "Sweden", "Unitedkingdom", "Switzerland", "Finland")) %>% 
  mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>%
  filter(Date > as.Date("1969-01-01") & Date < as.Date("2021-01-01")) %>% 
  ggplot(aes(x = Date, y = PerCapHouseAlt, color = Country)) + geom_line(linewidth = 1.2) + 
  scale_y_continuous(limits = c(250, 600)) + 
  geom_vline(xintercept = as.numeric(as.Date("1970-01-01")), linewidth = 1) + 
  geom_hline(yintercept = 250, linewidth = 1) + labs(title = "Homes Per Person in Ireland and Europe from 1970-2020", y = "Homes Per 1,000 People", x = "Year") + 
  scale_color_manual(values = c("#F39207", "#078E51", "#274F9E", "#CF5D9F", "#E6223F")) + 
  theme(panel.background = element_blank(),plot.title = element_text(size = 10)) + scale_x_date(breaks = as.Date(c("1970-01-01", "1980-01-01","1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")),
                                                                                                labels = c(1970, 1980, 1990, 2000, 2010, 2020))
IrelandGraph

ggsave("Figure 11.png", plot = IrelandGraph, width=7.5, height=4, dpi=300)
