setwd("C:/Users/samue/Documents/English Housing Processed/Data Output")

library(broom)
library(lfe)
library(geojsonio)
library(sf)
library(sp)
library(rgdal)
library(raster)
library(tidyverse) 
library(haven)
library(readxl)
library(janitor)
library(lubridate)
library(janitor)
library(haven) 
library(stargazer)
library(tidyr) 
library(fixest) 
library(tmap)
library(plm)

options(scipen=999)

Stata <- read_dta("Stata Data 2.dta") %>% 
         mutate(Year = ymd(Year)) %>% 
         mutate(Period = ifelse(Year == as.Date("1975-01-01"), ty,  
                         ifelse(Year >= as.Date("1976-01-01") & Year <= as.Date("1979-01-01"), 1979, 
                         ifelse(Year >= as.Date("1980-01-01") & Year <= as.Date("1983-01-01"), 1983,  
                         ifelse(Year >= as.Date("1984-01-01") & Year <= as.Date("1987-01-01"), 1987, 
                         ifelse(Year >= as.Date("1988-01-01") & Year <= as.Date("1991-01-01"), 1991, 
                         ifelse(Year >= as.Date("1992-01-01") & Year <= as.Date("1995-01-01"), 1995, 
                         ifelse(Year >= as.Date("1996-01-01") & Year <= as.Date("1999-01-01"), 1999, Period)))))))) %>% 
                         mutate(FinalShare_Con = ifelse(Year == as.Date("1975-01-01") | Year == as.Date("1979-01-01") | 
                             Year == as.Date("1983-01-01") | Year == as.Date("1987-01-01") | 
                             Year == as.Date("1991-01-01") | Year == as.Date("1995-01-01") |
                             Year == as.Date("1999-01-01") , Con_Share, NA)) %>% 
                         mutate(FinalShare_Lab = ifelse(Year == as.Date("1975-01-01") | Year == as.Date("1979-01-01") | 
                                   Year == as.Date("1983-01-01") | Year == as.Date("1987-01-01") | 
                                   Year == as.Date("1991-01-01") | Year == as.Date("1995-01-01") |
                                   Year == as.Date("1999-01-01") , Lab_Share, NA)) %>% 
                         mutate(FinalShare_Lib = ifelse(Year == as.Date("1975-01-01") | Year == as.Date("1979-01-01") | 
                                   Year == as.Date("1983-01-01") | Year == as.Date("1987-01-01") | 
                                   Year == as.Date("1991-01-01") | Year == as.Date("1995-01-01") |
                                   Year == as.Date("1999-01-01") , Lib_Share, NA)) %>% 
                         mutate(FinalShare_Nat = ifelse(Year == as.Date("1975-01-01") | Year == as.Date("1979-01-01") | 
                                   Year == as.Date("1983-01-01") | Year == as.Date("1987-01-01") | 
                                   Year == as.Date("1991-01-01") | Year == as.Date("1995-01-01") |
                                   Year == as.Date("1999-01-01") , Nat_Share, NA)) %>% 
                         group_by(District) %>% 
                         fill(FinalShare_Con, .direction = "up") %>%
                         fill(FinalShare_Lab, .direction = "up") %>% 
                         fill(FinalShare_Lib, .direction = "up") %>% 
                         fill(FinalShare_Nat, .direction = "up") %>% 
                         mutate(SocialHouseShareDiff = NonProfitHousingStock_Share - lag(NonProfitHousingStock_Share)) %>% 
                         mutate(SocialHouseShareDiff = lead(SocialHouseShareDiff)) %>% 
                         mutate(NetSocialHouse = NonProfitHousingStock - lag(NonProfitHousingStock)) %>% 
                         mutate(NetSocialHouse = lead(NetSocialHouse)) %>%
                         mutate(SocialHouseRate = NetSocialHouse *100 / TotalHousingStock) %>% 
                         mutate(SocialHouseBuild = (`HousingAssociations` + `LocalAuthority`) *100/ TotalHousingStock) %>% 
                         mutate(SocialHouseSaleRatio = RighttoBuySales*100 / NonProfitHousingStock ) %>% 
                         mutate(CumuHouseShareDiff = cumsum(SocialHouseShareDiff)) %>% 
                         mutate(HouseShareDiff79 = ifelse(Year == as.Date("1979-01-01"), CumuHouseShareDiff, NA)) %>% 
                         fill(HouseShareDiff79, .direction = "downup") %>% 
                         mutate(AdjHouseShareDiff = (CumuHouseShareDiff - HouseShareDiff79)) %>% 
                         mutate(OwnerOccupiedShareDiff = OwnerOccupiedStock_Share - lag(OwnerOccupiedStock_Share)) %>% 
                         mutate(OwnerOccupiedShareDiff = lead(OwnerOccupiedShareDiff)) %>% 
                         mutate(CumuOwnerShareDiff = cumsum(OwnerOccupiedShareDiff)) %>% 
                         mutate(OwnerShareDiff79 = ifelse(Year == as.Date("1979-01-01"), CumuOwnerShareDiff, NA)) %>% 
                         fill(OwnerShareDiff79, .direction = "downup") %>% 
                         mutate(AdjOwnerShareDiff = CumuOwnerShareDiff - OwnerShareDiff79) %>% 
                         mutate(CumuHouseSales = cumsum(RighttoBuySales)) %>% 
                         mutate(HouseSaleShare = CumuHouseSales / TotalHousingStock) %>%
                         mutate(HouseSales79 = ifelse(Year == as.Date("1979-01-01"), HouseSaleShare, NA)) %>% 
                         fill(HouseSales79, .direction = "downup") %>% 
                         mutate(AdjHouseSales = HouseSaleShare - HouseSales79 )
  

view(Stata) 

StataSum <- Stata %>% 
            group_by(Year, Council_Control) %>% 
            mutate(PctSales = 100 * PctSales) %>%
            dplyr::select(c("rindex2":"llds", "PctSales", "OwnerOccupiedStock_Share","NonProfitHousingStock_Share", "SocialHouseShareDiff":"SocialHouseSaleRatio")) %>%
            summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
            filter(Council_Control != "Lib" & Council_Control != "Nat")

view(StataSum) 

StataName <- colnames(StataSum)
StataNames <- StataName[-(1:2)]
StataNames

Stata80Sum <- Stata %>% 
  group_by(Year, Council_Control80) %>% 
  mutate(Council_Control80 = gsub("Con", "Conservative", Council_Control80), Council_Control80 = gsub("Lab", "Labour", Council_Control80)) %>% 
  mutate(PctSales = 100 * PctSales) %>%
  dplyr::select(c("rindex2":"llds", "PctSales", "OwnerOccupiedStock_Share", "NonProfitHousingStock_Share", "SocialHouseShareDiff":"SocialHouseSaleRatio")) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  filter(Council_Control80 != "Lib" & Council_Control80 != "Nat") %>% 
  mutate(SocialHouseShareDiff = SocialHouseShareDiff*100)

view(Stata80Sum) 

setwd("C:/Users/samue/Documents/English Housing Processed/Graphics Output/Council Comparisons")
CompGraph <- function(Data, Group, Variable) { 
  Group <- enquo(Group)
  Variable <- enquo(Variable) 
  p <- Data %>% 
       dplyr::select(c("Year", !!Group, !!Variable)) %>% 
       ggplot(aes(x = Year, y = !!Variable)) + geom_line(aes(color = !!Group), linewidth = 1) + 
       geom_point(aes(color = !!Group)) + 
       geom_vline(xintercept = as.Date("1974-01-01"), linewidth = 1 ) + 
       scale_colour_manual(values = c("#0087DC","#E4003B" ,"black")) + 
       geom_vline(xintercept = as.Date("1980-01-01"), linewidth = 0.7, linetype = 2) 
}

StataSum %>% 
CompGraph(Council_Control, realhp) 

StataSum %>% 
CompGraph(Council_Control, OwnerOccupiedStock_Share) 

StataSum %>% 
  CompGraph(Council_Control, NonProfitHousingStock_Share) 


StataSum %>% 
CompGraph(Council_Control, rindex2) 

StataSum %>% 
CompGraph(Council_Control, male_earn_real)

StataSum %>% 
  CompGraph(Council_Control, pred_emp71)

StataSum %>% 
  CompGraph(Council_Control, llds) 

setwd("C:/Users/samue/Documents/English Housing Processed/Graphics Output/1980 Council Comparisons")

Stata80Sum %>% 
  CompGraph(Council_Control80, OwnerOccupiedStock_Share) 

StockGraph <- Stata80Sum %>% 
  mutate(NonProfitHousingStock_Share = 100 * NonProfitHousingStock_Share) %>%
  CompGraph(Council_Control80,NonProfitHousingStock_Share)  + 
  geom_hline(yintercept = 0, linewidth = 1) + 
  scale_y_continuous(limits = c(0,50)) + labs(y = "Non-Profit % of the Housing Stock", color = "Council Control\nin 1980", 
                                              title = "Share of Non-Profit Housing in English Councils") + 
  geom_text(label = "1980 Housing\nAct", x = as.Date("1975-01-01"), y = 35 , size = 3, lineheight = 0.9, hjust = 0)
  
StockGraph 

ggsave("StockPlot.png", plot = StockGraph, width=7, height=4, dpi=300)

Stata80Sum %>% 
  CompGraph(Council_Control80, SocialHouseShareDiff) 

Stata80Sum %>% 
  CompGraph(Council_Control80, SocialHouseRate) 

Stata80Sum %>% 
  CompGraph(Council_Control80, SocialHouseBuild) 

Graph <-Stata80Sum %>% 
  CompGraph(Council_Control80, PctSales) + geom_text(label = "1980 Housing\nAct", x = as.Date("1975-01-01"), y = 1.5 , size = 3, lineheight = 0.9, hjust = 0) +
  geom_hline(yintercept = 0, linewidth = 1) + labs(title = "Sales of Local Authority Housing", y = "Annual Sales as % of the Housing Stock" , color = "Council Control\nin 1980") + 
  theme(plot.title = element_text(hjust = 0.5))

Graph 

Graph2 <-Stata80Sum %>% 
  CompGraph(Council_Control80, SocialHouseSaleRatio) + geom_text(label = "1980 Housing\nAct", x = as.Date("1975-01-01"), y = 1.5 , size = 3, lineheight = 0.9, hjust = 0) +
  geom_hline(yintercept = 0, linewidth = 1) + labs(title = "Sales of Local Authority Housing", y = "Annual Sales as % of the Social Housing Stock" , color = "Council Control\nin 1980") + 
  theme(plot.title = element_text(hjust = 0.5))

Graph2

ggsave("SalesPlot.png", plot = Graph, width=7, height=4, dpi=300)
ggsave("SalesPctPlot.png", plot = Graph2, width=7, height=4, dpi=300)

StataReg <- Stata %>% 
            mutate(Tot_shift = ifelse(Year == as.Date("1975-01-01"), 1, Tot_shift)) %>%
            mutate(Totalwards = ifelse(Year == as.Date("1975-01-01"), 1, Totalwards)) %>%
            filter(Tot_shift > 0 & Totalwards > 0) %>% 
            group_by(District, Period) %>% 
            dplyr::select(c("Year", "Totalwards":"Electorate","realhp":"NonProfitHousingStock", 
                     "lrhp":"Nat_Share", "PctSales":"AdjHouseShareDiff", "AdjOwnerShareDiff", "AdjHouseSales")) 

view(StataReg)

Filt74 <- StataReg %>% 
          filter(Year == as.Date("1975-01-01")) %>% 
          dplyr::select(-c("Year"))

view(Filt74)
            
RegSum <- StataReg %>% 
          filter(Period != 1974 & Period != 1975 & Period != 2002) %>% 
          summarise(across(where(is.numeric), ~ weighted.mean(.x, Totalcontestedwards))) %>% 
          rbind(Filt74) %>% 
          arrange(District, Period)  %>% 
          mutate(RTBStockChange = RTB *AdjHouseShareDiff) %>% 
          mutate(RTBOwnerChange = RTB *AdjOwnerShareDiff) %>% 
          mutate(RTBSaleChange = RTB* AdjHouseSales) %>% 
  mutate(lmale_earn_real = log(male_earn_real))%>%
  mutate(lrhp = log(realhp)) %>% 
  mutate(llds = log(pred_emp71))
  
view(RegSum) 

Con_plm <- plm(FinalShare_Con ~ RTBOwnerChange + lrhp + lmale_earn_real + llds, 
               data = RegSum, 
               index = c("District", "Period"), 
               model = "within", 
               effect = "twoways")

r.squared(Con_plm)

Lab_plm <- plm(FinalShare_Lab ~ RTBOwnerChange + lrhp + lmale_earn_real + llds, 
               data = RegSum, 
               index = c("District", "Period"), 
               model = "within", 
               effect = "twoways")

r.squared(Lab_plm)

Lib_plm <- plm(FinalShare_Lib ~ RTBOwnerChange + lrhp + lmale_earn_real + llds, 
               data = RegSum, 
               index = c("District", "Period"), 
               model = "within", 
               effect = "twoways")

r.squared(Lib_plm)

Ind_plm <- plm(FinalShare_Nat ~ RTBOwnerChange + lrhp + lmale_earn_real + llds, 
               data = RegSum, 
               index = c("District", "Period"), 
               model = "within", 
               effect = "twoways")

r.squared(Ind_plm)

FixedRegLib <- felm(FinalShare_Lib ~ RTBStockChange | District + Period| 0 | District, RegSum) 
summary(FixedRegLib)

FixedRegNat <- felm(FinalShare_Nat ~ RTBStockChange | District + Period| 0 | District, RegSum)
summary(FixedRegNat)

FixedRegConCont <- felm(FinalShare_Con ~ RTBStockChange + lrhp + lmale_earn_real + llds | District + Period| 0 | District, RegSum) 
summary(FixedRegConCont) 

FixedRegLabCont <- felm(FinalShare_Lab ~ RTBStockChange + lrhp + lmale_earn_real + llds | District + Period| 0 | District, RegSum)

summary(FixedRegLabCont)

FixedRegLibCont <- felm(FinalShare_Lib ~ RTBStockChange + lrhp + lmale_earn_real + llds | District + Period| 0 | District, RegSum) 

summary(FixedRegLibCont)

FixedRegNatCont <- felm(FinalShare_Nat ~ RTBStockChange + lrhp + lmale_earn_real + llds | District + Period| 0 | District, RegSum)
summary(FixedRegNatCont)

TidyCon <- tidy(FixedRegConCont)

view(TidyCon)

results <- list()

Variable <- c("FinalShare_Con", "FinalShare_Lab", "FinalShare_Lib", "FinalShare_Nat")

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBStockChange | District + Period| 0 | District"))
  model <- felm(formula, RegSum) 
  results[[i]] <- model
} 


setwd("C:/Users/samue/Documents/English Housing Processed/Regression Output")
stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFE.tex") 

results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBStockChange + lrhp + lmale_earn_real + llds | District + Period| 0 | District"))
  model <- felm(formula, RegSum) 
  results[[i]] <- model
}

results[["FinalShare_Con"]] 


stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFEControl.tex") 

results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBSaleChange | District + Period| 0 | District"))
  model <- felm(formula, RegSum) 
  results[[i]] <- model
} 

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFESale.tex") 

results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBSaleChange + lrhp + lmale_earn_real + llds | District + Period| 0 | District"))
  model <- felm(formula, RegSum) 
  results[[i]] <- model
}

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFESaleCont.tex") 

#Hetergenouseffects

Regfilt <- Stata %>% 
           filter(Year == as.Date("1980-01-01")) %>% 
           mutate(Lab_80 = replace_na(Lab_80, 0)) %>% 
           mutate(Con_80 = replace_na(Con_80, 0)) %>% 
           mutate(NOC_80 = ifelse(Council_Control80 == "NOC", 1, 0)) %>%
           ungroup() %>%
           mutate(Income = median(male_earn_real)) %>% 
           mutate(Wealth = ifelse(male_earn_real > Income, 1, 0)) %>% 
           mutate(Urb = median(pdevel90_m2)) %>% 
           mutate(City = ifelse(pdevel90_m2 > Urb, 1, 0)) %>% 
           select(c("District","Lab_80", "Con_80", "NOC_80", "Wealth", "City")) %>% 
           right_join(RegSum, by = c("District" = "District"))
           
view(Regfilt) 

results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBStockChange * NOC_80 + lrhp + lmale_earn_real + llds | District + Period| 0 | District"))
  model <- felm(formula, Regfilt) 
  results[[i]] <- model
} 

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFESaleContIntNOC.tex") 

results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBStockChange * Con_80 + lrhp + lmale_earn_real + llds | District + Period| 0 | District"))
  model <- felm(formula, Regfilt) 
  results[[i]] <- model
} 

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFESaleContIntCon.tex") 

results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBStockChange * Lab_80 + lrhp + lmale_earn_real + llds | District + Period| 0 | District"))
  model <- felm(formula, Regfilt) 
  results[[i]] <- model
} 

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFESaleContIntLab.tex") 

results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBStockChange * Wealth + lrhp + lmale_earn_real + llds | District + Period| 0 | District"))
  model <- felm(formula, Regfilt) 
  results[[i]] <- model
} 

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFESaleContIntWealth.tex") 

results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBStockChange * pdevel90_m2 + lrhp + lmale_earn_real + llds | District + Period| 0 | District"))
  model <- felm(formula, Regfilt) 
  results[[i]] <- model
} 

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFESaleContIntCity.tex") 
  
#RobustnessChecks 

EventStudy <- RegSum %>%  
              mutate(time_to_treat = (Period - 1983)/4) 

view(EventStudy)
               
names(EventStudy) 

DataShare <- data.frame(term = character(),estimate = double(), std.error = double(), statistic = double(), p.value = double(), 
                         Variable = character())

for (i in Variable) { 
  formula <- as.formula(paste0(i, "~ i(time_to_treat, AdjHouseShareDiff, ref = -1) + lrhp + llds + lmale_earn_real | District + Period"))
  model <- feols(formula, cluster = ~ District, data = EventStudy)
  Tidy <- tidy(model) %>%
          mutate(Variable = i) 
  DataShare <- DataShare %>% rbind(Tidy)
}
  
view(DataShare)

twfe_bind <- data.frame(Variable) %>% 
             mutate(term = -1) %>% 
             mutate(estimate = 0) %>% 
             mutate(std.error = 0) %>% 
             mutate(statistic = 0) %>% 
             mutate(p.value = 0) 

view(twfe_bind)

clean_twfe <- DataShare %>% 
              mutate(term = gsub("time_to_treat", "", term)) %>% 
              mutate(term = gsub("AdjHouseShareDiff", "", term)) %>% 
              mutate(term = gsub("\\:", "", term)) %>% 
              mutate(term = as.numeric(term)) %>% 
              filter(!is.na(term)) %>% 
              rbind(twfe_bind) %>% 
              arrange(term) %>% 
              arrange(Variable) %>%
              mutate(se = 1.96 * std.error) %>% 
  mutate(Variable = gsub("FinalShare_Con", "Conservative Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Lab", "Labour Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Lib", "Liberal Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Nat", "Independent Share", Variable)) 
              
view(clean_twfe) 

clean_twfe$Variable <- factor(clean_twfe$Variable, levels = c("Conservative Share", "Labour Share", "Liberal Share", "Independent Share")) 

SumEvent <- clean_twfe %>% 
            dplyr::select(-c("std.error", "statistic")) %>%
            mutate(estimate = round(estimate, digits = 4)) %>% 
            mutate(se = round(se, digits = 4)) %>% 
            mutate(se = round(se, digits = 4)) %>% 
            mutate(se = gsub("^", "\\(", se)) %>% 
            mutate(se = gsub("$", "\\)", se)) %>%
            mutate(estimate = ifelse(p.value < 0.1,  gsub("$", "\\*", estimate), estimate)) %>%
            mutate(estimate = ifelse(p.value < 0.05,  gsub("$", "\\*", estimate), estimate)) %>%        
            mutate(estimate = ifelse(p.value < 0.01,  gsub("$", "\\*", estimate), estimate)) %>% 
            dplyr::select(-c("p.value")) %>% 
            mutate(se = as.character(se)) %>%
            pivot_longer(-c("term", "Variable"), names_to = "Item", values_to = "Value") %>% 
            pivot_wider(names_from = Variable, values_from = Value) %>% 
            filter(term != -1) %>% 
            mutate(term = term * 4 + 1983) %>% 
            rename("Period" = "term") %>% 
            mutate(Period = ifelse(Item == "se", "", Period)) %>% 
            rename(" " = "Item")

view(SumEvent) 

stargazer(SumEvent, summary=FALSE, rownames=FALSE, out = "EventStudy.tex")

graph_twfe <- clean_twfe %>% 
              ggplot(aes(x = term, y = estimate)) + geom_point() + 
              geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), width= .2,
                                          position=position_dodge(.9)) + 
              geom_hline(yintercept = 0) + geom_vline(xintercept = -1, linetype = 2) + 
              theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) + 
              labs(y = "Coefficient for Council Share and Period ", x = "Time to Treatment", title = "Event Study - Social Housing Share") + 
              scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4)) +
              facet_wrap(vars(Variable))
    
graph_twfe 

DataShare <- data.frame(term = character(),estimate = double(), std.error = double(), statistic = double(), p.value = double(), 
                        Variable = character())

for (i in Variable) { 
  formula <- as.formula(paste0(i, "~ i(time_to_treat, AdjHouseSales, ref = -1) + lrhp + llds + lmale_earn_real | District + Period"))
  model <- feols(formula, cluster = ~ District, data = EventStudy)
  Tidy <- tidy(model) %>%
    mutate(Variable = i) 
  DataShare <- DataShare %>% rbind(Tidy)
}

clean_twfe <- DataShare %>% 
  mutate(term = gsub("time_to_treat", "", term)) %>% 
  mutate(term = gsub("AdjHouseSales", "", term)) %>% 
  mutate(term = gsub("\\:", "", term)) %>% 
  mutate(term = as.numeric(term)) %>% 
  filter(!is.na(term)) %>% 
  rbind(twfe_bind) %>% 
  arrange(term) %>% 
  arrange(Variable) %>%
  mutate(se = 1.96 * std.error) %>% 
  mutate(Variable = gsub("FinalShare_Con", "Conservative Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Lab", "Labour Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Lib", "Liberal Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Nat", "Independent Share", Variable)) 

clean_twfe$Variable <- factor(clean_twfe$Variable, levels = c("Conservative Share", "Labour Share", "Liberal Share", "Independent Share")) 

SumEvent2 <- clean_twfe %>% 
  dplyr::select(-c("std.error", "statistic")) %>%
  mutate(estimate = round(estimate, digits = 4)) %>% 
  mutate(se = round(se, digits = 4)) %>% 
  mutate(se = round(se, digits = 4)) %>% 
  mutate(se = gsub("^", "\\(", se)) %>% 
  mutate(se = gsub("$", "\\)", se)) %>%
  mutate(estimate = ifelse(p.value < 0.1,  gsub("$", "\\*", estimate), estimate)) %>%
  mutate(estimate = ifelse(p.value < 0.05,  gsub("$", "\\*", estimate), estimate)) %>%        
  mutate(estimate = ifelse(p.value < 0.01,  gsub("$", "\\*", estimate), estimate)) %>% 
  dplyr::select(-c("p.value")) %>% 
  mutate(se = as.character(se)) %>%
  pivot_longer(-c("term", "Variable"), names_to = "Item", values_to = "Value") %>% 
  pivot_wider(names_from = Variable, values_from = Value) %>% 
  filter(term != -1) %>% 
  mutate(term = term * 4 + 1983) %>% 
  rename("Period" = "term") %>% 
  mutate(Period = ifelse(Item == "se", "", Period)) %>% 
  rename(" " = "Item")

view(SumEvent2) 

stargazer(SumEvent2, summary=FALSE, rownames=FALSE, out = "EventStudySale.tex") 
 
PlaceboCombo <- SumEvent %>% 
                mutate(Variable = "Social Housing")

SumEvent3 <- SumEvent2 %>% 
  mutate(Variable = "House Sales") %>% 
  rbind(PlaceboCombo) %>% 
  slice(1:2, 13:14) %>% 
  pivot_longer(-c("Period", " ", "Variable"), names_to = "Share", values_to = "Value") %>% 
  pivot_wider(names_from = "Share", values_from = "Value")

view(SumEvent3)  

stargazer(SumEvent3, summary=FALSE, rownames=FALSE, out = "Placebo.tex") 

graph_twfe_sale <- clean_twfe %>% 
  ggplot(aes(x = term, y = estimate)) + geom_point() + 
  geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), width= .2,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = -1, linetype = 2) + 
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(y = "Coefficient for Council Share and Period ", x = "Time to Treatment", title = "Event Study - Owner Occupied Housing Share") + 
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4)) +
  facet_wrap(vars(Variable))

graph_twfe_sale

StataPac <- RegSum %>% 
  group_by(Period) %>%
  mutate(Quart_75 = quantile(AdjHouseShareDiff, prob = .75)) %>%
  mutate(Quart_25 = quantile(AdjHouseShareDiff, prob = .25)) %>% 
  mutate(across(Quart_75:Quart_25, ~ ifelse(Period == 1999, .x, NA), .names = "{col}_Fin")) %>%
  group_by(District) %>% 
  fill(Quart_75_Fin, .direction = "up") %>% 
  fill(Quart_25_Fin, .direction = "up") %>% 
  mutate(AdjHouseShareDiff_Fin = ifelse(Period == 1999, AdjHouseShareDiff, NA)) %>% 
  fill(AdjHouseShareDiff_Fin,.direction = "up")  

view(StataPac)

ParallelTrends <- StataPac %>% 
                  mutate(Treatment = ifelse(AdjHouseShareDiff_Fin >= Quart_75_Fin, "Lower Quartile", 
                            ifelse(AdjHouseShareDiff_Fin <= Quart_25_Fin, "Upper Quartile" , NA))) %>%
                  filter(!is.na(Treatment)) %>% 
                  group_by(Period, Treatment) %>% 
                  summarise(across(FinalShare_Con:FinalShare_Nat, ~ mean(.x))) %>% 
                  pivot_longer(-c("Period", "Treatment"), names_to = "Variable", values_to = "Council Share") %>% 
  mutate(Variable = gsub("FinalShare_Con", "Conservative Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Lab", "Labour Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Lib", "Liberal Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Nat", "Independent Share", Variable))

view(ParallelTrends) 

ParallelTrends$Variable <- factor(ParallelTrends$Variable, levels = c("Conservative Share", "Labour Share", "Liberal Share", "Independent Share"))

ParallelGraph <- ParallelTrends %>% 
                 ggplot(aes(x = Period, y = `Council Share`)) + geom_line(aes(color = Treatment)) + 
                 geom_point(aes(color = Treatment)) + scale_x_continuous(breaks = c(1975, 1985, 1995)) + 
                 geom_hline(yintercept = 0) + geom_vline(xintercept = 1980) +
                 facet_wrap(vars(Variable)) + labs(title = "Council Composition Trends") + 
                 scale_color_manual(labels = c("Lower\nQuartile", "Upper\nQuartile"), values = c("blue", "red")) + 
                 theme_minimal() +
                 theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5), legend.spacing.y = unit(0.2, 'cm')) + 
                 guides(color = guide_legend(byrow = TRUE))

ParallelGraph  

setwd("C:/Users/samue/Documents/English Housing Processed/Graphics Output/Coefficient Graph")

ggsave("Social Housing Time Series.png", plot = graph_twfe, width=6, height=4, dpi=300)
ggsave("Social Housing Sales Time Series.png", plot = graph_twfe_sale, width=6, height=4, dpi=300)
ggsave("Owned Housing Time Series.png", plot = graph_twfe_own, width=6, height=4, dpi=300) 
ggsave("Parallel Trends.png", plot = ParallelGraph, width=6, height=4, dpi=300)

#SummaryStatistics

view(Stata) 

Summary <- Stata %>% 
           mutate(PctSales = 100*PctSales) %>% 
           mutate(Epoch = ifelse(Year >= as.Date("1974-01-01") & Year <= as.Date("1979-01-01"), "1974 to 1979", 
                          ifelse(Year >= as.Date("1980-01-01") & Year <= as.Date("1989-01-01"), "1980 to 1989", 
                          ifelse(Year >= as.Date("1990-01-01") & Year <= as.Date("1999-01-01"), "1990 to 1999", NA)))) %>% # 
           filter(Year != as.Date("2001-01-01")) %>%
           group_by(Year) %>%
           mutate(Quart_75 = quantile(AdjHouseShareDiff, prob = 0.75)) %>% 
           mutate(Quart_25 = quantile(AdjHouseShareDiff, prob = 0.25)) %>% 
  mutate(across(Quart_75:Quart_25, ~ ifelse(Year == as.Date("2000-01-01"), .x, NA), .names = "{col}_Fin")) %>%
  group_by(District) %>% 
  fill(Quart_75_Fin, .direction = "up") %>% 
  fill(Quart_25_Fin, .direction = "up") %>% 
  mutate(AdjHouseShareDiff_Fin = ifelse(Year == as.Date("2000-01-01"), AdjHouseShareDiff, NA)) %>% 
  fill(AdjHouseShareDiff_Fin,.direction = "up") %>% 
  mutate(Treatment = ifelse(AdjHouseShareDiff_Fin >= Quart_75_Fin, "Lower Quartile", 
                            ifelse(AdjHouseShareDiff_Fin <= Quart_25_Fin, "Upper Quartile" , "Middle Quartiles"))) 
  
view(Summary) 

Check <- Summary %>% 
         dplyr::select(c("Year", "District", "Total":"Other")) %>% 
         ungroup()

view(Check) 

QuartGraphData <- Summary %>% 
                  dplyr::select(c("Year", "District", "AdjHouseShareDiff", "AdjOwnerShareDiff", "Treatment", "AdjHouseSales", "PctSales")) %>% 
                  group_by(Year, Treatment) %>% 
                  summarise(across(where(is.numeric), ~ mean(.x))) 
                         
view(QuartGraphData) 

PubQuartGraph <- QuartGraphData %>% 
                 ggplot(aes(x = Year, y = AdjHouseShareDiff)) + geom_vline(xintercept = as.Date("1979-01-01"), linewidth = 1) + 
                 geom_text(label = "1979 Base", x = as.Date("1985-01-01"), y = 0.01 , size = 3.25, lineheight = 0.9, hjust = 0) +
                 geom_hline(yintercept = 0, linetype = 2, linewidth = 0.75) +
                 geom_line(aes(color = Treatment)) + geom_point(aes(color = Treatment)) + 
                 scale_color_manual(labels = c("Lower\nQuartile","Middle\nQuartiles", "Upper\nQuartile"),values = c("blue", "black", "red")) + scale_y_continuous(limits = c(-0.25, 0.05)) + 
                 labs(title = "Change in Social Housing Share", y = "Share of Social Housing Relative to 1980") + 
                 theme_minimal() +  theme(plot.title = element_text(hjust = 0.5), legend.spacing.y = unit(0.2, 'cm')) + 
                 guides(color = guide_legend(byrow = TRUE))
  
PubQuartGraph 

OwnQuartGraph <- QuartGraphData %>% 
  ggplot(aes(x = Year, y = AdjOwnerShareDiff)) + geom_vline(xintercept = as.Date("1979-01-01"), linewidth = 1) + 
  geom_text(label = "1979 Base", x = as.Date("1985-01-01"), y = 0.01 , size = 3.25, lineheight = 0.9, hjust = 0) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.75) +
  geom_line(aes(color = Treatment)) + geom_point(aes(color = Treatment)) + scale_y_continuous(limits = c(-0.05, 0.20)) + 
  scale_color_manual(labels = c("Lower\nQuartile","Middle\nQuartiles", "Upper\nQuartile"), values = c("blue", "black", "red")) + 
  labs(title = "Change in Owner Occupied Housing Share", y = "Share of Owner Occupied Housing Relative to 1980") + 
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5), legend.spacing.y = unit(0.2, 'cm')) + 
  guides(color = guide_legend(byrow = TRUE))

OwnQuartGraph 

SaleQuartGraph <- QuartGraphData %>% 
  ggplot(aes(x = Year, y = AdjHouseSales)) + geom_vline(xintercept = as.Date("1979-01-01"), linewidth = 1) + 
  geom_text(label = "1979 Base", x = as.Date("1985-01-01"), y = 0.01 , size = 3.25, lineheight = 0.9, hjust = 0) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.75) +
  geom_line(aes(color = Treatment)) + geom_point(aes(color = Treatment)) + scale_y_continuous(limits = c(-0.025, 0.15)) + 
  scale_color_manual(labels = c("Lower\nQuartile","Middle\nQuartiles", "Upper\nQuartile"), values = c("blue", "black", "red")) + 
  labs(title = "Change in Share of Social Housing Sold", y = "Cumulative House Sales Relative to 1979") + 
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5), legend.spacing.y = unit(0.2, 'cm')) + 
  guides(color = guide_legend(byrow = TRUE))

SaleQuartGraph

SalesAnnualQuartGraph <- QuartGraphData %>% 
  ggplot(aes(x = Year, y = PctSales)) + geom_vline(xintercept = as.Date("1980-01-01"), linewidth = 0.75, linetype = 2) + 
  geom_hline(yintercept = 0, linetype = 1, linewidth = 0.75) +
  geom_line(aes(color = Treatment)) + geom_point(aes(color = Treatment)) + scale_y_continuous(limits = c(0, 2)) + 
  scale_color_manual(labels = c("Lower\nQuartile","Middle\nQuartiles", "Upper\nQuartile"), values = c("blue", "black", "red")) + 
  labs(title = "Annual Social Housing Sales", y = "Annual Percentage Sales") + 
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5), legend.spacing.y = unit(0.2, 'cm')) + 
  guides(color = guide_legend(byrow = TRUE))

SalesAnnualQuartGraph

setwd("C:/Users/samue/Documents/English Housing Processed/Graphics Output/Treatment Graph") 
ggsave("SocHouseDiff.png", plot = PubQuartGraph , width=6, height=4, dpi=300) 
ggsave("PrivHouseDiff.png", plot = OwnQuartGraph , width=6, height=4, dpi=300)
ggsave("SaleHouseDiff.png", plot = SaleQuartGraph, width=6, height=4, dpi=300)
ggsave("AnnualSaleHouseDiff.png", plot = SalesAnnualQuartGraph, width=6, height=4, dpi=300)

EpochSummary <- Summary %>% 
                filter(!is.na(Epoch)) %>% 
                dplyr::select(c("Year", "District", "rindex2":"llds", "lrhp":"Nat_Share", "PctSales", "OwnerOccupiedStock_Share", "NonProfitHousingStock_Share", "AdjHouseShareDiff","AdjOwnerShareDiff" , "AdjHouseSales", "Epoch", "Treatment")) %>% 
                group_by(Epoch, Treatment) %>% 
                filter(!is.na(Con_Share)) %>% 
                rename_with(~ gsub("\\_", "", .x)) %>%
                summarise(across(where(is.numeric), list(mean, sd))) %>% 
  ungroup() %>%
  mutate(lmaleearnreal_1 = log(maleearnreal_1)) %>%
  mutate(llds_1 = log(predemp71_1)) %>%
  mutate(lrhp_1 = log(realhp_1)) %>%
                rename_with(~ gsub("_1", "_mean", .x)) %>% 
                rename_with(~ gsub("_2", "_sd", .x)) %>% 
                pivot_longer(-c("Epoch", "Treatment"), names_to = "Variable", values_to = "Value") %>% 
                separate(Variable, c("Variable", "Statistic"), "_") %>% 
                relocate(Statistic, .after = Epoch) %>% 
                unite("Section", Epoch:Statistic, sep = "\n") %>% 
                pivot_wider(names_from = Section , values_from = Value) %>% 
                relocate(Variable, .before = Treatment) %>% 
                arrange(Variable) %>% 
                filter(Variable %in% c("ConShare", "LabShare", "LibShare", "NatShare", "AdjOwnerShareDiff", "AdjHouseShareDiff","AdjHouseSales", "lrhp", "llds", "lmaleearnreal")) %>%
                mutate(Variable = gsub("ConShare", "Conservative Share", Variable)) %>% 
                mutate(Variable = gsub("LabShare", "Labour Share", Variable)) %>% 
                mutate(Variable = gsub("LibShare", "Liberal Share", Variable)) %>% 
                mutate(Variable = gsub("NatShare", "Indepedent Share", Variable)) %>% 
                mutate(Variable = gsub("AdjOwnerShareDiff", "Change in Owner Occupied Share", Variable)) %>% 
                mutate(Variable = gsub("AdjHouseShareDiff", "Change in Non Profit Share", Variable)) %>% 
                mutate(across(where(is.numeric), ~ signif(.x, digits = 4))) %>% 
                arrange(Variable) %>% 
                mutate(Variable = ifelse(Treatment == "Middle Quartiles" | Treatment == "Upper Quartile", "", Variable))

view(EpochSummary) 

setwd("C:/Users/samue/Documents/English Housing Processed/Regression Output")
stargazer(EpochSummary, summary=FALSE, rownames=FALSE, out = "summary.tex") 

view(Stata)

OLSModel <- Stata %>% 
           ungroup() %>%
           mutate(RTBStockChange = RTB *AdjHouseShareDiff) %>% 
           mutate(Share80Con = ifelse(Year == as.Date("1980-01-01"), Con_Share, NA)) %>% 
           mutate(Share80Lab = ifelse(Year == as.Date("1980-01-01"), Lab_Share, NA)) %>%
           mutate(Share80Lib = ifelse(Year == as.Date("1980-01-01"), Lib_Share, NA)) %>%
           mutate(Share80Ind = ifelse(Year == as.Date("1980-01-01"), Nat_Share, NA)) %>% 
           mutate(ShareSoc80 = ifelse(Year == as.Date("1980-01-01"), NonProfitHousingStock_Share, NA)) %>% 
           mutate(LWage80 = ifelse(Year == as.Date("1980-01-01"), lmale_earn_real, NA)) %>% 
           mutate(LHP80 = ifelse(Year == as.Date("1980-01-01"), lrhp, NA)) %>% 
           mutate(Llds80 = ifelse(Year == as.Date("1980-01-01"), llds, NA)) %>%
           mutate(Change00 = ifelse(Year == as.Date("2000-01-01"), RTBStockChange, NA)) %>%
           group_by(District) %>%
           fill(Share80Con, .direction = "downup") %>% 
           fill(Share80Lab, .direction = "downup") %>% 
           fill(Share80Lib, .direction = "downup") %>% 
           fill(Share80Ind, .direction = "downup") %>% 
           fill(ShareSoc80, .direction = "downup") %>% 
           fill(LWage80, .direction = "downup") %>%
           fill(LHP80, .direction = "downup") %>% 
           fill(Llds80, .direction = "downup") %>%
           fill(Change00, .direction = "downup") %>% 
           summarise(across(`Share80Con`:Change00, ~ mean(.x)))

view(OLSModel)
           
names(OLSModel)

OLSRes <- lm(Change00 ~ ShareSoc80 + Share80Con + Share80Lab + Share80Lib + LWage80 + LHP80 + Llds80, data =  OLSModel)
OLSRes2 <- lm(Change00 ~  Share80Con + Share80Lab + Share80Lib + LWage80 + LHP80 + Llds80, data =  OLSModel)
summary(OLSRes)

stargazer(OLSRes, OLSRes2, summary=FALSE, rownames=FALSE, out = "OneWay.tex") 


PartySummary <- Stata %>% 
                group_by(Year) %>% 
                summarise(across(Total:Nat, ~ sum(.x, na.rm = TRUE))) %>% 
                mutate(across(Con:Nat, ~ .x / Total)) %>% 
                dplyr::select(-c("Total")) %>% 
                pivot_longer(-c("Year"), names_to = "Party", values_to = "Share") %>% 
                ggplot(aes(x = Year, y = Share)) + geom_vline(xintercept = as.Date("1974-01-01"), linewidth = 0.75) +
                geom_line(aes(color = Party)) + geom_point(aes(color = Party)) + 
                theme_minimal() + scale_color_manual(labels = c("Conservative", "Labour", "Liberal", "Independent"), values = c("#0087DC","#E4003B", "#FAA61A", "grey")) + 
                scale_y_continuous(limits = c(0, 0.55)) + geom_hline(yintercept = 0, linewidth = 0.75) + labs(title = "Share of Local Councillors by Party") + 
                theme(plot.title = element_text(hjust = 0.5))

view(PartySummary)    

setwd("C:/Users/samue/Documents/English Housing Processed/Graphics Output/Treatment Graph") 
ggsave("PartyShare.png", plot = PartySummary, width=6, height=4, dpi=300)

#Maps

setwd("C:/Users/samue/Documents/Dissertation Shapefiles") 

transform <- read_sf("hexudmtransform.shp")
head(transform)

sf::st_crs(transform) <- CRS("+init=epsg:27700") 

head(transform)
Dist01 <- read_sf("england_dt_2001.shp") 
Dist01UA <- read_sf("england_ua_2001.shp") 

head(Dist01)

Stata00 <- Stata %>% 
           dplyr::select(c("District", "Year", "AdjHouseShareDiff", "lpa_code")) %>% 
           filter(Year == as.Date("2000-01-01")) 

view(Stata00)

trans <- transform %>% 
         mutate(LANAME = gsub("\\-", " ", LANAME)) %>% 
         mutate(LANAME = gsub("\\'", "", LANAME)) %>%
         mutate(LANAME = gsub("Bristol", "Bristol, City Of", LANAME)) %>%
         mutate(LANAME = gsub("Westminster", "City Of Westminster", LANAME)) %>%
         mutate(LANAME = gsub("South Bucks", "South Buckinghamshire", LANAME)) %>%
         mutate(LANAME = gsub("Stratford upon Avon", "Stratford On Avon", LANAME)) %>% 
         mutate(LANAME = gsub("Middlesborough", "Middlesbrough", LANAME)) %>% 
         mutate(LANAME = gsub("Newcasle under Lyme", "Newcastle Under Lyme", LANAME)) %>%
         mutate(LANAME = gsub("Reigate and Barnstead", "Reigate And Banstead", LANAME)) %>%
         mutate(LANAME = str_to_title(LANAME)) %>%
         filter(GOREGION != "Wales" & GOREGION != "Scotland" & GOREGION != "Northern Ireland") %>%
         left_join(Stata00, by = c("LANAME"= "District")) %>% 
         mutate(map = "Qequal") %>% 
         rename("name" = "LANAME") %>% 
         dplyr::select(c("name", "geometry", "Year", "AdjHouseShareDiff", "map"))

view(trans) 
head(trans)

saved <- tm_shape(trans) +
  tm_polygons("AdjHouseShareDiff", style = "cont",title = "Social Housing\nShare Change") +
  tm_layout(frame.lwd = 0, legend.position = c("right", "top")) +
  tm_layout(legend.title.size = 0.2, legend.text.size = 0.2, main.title.size = 0.3,
    main.title = "Change in Social Housing Share From \n 1980-2000", 
    main.title.position = "center")


saved 

Stata01 <- Stata00 %>% 
           mutate(District = str_squish(District))

view(Stata01)

Smash <- Dist01 %>% 
         mutate(name = gsub("UA", "", name)) %>% 
         mutate(name = gsub("\\-", " ", name)) %>%
         mutate(name = str_to_title(name)) %>% 
         mutate(name = str_squish(name)) %>% 
         mutate(name = gsub("Basingstoke And Dean", "Basingstoke And Deane", name)) %>% 
         mutate(name = gsub("Shrewsbury And Atcha", "Shrewsbury And Atcham", name)) %>% 
         mutate(name = gsub("County Of Herefordshire", "Herefordshire", name)) %>%
         mutate(name = gsub("South Northamptonshi", "South Northamptonshire", name)) %>%
         mutate(name = gsub("Hinckley And Boswort", "Hinckley And Bosworth", name)) %>%
         mutate(name = gsub("Staffordshire Moorla", "Staffordshire Moorlands", name)) %>%
         mutate(name = gsub("King's Lynn And West", "Kings Lynn And West Norfolk", name)) %>% 
         mutate(name = gsub("Ellesmere Port & Nes", "Ellesmere Port And Neston", name)) %>% 
         mutate(name = gsub("Weymouth And Portlan", "Weymouth And Portland", name)) %>%
         mutate(name = gsub("Bath And North East Some", "Bath And North East Somerset", name)) %>%
         mutate(name = gsub("South Bucks", "South Buckinghamshire", name)) %>%
         mutate(name = gsub("Nuneaton And Bedwort", "Nuneaton And Bedworth", name)) %>% 
         mutate(name = gsub("Tonbridge And Mallin", "Tonbridge And Malling", name)) %>% 
         mutate(name = gsub("North West Leicester", "North West Leicestershire", name)) %>%
         mutate(name = gsub("North East Derbyshir", "North East Derbyshire", name)) %>%
         mutate(name = gsub("Kingston Upon Hull, City", "Kingston Upon Hull", name)) %>% 
         mutate(name = gsub("Westminster", "City Of Westminster", name)) %>%
         left_join(Stata00, by = c("name" = "District")) %>% 
         distinct() %>% 
         mutate(map = "Normal") %>% 
         dplyr::select(c("name", "geometry", "Year", "AdjHouseShareDiff", "map")) %>%
         rbind(trans)

view(Smash) 
head(Smash)

bbox_new <- st_bbox(transform)

SmashGraph <- tm_shape(Smash) + tm_polygons("AdjHouseShareDiff", style = "cont",title = "Social Housing\nShare Change") +
  tm_layout(frame.lwd = 0, legend.position = c("right", "top")) +
  tm_layout(legend.title.size = 0.3, legend.text.size = 0.3, main.title.size = 0.4, legend.outside = TRUE,
            main.title = "Change in Social Housing Share From \n 1980-2000", 
            legend.outside.size = 0.15 ,asp = 0.7, 
            main.title.position = "center", panel.show = FALSE) + tm_facets(by = "map")

SmashGraph 


setwd("C:/Users/samue/Documents/English Housing Processed/Graphics Output/Maps") 
tmap_save(SmashGraph, "Map.png",width = 1000, height = 750, dpi = 300)

#EUReg

EUStata <- Stata %>% 
           mutate(District = gsub("Alnwick", "Northumberland", District)) %>% 
           mutate(District = gsub("Berwick Upon Tweed", "Northumberland", District)) %>% 
  mutate(District = gsub("Blyth Valley", "Northumberland", District)) %>% 
  mutate(District = gsub("Bridgnorth", "Shropshire", District)) %>% 
  mutate(District = gsub("Caradon", "Cornwall", District)) %>% 
  mutate(District = gsub("Carrick", "Cornwall", District)) %>% 
  mutate(District = gsub("Castle Morpeth", "Northumberland", District)) %>% 
  mutate(District = gsub("Chester Le Street", "Northumberland", District)) %>%
  mutate(District = gsub("Chester", "Cheshire West And Chester", District)) %>%
  mutate(District = gsub("Cheshire West And Chesterfield", "Chesterfield", District)) %>%
  mutate(District = gsub("Congleton", "Cheshire East", District)) %>% 
  mutate(District = gsub("Crewe And Nantwich", "Cheshire East", District)) %>%
  mutate(District = gsub("Durham", "County Durham", District)) %>%
  mutate(District = gsub("Derwentside", "County Durham", District)) %>% 
  mutate(District = gsub("Easington", "County Durham", District)) %>% 
  mutate(District = gsub("Ellesmere Port And Neston", "Cheshire West And Chester", District)) %>%
  mutate(District = gsub("Herefordshire", "Herefordshire, County Of", District)) %>%
  mutate(District = gsub("Kennet", "Wiltshire", District)) %>%
  mutate(District = gsub("Kerrier", "Cornwall", District)) %>% 
  mutate(District = gsub("Kingston Upon Hull", "East Riding of Yorkshire", District)) %>%
  mutate(District = gsub("Macclesfield", "Cheshire East", District)) %>% 
  mutate(District = gsub("Mid Bedfordshire", "Central Bedfordshire", District)) %>% 
  mutate(District = gsub("North Cornwall", "Cornwall", District)) %>%
  mutate(District = gsub("North Shropshire", "Shropshire", District)) %>% 
  mutate(District = gsub("North Wiltshire", "Wiltshire", District)) %>% 
  mutate(District = gsub("Oswestry", "Shropshire", District)) %>% 
  mutate(District = gsub("Penwith", "Cornwall", District)) %>% 
  mutate(District = gsub("Restormel", "Cornwall", District)) %>% 
  mutate(District = gsub("Salisbury", "Wiltshire", District)) %>% 
  mutate(District = gsub("Sedgefield", "County Durham", District)) %>% 
  mutate(District = gsub("Shrewsbury And Atcham", "Shropshire", District)) %>% 
  mutate(District = gsub("South Shropshire", "Shropshire", District)) %>%
  mutate(District = gsub("Teesdale", "County Durham", District)) %>% 
  mutate(District = gsub("Tynedale", "Northumberland", District)) %>% 
  mutate(District = gsub("Vale Royal", "Cheshire West And Chester", District)) %>% 
  mutate(District = gsub("Wansbeck", "Northumberland", District)) %>%
  mutate(District = gsub("Wear Valley", "County Durham", District)) %>%
  mutate(District = gsub("West Wiltshire", "Wiltshire", District)) %>% 
  mutate(District = gsub("East Riding of Yorkshire", "East Riding Of Yorkshire", District)) %>%
  group_by(Year, District) %>% 
  summarise(across(Total:Other, ~ sum(.x)), Totalcontestedwards = sum(Totalcontestedwards), across(rindex2:llds, ~ mean(.x)), 
            across(RighttoBuySales:NonProfitHousingStock, ~ sum(.x))) %>% 
  ungroup() %>%
  arrange(District, Year) %>% 
  group_by(Year, District) %>% 
  mutate(across(OwnerOccupiedStock:NonProfitHousingStock, ~ .x / TotalHousingStock, .names = "{.col}_Share")) %>% 
  mutate(across(Con:Nat, ~ .x / Total, .names = "{.col}_Share")) %>%
  group_by(District) %>% 
  mutate(Period = ifelse(Year == as.Date("1975-01-01"), 1975,  
                         ifelse(Year >= as.Date("1976-01-01") & Year <= as.Date("1979-01-01"), 1979, 
                                ifelse(Year >= as.Date("1980-01-01") & Year <= as.Date("1983-01-01"), 1983,  
                                       ifelse(Year >= as.Date("1984-01-01") & Year <= as.Date("1987-01-01"), 1987, 
                                              ifelse(Year >= as.Date("1988-01-01") & Year <= as.Date("1991-01-01"), 1991, 
                                                     ifelse(Year >= as.Date("1992-01-01") & Year <= as.Date("1995-01-01"), 1995, 
                                                            ifelse(Year >= as.Date("1996-01-01") & Year <= as.Date("1999-01-01"), 1999, NA)))))))) %>%
  fill(Period, .direction = "up") %>%
  mutate(FinalShare_Con = ifelse(Year == as.Date("1975-01-01") | Year == as.Date("1979-01-01") | 
                                   Year == as.Date("1983-01-01") | Year == as.Date("1987-01-01") | 
                                   Year == as.Date("1991-01-01") | Year == as.Date("1995-01-01") |
                                   Year == as.Date("1999-01-01") , Con_Share, NA)) %>% 
  mutate(FinalShare_Lab = ifelse(Year == as.Date("1975-01-01") | Year == as.Date("1979-01-01") | 
                                   Year == as.Date("1983-01-01") | Year == as.Date("1987-01-01") | 
                                   Year == as.Date("1991-01-01") | Year == as.Date("1995-01-01") |
                                   Year == as.Date("1999-01-01") , Lab_Share, NA)) %>% 
  mutate(FinalShare_Lib = ifelse(Year == as.Date("1975-01-01") | Year == as.Date("1979-01-01") | 
                                   Year == as.Date("1983-01-01") | Year == as.Date("1987-01-01") | 
                                   Year == as.Date("1991-01-01") | Year == as.Date("1995-01-01") |
                                   Year == as.Date("1999-01-01") , Lib_Share, NA)) %>% 
  mutate(FinalShare_Nat = ifelse(Year == as.Date("1975-01-01") | Year == as.Date("1979-01-01") | 
                                   Year == as.Date("1983-01-01") | Year == as.Date("1987-01-01") | 
                                   Year == as.Date("1991-01-01") | Year == as.Date("1995-01-01") |
                                   Year == as.Date("1999-01-01") , Nat_Share, NA)) %>% 
  group_by(District) %>% 
  fill(FinalShare_Con, .direction = "up") %>%
  fill(FinalShare_Lab, .direction = "up") %>% 
  fill(FinalShare_Lib, .direction = "up") %>% 
  fill(FinalShare_Nat, .direction = "up") %>% 
  mutate(SocialHouseShareDiff = NonProfitHousingStock_Share - lag(NonProfitHousingStock_Share)) %>% 
  mutate(SocialHouseShareDiff = lead(SocialHouseShareDiff)) %>% 
  mutate(NetSocialHouse = NonProfitHousingStock - lag(NonProfitHousingStock)) %>% 
  mutate(NetSocialHouse = lead(NetSocialHouse)) %>%
  mutate(CumuHouseShareDiff = cumsum(SocialHouseShareDiff)) %>% 
  mutate(HouseShareDiff79 = ifelse(Year == as.Date("1979-01-01"), CumuHouseShareDiff, NA)) %>% 
  fill(HouseShareDiff79, .direction = "downup") %>% 
  mutate(AdjHouseShareDiff = (CumuHouseShareDiff - HouseShareDiff79)) %>% 
  mutate(OwnerOccupiedShareDiff = OwnerOccupiedStock_Share - lag(OwnerOccupiedStock_Share)) %>% 
  mutate(OwnerOccupiedShareDiff = lead(OwnerOccupiedShareDiff)) %>% 
  mutate(CumuOwnerShareDiff = cumsum(OwnerOccupiedShareDiff)) %>% 
  mutate(OwnerShareDiff79 = ifelse(Year == as.Date("1979-01-01"), CumuOwnerShareDiff, NA)) %>% 
  fill(OwnerShareDiff79, .direction = "downup") %>% 
  mutate(AdjOwnerShareDiff = CumuOwnerShareDiff - OwnerShareDiff79) %>% 
  mutate(CumuHouseSales = cumsum(RighttoBuySales)) %>% 
  mutate(HouseSaleShare = CumuHouseSales / TotalHousingStock) %>%
  mutate(HouseSales79 = ifelse(Year == as.Date("1979-01-01"), HouseSaleShare, NA)) %>% 
  fill(HouseSales79, .direction = "downup") %>% 
  mutate(AdjHouseSales = HouseSaleShare - HouseSales79)
  
view(EUStata)

Filt75 <- EUStata %>% 
  filter(Year == as.Date("1975-01-01")) %>% 
  dplyr::select(-c("Year"))

view(Filt75)

EURegSum <- EUStata %>% 
  filter(Period != 1974 & Period != 1975 & Period != 2002) %>% 
  group_by(District, Period)%>%
  summarise(across(where(is.numeric), ~ weighted.mean(.x, Totalcontestedwards))) %>% 
  rbind(Filt75) %>% 
  arrange(District, Period)  %>% 
  mutate(RTB = ifelse(Period >= 1983, 1, 0)) %>%
  mutate(RTBStockChange = RTB *AdjHouseShareDiff) %>% 
  mutate(RTBOwnerChange = RTB *AdjOwnerShareDiff) %>% 
  mutate(RTBSaleChange = RTB* AdjHouseSales) %>% 
  mutate(lmale_earn_real = log(male_earn_real))%>%
  mutate(lrhp = log(realhp)) %>% 
  mutate(llds = log(pred_emp71))

view(EURegSum) 
         

setwd("C:/Users/samue/Documents/English Housing Processed/Local Elections Data")


EURes <- read_csv("Eu-referendum-result-data.csv") %>% 
         dplyr::select(c("Area", "Remain", "Leave")) %>% 
         mutate(Area = str_to_title(Area)) %>% 
         mutate(Area = gsub("\\-", " ", Area)) %>% 
         mutate(Area = gsub("St ", "St. ", Area)) %>% 
         mutate(Area = gsub("King's Lynn And West Norfolk", "Kings Lynn And West Norfolk", Area)) %>% 
         mutate(Area = gsub("Westminster", "City Of Westminster", Area)) %>% 
         mutate(Area = gsub("South Bucks", "South Buckinghamshire", Area))

view(EURes) 


SplitJoin <- EURegSum %>% 
             left_join(EURes, by = c("District" = "Area")) %>% 
             mutate(LeaveShare = Leave / (Leave + Remain)) %>% 
             mutate(Left = ifelse(LeaveShare > 0.5, 1, 0))

view(SplitJoin) 

setwd("C:/Users/samue/Documents/English Housing Processed/Regression Output")
results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBStockChange * Left + lmale_earn_real + llds + lrhp | District + Period| 0 | District"))
  model <- felm(formula, SplitJoin) 
  results[[i]] <- model
}

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFE75.tex")

#rejection

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBOwnerChange | District + Period| 0 | District"))
  model <- felm(formula, RegSum) 
  results[[i]] <- model
} 

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFEOwner.tex") 

results <- list()

for (i in Variable) {
  formula <- as.formula(paste(i, " ~ RTBOwnerChange + lrhp + lmale_earn_real + llds | District + Period| 0 | District"))
  model <- felm(formula, RegSum) 
  results[[i]] <- model
}

stargazer(results, summary=FALSE, rownames=FALSE, out = "TWFEOwnerCont.tex") 

DataShare <- data.frame(term = character(),estimate = double(), std.error = double(), statistic = double(), p.value = double(), 
                        Variable = character())

for (i in Variable) { 
  formula <- as.formula(paste0(i, "~ i(time_to_treat, AdjOwnerShareDiff, ref = -1) + lrhp + llds + lmale_earn_real | District + Period"))
  model <- feols(formula, cluster = ~ District, data = EventStudy)
  Tidy <- tidy(model) %>%
    mutate(Variable = i) 
  DataShare <- DataShare %>% rbind(Tidy)
}

clean_twfe <- DataShare %>% 
  mutate(term = gsub("time_to_treat", "", term)) %>% 
  mutate(term = gsub("AdjOwnerShareDiff", "", term)) %>% 
  mutate(term = gsub("\\:", "", term)) %>% 
  mutate(term = as.numeric(term)) %>% 
  filter(!is.na(term)) %>% 
  rbind(twfe_bind) %>% 
  arrange(term) %>% 
  arrange(Variable) %>%
  mutate(se = 1.96 * std.error) %>% 
  mutate(Variable = gsub("FinalShare_Con", "Conservative Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Lab", "Labour Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Lib", "Liberal Share", Variable)) %>% 
  mutate(Variable = gsub("FinalShare_Nat", "Independent Share", Variable)) 

clean_twfe$Variable <- factor(clean_twfe$Variable, levels = c("Conservative Share", "Labour Share", "Liberal Share", "Independent Share")) 

SumEvent2 <- clean_twfe %>% 
  dplyr::select(-c("std.error", "statistic")) %>%
  mutate(estimate = round(estimate, digits = 4)) %>% 
  mutate(se = round(se, digits = 4)) %>% 
  mutate(se = round(se, digits = 4)) %>% 
  mutate(se = gsub("^", "\\(", se)) %>% 
  mutate(se = gsub("$", "\\)", se)) %>%
  mutate(estimate = ifelse(p.value < 0.1,  gsub("$", "\\*", estimate), estimate)) %>%
  mutate(estimate = ifelse(p.value < 0.05,  gsub("$", "\\*", estimate), estimate)) %>%        
  mutate(estimate = ifelse(p.value < 0.01,  gsub("$", "\\*", estimate), estimate)) %>% 
  dplyr::select(-c("p.value")) %>% 
  mutate(se = as.character(se)) %>%
  pivot_longer(-c("term", "Variable"), names_to = "Item", values_to = "Value") %>% 
  pivot_wider(names_from = Variable, values_from = Value) %>% 
  filter(term != -1) %>% 
  mutate(term = term * 4 + 1983) %>% 
  rename("Period" = "term") %>% 
  mutate(Period = ifelse(Item == "se", "", Period)) %>% 
  rename(" " = "Item")

view(SumEvent2) 

stargazer(SumEvent2, summary=FALSE, rownames=FALSE, out = "EventStudyOwn.tex") 


PlaceboCombo <- SumEvent %>% 
  mutate(Variable = "Social Housing")

SumEvent3 <- SumEvent2 %>% 
  mutate(Variable = "Owned Housing") %>% 
  rbind(PlaceboCombo) %>% 
  slice(1:2, 13:14) %>% 
  pivot_longer(-c("Period", " ", "Variable"), names_to = "Share", values_to = "Value") %>% 
  pivot_wider(names_from = "Share", values_from = "Value")

view(SumEvent3)  

stargazer(SumEvent3, summary=FALSE, rownames=FALSE, out = "Placebo.tex") 

graph_twfe_own <- clean_twfe %>% 
  ggplot(aes(x = term, y = estimate)) + geom_point() + 
  geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), width= .2,
                position=position_dodge(.9)) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = -1, linetype = 2) + 
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(y = "Coefficient for Council Share and Period ", x = "Time to Treatment", title = "Event Study - Owner Occupied Housing Share") + 
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4)) +
  facet_wrap(vars(Variable))

graph_twfe_own
