setwd("C:/Users/samue/Documents/Dissertation Shapefiles") 

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

#joinings 

Dist01 <- read_sf("england_dt_2001.shp") 

head(Dist01)

Dist91 <- read_sf("england_dt_1991.shp")

head(Dist91)

Ward91 <- read_sf("england_wa_1991.shp") 

Centroid91 <- st_centroid(Ward91) 

head(Centroid91)

Join01 <- st_intersection(Dist01, Centroid91) %>% 
          sf::st_set_geometry(NULL) %>% 
          rename("2001 District" = "name", "1991 Ward" = "name.1", 
                 "1991 Ward Label" = "label.1", "2001 District Label" = "label")
        
head(Join01) 

Join91 <- st_intersection(Dist91, Centroid91) %>% 
          st_set_geometry(NULL) %>% 
          rename("1991 District" = "name", "1991 District Label" = "label",
                 "1991 Ward" = "name.1", "1991 Ward Label" = "label.1")

head(Join91) 

Joined <- Join91 %>% 
          left_join(Join01, by = c("1991 Ward" = "1991 Ward", "1991 Ward Label" = "1991 Ward Label"))

view(Joined)

setwd("C:/Users/samue/Documents/English Housing Processed/Hilber Data/datasets/dta files")

Hilber <- read_dta("data LPA.dta") %>% 
          dplyr::select(c("year", "lpa_code" , "lpa_name_until2004", "rindex2", "realhp", "lrindex2", 
                   "male_earn_real" , "lmale_earn_real", "pdevel90_m2", "pred_emp71", "llds")) %>% 
          mutate(year = as.numeric(year), year = parse_date_time(year, orders = c("Y", "dmy"))) %>% 
          mutate(lpa_name_until2004 = gsub("[[:space:]]*UA", "", lpa_name_until2004)) %>% 
          mutate(lpa_name_until2004 = gsub("North Bedfordshire \\(from 2003: Bedford\\)", "Bedford", lpa_name_until2004))

head(Hilber)
view(Hilber) 
#Righttobuy
        
setwd("C:/Users/samue/Documents/English Housing Processed/Right to Buy Sales") 
names(Pre80Sale)
Pre80Sale <- read_excel("House Sales Output.xlsx") %>%
             dplyr::select(c("District", "Total Sales", "Year")) %>% 
  mutate(`2004 District` = District) %>% 
  mutate(`2004 District` = gsub("Aycliffe", "Durham", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Peterlee", "Durham", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Barking", "Barking And Dagenham", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Bath", "Bath and North East Somerset", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Blackburn", "Blackburn with Darwen", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Beverley", "East Riding of Yorkshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Cleethorpes", "North East Lincolnshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Holderness", "North East Lincolnshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Central Lancashire", "Preston", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Great Grimsby", "North East Lincolnshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Glanford", "North Lincolnshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Gillingham", "Medway Towns", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Hemel Hempstead", "Dacorum", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Kingswood", "South Gloucestershire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Newark", "Newark and Sherwood", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Langbaurgh On Tees", "Redcar and Cleveland", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Langbaurgh", "Redcar and Cleveland", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Runcorn", "Halton", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Scunthorpe", "North Lincolnshire", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Skelmersdale", "West Lancashire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Medina", "Isle of Wight", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("South Wight", "Isle of Wight", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("North Bedfordshire", "Bedford", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Thamesdown", "Swindon", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Wandsdyke", "Bath and North East Somerset", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Washington", "Sunderland", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Westminster", "City of Westminster", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Yeovil", "South Somerset", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Woodspring", "North Somerset", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Wimborne", "East Dorset", `2004 District`)) %>% 
  mutate(`2004 District` = str_to_title(`2004 District`)) %>% 
  mutate(`Total Sales` = as.numeric(`Total Sales`)) %>% 
  mutate(Year = parse_date_time(Year, orders = c("Y", "dmy"))) %>%
  arrange(District, Year)

view(Pre80Sale) 

Pre80Join <- Hilber %>% 
             mutate(lpa_name_until2004 = str_to_title(lpa_name_until2004)) %>% 
             mutate(lpa_name_until2004 = gsub("\\-", " ", lpa_name_until2004)) %>%
             filter(year <= as.Date("1980-01-01")) %>% 
             right_join(Pre80Sale, by = c("lpa_name_until2004" = "2004 District", "year" = "Year"), keep = TRUE)  
             
view(Pre80Join)

Pre80Check <- Pre80Join %>% 
              filter(is.na())

Pre80Done <- Pre80Join %>% 
             filter(!is.na(lpa_code)) %>% 
             dplyr::select(c("2004 District", "year", "Total Sales")) %>%
             rename("Local Authority Name" = "2004 District", "Year" = "year", "Right to Buy Sales" = "Total Sales") %>% 
             group_by(`Local Authority Name`, Year) %>% 
             summarise(`Right to Buy Sales` = sum(`Right to Buy Sales`, na.rm = TRUE)) %>%
             mutate(`Region Name` = NA, `ONS Code` = NA)

view(Pre80Done)

RighttoBuy <- read_excel("Right to Buy Sales Annual.xlsx", sheet = 6) %>% 
              slice(-(1:2)) %>% 
              row_to_names(row_number = 1) %>% 
              mutate(`Local Authority Name` = coalesce(`Local Authority Name`, `Region Name`)) %>% 
              filter(!is.na(`Local Authority Name`)) %>% 
              fill(`Region Name`) %>% 
              pivot_longer(-c("Region Name", "ONS Code", "Local Authority Name"), names_to = "Year", values_to = "Right to Buy Sales") %>% 
              mutate(Year = gsub("\\-.*", "", Year)) %>% 
              mutate(Year = parse_date_time(Year, orders = c("Y", "dmy"))) %>% 
              mutate(`Local Authority Name`  = gsub("Westminster", "City of Westminster", `Local Authority Name`)) %>% 
              mutate(`Local Authority Name`  = gsub("Kingston upon Hull, City of", "Kingston upon Hull", `Local Authority Name`)) %>% 
              mutate(`Local Authority Name`  = gsub("Herefordshire, County of", "Herefordshire", `Local Authority Name`)) %>% 
              mutate(`Local Authority Name`  = gsub("Medway", "Medway Towns", `Local Authority Name`)) %>% 
              mutate(`Local Authority Name`  = gsub("Bedford UA", "North Bedfordshire (from 2003: Bedford)", `Local Authority Name`)) %>% 
              mutate(`Local Authority Name`  = gsub("Ellesmere Port & Neston", "Ellesmere Port and Neston", `Local Authority Name`)) %>% 
              mutate(`Local Authority Name`  = gsub("St[[:space:]]", "St\\. ", `Local Authority Name`)) %>% 
              mutate(`Local Authority Name`  = gsub("Folkestone and Hythe", "Shepway", `Local Authority Name`)) %>% 
              arrange(`Local Authority Name`) %>% 
              rbind(Pre80Done) %>% 
              mutate(`Right to Buy Sales` = as.numeric(`Right to Buy Sales`)) %>% 
              mutate(`Local Authority Name` = str_to_title(`Local Authority Name`)) %>%
              group_by(`Local Authority Name`, Year) %>% 
              summarise(`Right to Buy Sales` = sum(`Right to Buy Sales`, na.rm = TRUE)) %>% 
              arrange(Year) %>% 
              arrange(`Local Authority Name`)

names(RighttoBuy)
view(RighttoBuy) 
view(Hilber)
 
RtBmerge <-   Hilber %>% 
              mutate(lpa_name_until2004 = str_to_title(lpa_name_until2004)) %>%
              full_join(RighttoBuy, by = c("lpa_name_until2004" = "Local Authority Name" ,"year" = "Year"), keep = TRUE) %>% 
              mutate(lpa_name_until2004 = str_to_title(lpa_name_until2004)) %>% 
              mutate(lpa_name_until2004 = gsub("\\-", " ", lpa_name_until2004)) %>% 
              mutate(lpa_name_until2004 = gsub("\\'", "", lpa_name_until2004)) %>% 
              mutate(lpa_name_until2004 = gsub("South Bucks", "South Buckinghamshire", lpa_name_until2004))
              
view(RtBmerge) 

FiltRtB <- RtBmerge %>% 
           filter(lpa_name_until2004 == "Telford And Wrekin")

view(FiltRtB)

write.csv(RtBmerge, "Merged Hilber and Right to Buy.csv")

missing <- RtBmerge %>% 
           filter(year >= as.Date("1980-01-01")) %>%
           filter(is.na(`Local Authority Name`)) 

missing2 <- RtBmerge %>% 
            filter(Year >= as.Date("1980-01-01") & Year <= as.Date("2008-01-01")) %>% 
            filter(is.na(`lpa_name_until2004`))  
            
view(missing2)

setwd("C:/Users/samue/Documents/English Housing Processed/Post-1980 Housing Data")

books <- c("Completions by LA Live Table 253 2017.xlsx")  

Completions <- books %>% 
               excel_sheets() %>% 
               set_names() %>% 
               map_dfr(read_excel, path = books) %>% 
               dplyr::select(-c("...7":"...12")) %>%
               row_to_names(row_number = 1)

view(Completions) 

books2 <- c("LiveTable253Full.xlsx")  

CompletionsStack <- books2 %>% 
                    excel_sheets() %>% 
                    set_names() %>% 
                    map_dfr(read_excel, path = books2) %>% 
                    dplyr::select(-c("...8":"...13")) %>% 
                    row_to_names(row_number = 1) %>%
                    mutate(`National, Met and Shire County Totals` = coalesce(`National and Regional Totals`, `Met and Shire County Totals`)) %>% 
                    dplyr::select(-c("National and Regional Totals", "Met and Shire County Totals")) %>% 
                    relocate(`National, Met and Shire County Totals`, .before = `Lower and Single Tier Authority Data`) %>% 
                    rename("DCLG code" = "DLUHC code") %>% 
                    rbind(Completions) %>% 
                    fill(Year) %>% 
                    mutate(Year = as.numeric(Year)) %>%
                    filter(!is.na(Year)) %>% 
                    mutate(Year = parse_date_time(Year, orders = c("Y", "dmy"))) %>% 
                    fill(`National, Met and Shire County Totals`) %>% 
                    filter(!is.na(`Lower and Single Tier Authority Data`)) %>%
                    arrange(`Lower and Single Tier Authority Data`)

view(CompletionsStack) 

Kingston <- CompletionsStack %>% 
            filter(`Lower and Single Tier Authority Data` == "Swindon")

view(Kingston)

write.csv(Kingston, "Kingston.csv")

setwd("C:/Users/samue/Documents/English Housing Processed/Census Data") 

Census1981 <- read_excel("1981 District Households Edited.xlsx") %>% 
  row_to_names(row_number = 5) %>% 
  mutate(year = 1981) %>% 
  mutate(year = parse_date_time(year, orders = c("Y", "dmy"))) %>% 
  mutate(across(`Total Residents`:`Temporary Accomodation`, ~ as.numeric(.x))) %>% 
  mutate(`Total Private Rented` = `Privately Rented Furnished` + `Privately Rented Unfurnished` + `Rented with job`, + `Rented with business`) %>%
  select(c("pre-1996 local authority district", "Code", "year", "Total Residents", "Total Households", 
           "Owner Occupied Total", "Total Private Rented", "Housing Association", "Public Housing", 
           "Temporary Accomodation"))

view(Census1981)

Census1991 <- read_excel("1991 District Households Edited.xlsx") %>% 
  row_to_names(row_number = 4) %>% 
  mutate(year = 1991) %>% 
  mutate(year = parse_date_time(year, orders = c("Y", "dmy"))) %>% 
  mutate(across(`Total Residents`:`Temporary Accomodation`, ~ as.numeric(.x))) %>%
  mutate(`Owner Occupied Total` = `Owner Occupied` + `Owned with Mortgage`) %>% 
  mutate(`Total Private Rented` = `Privately Rented Furnished` + `Privately Rented Unfurnished` + `Privately Rented with job`) %>% 
  select(c("pre-1996 local authority district", "Code", "year", "Total Residents", "Total Households", 
           "Owner Occupied Total", "Total Private Rented", "Housing Association", "Public Housing", 
           "Temporary Accomodation")) %>% 
  rbind(Census1981) %>% 
  mutate(`pre-1996 local authority district` = gsub("East Yorkshire Borough of Beverley", "Beverley", `pre-1996 local authority district`)) 

Census01I <- read_excel("2001 District Households.xlsx") %>% 
  row_to_names(row_number = 8)

view(Census01I)

Census01 <- read_excel("2001 District Residents.xlsx") %>% 
  row_to_names(row_number = 9) %>% 
  select(-c("pre-1996 local authority district")) %>%
  full_join(Census01I, by = ("Code" = "Code")) %>% 
  mutate(year = 2001) %>% 
  mutate(year = parse_date_time(year, orders = c("Y", "dmy"))) %>%
  mutate(`Temporary Accomodation` = NA) %>% 
  rbind(Census1991) %>% 
  mutate(`pre-1996 local authority district` = gsub("\\&", "and", `pre-1996 local authority district`)) %>% 
  mutate(`pre-1996 local authority district` = gsub("\\-", " ", `pre-1996 local authority district`)) %>% 
  mutate(`pre-1996 local authority district` = gsub("[[:space:]]*UA", "", `pre-1996 local authority district`)) %>%
  mutate(`pre-1996 local authority district` = gsub("Derby City", "Derby", `pre-1996 local authority district`)) %>% 
  mutate(`pre-1996 local authority district` = gsub("Leicester City", "Leicester", `pre-1996 local authority district`)) %>% 
  mutate(`pre-1996 local authority district` = gsub("Folkestone and Hythe", "Shepway", `pre-1996 local authority district`)) %>% 
  mutate(`pre-1996 local authority district` = gsub("Medway Towns", "Medway", `pre-1996 local authority district`)) %>%
  arrange(`pre-1996 local authority district`) 

view(Census01)  

Joined <- CompletionsStack %>% 
          mutate(`Lower and Single Tier Authority Data` = gsub("\\&", "and", `Lower and Single Tier Authority Data`)) %>% 
          mutate(`Lower and Single Tier Authority Data` = gsub("\\-", " ", `Lower and Single Tier Authority Data`)) %>% 
          mutate(`Lower and Single Tier Authority Data` = gsub("[[:space:]]*UA", "", `Lower and Single Tier Authority Data`)) %>% 
          mutate(`Lower and Single Tier Authority Data` = gsub("St. Helens", "St Helens", `Lower and Single Tier Authority Data`)) %>%
          mutate(`Lower and Single Tier Authority Data` = gsub("South Bucks", "South Buckinghamshire", `Lower and Single Tier Authority Data`)) %>% 
          mutate(`Lower and Single Tier Authority Data` = gsub("Westminster", "Westminster, City of", `Lower and Single Tier Authority Data`)) %>% 
          mutate(`Lower and Single Tier Authority Data` = gsub("Bristol, City of", "Bristol", `Lower and Single Tier Authority Data`)) %>% 
          mutate(`Lower and Single Tier Authority Data` = gsub("Kingston upon Hull, City of", "Kingston upon Hull", `Lower and Single Tier Authority Data`)) %>%
          left_join(Census01, by = c("Year" = "year", "Lower and Single Tier Authority Data" = "pre-1996 local authority district")) %>% 
          mutate(`Lower and Single Tier Authority Data` = str_to_upper(`Lower and Single Tier Authority Data`))

view(Joined) 

Check <- Joined %>% 
         filter(Year == as.Date("1981-01-01") | Year == as.Date("1991-01-01") | Year == as.Date("2001-01-01")) %>% 
         filter(is.na(Code))

view(Check) 

setwd("C:/Users/samue/Documents/English Housing Processed/Local Elections Data")

CouncilComp <- read_excel("history1973-2023.xlsx") %>% 
  select(c("Council":"Maj")) %>% 
  mutate(Year = parse_date_time(Year, orders = c("Y", "dmy"))) %>% 
  mutate(Council = gsub("\\&", "and", Council)) %>% 
  mutate(Council = gsub("\\-", " ", Council)) %>% 
  mutate(Council = gsub("Blackburn ", "Blackburn", Council)) %>% 
  filter(Year >= as.Date("1973-01-01") & Year <= as.Date("2001-01-01")) %>% 
  mutate(Council = str_to_upper(Council)) %>%
  arrange(Year) %>% 
  arrange(Council) 

write_csv(CouncilComp, "Council Processed.csv")
  
view(CouncilComp) 
  
PolMerge <- CouncilComp %>%   
            left_join(Joined, by = c("Year" = "Year", "Council" = "Lower and Single Tier Authority Data")) %>% 
            group_by(Year) %>% 
            arrange(Council) 

view(PolMerge) 

write_csv(PolMerge, "Building and Council Processed.csv")

PolCheck <- CouncilComp %>%   
  right_join(Joined, by = c("Year" = "Year", "Council" = "Lower and Single Tier Authority Data")) %>% 
  group_by(Year) %>% 
  arrange(Council) %>% 
  filter(is.na(Total))

view(PolCheck) 

write_csv(PolCheck, "Building Processed.csv") 

Processed <- read_excel("Building and Council Processed.xlsx") %>% 
             mutate(across(`DCLG code`:`Temporary Accomodation`, na_if, "NA")) %>% 
             group_by(Council) %>%
             fill(`National, Met and Shire County Totals`, .direction = "up") %>% 
             filter(!is.na(`National, Met and Shire County Totals`)) %>% 
             mutate(Year = gsub("T00:00:00Z", "", Year)) %>% 
             mutate(Year = ymd(Year)) %>% 
             mutate(Council = str_to_upper(Council)) %>% 
             mutate(Council = gsub("KING'S LYNN AND WEST NORFOLK", "KINGS LYNN AND WEST NORFOLK", Council)) %>% 
             mutate(Council = str_squish(Council))

view(Processed)  

filtered <- Processed %>% 
            filter(Council == "SWINDON")

view(filtered)

books3 <- c("District level turnout, candidates edited.xlsx")  

Elections <- books3 %>% 
             excel_sheets() %>% 
             set_names() %>% 
             map_dfr(read_excel, path = books3) %>% 
             dplyr::select(-c("...1")) %>% 
             fill(Year) %>% 
             mutate(Year = parse_date_time(Year, orders = c("Y", "ymd"))) %>% 
             mutate(Council = str_to_upper(Council)) %>% 
             mutate(Council = gsub("\\&", "AND", Council)) %>% 
             mutate(Council = gsub("\\-", " ", Council)) %>% 
             mutate(Council = gsub("THE WREKIN", "TELFORD AND WREKIN", Council)) 

view(Elections) 

Electfilt <- Elections %>% 
             filter(Council == "TELFORD AND WREKIN") 

view(Electfilt)

names(Processed)
view(Processed) 

PolMerge <- Processed %>% 
            left_join(Elections, by = c("Year" = "Year", "Council" = "Council")) %>% 
            mutate(Council = str_to_title(Council)) %>% 
            dplyr::select(-c("Code")) %>%
            mutate(across(`Private\r\r\nEnterprise`:`Temporary Accomodation`, ~ as.numeric(.x))) %>% 
            group_by(Year, Council) %>% 
            summarise(across(Total:Other, ~ mean(.x, na.rm = TRUE)), across(`Private\r\r\nEnterprise`:All, ~ mean(.x)), 
                      across(`Total Residents`: `Temporary Accomodation`, ~ mean(.x, na.rm = TRUE)), 
                      across(`Total wards`:`Total Ware vote`, ~ sum(.x)), across(`no. of candidates`:`UKIP candidates`, ~ sum(.x)))

names(PolMerge)
view(PolMerge) 

York <- PolMerge %>% 
        filter(Year == as.Date("1975-01-01")) %>% 
        filter(Remain == 0)

view(York)

Polfilt <- PolMerge %>% 
           filter(Year == as.Date("1986-01-01")) 

view(Polfilt)

PolCheck <- Processed %>% 
            right_join(Elections, by = c("Year" = "Year", "Council" = "Council")) %>% 
            filter(Year >= as.Date("1980-01-01")) %>% 
            filter(is.na(Total)) %>% 
            arrange(Council) 

view(PolCheck) 

PolCheck2 <- PolMerge %>% 
             filter(is.na(`Total wards`)) %>% 
             group_by(Council) %>% 
             summarise(n = n()) %>% 
             arrange(desc(n))

view(PolCheck2) 

setwd("C:/Users/samue/Documents/English Housing Processed/Data Output") 

Postwar <- read_excel("Pre-1980 Completions.xlsx") %>% 
           dplyr::select(-c("...1")) %>% 
           filter(!is.na(District)) %>% 
           mutate(Year = parse_date_time(Year, orders = c("Y", "dmy"))) %>% 
           mutate(across(Population:`Other Demolitions`, ~ gsub("\\,", "\\." , .x))) %>% 
           mutate(across(Population:`Other Demolitions`, ~ gsub("\\-", "\\." , .x))) %>% 
           mutate(across(Population:`Other Demolitions`, ~ gsub("[^0-9\\.]", "" , .x))) %>% 
           mutate(across(Population:`Other Demolitions`, ~ as.numeric(.x))) %>% 
           mutate(`2004 District` = District) %>% 
           mutate(`2004 District` = gsub("Barking", "Barking And Dagenham", `2004 District`)) %>%
           mutate(`2004 District` = gsub("Bracknell", "Bracknell Forest", `2004 District`)) %>% 
           mutate(`2004 District` = gsub("Peterlee", "Durham", `2004 District`)) %>% 
           mutate(`2004 District` = gsub("Aycliffe", "Durham", `2004 District`)) %>% 
           mutate(`2004 District` = gsub("Skelmersdale", "West Lancashire", `2004 District`)) %>% 
           mutate(`2004 District` = gsub("Runcorn", "Halton", `2004 District`)) %>% 
           mutate(`2004 District` = gsub("Washington", "Sunderland", `2004 District`)) %>%
           group_by(Year, `2004 District`) %>% 
           summarise(across(c("Population":"Other Demolitions"), ~ mean(.x, na.rm = TRUE))) %>% 
           mutate(District = `2004 District`)
           
view(Postwar) 

Durcheck <- PolMerge %>% filter(Council %in% c("Durham", "West Lancashire", "Halton"))

view(Durcheck)

AltPol <- PolMerge %>% 
          filter(Year >= as.Date("1974-01-01") & Year <= as.Date("1979-01-01")) %>% 
          left_join(Postwar, by = c("Council" = "District", "Year" = "Year")) %>% 
          dplyr::select(-c("Population", "Private\r\r\nEnterprise", "Housing Associations.x", "Local\r\r\nAuthority", "All")) %>% 
          rename("Private\r\r\nEnterprise" = "Private Sector", "Housing Associations" = "Housing Associations.y", 
                 "Local\r\r\nAuthority" = "Local Authorities", "All" = "Total.y") %>% 
          mutate(`Slum Clearance Demoliions` = gsub("NaN", 0, `Slum Clearance Demoliions`)) %>%
          mutate(`Other Demolitions` = gsub("NaN", 0, `Other Demolitions`)) %>% 
          mutate(across(`Slum Clearance Demoliions`:`Other Demolitions`, ~ as.numeric(.x))) %>%
          mutate(Demolitions = `Slum Clearance Demoliions` + `Other Demolitions`) %>% 
          dplyr::select(-c("Slum Clearance Demoliions", "Other Demolitions")) %>% 
          rename("Total" = "Total.x")
          
          
names(PolMerge)
names(AltPol)
view(AltPol) 
view(PolMerge)

FiltPol <- AltPol %>% 
           filter(is.na(Total.y)) %>% 
           arrange(Year)

view(FiltPol) 

Alt2Pol <- Postwar %>% 
  filter(Year >= as.Date("1974-01-01") & Year <= as.Date("1979-01-01")) %>% 
  left_join(PolMerge, by = c("District" = "Council", "Year" = "Year")) 

view(Alt2Pol) 

Filt2Pol <- Alt2Pol %>% 
  filter(is.na(Total.y)) %>% 
  arrange(District)

view(Filt2Pol) 

CleanCompletions <- PolMerge %>% 
                    mutate(`2004 District` = Council) %>% 
  mutate(Demolitions = NA) %>% 
  filter(Year >= as.Date("1980-01-01")) %>% 
  rbind(AltPol) %>%
  mutate(`2004 District` = gsub("Bath", "Bath and North East Somerset", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Wansdyke", "Bath and North East Somerset", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Westminster", "City of Westminster", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Langbaurgh On Tees", "Redcar and Cleveland", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Langbaurgh", "Redcar and Cleveland", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Blackburn", "Blackburn with Darwen", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("East Yorkshire", "East Riding of Yorkshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Holderness", "East Riding of Yorkshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Beverley", "East Riding of Yorkshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Cleethorpes", "North East Lincolnshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Great Grimsby", "North East Lincolnshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Glanford", "North Lincolnshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Scunthorpe", "North Lincolnshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Hereford", "Herefordshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("South Herefordshireshire", "Herefordshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Telford", "Telford and Wrekin", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("The Wrekin", "Telford and Wrekin", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Bristol", "Bristol, City of", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Woodspring", "North Somerset", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Kingswood", "South Gloucestershire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Mangotsfield", "South Gloucestershire", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Thamesdown", "Swindon", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Rochester Upon Medway", "Medway Towns", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Gillingham", "Medway Towns", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Newbury", "West Berkshire", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Brighton", "Brighton and Hove", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Hove", "Brighton and Hove", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Medina", "Isle of Wight", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("South Wight", "Isle of Wight", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Ellesmere Port \\& Neston", "Ellesmere Port and Neston", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("St Albans", "St. Albans", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("St Edmundsbury", "St. Edmundsbury", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("St Helens", "St. Helens", `2004 District`)) %>%
  mutate(`2004 District` = gsub("[[:space:]]*UA", "", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Bristol, City Of, City Of", "Bristol, City of", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Bath and North East Somerset And North East Somerset", "Bath and North East Somerset", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Kingston Upon Hull, City Of", "Kingston upon Hull", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Brighton and Brighton and Hove", "Brighton and Hove", `2004 District`)) %>%
  mutate(`2004 District` = gsub("Brighton and Hove And Brighton and Hove", "Brighton and Hove", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Peterlee", "Durham", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Aycliffe", "Durham", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Skelmersdale", "West Lancashire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Runcorn", "Halton", `2004 District`)) %>%  
  mutate(`2004 District` = gsub("Blackburn with Darwen With Darwen", "Blackburn With Darwen", `2004 District`)) %>%  
  mutate(`2004 District` = gsub("Northavon", "South Gloucestershire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Herefordshireshire", "Herefordshire", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Medway", "Medway towns", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Medway Towns Towns", "Medway towns", `2004 District`)) %>% 
  mutate(`2004 District` = gsub("Telford and Wrekin And Wrekin", "Telford And Wrekin", `2004 District`)) %>%
  mutate(`2004 District` = str_to_title(`2004 District`)) 

view(CleanCompletions) 

filter <- CleanCompletions %>% 
          filter(Council == "Thamesdown") 

filter2 <- CleanCompletions %>% 
           filter(`2004 District` == "Swindon") 

view(filter2)

`2004` <- CleanCompletions %>%  
          select(c("2004 District"))

view(`2004`)

view(RtBmerge)

Merged <- RtBmerge %>% 
          left_join(CleanCompletions, by = c("year" = "Year", "lpa_name_until2004" = "2004 District"), keep = TRUE) 

view(Merged) 

Check <- Merged %>% 
         filter(year >= as.Date("1980-01-01") & year <= as.Date("1997-01-01")) %>% 
         filter(is.na(Council))

view(Check) 

Final <- Merged %>% 
         filter(Council != "Isles Of Scilly") 

Merged2 <- CleanCompletions %>% 
           left_join(RtBmerge, by = c("Year" = "year",  "2004 District" = "lpa_name_until2004"), keep = TRUE) 

view(Merged2) 

Check2 <- Merged2 %>% 
  filter(year >= as.Date("1980-01-01") & year <= as.Date("1997-01-01")) %>% 
  select(c("lpa_name_until2004")) 

view(Check2) 

#SplittingOld Distrcts 

DistrictSales <- Pre80Sale %>% 
  filter(District %in% c("Boothferry", "Leominster", "Northavon")) %>% 
  dplyr::select(c("Year", "District", "Total Sales"))

Districted <- CleanCompletions %>% 
              filter(Council %in% c("Boothferry", "Leominster", "Northavon"))  %>% 
              left_join(DistrictSales, by = c("Year" = "Year", "Council" = "District")) %>% 
              rename("Right to Buy Sales" = "Total Sales")

view(CleanCompletions)

view(Districted) 

view(Pre80Sale)
 
view(DistrictSales)

setwd("C:/Users/samue/Documents/English Housing Processed/Census Data") 

Prop91 <- read_excel("1991 Ward Households Edited.xlsx") 

view(SelectDistrict)

SelectDistrict <- Joined %>% 
                  filter(`1991 District` %in% c("Boothferry", "Leominster")) %>% 
                  left_join(Prop91, by = c("1991 Ward" = "1991 frozen ward")) %>% 
                  mutate(`Owner Occupied Total` = `Owner Occupied` + `Owner Occupied Mortgage`) %>% 
                  mutate(`Total Private Rented` = `Privately Rented Furnished` + `Privately Rented Unfurnished` + 
                         `Company Rented`) %>% 
                  dplyr::select(-c("Owner Occupied", "Owner Occupied Mortgage", "Privately Rented Furnished", 
                            "Privately Rented Unfurnished", "Company Rented")) %>% 
                  distinct() %>% 
                  group_by(`1991 District`, `2001 District`) %>% 
                  summarise(across(`Total Households`:`Total Private Rented`, ~ sum(.x))) %>% 
                  pivot_longer(-c("1991 District", "2001 District"), names_to = "Item", values_to = "Value") %>% 
                  ungroup() %>% 
                  pivot_wider(names_from = `2001 District`, values_from = Value) %>% 
                  mutate(EastRidingRat = `East Riding of Yorkshire`/ (`North Lincolnshire UA` + `East Riding of Yorkshire`)) %>% 
                  mutate(NorthLincolnRat = `North Lincolnshire UA` / (`North Lincolnshire UA` + `East Riding of Yorkshire`)) %>% 
                  mutate(HertfordshireRat = `County of Herefordshire` / (`County of Herefordshire` + `Malvern Hills`)) %>% 
                  mutate(MalvernRat = `Malvern Hills` / (`County of Herefordshire` + `Malvern Hills`)) %>% 
                  dplyr::select(-c("East Riding of Yorkshire":"Malvern Hills")) %>% 
                  pivot_longer(-c("1991 District", "Item"), names_to = "2004 District", values_to = "Value") %>% 
                  filter(!is.na(Value)) %>% 
                  relocate(`2004 District`, .after = `1991 District`) %>% 
                  pivot_wider(names_from = Item, values_from = Value) %>%
                  left_join(Districted, by = c("1991 District" = "Council")) %>% 
                  relocate(Demolitions, `Right to Buy Sales`, .after = All) %>%
                  mutate(across(`Total Residents.y`:`Temporary Accomodation.y`, ~ as.numeric(.x))) %>% 
                  mutate(across(`Private\r\r\nEnterprise`:All, ~ as.numeric(.x))) %>%
                  mutate(`Total Residents` = `Total Residents.y`* `Total Residents.x`) %>% 
                  mutate(`Total Households` = `Total Households.y` * `Total Households.x`) %>% 
                  mutate(`Owner Occupied Total` = `Owner Occupied Total.y` * `Owner Occupied Total.x`) %>% 
                  mutate(`Public Housing` = `Public Housing.y` * `Public Housing.x`) %>% 
                  mutate(`Housing Association` = `Housing Association.y`  * `Housing Association.x`) %>% 
                  mutate(`Total Private Rented` = `Total Private Rented.y` * `Total Private Rented.x`) %>% 
                  mutate(`Temporary Accomodation` = `Temporary Accomodation.y` * `Temporary Accomodation.x`) %>% 
                  mutate(`Private\r\r\nEnterprise` = `Private\r\r\nEnterprise` * `Owner Occupied Total.x`) %>% 
                  mutate(`Housing Associations` = `Housing Associations` *`Housing Association.x`) %>% 
                  mutate(`Local\r\r\nAuthority` = `Local\r\r\nAuthority`* `Public Housing.x`) %>% 
                  mutate(Demolitions = Demolitions * `Public Housing.x`) %>% 
                  mutate(`Right to Buy Sales` = `Right to Buy Sales`* `Public Housing.x`) %>% 
                  mutate(All = `Private\r\r\nEnterprise` + `Housing Associations` + `Local\r\r\nAuthority`) %>% 
                  mutate(across(Total:Other, ~ .x * `Total Residents.x`)) %>% 
                  mutate(across(`Total wards`:`Total Ware vote`, ~ .x * `Total Residents.x`)) %>% 
                  mutate(`2004 District.x` = gsub("EastRidingRat", "East Riding of Yorkshire", `2004 District.x`)) %>%
                  mutate(`2004 District.x` = gsub("NorthLincolnRat", "North Lincolnshire", `2004 District.x`)) %>%
                  mutate(`2004 District.x` = gsub("HertfordshireRat", "Herefordshire", `2004 District.x`)) %>% 
                  mutate(`2004 District.x` = gsub("MalvernRat", "Malvern Hills", `2004 District.x`)) %>% 
                  dplyr::select(-c("Total Households.x":"Total Private Rented.x")) %>% 
                  dplyr::select(-c("Total Residents.y":"Temporary Accomodation.y")) %>% 
                  dplyr::select(-c("2004 District.y")) %>% 
                  rename("2004 District" = "2004 District.x", "Council" = "1991 District") 
  
view(SelectDistrict)  

view(RtBmerge)

view(CleanCompletions)

Compjoin <- CleanCompletions %>% 
            mutate(across(`Private\r\r\nEnterprise`:All, ~ as.numeric(.x))) %>%
            mutate(across(`Total Residents`:`Temporary Accomodation`, ~ as.numeric(.x))) %>% 
            mutate(`Right to Buy Sales` = NA) %>% 
            filter(`2004 District` != "Boothferry" & `2004 District` != "Leominster" & `2004 District` != "Glc") %>% 
            rbind(SelectDistrict) %>%  
            mutate(across(`Private\r\r\nEnterprise`:All, ~ replace_na(.x, 0))) %>% 
            mutate(across(`Total wards`:`SNP candidates`, ~ replace_na(.x, 0))) %>%
            mutate(`UKIP candidates` = replace_na(`UKIP candidates`, 0)) %>% 
            mutate(across(`Demolitions`:`Right to Buy Sales`, ~ replace_na(.x, 0))) %>% 
            group_by(Year, `2004 District`) %>%
            summarise(across(where(is.numeric), ~ sum(.x, na.rm = FALSE))) %>% 
            right_join(RtBmerge, by = c("Year" = "year", "2004 District" = "lpa_name_until2004")) %>% 
            filter(!is.na(`2004 District`)) %>%
            filter(Year <= as.Date("2001-01-01")) %>%
            dplyr::select(-c("Local Authority Name","Year.y")) %>% 
            mutate(`Right to Buy Sales.x` = na_if(`Right to Buy Sales.x`, 0)) %>%
            mutate(`Right to Buy Sales.y` = as.numeric(`Right to Buy Sales.y`)) %>% 
            mutate(`Right to Buy Sales` = coalesce(`Right to Buy Sales.x`, `Right to Buy Sales.y`)) %>% 
            dplyr::select(-c("Right to Buy Sales.x", "Right to Buy Sales.y")) %>% 
            mutate(`Right to Buy Sales` = replace_na(`Right to Buy Sales`, 0))
   
names(Compjoin)         
mutate(across(`Total`:`Other`, ~ replace_na(.x, 0))) 
view(Compjoin) 

filtered <- Compjoin %>% 
            filter(`2004 District` == "Telford And Wrekin")

view(filtered) 

filtered <- Compjoin %>%  
            filter(`2004 District` == "Herefordshire")

view(filtered) 

Compclean <- Compjoin %>% 
             mutate(across(`Private\r\r\nEnterprise`:All, ~ replace_na(.x, 0))) %>% 
             mutate(All = `Private\r\r\nEnterprise` + `Local\r\r\nAuthority` + `Housing Associations`) %>% 
             mutate(across(`Total wards`:`SNP candidates`, ~ replace_na(.x, 0))) %>%
             filter(!is.na(`2004 District`))

view(Compclean) 

CompTel <- Compclean %>% filter(`2004 District` == "Swindon") 

view(CompTel) 

view(Summarised)

Summarised <- Compjoin %>%  
              arrange(`2004 District`) %>% 
              mutate(Period = ifelse( Year >= as.Date("1981-01-01") & Year <= as.Date("1990-01-01"), 1, 
                         ifelse(Year >= as.Date("1991-01-01") & Year <= as.Date("2000-01-01"), 2, 0))) %>%
              group_by(`2004 District`, Period) %>% 
              mutate(`Right to Buy Sales` = replace_na(`Right to Buy Sales`, 0)) %>%
              mutate(CumuBuild = cumsum(All)) %>% 
              mutate(NetPriv = `Private\r\r\nEnterprise` + `Right to Buy Sales`) %>% 
              mutate(CumuPriv = cumsum(NetPriv)) %>% 
              mutate(NetPub = `Local\r\r\nAuthority` - `Right to Buy Sales`) %>% 
              mutate(CumuPub = cumsum(NetPub)) %>% 
              mutate(CumuHassoc = cumsum(`Housing Associations`)) %>% 
              mutate(TotBuild = sum(All)) %>%
              ungroup() %>% 
              mutate(Correct = ifelse(Year <= as.Date("1980-01-01"), -1, 1)) %>%
              group_by(Correct,`2004 District`) %>%
              mutate(CumuBuild = cumsum(All)) %>% 
              mutate(across(`CumuBuild`:`CumuHassoc`, ~ .x * Correct)) %>%
              group_by(`2004 District`) %>%
              filter(Year <= as.Date("2001-01-01")) %>% 
              mutate(BaseHouse = `Total Households`) %>% 
              mutate(BaseHouse = ifelse(Year == as.Date("1981-01-01"), BaseHouse, 0)) %>% 
              mutate(BaseHouse = na_if(BaseHouse, 0)) %>% 
              fill(BaseHouse, .direction = "updown")
              
view(Summarised) 

filt <- Summarised %>% 
        filter(`2004 District` == "Telford And Wrekin")

view(filt) 

Interpolate <- Summarised %>% 
               filter(Year >= as.Date("1981-01-01")) %>%
               mutate(BaseHouse = `Total Households`) %>%
               fill(BaseHouse, .direction = "downup") %>%
               mutate(Period = ifelse(Year >= as.Date("1981-01-01") & Year <= as.Date("1990-01-01")  , 1,
                               ifelse(Year >= as.Date("1991-01-01")  & Year <= as.Date("2000-01-01"), 2, 0))) %>% 
               group_by(Period, `2004 District`) %>% 
               mutate(CumuBuild = cumsum(All)) %>%
               mutate(`1980 Base` = BaseHouse + CumuBuild) %>%  
               group_by(`2004 District`) %>%
               mutate(BaseComp = lead(BaseHouse)) %>% 
               mutate(BaseComp = ifelse(Year == as.Date("1990-01-01") | Year == as.Date("2000-01-01"), BaseComp, NA)) %>% 
               mutate(Diff = BaseComp - `1980 Base`) %>% 
               fill(Diff, .direction = "up") %>% 
               mutate(Frac = All / TotBuild) %>% 
               mutate(Adjust = Frac * Diff) %>%
               mutate(Net = All + Adjust) %>% 
               group_by(Period, `2004 District`) %>%
               mutate(AdjCheck = cumsum(Adjust)) %>%
               mutate(CumuNet = cumsum(Net)) %>% 
               mutate(CumuAdjust = CumuNet - Net) %>% 
               mutate(CheckHousing = BaseHouse + CumuNet) %>%
               mutate(FullHousingStock = BaseHouse + CumuAdjust) %>% 
               mutate(FinalHousingStock = coalesce(FullHousingStock, BaseHouse)) %>% 
               mutate(BaseOwn = `Owner Occupied Total`) %>% 
               fill(BaseOwn, .direction = "down") %>% 
               mutate(NetOwn = `Private\r\r\nEnterprise` + `Right to Buy Sales`) %>% 
               mutate(CumuOwn = cumsum(NetOwn)) %>% 
               mutate(BaseOwn1980 = BaseOwn + CumuOwn) %>% 
               group_by(`2004 District`) %>%
               mutate(BaseOwnComp = lead(BaseOwn)) %>% 
               mutate(BaseOwnComp = ifelse(Year == as.Date("1990-01-01") | Year == as.Date("2000-01-01"), BaseOwnComp, NA)) %>% 
               mutate(DiffOwn = BaseOwnComp - BaseOwn1980) %>% 
               fill(DiffOwn, .direction = "up") %>% 
               group_by(Period, `2004 District`) %>%
               mutate(TotOwn = sum(NetOwn)) %>% 
               mutate(FracOwn = NetOwn / TotOwn) %>% 
               mutate(AdjustOwn = FracOwn * DiffOwn) %>% 
               mutate(AdjNetOwn = NetOwn + AdjustOwn) %>% 
               mutate(CumuAdjNetOwn = cumsum(AdjNetOwn) - NetOwn) %>% 
               mutate(FullOwnHousingStock = BaseOwn + CumuAdjNetOwn) %>% 
               mutate(FinalOwnHousingStock = coalesce(FullOwnHousingStock, BaseOwn)) %>% 
               mutate(BasePub = `Public Housing` + `Housing Association`) %>% 
               fill(BasePub, .direction = "down") %>% 
               mutate(NetPublic = `Local\r\r\nAuthority` + `Housing Associations` - `Right to Buy Sales`) %>% 
               mutate(CumuPublic = cumsum(NetPublic)) %>% 
               mutate(BasePub1980 = BasePub + CumuPublic) %>% 
               group_by(`2004 District`) %>%
               mutate(BasePubComp = lead(BasePub)) %>%
               mutate(BasePubComp = ifelse(Year == as.Date("1990-01-01") | Year == as.Date("2000-01-01"), BasePubComp, NA)) %>% 
               mutate(DiffPub = BasePubComp - BasePub1980) %>% 
               fill(DiffPub, .direction = "up") %>% 
               group_by(Period, `2004 District`) %>%
               mutate(TotPublic = sum(`Right to Buy Sales`)) %>% 
  mutate(FracPublic = `Right to Buy Sales` / TotPublic) %>% 
  mutate(AdjustPublic = FracPublic * DiffPub) %>% 
  mutate(AdjNetPublic = NetPublic + AdjustPublic) %>% 
  mutate(CumuAdjNetPublic = cumsum(AdjNetPublic) - NetPublic) %>% 
  mutate(FullPublicHousingStock = BasePub + CumuAdjNetPublic) %>% 
  mutate(FinalPublicHousingStock = coalesce(FullPublicHousingStock, BasePub)) %>% 
  ungroup() %>%
  dplyr::select(c("Year":"Right to Buy Sales", "FinalHousingStock", "FinalOwnHousingStock", "FinalPublicHousingStock")) %>% 
  rename("Total Housing Stock" = "FinalHousingStock", "Owner Occupied Stock" = "FinalOwnHousingStock", "Non Profit Housing Stock" = "FinalPublicHousingStock")

view(Interpolate) 
view(Summarised)

IntEarly <- Summarised %>% 
            mutate(BaseHouse = `Total Households`) %>%
            fill(BaseHouse,.direction = "downup") %>% 
            mutate(BaseOwner = `Owner Occupied Total`) %>% 
            fill(BaseOwner,.direction = "downup") %>%
            mutate(BasePub = `Public Housing` + `Housing Association`) %>% 
            fill(BasePub,.direction = "downup") %>%
            filter(Year <= as.Date("1980-01-01")) %>% 
            mutate(Net = All - Demolitions) %>%
            mutate(Cumubuild = cumsum(Net)) %>% 
            mutate(AdjBuild = Cumubuild - Net) %>%
            mutate(Totbuild = sum(Net)) %>% 
            mutate(Base74 = BaseHouse - TotBuild) %>% 
            mutate(FullHousingStock = Base74 + AdjBuild) %>% 
            mutate(NetOwner = `Private\r\r\nEnterprise` + `Right to Buy Sales`) %>%
            mutate(CumuPrivate = cumsum(NetOwner)) %>% 
            mutate(TotPriv = sum(NetOwner)) %>% 
            mutate(AdjPrivate = CumuPrivate - NetOwner) %>% 
            mutate(Priv74 = BaseOwner - TotPriv + AdjPrivate) %>% 
            mutate(NetPublic = `Local\r\r\nAuthority` + `Housing Associations` - `Right to Buy Sales`) %>% 
            mutate(CumuPublic = cumsum(NetPublic)) %>% 
            mutate(AdjPublic = CumuPublic - NetPublic) %>% 
            mutate(TotPublic = sum(NetPublic)) %>% 
            mutate(Pub74 = BasePub - TotPublic + AdjPublic) %>% 
            dplyr::select(c("Year":"Right to Buy Sales", "FullHousingStock", "Priv74", "Pub74")) %>% 
            rename("Total Housing Stock" = "FullHousingStock", "Owner Occupied Stock" = "Priv74", "Non Profit Housing Stock" = "Pub74") %>% 
            rbind(Interpolate) %>% 
            arrange(`2004 District`)
            
view(IntEarly) 

setwd("C:/Users/samue/Documents/English Housing Processed/Data Output")

write_csv(IntEarly, "District Data.csv")

Check <- IntEarly %>% 
         filter(is.na(Total)) 

view(Check)

Check <- IntEarly %>% 
         filter(Year == as.Date("1981-01-01") | Year == as.Date("1991-01-01") | Year == as.Date("2001-01-01")) %>% 
         filter(`Total Households` != `Total Housing Stock`)
view(Check)





