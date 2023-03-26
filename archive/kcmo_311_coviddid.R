# library(dplyr)
# library(reshape2)
# library(openxlsx)
# library(car)
# 
# #change directory
# setwd("C:/Users/bebut/Downloads")
# 
# #######################################
# # Data Cleaning 
# #######################################
# 
# #load data
# raw_a = read.xlsx("Group3-311DataPreCOVID-Warm Season.xlsx")[,-31] %>% mutate(season = "PreCOVID-Warm")
# raw_b = read.xlsx("Group2-311DataPreCOVID-Cold Season.xlsx")[,-31] %>% mutate(season = "PreCOVID-COld")
# raw_c = read.xlsx("Group1-311DataDuringCOVID.xlsx")[,-31] %>% mutate(season = "COVID-warm")
# 
# raw = rbind(raw_a,raw_b) 
# raw = rbind(raw,raw_c)
# 
# sec = read.xlsx("SocioEconomic.xlsx")
# 
# rm(raw_a)
# rm(raw_b)
# rm(raw_c)
# 
# #merge 
# inc = sec %>% select(NEIGH = NBH_NAME, Income.Level)
# dat = left_join(raw,inc, by="NEIGH")
# dat$Income.Level[dat$NEIGH == "Central Blue Valley and Park Tower Gardens"] = "L"      
# 
# #drop na neighborhood, income level, source, category, day to close 
# dat = dat %>% filter(!(is.na(NEIGH)|is.na(Income.Level)|is.na(SOURCE)|is.na(CATEGORY)|is.na(DAYTOCLOSE)))
# 
# #drop sept 2020 for low count 
# dat = dat %>% filter(!(CREATEYR==2020 & CREATEMO == 9))
# 
# #weight 
# dat$weight = 1
# 
# #######################################
# # Scraping 
# #######################################
# 
# diseasecontrol = dat %>% filter(REQTYPE == "Public Health-Disease Control-All")
# 
# library(rvest)
# 
# diseasecontrol$resol_sum = NA
# diseasecontrol$resol_des = NA
# 
# diseasecontrol$resol_sum2 = NA
# diseasecontrol$resol_des2 = NA
# 
# for (i in  1:length(diseasecontrol$CASEURL)){
#   
#   tryCatch({
#     diseasecontrol$resol_sum[i] = strsplit((read_html(diseasecontrol$CASEURL[i])%>% html_nodes("tr") %>% html_text()),"\r\n")[[1]][5]
#     diseasecontrol$resol_des[i] = strsplit((read_html(diseasecontrol$CASEURL[i])%>% html_nodes("tr") %>% html_text()),"\r\n")[[1]][7]
#     
#     diseasecontrol$resol_sum2[i] = gsub("  ","",toString(diseasecontrol$resol_sum[i]))
#     diseasecontrol$resol_des2[i] = gsub("  ","",toString(diseasecontrol$resol_des[i]))
#     
#     print(i)
#   }, error=function(e){"ERROR"})
# }
# 
# save(dat, "C:/Users/bebut/Downloads/kcmo2019_2020.rdata")

#######################################
# Analysis 
#######################################
setwd("C:/Users/bebut/Downloads")

library(dplyr)
library(reshape2)
library(openxlsx)
library(car)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(arm)
library(GGally)

#load data
load("C:/Users/bebut/Downloads/kcmo2019_2020.rdata")

# create year+month var
dat$date = dat$CREATEYR*100 +dat$CREATEMO

dat$date = as.Date(as.character(dat$date*100+1),"%Y%m%d")

# create covid related calls 
dat = dat %>%
  mutate(
    
    covid = as.numeric(grepl("covid",tolower(description))|
                         grepl("corona",tolower(description))|
                         grepl("pandemic",tolower(description))|
                         grepl("virus",tolower(description))|
                         grepl("positive",tolower(description))
    ),
    
    mask = as.numeric(grepl("mask",tolower(description))|
                        grepl("face cover",tolower(description))|
                        grepl("ppe ",tolower(description))|
                        grepl("coverings",tolower(description))
    ),
    
    sdistance = as.numeric(grepl("social",tolower(description))|
                             grepl("distanc",tolower(description))|
                             grepl("6 feet",tolower(description))|
                             grepl("quarantine",tolower(description))|
                             grepl("stay at home",tolower(description))|
                             grepl("gathering",tolower(description))
    ),
    
    essential = as.numeric(grepl("essential",tolower(description))|
                             grepl("still open",tolower(description))|
                             grepl("open for business",tolower(description))|
                             grepl("open and operating",tolower(description))|
                             grepl("still operating",tolower(description))
    ), 
    
    allcovid = as.numeric(covid==1|mask==1|sdistance==1|essential==1)
  )

###########################################################################
###########################################################################

#covid cases by zipcode as of january 15, 2022
zip_covid = read.csv("COVID-19_Data_by_ZIP_Code.csv") 
zip_covid = zip_covid %>% dplyr::select(ZipCode,Cases,Total.Residents.Tested) %>% mutate(ZIP = ZipCode,rate = Cases/Total.Residents.Tested ) %>% dplyr::select(ZIP, rate)

regdf = dat %>% 
  # share of requests from each zip
  mutate(count = sum(weight))%>%
  group_by(ZIP) %>%
  mutate(zip_count = sum(weight),zip_sh = zip_count/count) %>%
  #zip x date calc
  group_by(date,ZIP) %>% summarise(
  covid_sh = mean(allcovid),
  ph_sh = mean(CATEGORY=="Public Health"),
  ps_sh = mean(CATEGORY=="Public Safety"),
  trash_sh = mean(CATEGORY=="Trash / Recycling"),
  park_sh = mean(CATEGORY=="Parks & Recreation"),
  prop_sh = mean(CATEGORY=="Property / Buildings / Construction"),
  
  ph_t = mean(DAYTOCLOSE[CATEGORY=="Public Health"],na.rm=T),
  ps_t = mean(DAYTOCLOSE[CATEGORY=="Public Safety"],na.rm=T),
  trash_t = mean(DAYTOCLOSE[CATEGORY=="Trash / Recycling"],na.rm=T),
  park_t = mean(DAYTOCLOSE[CATEGORY=="Parks & Recreation"],na.rm=T),
  prop_t = mean(DAYTOCLOSE[CATEGORY=="Property / Buildings / Construction"],na.rm=T),
  
  count = mean(count),
  zip_count = mean(zip_count),
  zip_sh = mean(zip_sh),
  zipdate_count = sum(weight),
) %>% 
  left_join(zip_covid,by="ZIP") %>%
  #only allow those with positive in all zipcode all time 
  mutate(weight = 1) %>%
  group_by(ZIP) %>% mutate(ndate = sum(weight)) %>% filter(ndate == 18) %>%
  #covid indicator
  mutate(covid = as.numeric(date > as.Date("2020-02-01","%Y-%m-%d")))



regdf$date = as.factor(regdf$date)
regdf$ZIP = as.factor(regdf$ZIP)

#reduced form
# summary(lm(ph_sh~covid:rate+rate+date,data = regdf,weights = zip_sh))
# summary(lm(ps_sh~covid:rate+rate+date,data = regdf,weights = zip_sh))
# summary(lm(park_sh~covid:rate+rate+date,data = regdf,weights = zip_sh))
# summary(lm(trash_sh~covid:rate+rate+date,data = regdf,weights = zip_sh))
# summary(lm(prop_sh~covid:rate+rate+date,data = regdf,weights = zip_sh))

#structural form
summary(lm(ph_sh~date*rate,data = regdf,weights = zip_sh))
summary(lm(ps_sh~date*rate,data = regdf,weights = zip_sh))
summary(lm(park_sh~date*rate,data = regdf,weights = zip_sh))
summary(lm(trash_sh~date*rate,data = regdf,weights = zip_sh))
summary(lm(prop_sh~date*rate,data = regdf,weights = zip_sh))

#test
summary(lm(ph_sh~rate+ZIP,data = regdf))
summary(lm(ph_sh~rate,data = regdf[regdf$date=="2020-05-01",]))

summary(lm(ph_sh~date+ZIP,data = regdf))


#informational trends

trend_01=dat %>% mutate(covidt = as.numeric(date > as.Date("2020-02-01","%Y-%m-%d"))) %>% 
  group_by(date,covidt) %>% count()  

trend_01 %>% ggplot(aes(x=date, y =n)) +geom_line()+theme_bw()


#informational trends-categories
trend_02=dat %>% 
  group_by(season,date,CATEGORY) %>% count() %>%
  group_by(season,CATEGORY) %>% summarise(n=mean(n)) %>%
  dcast(CATEGORY~season)

dat %>% 
  group_by(date) %>% mutate(n_date = sum(weight)) %>%
  group_by(date,CATEGORY) %>% mutate(n_date_cat = sum(weight),sh_date_cat = n_date_cat/n_date) %>%
  group_by(season,CATEGORY) %>% summarise(n=mean(sh_date_cat)) %>%
  dcast(CATEGORY~season)


#geographical variations 
dat %>% group_by(ZIP,date,season)%>% 
  summarise(
  ph_sh = mean(CATEGORY=="Public Health")
  ) %>% 
  group_by(ZIP,season) %>% 
  summarise(ph_sh = mean(ph_sh)) %>%
  dcast(ZIP~season)


#requests level reg
regdf_req = dat %>% left_join(zip_covid,by="ZIP") %>% mutate(
  ph = as.numeric(CATEGORY=="Public Health"),
  ps = as.numeric(CATEGORY=="Public Safety"),
  trash = as.numeric(CATEGORY=="Trash / Recycling"),
  park = as.numeric(CATEGORY=="Parks & Recreation"),
  prop = as.numeric(CATEGORY=="Property / Buildings / Construction"),
  )

regdf_req$date = as.factor(regdf_req$date)
regdf_req$ZIP = as.factor(regdf_req$ZIP)

summary(lm(data=regdf_req,ph~date*rate+ZIP))
summary(lm(data=regdf_req,ps~date*rate+ZIP))
summary(lm(data=regdf_req,park~date*rate+ZIP))
summary(lm(data=regdf_req,trash~date*rate+ZIP))
summary(lm(data=regdf_req,prop~date*rate+ZIP))



#coef plot zip-month panel
library(broom)
library(plm)
m1 = (plm(data= regdf, ph_sh~date:rate, model = "within",index = c("date","ZIP"),weights = zip_sh ))
ggcoef(
  m1, exponentiate = FALSE, exclude_intercept = TRUE,
  errorbar_size = 3,errorbar_color = "grey", errorbar_height = .2, color = "maroon",size= 3,shape = 18)+
  ggtitle("Public Health")+theme_classic()

m2 = (plm(data= regdf, ps_sh~date:rate, model = "within",index = c("date","ZIP"),weights = zip_sh ))
ggcoef(
  m2, exponentiate = FALSE, exclude_intercept = TRUE,
  errorbar_size = 3,errorbar_color = "grey", errorbar_height = .2, color = "maroon",size= 3,shape = 18)+
  ggtitle("Public Safety")+theme_classic()

m3 = (plm(data= regdf, park_sh~date:rate, model = "within",index = c("date","ZIP"),weights = zip_sh ))
ggcoef(
  m3, exponentiate = FALSE, exclude_intercept = TRUE,
  errorbar_size = 3,errorbar_color = "grey", errorbar_height = .2, color = "maroon",size= 3,shape = 18)+
  ggtitle("Park & Recreations")+theme_classic()

m4 = (plm(data= regdf, trash_sh~date:rate, model = "within",index = c("date","ZIP"),weights = zip_sh ))
ggcoef(
  m4, exponentiate = FALSE, exclude_intercept = TRUE,
  errorbar_size = 3,errorbar_color = "grey", errorbar_height = .2, color = "maroon",size= 3,shape = 18)+
  ggtitle("Park & Recreations")+theme_classic()

m5 = (plm(data= regdf, prop_sh~date:rate, model = "within",index = c("date","ZIP"),weights = zip_sh ))
ggcoef(
  m5, exponentiate = FALSE, exclude_intercept = TRUE,
  errorbar_size = 3,errorbar_color = "grey", errorbar_height = .2, color = "maroon",size= 3,shape = 18)+
  ggtitle("Properties Violations")+theme_classic()


