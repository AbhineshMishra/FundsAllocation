library(stringr)
library(dplyr)
library(tidyr)

#Checkpoint 1
companies <- read.delim('companies.txt',stringsAsFactors=F)

#str(companies)

round2 <- read.csv('rounds2.csv',stringsAsFactors=F)

#str(round2)

#count unique companies in Round2
nrow(distinct(round2,str_to_lower(company_permalink)))

#count unique companies in companies
nrow(distinct(companies,str_to_lower(permalink)))

#companies not present in round2

check <- ifelse(is.na(str_to_lower(companies$permalink)==str(round2$company_permalink)),1,0)
head(check)
sum(check)

#merge files
permalink_low=str_to_lower(round2$company_permalink)
round2 <- cbind.data.frame(round2,permalink_low,stringsAsFactors=F)

permalink_low=str_to_lower(companies$permalink)
companies <- cbind(companies,permalink_low,stringsAsFactors=F)

master_frame <- merge.data.frame(round2,companies,by="permalink_low")

#Checkpoint 2
# Funding Type avg investment
fund_Type <- group_by(master_frame,funding_round_type)
Avg_investment <- summarise(fund_Type, Avg_funding=mean(raised_amount_usd, na.rm = T))

#funding type between 5M and 15M
Avg_investment[which(Avg_investment[,2]>=5000000 & Avg_investment[,2]<=15000000),1]


# Top countries for funding_group_type Venture
 venture_fund <- filter(master_frame,funding_round_type == "venture")
 
 country_funding <-group_by(venture_fund,country_code) %>% summarise(TotalFunding =  sum(raised_amount_usd, na.rm = T))

 top9 <-head(arrange(country_funding,desc(TotalFunding)),n=9)


#Checkpoint 3
 
 library(countrycode)

 country_iso <- codelist[ , c("iso3c","country.name.en")]

names(country_iso)[names(country_iso) == "iso3c"] <- "country_code"

top9 <- merge(x=top9,y=country_iso,by="country_code",all.x=T)
top9 <- arrange(top9,desc(TotalFunding))

 #From the doc USA,GBR,IND 

#checkpoint 4
master_frame <-separate(master_frame, category_list, into=c("primary_sector"),remove=FALSE,sep='\\|')

mapping <-read.csv("mapping.csv",stringsAsFactors = F)
  
Mapping_long <- gather(mapping, sector, category_val, Automotive...Sports:Social..Finance..Analytics..Advertising)
Mapping_long <- Mapping_long[!(Mapping_long$category_val == 0), ]

nrow(distinct(Mapping_long,sector))

names(Mapping_long)[names(Mapping_long) == "category_list"] <- "primary_sector"

master_frame <- merge(x=master_frame,y=Mapping_long,by="primary_sector",all.x = T)

#Checkpoint 5
#D1
D1 <- filter(master_frame,funding_round_type=='venture',country_code=='USA')
D1_group <- data.frame(table(D1$sector),stringsAsFactors = F)
names(D1_group)[names(D1_group) == "Var1"] <- "sector"
names(D1_group)[names(D1_group) == "Freq"] <- "TotalCount_Sector"
D1_group$sector<-as.character(D1_group$sector)
D1<-merge(x=D1,y=D1_group,by="sector",all.x=TRUE)

D1_group2 <- group_by(D1,sector)
D1_group2<-summarise(D1_group2,TotalInvestment_sector = sum(raised_amount_usd,na.rm=T))

D1<-merge(x=D1,y=D1_group,by="sector",all.x=TRUE)


#D2
D2 <- filter(master_frame,funding_round_type=='venture',country_code=='GBR')
D2_group <- data.frame(table(D2$sector),stringsAsFactors = F)
names(D2_group)[names(D2_group) == "Var1"] <- "sector"
names(D2_group)[names(D2_group) == "Freq"] <- "TotalCount_Sector"
D2_group$sector<-as.character(D2_group$sector)
D2<-merge(x=D2,y=D2_group,by="sector",all.x=TRUE)

D2_group2 <- group_by(D2,sector)
D2_group2<-summarise(D2_group2,TotalInvestment_sector = sum(raised_amount_usd,na.rm=T))

D2<-merge(x=D2,y=D2_group,by="sector",all.x=TRUE)

#D3
D3 <- filter(master_frame,funding_round_type=='venture',country_code=='IND')
D3_group <- data.frame(table(D3$sector),stringsAsFactors = F)
names(D3_group)[names(D3_group) == "Var1"] <- "sector"
names(D3_group)[names(D3_group) == "Freq"] <- "TotalCount_Sector"
D3_group$sector<-as.character(D3_group$sector)
D3<-merge(x=D3,y=D3_group,by="sector",all.x=TRUE)

D3_group2 <- group_by(D3,sector)
D3_group2<-summarise(D3_group2,TotalInvestment_sector = sum(raised_amount_usd,na.rm=T))

D3<-merge(x=D3,y=D3_group,by="sector",all.x=TRUE)

# #of Investment Country wise
nrow(D1)
nrow(D2)
nrow(D3)


sum(D1$raised_amount_usd,na.rm=TRUE)
sum(D2$raised_amount_usd,na.rm=TRUE)
sum(D3$raised_amount_usd,na.rm=TRUE)

#top sector
head(arrange(D1_group,desc(TotalCount_Sector)),n=3)

head(arrange(D2_group,desc(TotalCount_Sector)),n=3)
head(arrange(D3_group,desc(TotalCount_Sector)),n=3)

#top sector company D1
Sector1_D1<-filter(D1,sector=="Others")
Sector1_D1[which.max(Sector1_D1$raised_amount_usd),"company_permalink"]

#top 2nd sector company D1
sector2_D1<-filter(D1,sector=="Cleantech...Semiconductors")
sector2_D1[which.max(sector2_D1$raised_amount_usd),"company_permalink"]

#top sector company D2
Sector1_D2<-filter(D2,sector=="Others")
Sector1_D2[which.max(Sector1_D2$raised_amount_usd),"company_permalink"]

#top 2nd sector company D2
sector2_D2<-filter(D2,sector=="Cleantech...Semiconductors")
sector2_D2[which.max(sector2_D2$raised_amount_usd),"company_permalink"]


#top sector company D3
Sector1_D3<-filter(D3,sector=="Others")
Sector1_D3[which.max(Sector1_D3$raised_amount_usd),"company_permalink"]

#top 2nd sector company D3
sector2_D3<-filter(D3,sector=="News..Search.and.Messaging")
sector2_D3[which.max(sector2_D3$raised_amount_usd),"company_permalink"]