##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,data.table,
               tibble,pbapply,pbmcapply,here,readxl,rvest,
               downloader,curl,lubridate,hrbrthemes,plotly,
               ggthemes,scales)

rm(list = ls())

#Today's date
Sys.Date()

#Directory to save files: replace with your own
rawdatadir <- "M:/Analytics/NHS England Vaccinations/Weekly/"
rawdataparentdir <- "M:/Analytics/NHS England Vaccinations/"

##############################################
################### SCRAPE ###################
##############################################

#NHS England Vaccination data website
nhse_link <- "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/"

#Scrape all download links
xlxs_links <- read_html(nhse_link) %>%
  html_nodes(xpath="//a[contains(text(),'weekly')]/@href") %>%
  html_text() %>%
  as.data.frame() %>%
  dplyr::rename(.,link=.) %>%
  mutate(.,is_xlsx=str_detect(link, "xlsx")) %>%
  filter(.,is_xlsx==TRUE) %>%
  select(.,link)

#Download files (that haven't already been downloaded!)
already.there <- list.files(rawdatadir)
to.download <- xlxs_links$link[which((basename(xlxs_links$link) %in% already.there)==FALSE)]

if(length(to.download)>0){
  setwd(rawdatadir)
  for (k in 1:length(to.download)){
    curl::curl_download(to.download[k], destfile=basename(to.download[k]))
  }
} else {
}

rm(nhse_link,xlxs_links,to.download,already.there)

#####################################################
################### READ IN FILES ###################
#####################################################

#Read in and append all files into one
setwd(rawdatadir)
files.to.read <- list.files(rawdatadir)
files.to.read <- files.to.read[!str_detect(files.to.read, "~")]

#Sort files by date
file.dates.fct <- function(s){
  day <- file.dates[[s]][which(file.dates[[s]] %in% c("2020","2021"))-2]
  month <- file.dates[[s]][which(file.dates[[s]] %in% c("2020","2021"))-1]
  year <- file.dates[[s]][which(file.dates[[s]] %in% c("2020","2021"))]
  dmy <- paste(day,month,year,sep=" ")
  return(as.data.frame(dmy))
}
file.dates <- files.to.read %>% str_replace_all(.,".xlsx","") %>% strsplit(.,split="-")
file.dates.clean <- lapply(1:length(file.dates), file.dates.fct) %>%
  rbindlist(.) %>%
  mutate(.,dmy=lubridate::dmy(dmy))
files.to.read <- data.frame(file=files.to.read,date=file.dates.clean$dmy)
rm(file.dates,file.dates.clean)

#Sheet names to read in
sheet.names <- data.frame(date=lubridate::ymd(c("2020-12-31","2021-01-07")),
                          sheetname_ageregion=c("Tab1 Vaccinations by age","Total Vaccinations"))
files.to.read <- left_join(files.to.read,sheet.names,by="date") %>%
  mutate(.,sheetname_ageregion=ifelse(date>ymd("2021-01-07"),"Vaccinations by Region & Age",sheetname_ageregion),
         sheetname_ethn=ifelse(date>=ymd("2021-02-04"),"Vaccinations by Ethnicity",NA))
rm(sheet.names)

#Cell ranges for each sheet
cell.ranges.agedose <- data.frame(date=lubridate::ymd(c("2020-12-31","2021-01-07","2021-01-14",
                                                "2021-01-21","2021-01-28")),
                          cellrange_ageregion=c("B14:J100","B13:J100","B12:J100",
                                      "B12:J100","B12:L100"))
files.to.read <- left_join(files.to.read,cell.ranges.agedose,by="date") %>%
  mutate(.,cellrange_ageregion=ifelse(date>ymd("2021-01-28"),"B12:Z100",cellrange_ageregion),
         cellrange_ethn=ifelse(date>=ymd("2021-02-04"),"B12:Z33",""))
rm(cell.ranges.agedose)

#####################################################
################### READ IN AGE-REGION ##############
#####################################################

#Age and region: read in files
files.to.read.ageregion <- filter(files.to.read,!is.na(sheetname_ageregion))
datasets_list_ageregion <- vector(mode = "list", length = nrow(files.to.read.ageregion))
names(datasets_list_ageregion) <- files.to.read.ageregion$date

setwd(rawdatadir)
for (k in 1:nrow(files.to.read.ageregion)){
  # filename <- paste0("NHSE_vaccinations_",files.to.read$date[k]) %>% str_replace_all(.,"-","_")
  datasets_list_ageregion[[k]] <- read_excel(files.to.read.ageregion$file[k], sheet = files.to.read.ageregion$sheetname_ageregion[k],
                                   range=files.to.read.ageregion$cellrange_ageregion[k],col_names=FALSE)
}

####################################################
################### READ IN ETHNICITY ##############
####################################################

#Ethnicity: read in files
files.to.read.ethn <- filter(files.to.read,!is.na(sheetname_ethn))
datasets_list_ethn <- vector(mode = "list", length = nrow(files.to.read.ethn))
names(datasets_list_ethn) <- files.to.read.ethn$date

setwd(rawdatadir)
for (k in 1:nrow(files.to.read.ethn)){
  # filename <- paste0("NHSE_vaccinations_",files.to.read$date[k]) %>% str_replace_all(.,"-","_")
  datasets_list_ethn[[k]] <- read_excel(files.to.read.ethn$file[k], sheet = files.to.read.ethn$sheetname_ethn[k],
                                             range=files.to.read.ethn$cellrange_ethn[k],col_names=FALSE)
}

############################################################
################### PREPARE DATA FOR CLEANING ##############
############################################################

#Cleaning function to apply to each dataset

returncol <- function(dfinput,val){return(which(dfinput==val, arr.ind = TRUE)[,2])}

#Clean age and region data
clean_weekly_data_region_age <- function(j){
  data <- datasets_list_ageregion[[j]]
  date_list <- names(datasets_list_ageregion)[j]
  if(date_list<lubridate::dmy('01-01-2021')) {
    trdata <- data[3,c(1:6,8:9)]
    names(trdata) <- c("region","date",
                       "under80_first","under80_second",
                       "over80_first","over80_second",
                       "allages_first","allages_second")
    trdata$date <- files.to.read$date[j]
    trdata$region <- "Total"
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("region","date")) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  } else if (date_list==lubridate::dmy('07-01-2021')){
    trdata <- data[3,c(1:6,8:9)]
    names(trdata) <- c("region","date",
                       "under80_first","under80_second",
                       "over80_first","over80_second",
                       "allages_first","allages_second")
    trdata$date <- files.to.read$date[j]
    trdata$region <- "Total"
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("region","date")) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  } else if (date_list>lubridate::dmy('07-01-2021')&
             date_list<=lubridate::dmy('21-01-2021')){
    trdata <- data[3:12,c(1:4,6:7)]
    names(trdata) <- c("region","date",
                       "under80_first","over80_first",
                       "under80_second","over80_second")
    trdata$date <- files.to.read$date[j]
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("region","date")) %>%
      filter(.,!is.na(region)) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  } else if (date_list==lubridate::dmy('28-01-2021')){
    trdata <- data[3:11,c(1:4,7:8)]
    names(trdata) <- c("region","date",
                       "under80_first","over80_first",
                       "under80_second","over80_second")
    trdata$date <- files.to.read$date[j]
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("region","date")) %>%
      filter(.,!is.na(region)) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  } else if (date_list>lubridate::dmy('28-01-2021')){
    
    # cols <- c("Under 70","70-74","75-79","80+") %>% purrr::map(returncol) %>% unlist(.)
    ages <- c("Under 70","70-74","75-79","80+")
    
    # cols <- ages %>% purrr::pmap(~returncol(.val,data)) %>% unlist(.)
    cols <- mapply(returncol, val = ages, MoreArgs = list(dfinput = data)) %>% unlist(.)

    trdata <- data[3:11,c(1,2,cols)]
    
    names(trdata) <- c("region","date",
                       "under70_first","under70_second",
                       "70to74_first","70to74_second",
                       "75to79_first","75to79_second",
                       "over80_first","over80_second")

    trdata$date <- files.to.read$date[j]
    trdata <- trdata %>%
      reshape2::melt(., id.vars=c("region","date")) %>%
      filter(.,!is.na(region)) %>%
      mutate(.,age=word(variable,1,sep=fixed("_"))) %>%
      mutate(.,dose=word(variable,2,sep=fixed("_"))) %>%
      mutate(.,value=ifelse(value=="-",NA,value)) %>%
      select(.,-variable) %>%
      dplyr::rename(.,cumnumber.people=value)
    return(trdata)
  }
}
clean_data_region_age <- lapply(1:nrow(files.to.read.ageregion),clean_weekly_data_region_age) %>% rbindlist(.)
rm(datasets_list_ageregion,files.to.read.ageregion)

#Clean ethnicity data
clean_weekly_data_ethn <- function(j){
  data <- datasets_list_ethn[[j]]
  date_list <- names(datasets_list_ethn)[j]
  if(date_list>=lubridate::dmy('04-02-2021')) {
  
    doses <- c("1st Dose","2nd Dose","Cumulative Total Doses to Date")
    cols <- mapply(returncol, val = doses, MoreArgs = list(dfinput = data)) %>% unlist(.)
    
    trdata <- data[3:100,c(1:2,cols)]
    names(trdata) <- c("ethnicity","date",
                       "first","second","cumulative")
    trdata$date <- files.to.read.ethn$date[j]
    trdata <- trdata %>%
      filter(.,!is.na(ethnicity)) %>%
      reshape2::melt(., id.vars=c("ethnicity","date")) %>%
      dplyr::rename(dose=variable,people=value)
    return(trdata)
  } else if (date_list<lubridate::dmy('04-02-2021')){
  }
}
clean_data_ethn <- lapply(1:nrow(files.to.read.ethn),clean_weekly_data_ethn) %>% rbindlist(.)
rm(datasets_list_ethn,files.to.read.ethn)

###########################################################
################### CLEAN DATA: AGE AND DOSE ##############
###########################################################

#Create under 80 variable
detach(package:plyr)
clean_data_region_age <- clean_data_region_age %>%
  mutate(.,ageband_bis=ifelse(!(age %in% c("over80","allages")),"under80",age),
         cumnumber.people=as.numeric(cumnumber.people)) %>%
  group_by(date,region,dose,ageband_bis) %>%
  summarise(cumnumber.people=sum(cumnumber.people)) %>%
  ungroup(.)

#Mid-year pop by region
pop_by_GOR <- read_excel(paste0(rawdataparentdir,"Population data/ukmidyearestimates20192019ladcodes.xlsx"),
                         sheet = "MYE2 - Persons",skip=4) %>%
  filter(.,Geography1 %in% c("Region","Country")) %>%
  select(.,Name,`All ages`) %>%
  dplyr::rename(.,pop19=`All ages`,region=Name) %>%
  mutate(.,region=ifelse(region=="ENGLAND","TOTAL",region)) %>%
  mutate(.,region=ifelse(region=="EAST","EAST OF ENGLAND",region)) %>%
  mutate(.,region=str_to_title(region),ageband_bis="allages")

#Add in custom regions
pop_midlands <- pop_by_GOR %>% filter(.,region %in% c("East Midlands","West Midlands")) %>%
  select(.,pop19) %>% sum(.)
pop_ne_yorkshire <- pop_by_GOR %>% filter(.,region %in% c("Yorkshire And The Humber","North East")) %>%
  select(.,pop19) %>% sum(.)
extra_pop <- data.frame(region=c("Midlands","North East And Yorkshire"),pop19=c(pop_midlands,pop_ne_yorkshire),
                        ageband_bis=rep("allages",2))
pop_by_GOR <- plyr::rbind.fill(pop_by_GOR,extra_pop)
rm(pop_midlands,pop_ne_yorkshire,extra_pop)

#Add age totals row for all regions
detach(package:plyr)
allage_totals <- clean_data_region_age %>%
  mutate(.,is_all_ages=ifelse(ageband_bis!="allages",0,1)) %>%
  filter(.,is_all_ages==0) %>%
  group_by(date,region,dose) %>%
  summarise(cumnumber.people=sum(cumnumber.people)) %>%
  mutate(.,ageband_bis="allages") %>%
  ungroup(.)
clean_data_region_age <- plyr::rbind.fill(clean_data_region_age,allage_totals)

#Merge in population totals
clean_data_region_age <- left_join(clean_data_region_age,pop_by_GOR,by=c("region","ageband_bis")) %>%
  mutate(.,rate_pop=cumnumber.people/pop19*100)

########################################################
################### CLEAN DATA: ETHNICITY ##############
########################################################

#Aggregate ethnicities

clean_data_ethn$ethnicity_small <- plyr::mapvalues(clean_data_ethn$ethnicity,
                                             from = c("Total", "A: White - British", "B: White - Irish","C: White - Any other White background",
                                                      "D: Mixed - White and Black Caribbean", "E: Mixed - White and Black African","F: Mixed - White and Asian","G: Mixed - Any other Mixed background",
                                                      "H: Asian or Asian British - Indian", "J: Asian or Asian British - Pakistani", "K: Asian or Asian British - Bangladeshi",
                                                      "L: Asian or Asian British - Any other Asian background","M: Black or Black British - Caribbean",
                                                      "N: Black or Black British - African","P: Black or Black British - Any other Black background","R: Other ethnic groups - Chinese",
                                                      "S: Other ethnic groups - Any other ethnic group","Not stated/Unknown"),
                                             to = c("Total","White","White","White","Mixed","Mixed","Mixed","Mixed","Asian","Asian","Asian","Asian",
                                                    "Black","Black","Black","Other","Other","Unknown"))

clean_data_ethn <- clean_data_ethn %>%
  mutate(.,people=as.numeric(people)) %>%
  group_by(date,dose,ethnicity_small) %>%
  summarise(.,people=sum(people)) %>% ungroup()

#Merge in population totals by ethnicity
popgrowth_end_1619 <- (1+(56287000-55268100)/55268100) #Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/enpop/pop
pop_by_ethn <- read_excel(paste0(rawdataparentdir,"Population data/supportingtablesforpub.xlsx"),
                         sheet = "Table A",skip=6) %>%
  filter(.,`Area Name`=="England") %>%
  select(.,c(4:10)) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(.,White=`White British`+`All Other White`,
         Mixed=`Mixed / Multiple ethnic groups`,
         Asian=`Asian / Asian British`,
         Black=`Black / African / Caribbean / Black British`,
         Other=`Other ethnic group`,
         region="England") %>%
  select(.,region,Total,White,Asian,Black,Mixed,Other) %>%
  reshape2::melt(., id.vars=c("region")) %>%
  dplyr::rename(.,pop.total19=value,ethnicity_small=variable) %>%
  mutate(pop.total19=round(popgrowth_end_1619*pop.total19*1000,0))
rm(popgrowth_end_1619)

#Merge in population totals
clean_data_ethn <- left_join(clean_data_ethn,pop_by_ethn,by="ethnicity_small") %>%
  mutate(.,eth_rate=people/pop.total19*100)

#Save data
fwrite(clean_data_region_age, file = paste0(rawdataparentdir,"Summary/age_region_summary.csv"), sep = ",")
fwrite(clean_data_ethn, file = paste0(rawdataparentdir,"Summary/ethn_summary.csv"), sep = ",")