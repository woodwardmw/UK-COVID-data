library(tidyverse)
library(RCurl)

report_date <- format(Sys.Date(), "%Y-%m-%d")  # The date (as a string) up to which data should be downloaded, to be entered into the URL. Maximum value should be the last government report that is available, probably today or yesterday.
url <- paste("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newCasesByPublishDateRollingSum&format=csv&release=", report_date, sep = '')
if(!url.exists(url)){  # If today's report doesn't yet exist, use yesterday's
report_date <- format(Sys.Date() - 1, "%Y-%m-%d")  # The date (as a string) up to which data should be downloaded, to be entered into the URL. Maximum value should be the last government report that is available, probably today or yesterday.
url <- paste("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newCasesByPublishDateRollingSum&format=csv&release=", report_date, sep = '')
  }
COVID_cases <- read_csv(url)
COVID_cases <- COVID_cases %>% mutate(sevenDayCases = newCasesByPublishDateRollingSum / 7)
COVID_cases <- COVID_cases %>% mutate(date_chr = as.character(date))
url <- paste("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newAdmissionsRollingSum&format=csv&release=", report_date, sep = "")
COVID_hosp <- read_csv(url)
COVID_hosp <- COVID_hosp %>% mutate(sevenDayCases = newAdmissionsRollingSum / 7)
COVID_hosp <- COVID_hosp %>% mutate(date_chr = as.character(date))
COVID_data <- left_join(COVID_cases, COVID_hosp, by = "date_chr")
COVID_data <- COVID_data %>% select(areaCode = areaCode.x, date = date.x, sevenDayCases = sevenDayCases.x, sevenDayHosp = sevenDayCases.y)
coeff <- 14  # Cases per hospitalisation, to make the axis more or less match up

dir_name <- paste('reports/', report_date, sep = '') 
dir.create(dir_name)
png(file = paste(dir_name, "/HospVCases.png", sep = ''), width = 1200, height = 800)
ggplot(COVID_data, aes(x = date)) + geom_line(aes(y = sevenDayCases), color = "red") + geom_line(aes(y = sevenDayHosp * coeff), color = "blue") +   scale_y_continuous(
  
  # Features of the first axis
  name = "Cases",
  
  # Add a second axis and specify its features
  sec.axis = sec_axis(~./coeff, name="Hospitalisations")
) +
theme(
  axis.title.y = element_text(color = "red", size=13),
  axis.title.y.right = element_text(color = "blue", size=13)
) + xlab("Date") + ylab("Cases") + labs(title = "UK COVID-19 Cases and Hospitalisations: 2020-2021") + scale_x_date(date_labels = "%B %Y")
dev.off() 

png(file = paste(dir_name, "/7dayCasesJanPresent.png", sep = ''), width = 1200, height = 800)
COVID_cases %>% filter(date >= as.Date("2021-01-01")) %>% ggplot(aes(x = date, y = sevenDayCases)) + geom_line() + scale_y_continuous(trans = "log10") + xlab("Date") + ylab("Cases") + labs(title = "UK Rolling 7-day COVID-19 cases: January - July 2021") + scale_x_date(date_labels = "%B %Y")
dev.off() 
png(file = paste(dir_name, "/7dayHospJanPresent.png", sep = ''), width = 1200, height = 800)
COVID_hosp %>% filter(date >= as.Date("2021-01-01")) %>% ggplot(aes(x = date, y = sevenDayCases)) + geom_line() + scale_y_continuous(trans = "log10") + xlab("Date") + ylab("Hospitalisations") + labs(title = "UK Rolling 7-day hospitalisations: January - July 2021") + scale_x_date(date_labels = "%B %Y")
dev.off() 

png(file = paste(dir_name, "/7dayCasesMayPresent.png", sep = ''), width = 1200, height = 800)
COVID_cases %>% filter(date >= as.Date("2021-05-01")) %>% ggplot(aes(x = date, y = sevenDayCases)) + geom_line() + scale_y_continuous(trans = "log10") + xlab("Date") + ylab("Cases") + labs(title = "UK Rolling 7-day COVID-19 cases: May - July 2021") + scale_x_date(date_labels = "%B %Y")
dev.off() 

png(file = paste(dir_name, "/7dayHospMayPresent.png", sep = ''), width = 1200, height = 800)
COVID_hosp %>% filter(date >= as.Date("2021-05-01")) %>% ggplot(aes(x = date, y = sevenDayCases)) + geom_line() + scale_y_continuous(trans = "log10") + xlab("Date") + ylab("Hospitalisations") + labs(title = "UK Rolling 7-day COVID-19 hospitalisations: May - July 2021") + scale_x_date(date_labels = "%B %Y")
dev.off() 

