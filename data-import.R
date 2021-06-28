library(tidyverse)
report_date <- "2021-06-27"  # The date (as a string) up to which data should be downloaded, to be entered into the URL. Maximum value should be the last government report that is available, probably today or yesterday.
url <- paste("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newCasesBySpecimenDateRollingSum&format=csv&release=", report_date, sep = '')
COVID_cases <- read_csv(url)
COVID_cases <- COVID_cases %>% mutate(sevenDayCases = newCasesBySpecimenDateRollingSum / 7)
COVID_cases <- COVID_cases %>% mutate(date_chr = as.character(date))
url <- paste("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newAdmissionsRollingSum&format=csv&release=", report_date, sep = "")
COVID_hosp <- read_csv(url)
COVID_hosp <- COVID_hosp %>% mutate(sevenDayCases = newAdmissionsRollingSum / 7)
COVID_hosp <- COVID_hosp %>% mutate(date_chr = as.character(date))
COVID_data <- left_join(COVID_cases, COVID_hosp, by = "date_chr")
COVID_data <- COVID_data %>% select(areaCode = areaCode.x, date = date.x, sevenDayCases = sevenDayCases.x, sevenDayHosp = sevenDayCases.y)
coeff <- 14  # Cases per hospitalisation, to make the axis more or less match up
ggplot(COVID_data, aes(x = date)) + geom_line(aes(y = sevenDayCases), color = "red") + geom_line(aes(y = sevenDayHosp * coeff), color = "blue") +   scale_y_continuous(
  
  # Features of the first axis
  name = "Cases",
  
  # Add a second axis and specify its features
  sec.axis = sec_axis(~./coeff, name="Hospitalisations")
) +
theme(
  axis.title.y = element_text(color = "red", size=13),
  axis.title.y.right = element_text(color = "blue", size=13)
) 
