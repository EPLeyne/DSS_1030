library(tidyverse)
library(readxl)
library(unpivotr)
library(openxlsx)
library(janitor)

datasheets <- 'dss-income-support-recipient-monthly-time-series-september-2023.xlsx'


data_head <- read_excel(datasheets, sheet = 'Parenting Payment Single', skip = 1,
                                            n_max = 4, 
                                            col_names = FALSE)

for (i in 2:ncol(data_head)) {
  prev <- data_head[, i-1]
  this <- data_head[, i]
  missing <- is.na(this)
  this[missing, ] <- prev[missing, ]
  data_head[, i] <- this
}

new_names <- data_head %>%
  summarise(across(.fns = paste, collapse = ".")) %>%
  unlist() %>% unname()

new_names

dss <- read_excel(datasheets, 
                  sheet = 'Parenting Payment Single', skip = 5, col_names = new_names)

colnames(dss)<-gsub(" ","",colnames(dss)) 
colnames(dss)<-gsub(".NA","",colnames(dss)) 

qplot(x=Date, y=Allrecipients,
      data=dss, na.rm=TRUE)

dssPost2020 <- dss[dss$Date >= "2020-12-01", ]

plot_post2020 <- qplot(x=Date, y=Allrecipients,
      data=dssPost2020, na.rm=TRUE)

ggplot(dssPost2020, aes(x=Date, y=Allrecipients)) +
  geom_line() +
  expand_limits(y = 0)

columns <- colnames(dssPost2020)

ggplot(dssPost2020, aes(x=Date, fill = c(Gender.Male, Gender.Female))) +
  geom_area()


#### Gender

dss_Gender <- dssPost2020 %>% select(c(Date,Gender.Male, Gender.Female)) %>% 
  pivot_longer(c(Gender.Male, Gender.Female))

dss_Gender %>%   ggplot(aes(x = Date, y = value, fill = factor(name))) +
  geom_area(position = "stack")

#### Age

dss_Age <- dssPost2020 %>% select(c(Date, starts_with('Age'))) %>% 
  pivot_longer(c(starts_with('Age')))

dss_Age %>% ggplot(aes(x = Date, y = value, fill = factor(name))) +
  geom_area(position = "stack")

#### Male by Age

dss_MaleAge <- dssPost2020 %>% select(c(Date, starts_with('GenderbyAge.Male'))) %>% 
  pivot_longer(c(starts_with('GenderbyAge.Male')))

dss_MaleAge %>% ggplot(aes(x = Date, y = value, fill = factor(name))) +
  geom_area(position = "stack")

#### Female by Age

dss_FemaleAge <- dssPost2020 %>% select(c(Date, starts_with('GenderbyAge.Female'))) %>% 
  pivot_longer(c(starts_with('GenderbyAge.Female')))

dss_FemaleAge %>% ggplot(aes(x = Date, y = value, fill = factor(name))) +
  geom_area(position = "stack")

#### Indigenous Female

dss_IndigenousF <- dssPost2020 %>% select(c(Date, starts_with('Indigenous'))) %>% 
  pivot_longer(c(starts_with('Indigenous')))

dss_IndigenousF %>% ggplot(aes(x = Date, y = value, fill = factor(name))) +
  geom_area(position = "stack")

#### State - Female

dss_StateF <- dssPost2020 %>% select(c(Date, starts_with('State'))) %>% 
  pivot_longer(c(starts_with('State')))

dss_StateF %>% ggplot(aes(x = Date, y = value, fill = factor(name))) +
  geom_area(position = "stack")

#### Payment rate - Female

dss_RopF <- dssPost2020 %>% select(c(Date, starts_with('Rateofpayment'))) %>% 
  pivot_longer(c(starts_with('Rateofpayment')))

dss_RopF %>% ggplot(aes(x = Date, y = value, fill = factor(name))) +
  geom_area(position = "stack")

#### Earning - F

dss_Earn <- dssPost2020 %>% select(c(Date, ends_with('earnings'))) %>% 
  pivot_longer(c(ends_with('earnings')))

dss_Earn %>% ggplot(aes(x = Date, y = value, fill = factor(name))) +
  geom_area(position = "stack")

set.seed(123)
n <- 50
date_seq <- seq(as.Date("2022-01-01"), by = "1 day", length.out = n)
data <- data.frame(
  Date = rep(date_seq, times = 7),
  Series = rep(1:7, each = n),
  Value = runif(n * 7, min = 0, max = 10)
)

data %>%
  ggplot(aes(x = Date, y = Value, group = Series, color = factor(Series))) +
  geom_line(stat = "identity") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Time Series of Stacked Lines",
       x = "Date",
       y = "Value",
       color = "Series") +
  theme_minimal()

data %>%
  ggplot(aes(x = Date, y = Value, fill = factor(Series))) +
  geom_area(position = "stack") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Time Series of Stacked Lines",
       x = "Date",
       y = "Value",
       fill = "Series") +
  theme_minimal()
