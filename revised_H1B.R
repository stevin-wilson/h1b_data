# rejection, total applications vs year
# timeline for companies with largest number of rejections in 2019
# Does the origin of the country have any association with rejection?
# What about the industry


# load required packages
library(tidyverse)
library(viridis)

# load dataset files
# select columns for further analysis
# gather data from mulitple columns to separate rows for 
# the same employer


# Function to import csv files from the USCIS website
baselink <- "https://www.uscis.gov/sites/default/files/USCIS/Data/Employment-based/H-1B/h1b_datahubexport-"
format <- ".csv"

?read_csv

# large numbers have , in them. Therefore import csv with character column
# replace , after importing
fileGrab <- function(year){
  download_link <-  paste(baselink,year,format, sep = "")
  csv_file <- read_csv(download_link, col_types = cols(`Fiscal Year` = col_double(),
                                                       Employer = col_character(),
                                                       `Initial Approvals` = col_character(),
                                                       `Initial Denials` = col_character(),
                                                       `Continuing Approvals` = col_character(),
                                                       `Continuing Denials` = col_character(),
                                                       NAICS = col_double(),
                                                       `Tax ID` = col_character(),
                                                       State = col_character(),
                                                       City = col_character(),
                                                       ZIP = col_character()))
  csv_file <- csv_file %>%
    mutate(`Initial Approvals` = gsub(",","",`Initial Approvals`),
           `Initial Denials` = gsub(",","",`Initial Denials`),
           `Continuing Approvals` = gsub(",","",`Continuing Approvals`),
           `Continuing Denials` = gsub(",","",`Continuing Denials`),
           `Initial Approvals` = as.numeric(`Initial Approvals`),
           `Initial Denials` = as.numeric(`Initial Denials`),
           `Continuing Approvals` = as.numeric(`Continuing Approvals`),
           `Continuing Denials` = as.numeric(`Continuing Denials`))
  return(csv_file)
}

# Looping over the years for which the data is available
years <- c("2009","2010","2011","2012",
           "2013","2014","2015","2016",
           "2017","2018","2019")

df_list = c()
for (year in years){
  data_file <- fileGrab(year)
  df_name <- paste('data_',year, sep = "")
  assign(df_name, data_file)
  remove(data_file)
}

# list of tibbles
df_list <- list(data_2009,data_2010,data_2011,data_2012,
                data_2013,data_2014,data_2015,data_2016,
                data_2017,data_2018,data_2019)
df_list 

# EDA
str(data_2009)

data_2009 %>%
  filter(grepl('MICROSOFT', Employer))
# NAICS industry classification
# 99 = 'Unknown' https://www.uscis.gov/tools/reports-studies/understanding-our-h-1b-employer-data-hub
# data from https://www.census.gov/cgi-bin/sssd/naics/naicsrch?chart=2017
naics_class <- tibble( code = c('11','21','22','23',
                                '31','32','33','42',
                                '44','45','48','49',
                                '51','52','53','54',
                                '55','56','61','62',
                                '71','72','81','92', '99'),
                       industry = c('Agriculture, Forestry, Fishing and Hunting',
                                    'Mining, Quarrying, and Oil and Gas Extraction',
                                    'Utilities',
                                    'Construction',
                                    'Manufacturing',
                                    'Manufacturing',
                                    'Manufacturing',
                                    'Wholesale Trade',
                                    'Retail Trade',
                                    'Retail Trade',
                                    'Transportation and Warehousing',
                                    'Transportation and Warehousing',
                                    'Information',
                                    'Finance and Insurance',
                                    'Real Estate and Rental and Leasing',
                                    'Professional, Scientific, and Technical Services',
                                    'Management of Companies and Enterprises',
                                    'Administrative and Support and Waste Management and Remediation Services',
                                    'Educational Services',
                                    'Health Care and Social Assistance',
                                    'Arts, Entertainment, and Recreation',
                                    'Accommodation and Food Services',
                                    'Other Services (except Public Administration)',
                                    'Public Administration', 'Unknown'))

# Get a tibble witht he following columns
# year, NAICS, total application, total rejects, new applications, new rejects

industry_df <- tibble(year = numeric(),
                      NAICS = character(),
                      total_applications = numeric(),
                      total_rejects = numeric(),
                      initial_applications = numeric(),
                      initial_rejects = numeric())
industry_df

for (entry in df_list){
  processed_entry <- entry %>%
    replace(is.na(.),0) %>%
    group_by(Employer) %>%
    select(c(1,2,3,4,5,6,7)) %>%
    rename(year = `Fiscal Year`, 
           employer = Employer,
           initial_approvals = `Initial Approvals`,
           initial_rejects = `Initial Denials`, 
           continuing_approvals = `Continuing Approvals`,
           continuing_rejects = `Continuing Denials`) %>%
    group_by(year, NAICS) %>%
    summarise(initial_approvals = sum(initial_approvals),
              initial_rejects = sum(initial_rejects),
              continuing_approvals = sum(continuing_approvals),
              continuing_rejects = sum(continuing_rejects)) %>%
    mutate(total_applications = (initial_approvals+initial_rejects+continuing_approvals+continuing_rejects),
           total_rejects = (initial_rejects + continuing_rejects),
           initial_applications = (initial_approvals + initial_rejects)) %>%
    select(year, NAICS, total_applications,total_rejects,initial_applications,initial_rejects) %>%
    mutate(NAICS = as.character(NAICS), NAICS = recode(NAICS, '11' = 'Agriculture, Forestry, Fishing and Hunting',
                                                       '21' = 'Mining, Quarrying, and Oil and Gas Extraction',
                                                       '22' = 'Utilities',
                                                       '23' = 'Construction',
                                                       '31' = 'Manufacturing',
                                                       '32' = 'Manufacturing',
                                                       '33' = 'Manufacturing',
                                                       '42' = 'Wholesale Trade',
                                                       '44' = 'Retail Trade',
                                                       '45' = 'Retail Trade',
                                                       '48' = 'Transportation and Warehousing',
                                                       '49' = 'Transportation and Warehousing',
                                                       '51' = 'Information',
                                                       '52' = 'Finance and Insurance',
                                                       '53' = 'Real Estate and Rental and Leasing',
                                                       '54' = 'Professional, Scientific, and Technical Services',
                                                       '55' = 'Management of Companies and Enterprises',
                                                       '56' = 'Administrative and Support and Waste Management and Remediation Services',
                                                       '61' = 'Educational Services',
                                                       '62' = 'Health Care and Social Assistance',
                                                       '71' = 'Arts, Entertainment, and Recreation',
                                                       '72' = 'Accommodation and Food Services',
                                                       '81' = 'Other Services (except Public Administration)',
                                                       '92' = 'Public Administration',
                                                       '99' = 'Unknown')) %>%
    group_by(year, NAICS) %>%
    summarise(total_applications = sum(total_applications),
              total_rejects = sum(total_rejects),
              initial_applications = sum(initial_applications),
              initial_rejects = sum(initial_rejects))
  
  industry_df <- bind_rows(industry_df,processed_entry)
}

# looking at the data for 'Professional, Scientific, and Technical Services' over the years
industry_df[which(industry_df$NAICS == 'Professional, Scientific, and Technical Services'),]


# initial rejection ratio over the years
industry_df %>%
  group_by(year) %>%
  mutate(year <- as.factor(year)) %>%
  select(-NAICS) %>%
  summarise(total_applications = sum(total_applications),
            total_rejects = sum(total_rejects),
            initial_applications = sum(initial_applications),
            initial_rejects = sum(initial_rejects)) %>%
  ggplot(aes(x = as.factor(year), y = (initial_rejects/initial_applications)*100, group = 1))+
  geom_point(colour = 'red', size = 5) + 
  geom_path(linetype = "dashed") +
  theme_linedraw(base_size = 22) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Denial Ratio for new H1B applications" , x = "Year" , y = "% Denied" )

industry_df %>%
  group_by(year) %>%
  mutate(year <- as.factor(year)) %>%
  select(-NAICS) %>%
  summarise(total_applications = sum(total_applications),
            total_rejects = sum(total_rejects),
            initial_applications = sum(initial_applications),
            initial_rejects = sum(initial_rejects)) %>%
  ggplot(aes(x = as.factor(year), y = (total_rejects/total_applications)*100, group = 1))+
  geom_point(colour = 'green', size = 5) + 
  geom_path(linetype = "dashed") +
  theme_linedraw(base_size = 22) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Denial Ratio for H1B applications" , x = "Year" , y = "% Denied" )

labels <- c('initial_applications' = 'Count (Approvals + Denials)',  'initial_rejects' = 'Count (Denials)') 
industry_df %>%
  group_by(year) %>%
  select(year,NAICS,initial_rejects, initial_applications) %>%
  pivot_longer(c(initial_rejects, initial_applications), names_to = "status", 
               values_to = "count") %>%
  filter(NAICS ==  'Professional, Scientific, and Technical Services' |
           NAICS ==  'Educational Services' |
           NAICS ==   'Finance and Insurance'|
           NAICS ==   'Health Care and Social Assistance'|
           NAICS ==   'Information'|
           NAICS ==   'Management of Companies and Enterprises'|
           NAICS ==   'Manufacturing') %>%
  ggplot(aes(x = as.factor(year), y = count/1000, colour = NAICS,group = NAICS))+
  geom_point(size = 3) + 
  geom_path(linetype = "dashed") + 
  facet_grid(status ~ ., labeller = labeller(status = labels)) +
  theme_linedraw(base_size = 17) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.text=element_text(size=10),
        strip.background=element_rect(fill="maroon"))+
  labs(title = "Increase in Denials for the Scientific and Technical Industry " , x = "Year" , y = "Count (in thousands)" , colour = 'NAICS Code')



# employers with max number of h1b applications in 2019
top_2019 <- data_2019 %>%
  replace(is.na(.),0) %>%
  select(c(1,2,3,4,5,6)) %>%
  rename(year = `Fiscal Year`, 
         employer = Employer,
         initial_approvals = `Initial Approvals`,
         initial_rejects = `Initial Denials`, 
         continuing_approvals = `Continuing Approvals`,
         continuing_rejects = `Continuing Denials`) %>%
  group_by(employer, year) %>%
  summarise(initial_approvals = sum(initial_approvals),
            initial_rejects = sum(initial_rejects),
            continuing_approvals = sum(continuing_approvals),
            continuing_rejects = sum(continuing_rejects)) %>%
  mutate(total_applications = (initial_approvals+initial_rejects+continuing_approvals+continuing_rejects),
         reject_ratio = (initial_rejects + continuing_rejects)/total_applications,
         initial_applications = initial_approvals+initial_rejects,
         initial_reject_ratio = initial_rejects / initial_applications) %>%
  select(year, employer, initial_reject_ratio, initial_applications) %>%
  arrange(desc(initial_applications)) %>%
  filter(initial_applications >= 50 )

dim(top_2019)
top_2019

# Top 25 employers in terms of new H1b applications in 2019
top_2019 %>%
  select(employer, initial_applications) %>%
  arrange(desc(initial_applications)) %>%
  ungroup() %>%
  top_n(n = 25, wt = initial_applications) %>%
  ggplot(aes(x = reorder(employer, -initial_applications), y = initial_applications))+
  geom_col(fill = 'maroon') +
  theme_linedraw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  labs(title = "Employers with the most H1B applications in 2019" , x = "Employer" , y = "Count ( Approval + Denial)" )


top_h1b_employers_2019 <- unique(top_2019$employer)
top_h1b_employers_2019
length(top_h1b_employers_2019)

# write the list of employers to a csv file
write.csv(top_h1b_employers_2019, file = "top_h1b_employers_2019.csv")

# read csv file of top h1b employers with industry and country info
top_h1b_employers_2019_filled <- read_csv('top_h1b_employers_2019_filled.csv')
head(top_h1b_employers_2019_filled)
length(top_h1b_employers_2019_filled$employer)
length(top_2019$employer)


# Combine industry and country information with the h1b application counts
top_2019 <- left_join(top_2019,top_h1b_employers_2019_filled, by = 'employer')

# Check if there are any missing values
which(is.na(top_2019))

# Industries applying for the most H1B visas
top_2019 %>%
  ggplot(aes(x = industry, y = initial_applications/1000))+
  geom_col(fill = 'darkgoldenrod1')+
  theme_linedraw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "H1B is an IT visa\n(2019 Data)" , x = "Industry" , y = "Count (Approvals + Denials) [ in thousands ]" , caption = "Only employers with >= 50 H1B applications in 2019 included.")


# Denial ratios across industries in 2019
top_2019 %>%
  group_by(industry) %>%
  summarise(reject_ratio = mean(initial_reject_ratio)) %>%
  ggplot(aes(x = industry, y = reject_ratio * 100 ))+
  geom_col(fill = 'coral1')+
  theme_linedraw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "H1B Denial Rates by Industry\n(2019 Data ; New applications)" , x = "Industry" , y = "% Denied (Mean)" , caption = "Only employers with >= 50 H1B applications in 2019 included.")


# compare h1b patterns for giants
# clerical differences in filing company names prevent the 
# use of inner_join to combine dataframe
# regex to filter entries

# DuPont de Nemours, Johnson & Johnson, Pfizer ,Merck,
# Abbott Laboratories, Thermo Fisher Scientific, Eli Lilly 
# Amgen, Bristol-Myers Squibb

# "^Abbott lab|^Amgen inc|Bristol.*Squibb|Eli.*Lilly|
# Thermo.*Fisher|Du.*Pont.*de.*Nemours|Johnson.*&.*Johnson|^merck

# Cummins, Tesla, Ford Motor, General Motors
# cummins.*inc|^TESLA.*|ford.*motor|General.*Motors

# Bank of America , Walmart, JPMorgan Chase , Boeing
# ^WAL.*MART|BANK.*OF.*AMERICA|J.*P.*MORGAN.*CHASE|Boeing 

# Apple, Amazon.com, Alphabet, Microsoft, Dell Technologies
# IBM, Intel, Facebook, Cisco Systems
# ^APPLE.*INC|^GOOGLE|^MICROSOFT|
# ^AMAZON.*WEB|^AMAZON.*CO|
# ^IBM |^INTEL .*cor|^Dell|^cisco |^facebook 

# build a timeline for a select list of companies
# Repeat for every year



employer_list <- c('Abbott Laboratories','Amgen','Bristol-Myers Squibb', 'Eli Lilly', 'Johnson & Johnson','Thermo Fisher',
                   'DuPont de Nemours','Merck','Cummins','Tesla','Ford Motors','General Motors','Walmart',
                   'Bank of America','JPMorgan Chase','Boeing', 'Apple','Google','Microsoft','Amazon','IBM',
                   'Intel','Dell','Cisco','Facebook')

# create an empty tibbe to bind rows after processing

unified_short <- tibble(year = numeric(), 
                        employer = character(),
                        initial_reject_ratio = numeric(), 
                        initial_approvals = numeric(),
                        initial_rejects = numeric())

# Create a function to filter employers and concatenate the data over the years
filter_df <- function(df_name){
processing_df <- df_name %>%
  select(c(1,2,3,4)) %>%
  rename(year = `Fiscal Year`, 
         employer = Employer,
         initial_approvals = `Initial Approvals`,
         initial_rejects = `Initial Denials`) %>%
  group_by(employer, year) %>%
  summarise(initial_approvals = sum(initial_approvals),
            initial_rejects = sum(initial_rejects)) %>%
  filter(grepl("^Abbott lab|^Amgen inc|Bristol.*Squibb|Eli.*Lilly|Thermo.*Fisher|Du.*Pont.*de.*Nemours|Johnson.*&.*Johnson|^merck|cummins.*inc|^TESLA.*|ford.*motor|General.*Motors|^WAL.*MART|BANK.*OF.*AMERICA|J.*P.*MORGAN.*CHASE|Boeing |^APPLE.*INC|^GOOGLE|^MICROSOFT|^AMAZON.*WEB|^AMAZON.*CO|^IBM |^INTEL .*cor|^Dell|^cisco |^facebook ", employer, ignore.case = TRUE))

processing_df[which(grepl("^Abbott lab",processing_df$employer, ignore.case = TRUE)),1] <-  'Abbott Laboratories'
processing_df[which(grepl("^Amgen inc",processing_df$employer, ignore.case = TRUE)),1] <-  'Amgen'
processing_df[which(grepl("Bristol.*Squibb",processing_df$employer, ignore.case = TRUE)),1] <-  'Bristol-Myers Squibb'
processing_df[which(grepl("Eli.*Lilly",processing_df$employer, ignore.case = TRUE)),1] <-  'Eli Lilly'
processing_df[which(grepl("Johnson.*&.*Johnson",processing_df$employer, ignore.case = TRUE)),1] <-  'Johnson & Johnson'
processing_df[which(grepl("Thermo.*Fisher",processing_df$employer, ignore.case = TRUE)),1] <-  'Thermo Fisher'
processing_df[which(grepl("Du.*Pont.*de.*Nemours",processing_df$employer, ignore.case = TRUE)),1] <-  'DuPont de Nemours'
processing_df[which(grepl("^merck",processing_df$employer, ignore.case = TRUE)),1] <-  'Merck'
processing_df[which(grepl("cummins.*inc",processing_df$employer, ignore.case = TRUE)),1] <-  'Cummins'
processing_df[which(grepl("^TESLA.*",processing_df$employer, ignore.case = TRUE)),1] <-  'Tesla'
processing_df[which(grepl("ford.*motor",processing_df$employer, ignore.case = TRUE)),1] <-  'Ford Motors'
processing_df[which(grepl("General.*Motors",processing_df$employer, ignore.case = TRUE)),1] <-  'General Motors'
processing_df[which(grepl("^WAL.*MART",processing_df$employer, ignore.case = TRUE)),1] <-  'Walmart'
processing_df[which(grepl("BANK.*OF.*AMERICA",processing_df$employer, ignore.case = TRUE)),1] <-  'Bank of America'
processing_df[which(grepl("J.*P.*MORGAN.*CHASE",processing_df$employer, ignore.case = TRUE)),1] <-  'JPMorgan Chase'
processing_df[which(grepl("Boeing ",processing_df$employer, ignore.case = TRUE)),1] <-  'Boeing'
processing_df[which(grepl("^APPLE.*INC",processing_df$employer, ignore.case = TRUE)),1] <-  'Apple'
processing_df[which(grepl("^GOOGLE",processing_df$employer, ignore.case = TRUE)),1] <-  'Google'
processing_df[which(grepl("^MICROSOFT",processing_df$employer, ignore.case = TRUE)),1] <-  'Microsoft'
processing_df[which(grepl("^AMAZON.*WEB",processing_df$employer, ignore.case = TRUE)),1] <-  'Amazon'
processing_df[which(grepl("^AMAZON.*CO",processing_df$employer, ignore.case = TRUE)),1] <-  'Amazon'
processing_df[which(grepl("^IBM ",processing_df$employer, ignore.case = TRUE)),1] <-  'IBM'
processing_df[which(grepl("^INTEL .*cor",processing_df$employer, ignore.case = TRUE)),1] <-  'Intel'
processing_df[which(grepl("^Dell",processing_df$employer, ignore.case = TRUE)),1] <-  'Dell'
processing_df[which(grepl("^cisco ",processing_df$employer, ignore.case = TRUE)),1] <-  'Cisco'
processing_df[which(grepl("^facebook ",processing_df$employer, ignore.case = TRUE)),1] <-  'Facebook'

processing_df <- processing_df %>%
  group_by(employer, year) %>%
  summarise(initial_approvals = sum(initial_approvals),
            initial_rejects = sum(initial_rejects)) %>%
  mutate(initial_reject_ratio = initial_rejects / (initial_approvals + initial_rejects)) %>%
  select(year, employer,initial_reject_ratio, initial_approvals, initial_rejects)

return(processing_df)
}

for (df in df_list){
  unified_short <- bind_rows(unified_short,filter_df(df))
}

head(unified_short)
str(unified_short)

# Heatmap new applications (approval + denial)
unified_short %>%
  filter(employer != 'Boeing') %>%
  ggplot(aes(x = factor(year), y = factor(employer)))+
  geom_raster(aes(fill = initial_approvals + initial_rejects)) + scale_fill_viridis(option="viridis")+
  coord_equal()+
  theme_linedraw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Demand for H1B among the giants" , x = "Year" , y = "Employer" , fill = 'Count\n(Approvals + Denials)')

# Heatmap reject ratio 
unified_short %>%
  filter(employer != 'Boeing') %>%
  ggplot( aes(x = factor(year), y = factor(employer)))+
  geom_raster(aes(fill = initial_reject_ratio)) + scale_fill_viridis(option="viridis")+
  coord_equal()+
  theme_linedraw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Increase in H1B denials for the giants" , x = "Year" , y = "Employer" , fill = '% denied')


#Trial area
data_2009 %>%
  replace(is.na(.),0) %>%
  group_by(Employer) %>%
  select(c(1,2,3,4,5,6,7)) %>%
  rename(year = `Fiscal Year`, 
         employer = Employer,
         initial_approvals = `Initial Approvals`,
         initial_rejects = `Initial Denials`, 
         continuing_approvals = `Continuing Approvals`,
         continuing_rejects = `Continuing Denials`) %>%
  group_by(year, NAICS) %>%
  summarise(initial_approvals = sum(initial_approvals),
            initial_rejects = sum(initial_rejects),
            continuing_approvals = sum(continuing_approvals),
            continuing_rejects = sum(continuing_rejects)) %>%
  mutate(total_applications = (initial_approvals+initial_rejects+continuing_approvals+continuing_rejects),
         total_rejects = (initial_rejects + continuing_rejects),
         initial_applications = (initial_approvals + initial_rejects)) %>%
  select(year, NAICS, total_applications,total_rejects,initial_applications,initial_rejects) %>%
  mutate(NAICS = as.character(NAICS), NAICS = recode(NAICS, '11' = 'Agriculture, Forestry, Fishing and Hunting',
                                                     '21' = 'Mining, Quarrying, and Oil and Gas Extraction',
                                                     '22' = 'Utilities',
                                                     '23' = 'Construction',
                                                     '31' = 'Manufacturing',
                                                     '32' = 'Manufacturing',
                                                     '33' = 'Manufacturing',
                                                     '42' = 'Wholesale Trade',
                                                     '44' = 'Retail Trade',
                                                     '45' = 'Retail Trade',
                                                     '48' = 'Transportation and Warehousing',
                                                     '49' = 'Transportation and Warehousing',
                                                     '51' = 'Information',
                                                     '52' = 'Finance and Insurance',
                                                     '53' = 'Real Estate and Rental and Leasing',
                                                     '54' = 'Professional, Scientific, and Technical Services',
                                                     '55' = 'Management of Companies and Enterprises',
                                                     '56' = 'Administrative and Support and Waste Management and Remediation Services',
                                                     '61' = 'Educational Services',
                                                     '62' = 'Health Care and Social Assistance',
                                                     '71' = 'Arts, Entertainment, and Recreation',
                                                     '72' = 'Accommodation and Food Services',
                                                     '81' = 'Other Services (except Public Administration)',
                                                     '92' = 'Public Administration',
                                                     '99' = 'Unknown')) %>%
  group_by(year, NAICS) %>%
  summarise(total_applications = sum(total_applications),
            total_rejects = sum(total_rejects),
            initial_applications = sum(initial_applications),
            initial_rejects = sum(initial_rejects))


