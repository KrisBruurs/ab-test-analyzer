###---Libraries---###
library(tidyverse) # Data Handling

###---Input---###
control <- read_delim('data/control_group.csv', delim = ';')
test <- read_delim('data/test_group.csv', delim = ';')

###---Processing---###

# Merge two files together
combined_data <- control %>% 
  bind_rows(test)

# Renaming variables for easier handling
combined_data <- combined_data %>% 
  rename(Campaign_Name = `Campaign Name`,
         Spend = `Spend [USD]`,
         Impressions = `# of Impressions`,
         Clicks = `# of Website Clicks`,
         Searches = `# of Searches`,
         View_Content = `# of View Content`,
         Add_Cart = `# of Add to Cart`,
         Purchase = `# of Purchase`)

# Checking for NA's
anyNA(combined_data) # Shows TRUE for NA's in data. Dataset is small enought
                     # to manually check for NA's

# Day 5 of the control campaign shows missing values and will be romoved
# for analysis
clean_data <- combined_data %>%
  filter(!(is.na(Impressions) & is.na(Clicks) & is.na(Searches) &
           is.na(View_Content) & is.na(Add_Cart) & is.na(Purchase)))

###---Output---###
write_csv(clean_data, 'data/cleaned_data.csv')
