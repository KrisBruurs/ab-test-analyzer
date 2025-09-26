###---Libraries---###
library(tidyverse)

###---Input---###
data <- read_csv('data/cleaned_data.csv')

###---Processing---###

# Create variable for CPP
data <- data %>% 
  mutate(cpp = Purchase/Spend)

# Data Exploration
mean_spend_control <- data %>% 
  filter(Campaign_Name == 'Control Campaign') %>% 
  summarise(mean_spend = mean(Spend)) %>% 
  pull(mean_spend)

mean_spend_test <- data %>% 
  filter(Campaign_Name == 'Test Campaign') %>% 
  summarise(mean_spend = mean(Spend)) %>% 
  pull(mean_spend)

mean_spend_dif <- 
  mean_spend_test - mean_spend_control

fig_spend <- data %>% 
  ggplot(aes(x = Campaign_Name, y = Spend, fill = Campaign_Name)) +
  geom_col() +
  labs(y = 'Spend in USD',
       x = NULL,
       fill = "Version",
       title = 'Spenditure Between Campaign Versions') +
  theme_classic()+
  scale_fill_manual(values = c('tan2', 'sienna4'))

fig_spend

# prepare data for analysis

totals <- data %>% 
  group_by(Campaign_Name) %>% 
  summarise(sum_impressions = sum(Impressions),
            sum_purchase = sum(Purchase),
            sum_reach = sum(Reach),
            sum_add_cart = sum(Add_Cart),
            sum_clicks = sum(Clicks),
            sum_view_content = sum(View_Content),
            sum_searches = sum(Searches),
            sum_spend = sum(Spend))

# H1
prop.test(x = totals$sum_purchase, 
          n = totals$sum_impressions,
          alternative = 'two.sided')

fig_h1 <- totals %>% 
  mutate(conv_rate = sum_purchase/sum_impressions) %>% 
  ggplot(aes(x = Campaign_Name, y = conv_rate, fill = Campaign_Name)) +
  geom_col()+
  labs(
    x = 'Conversion Rate',
    y = NULL,
    fill = 'Version',
    title = 'Conversion Rate Between Campaign Versions'
  ) +
  theme_classic() +
  scale_fill_manual(values = c('tan2', 'sienna4'))

fig_h1

# H2
prop.test(x = totals$sum_reach,
          n = totals$sum_impressions,
          alternative = 'two.sided')

chisq.test(totals$sum_reach)

fig_h2 <- totals %>% 
  mutate(conv_rate = sum_reach/sum_impressions) %>% 
  ggplot(aes(x = Campaign_Name, y = conv_rate, fill = Campaign_Name)) +
  geom_col()+
  labs(
    x = 'Conversion Rate',
    y = NULL,
    fill = 'Version',
    title = 'Conversion Rate Between Campaign Versions'
  ) +
  theme_classic() +
  scale_fill_manual(values = c('tan2', 'sienna4'))

fig_h2

# H3
prop.test(x = totals$sum_purchase,
          n = totals$sum_add_cart,
          alternative = 'two.sided')

fig_h3

# H4
t.test(data$cpp ~ data$Campaign_Name)
