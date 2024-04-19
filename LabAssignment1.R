#1 LabAssignment

#
gac <- read.csv('GACTT_RESULTS_ANONYMIZED_HW1.csv')
print(head(gac,6))
print(tail(gac,4))

#print out the variable classes
sapply(gac, class)

#

table(gac$party,gac$cups_num)

cleaned_data <- gac[complete.cases(gac$cups_num, gac$party), ]
cleaned_data$party <- factor(cleaned_data$party) 

plot(cleaned_data$cups_num~cleaned_data$party)

#another approach

library(ggplot2)
cleaned_data |> 
  ggplot(aes(x = cups_num, fill = cups_num)) +
  geom_bar() +
  labs(title = "The Relationship between cups_num and party",
       x = "cups_num",
       y = "party")+
  theme(legend.position = "none")+
  facet_wrap(~party)




#2

zips <- read.csv("zip_code_database.csv")

#did I already tell you that R has some amazing built-in functions?

zips$state_name <- state.name[match(zips$state,state.abb)]

head(zips)

table(gac$zip %in% zips$zip)
#merge by zip code
merged_data <- merge(cleaned_data, zips, by.x = "zip", by.y = "zip", all.x = TRUE)
merged_data

#grouped data is the dataframe with the mean number of cups by states
library(dplyr)
grouped_data <- merged_data %>%
  group_by(state_name) %>%
  summarise(
    coffee_avg = mean(cups_num)
  )


library(vctrs)

library(tidyr)
library(dplyr)

#separate the answers with several different options
dataset_split <- merged_data %>%
  separate_rows(home_brew, sep = ", ")
dataset_split

#home_brew method count
favorite_method <- dataset_split %>%
  group_by(state_name, home_brew) %>%
  summarise(count = n()) %>%
  arrange(state_name, desc(count)) %>%
  slice(1)

#1.3.3
preferred <- dataset_split %>%
  group_by(state_name, home_brew) %>%
  summarise(count = n()) %>%
  arrange(home_brew, desc(count)) %>%
  group_by(home_brew) %>%
  slice(1)
#all california though, I think it's because california has much more participants in the survey

#1.3.4
breakdown <- merged_data %>%
  group_by(state_name,party) %>%
  summarise(count = n())

#2.1
election <- read.csv('election_2020.csv')
View(election)
sapply(election, class)
election$total_votes <- as.numeric(gsub(",", "", election$total_votes))
election$biden_votes <- as.numeric(gsub(",", "", election$biden_votes))
election$trump_votes <- as.numeric(gsub(",", "", election$trump_votes))
election$other_votes <- as.numeric(gsub(",", "", election$other_votes))
#remove % and change into the numeric 
columns_to_convert <- c('biden_votes_share','trump_votes_share','other_votes_share')
election <- election %>%
  mutate_at(columns_to_convert, ~as.numeric(gsub("%", "", .)))%>%
  mutate(across(all_of(columns_to_convert), ~./100))

#2.2 merge

merged_party <- merge(election, merged_data, by.x = "state", by.y = "state_name")

#2.3 
total_count_per_state = breakdown.groupby('state_').sum()['count']

breakdown <- breakdown %>%
  group_by(state_name)%>%
  mutate(total_count = sum(count))

breakdown <- breakdown %>%
  group_by(state_name) %>%
  mutate(percentage = count/ total_count)

merged_party_new <- merge(election, breakdown, by.x = "state", by.y = "state_name")



election$total_votes <- as.numeric(election$total_votes)
election <- election %>%
  mutate_at(vars(total_votes:other_votes_share),~as.numeric(gsub('%',"",',','.')))

df <- df %>%
  mutate_at(vars(column1:column2), ~as.numeric(gsub("%", "", .)))

democrat_rows <- merged_party_new %>%
  filter(party == 'Democrat') 

n1 <- democrat_rows %>% 
  select(biden_votes_share, percentage)

plot(data=n1, biden_votes_share~percentage)

#2.4
merged_coffee <- merge(election, merged_data, by.x = 'state', by.y = 'state_name')
plot(data = merged_coffee, biden_votes_share ~ cups_num)
plot(data= merged_coffee, trump_votes_share ~ cups_num)
plot(data = merged_coffee, other_votes_share ~ cups_num)

#2.5 
library(openxlsx)
write.xlsx(merged_party_new, "overview_hw1.xlsx", row.names = FALSE)

#3

library(knitr)
library(gtrendsR)
keyword = unique(favorite_method$home_brew)
googletrend_hb = data.frame(location = na.omit(merged_data$state_name))
for (k in keyword){
  trends_k <- gtrends(keyword = k, geo = "US")
  trends_k = trends_k$interest_by_region[, 1:2]
  names(trends_k)[names(trends_k) == "hits"] <- paste0("hits_", k)
  print(kable(trends_k, caption = paste("Interest by State for", k)))
  googletrend_hb = left_join(googletrend_hb,
                             trends_k,
                             by = "location")
  googletrend_hb = unique(googletrend_hb)
}


googletrend_hb[-1] <- sapply(googletrend_hb[-1], as.numeric)
googletrend_hb$highest_hit = apply(googletrend_hb[-1], 1,
                                   function(x)
                                     names(googletrend_hb[-1])[which.max(x)])
googletrend_hb$highest_hit <- gsub("hits_", "", googletrend_hb$highest_hit)
table(googletrend_hb$highest_hit %in% favorite_method$home_brew)


plot(data=n1, biden_votes_share~percentage)
model <- lm(biden_votes_share~percentage, data=n1)
abline(model,col='red')
