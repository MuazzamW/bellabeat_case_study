# 1. What are some trends in smart device usage?
# 2. How could these trends apply to Bellabeat customers?
# 3. How could these trends help influence Bellabeat marketing strategy?

root <- "C:\\Users\\muazzam\\Documents\\bellabeat_case_study\\"

read = function(s1){
  paste(root,s1,sep="") %>%
    read.csv()
}

camel_to_snake <- function(camel_case_names) {
  # Use str_replace_all from the stringr package to add underscores before uppercase letters
  snake_case_names <- str_replace_all(camel_case_names, "(?<!^)([A-Z])", "_\\1")
  # Convert all characters to lowercase
  snake_case_names <- tolower(snake_case_names)
  return(snake_case_names)
}

daily_activity <- read("dailyActivity_merged.csv")
sleep_day <- read("sleepDay_merged.csv")

#edit variable names in daily_activity
daily_activity <- setNames(daily_activity,camel_to_snake(names(daily_activity)))

#edit variable names in sleep_day
sleep_day <- setNames(sleep_day,camel_to_snake(names(sleep_day)))


#cleaning the names
clean_names(daily_activity)


ggplot(data = daily_activity, mapping = aes(y = TotalSteps)) + 
  geom_boxplot() + 
  scale_x_continuous()

ggplot(data=sleep_day, aes(x=total_minutes_asleep, y=total_time_in_bed)) + 
  geom_point(aes(color="red")) +
  geom_smooth(method="lm",se=FALSE) + 
  labs(caption = lm(sleep_day$total_time_in_bed ~ sleep_day$total_minutes_asleep))


thirtythree <- c("01", "02", "03", "04", "05", "06", 
                 "07", "08", "09", 10:33)

Users <- paste("User", thirtythree, sep = " ")

Id <- unlist(distinct(full_day_cleaned,  id))

users_id <- tibble(Users, Id)


AL <- full_day_cleaned %>% 
  group_by(id) %>%  
  summarize(
    median_very_active = median(very_active_minutes)
  ) %>% 
  mutate(
    activity_level = case_when(
      median_very_active < 10 ~ "Low",
      median_very_active <= 30 ~ "Med",
      median_very_active < 210 ~ "High"
    ),
    activity_level = factor(
      activity_level,
      levels=c('Low', 'Med', 'High')
    )
  ) %>% 
  subset(select = -c(median_very_active))  

df_1 <- minute_intensities_narrow_merged %>% 
  group_by(Id) %>% 
  mutate(countId= n()) %>% 
  ungroup %>% 
  group_by(Id,countId,Intensity) %>% 
  summarise(count=n(),
            .groups='drop' ) %>%
  group_by(Id) %>% 
  mutate(cumcount=cumsum(count),
         pos=cumsum(count)-count/2,
         per=paste0(round(100*count/countId,2),'%')) %>% 
  ungroup



df_2 <- full_day_cleaned %>%
  group_by(id) %>%
  summarise(very_active_mins = median(very_active_minutes),
            lightly_active_mins = median(lightly_active_minutes),
            fairly_active_mins = median(fairly_active_minutes),
            sedentary_mins = median(sedentary_minutes)) %>%
  mutate(total = rowSums(df_2[, -1]))

df_2 <- df_2 %>%
  mutate(
    fairly_active_prop = fairly_active_mins / total,
    lightly_active_prop = lightly_active_mins / total,
    very_active_prop = very_active_mins / total,
    sedentary_prop = sedentary_mins / total
  )

df_long <- df_2 %>%
  select(id, fairly_active_prop, lightly_active_prop, very_active_prop, sedentary_prop) %>%
  pivot_longer(cols = -id, names_to = "activity", values_to = "proportion")

df_long <- df_long %>%
  full_join(users_id, by = c("id" = "id")) %>%
  full_join(AL, by = c("id" = "id"))

labels <- c(fairly_active_prop = "Fairly Active", lightly_active_prop = "Lightly Active",
            very_active_prop = "Very Active",
            sedentary_prop = "Sedentary")
colors <- c(fairly_active_prop = "blue", lightly_active_prop = "green", very_active_prop = "red", sedentary_prop = "gray")

ggplot(df_long, aes(x = "", y = proportion, fill = activity)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~Users~activity_level) +
  labs(title = "Proportion of Time Spent in Each Activity Category by ID and Activity Level") +
  theme_void() +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = colors, labels = labels)

ids <- c("01", "02", "03", "04", "05", "06", 
                 "07", "08", "09", 10:33)

Users <- paste("User", c(1:33), sep = " ")

Id <- distinct(full_day_new, id)

users_id <- tibble(Users, Id)


heart_rate$Time <- parse_date_time(
  heart_rate$Time, 
  "%m/%d/%y %I:%M:%S %p"
)

heart_rate <- heart_rate %>%
  mutate(ActivityDay = date(Time))

heart_rate_AL <- heart_rate %>%
  left_join(AL, by = c("Id"="id"))

heart_rate_AL <- heart_rate_AL %>% 
  mutate(
    Hour = format(Time, "%k")
  )

heart_rate_AL %>%
  group_by(Id, ActivityDay,Hour)

heart_rate_pivot <- heart_rate_AL %>%
  group_by(Id,ActivityDay,Hour) %>%
  summarise(mean_bpm_hour = mean(Value))


most_hours_day <- heart_rate_AL %>%
  group_by(Id, ActivityDay) %>%
  summarize(hours_recorded = n()) %>%
  ungroup() %>%
  group_by(Id) %>%
  filter(hours_recorded == max(hours_recorded)) %>%
  slice(1) %>%  # In case of ties, select the first occurrence
  select(Id, ActivityDay)

# Filter the data for the random day for each user
filtered_heart_rate <- heart_rate_pivot %>%
  inner_join(most_hours_day, by = c("Id", "ActivityDay" = "ActivityDay")) %>%
  inner_join(AL, by = c("Id" = "id"))


ggplot(filtered_heart_rate, aes(x=Hour, y = mean_bpm_hour, color = as.factor(Id), 
                             group = interaction(Id,ActivityDay))) + 
  geom_line() +
  facet_wrap(~Id~activity_level) + 
  labs(title = "Mean BPM per Hour for Each User ID",
       x = "Hour of Day",
       y = "Mean BPM",
       color = "User ID") + 
  theme_minimal()
