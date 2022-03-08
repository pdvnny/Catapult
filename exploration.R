# load packages
library(catapultR)
library(catapultRcpp)
library(tidyverse)
library(RcppRoll)
options(digits = 12)



# load data -------------------------------------------------------------------
sd_df <- read_rds("data/sd_df_GHA_20200229.rds")
ci_df <- read_rds("data/contact_involvements.rds")
kick_df <- read_rds("data/kicks.rds")

# these are just vectors with start and end times
first_half <- read_rds("data/first_half.rds") 
second_half <- read_rds("data/second_half.rds")

athletes <- read_rds("data/athletes.rds")
athlete_info <- read_rds("data/athlete_info.rds")


# exploration / viz -------------------------------------------------------
sd_list <- sd_df %>% group_by(athlete_id) %>% group_split()

for (i in seq_along(sd_list)) {
  ln <- sd_list[[i]]$last_name[1]
  write_csv(sd_list[[i]], paste0("data/tenHz_rugby_", ln, ".csv"))
}  


sd_df %>% 
  group_by(cs_time_unix) %>% 
  summarize(avg_x = mean(X, na.rm = TRUE),
            avg_y = mean(Y, na.rm = TRUE)) %>% 
  ggplot(aes(x = avg_x, y = avg_y)) + 
  geom_path()

sd_df %>% 
  group_by(cs_time_unix) %>% 
  summarize(X = mean(X, na.rm = TRUE),
            Y = mean(Y, na.rm = TRUE)) %>% 
  mutate(first_name = "Team Average",
         last_name = "Team Average") %>% 
  bind_rows(sd_df) %>% 
  mutate(time_min = (cs_time_unix - first_half[1]*100)/100/60) %>% 
  filter(last_name %in% c("Team Average", "Ewing")) %>% 
  # filter(avg_y >= 5) %>% 
  ggplot(aes(x = time_min, y = X, color = last_name)) + 
  geom_line() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(title = "Average GHA Location Over Time: GHA vs. Musselburgh",
       subtitle = "0 and 100 represent Goal Lines")

sd_df %>% 
  filter(!is.na(period)) %>% 
  group_by(cs_time_unix, position_name, period) %>% 
  summarize(X = mean(X, na.rm = TRUE),
            Y = mean(Y, na.rm = TRUE)) %>% 
  mutate(time_min = (cs_time_unix - first_half[1]*100)/100/60) %>% 
  ggplot(aes(x = time_min, y = X, color = position_name)) + 
  geom_line() +
  geom_vline(data = kick_df %>% 
               mutate(period = case_when(
                 start_time*100 >= first_half[1]*100 & start_time*100 <= first_half[2]*100 ~ "1st Half",
                 start_time*100 >= second_half[1]*100 & start_time*100 <= second_half[2]*100 ~ "2nd Half"
               )) %>% 
               filter(start_time <= first_half[1]*100), 
             aes(xintercept = (start_time*100 - first_half[1]*100)/100/60, alpha = confidence)) +
  theme_minimal() +
  facet_wrap(~ period, ncol = 1, scales = "free_x") +
  theme(legend.title = element_blank()) +
  labs(title = "Average GHA Location Over Time: GHA vs. Musselburgh",
       subtitle = "Vertical lines represent kicks.  0 and 100 represent Goal Lines")


# I think we can tell that in the 1st half, GHA is defending the goal line indicated at y = 0.  (Opposite for second half)

avg_team_loc <- sd_df %>% 
  group_by(cs_time_unix, period) %>% 
  summarize(X = mean(X, na.rm = TRUE),
            Y = mean(Y, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(X = ifelse(is.nan(X), NA, X),
         Y = ifelse(is.nan(Y), NA, Y)) %>% 
  fill(X, .direction = "up") %>% 
  fill(Y, .direction = "up") %>% 
  mutate(smoothed_x_rollmean = RcppRoll::roll_mean(x = X, n = 100, fill = NA),
         smoothed_x_butter = butter_groupdel.adj(X, n = 2, W = 0.005))


# compare different smoothings (positive/negative slope represents offense/defense, close to undefined slope is "other" ??)
avg_team_loc %>% 
  filter(!is.na(period)) %>% 
  ggplot() + 
  geom_line(aes(x = cs_time_unix, y = X), color = "black") +
  geom_line(aes(x = cs_time_unix, y = smoothed_x_butter), color = "blue") +
  geom_line(aes(x = cs_time_unix, y = smoothed_x_rollmean), color = "red") +
  geom_rug(data = ci_df %>% 
             filter(athlete_id == athletes$id[athletes$last_name == "Ewing"]) %>%  # ewing's contact involvements
             mutate(period = case_when(
               start_time*100 >= first_half[1]*100 & start_time*100 <= first_half[2]*100 ~ "1st Half",
               start_time*100 >= second_half[1]*100 & start_time*100 <= second_half[2]*100 ~ "2nd Half"
             )),
           aes(x = start_time*100)) +
  facet_wrap(~ period, ncol = 1, scales = "free_x")

# I kind of like the rolling 10 second mean here... (but this could use some more work and validation.)
# label offense / defense

avg_team_loc <- avg_team_loc %>% 
  mutate(x_diff_avg = smoothed_x_rollmean - lag(smoothed_x_rollmean),
         phase = case_when(
           x_diff_avg > 0 & period == "1st Half" ~ "Offense",
           x_diff_avg <= 0 & period == "1st Half" ~ "Defense",
           x_diff_avg > 0 & period == "2nd Half" ~ "Defense",
           x_diff_avg <= 0 & period == "2nd Half" ~ "Offense"))

avg_team_loc %>% 
  filter(!is.na(period)) %>% 
  mutate(time_min = (cs_time_unix - first_half[1]*100)/100/60) %>% 
  ggplot(aes(x = time_min, y = x_diff_avg)) + 
  geom_line() +
  geom_area(aes(fill = phase)) +
  geom_vline(data = kick_df %>% 
               mutate(period = case_when(
                 start_time*100 >= first_half[1]*100 & start_time*100 <= first_half[2]*100 ~ "1st Half",
                 start_time*100 >= second_half[1]*100 & start_time*100 <= second_half[2]*100 ~ "2nd Half"
               )) %>% 
               filter(start_time <= first_half[1]*100), 
             aes(xintercept = (start_time*100 - first_half[1]*100)/100/60, alpha = confidence)) +
  facet_wrap(~ period, ncol = 1, scales = "free_x") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(title = "Differential X-Position Used to Determine Offense / Defense",
       subtitle = "Phase is determined based purely on 10-sec rolling average team movement.")

# Maybe add in some kicking logic to determine transitions?
# any phase immediately following a kick is a "transition"...
# add kicks...
avg_team_loc <- avg_team_loc %>% 
  left_join(kick_df %>% 
              mutate(cs_time_unix = round(start_time)*100) %>% 
              select(cs_time_unix, class)) %>% 
  rename(kick = class)

avg_team_loc %>% 
  filter(!is.na(kick)) %>% 
  group_by(phase) %>% 
  count()
# most seem legit... not sure what's going on with those kicks on defense.

# add 10 sec transition phase after each kick.
for (i in seq_len(nrow(avg_team_loc))) {
  if (!is.na(avg_team_loc$kick[i])) {
    avg_team_loc$phase[i:(i+100)] <- "Transition"
  }
}

# add group index to each phase for plotting
group_index <- c()
group_id = 1
for (i in seq_len(nrow(avg_team_loc))) {
  if (i == 1) {
    group_index[i] <- group_id
  } else if (i != 1) {
    phase_current <- avg_team_loc$phase[i]
    phase_previous <- avg_team_loc$phase[(i-1)]
    
    if (identical(phase_current, phase_previous)) {
      group_index[i] <- group_id
    } else {
      group_index[i] <- group_id + 1
      group_id <- group_id + 1
    }
  }
  
}

avg_team_loc <- avg_team_loc %>% 
  mutate(phase_id = group_index)

avg_team_loc %>% 
  filter(!is.na(period)) %>% 
  mutate(time_min = (cs_time_unix - first_half[1]*100)/100/60) %>% 
  ggplot(aes(x = time_min, y = x_diff_avg)) + 
  geom_line() +
  geom_area(aes(fill = phase, group = phase_id)) +
  geom_vline(data = kick_df %>% 
               mutate(period = case_when(
                 start_time*100 >= first_half[1]*100 & start_time*100 <= first_half[2]*100 ~ "1st Half",
                 start_time*100 >= second_half[1]*100 & start_time*100 <= second_half[2]*100 ~ "2nd Half"
               )) %>% 
               filter(start_time <= first_half[1]*100), 
             aes(xintercept = (start_time*100 - first_half[1]*100)/100/60, alpha = confidence)) +
  facet_wrap(~ period, ncol = 1, scales = "free_x") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(title = "Differential X-Position Used to Determine Offense / Defense",
       subtitle = "Phase is determined based purely on 10-sec rolling average team movement.",
       caption = "Now with 10-sec transition phase after each kick.")


# combine athlete sensor data, phase, contact involvements...
combined_df <- sd_df %>% 
  left_join(ci_df %>% 
              mutate(cs_time_unix = round(start_time)*100,
                     contact_involvement = TRUE) %>% 
              select(-c(start_time, end_time, version))) %>% 
  filter(contact_involvement == TRUE) %>% 
  left_join(avg_team_loc %>% 
              select(cs_time_unix, phase)) 

combined_df %>% 
  filter(!is.na(period)) %>% 
  ggplot(aes(x = X, y = Y, color = last_name)) +
  geom_point() +
  facet_grid(phase ~ period) +
  theme_bw() +
  labs("Contact Involvements by Location")


# normalize y-axis by half... (flip data)
combined_df %>% 
  mutate(normalized_y = case_when(
    period == "1st Half" ~ Y,
    period == "2nd Half" ~ (70-Y)
  )) %>% 
  filter(!is.na(period)) %>% 
  group_by(phase, last_name) %>% 
  add_count() %>% 
  filter(n >= 10) %>% 
  ggplot(aes(normalized_y, color = last_name, fill = last_name)) +
  geom_density(alpha = 0.2) +
  facet_grid(.~ phase) +
  theme_minimal() +
  labs(title = "Contact Involvement (Horizontal Control)")


# I'm guessing that there is too much side to side movement on the field to see a meaningful distribution that indicates
# an area of pitch control for each athlete.

# Potential Solution... 
# for each contact involvement,
#   determine min/max Y for entire team
#   calculate relative location within this range

# add normalized Y
combined_df <- combined_df %>% 
  mutate(normalized_y = case_when(
    period == "1st Half" ~ Y,
    period == "2nd Half" ~ (70-Y)
  ))

sd_df <- sd_df %>% 
  mutate(normalized_y = case_when(
    period == "1st Half" ~ Y,
    period == "2nd Half" ~ (70-Y)
  ))

combined_df$min_y <- NA
combined_df$max_y <- NA
for (i in seq_len(nrow(combined_df))) {
  time_stamp <- combined_df$cs_time_unix[i]
  Y_ci <- combined_df$Y[i]
  
  min_max_y <- sd_df %>% 
    filter(cs_time_unix == time_stamp,
           normalized_y >= 0) %>% 
    summarize(min_y = min(normalized_y),
              max_y = max(normalized_y)) %>% 
    mutate(range = max_y - min_y)
  
  combined_df$min_y[i] <- min_max_y$min_y
  combined_df$max_y[i] <- min_max_y$max_y
  
}

combined_df <- combined_df %>% 
  mutate(y_range = max_y - min_y,
         relative_y = ((normalized_y - min_y) / (max_y - min_y)))


# now let's plot again...
combined_df %>% 
  filter(!is.na(period)) %>% 
  group_by(phase, last_name) %>% 
  add_count() %>% 
  filter(n >= 10,
         phase == "Defense"
         ) %>% 
  ggplot(aes(relative_y, color = last_name, fill = last_name)) +
  geom_density(alpha = 0.2, adjust = .2) + # play with this parameter some
  facet_grid(.~ phase) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(title = "Contact Involvement (Horizontal Control)",
       subtitle = "Uses relative position of athlete.") +
  facet_wrap(~ last_name, ncol = 1)

combined_df %>% 
  filter(!is.na(period)) %>% 
  group_by(phase, position_name) %>% 
  add_count() %>% 
  filter(n >= 10,
         phase == "Defense"
  ) %>% 
  ggplot(aes(relative_y, color = position_name, fill = position_name)) +
  geom_density(alpha = 0.2, adjust = .2) + # play with this parameter some
  facet_grid(.~ phase) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(title = "Contact Involvement (Horizontal Control)",
       subtitle = "Uses relative position of athlete.") +
  facet_wrap(~ position_name, ncol = 1)


# maybe try some outlier removal... within 1 stdev of median or 70% high density region??



combined_df_summary <- combined_df %>% 
  filter(!is.na(period)) %>% 
  group_by(phase, period, last_name) %>% 
  summarize(avg_y = mean(Y),
            std_dev_y = sd(Y),
            count = n()) 

## also try plotting team horizontal width over time
sd_df %>% 
  filter(normalized_y >= 5,
         normalized_y <= 65,
         !is.na(period)) %>% 
  group_by(period, cs_time_unix) %>% 
  summarize(max_y = max(normalized_y), 
            min_y = min(normalized_y),
            median_y = median(normalized_y),
            sd_y = sd(normalized_y)) %>% 
  mutate(time_min = (cs_time_unix - first_half[1]*100)/100/60) %>% 
  ggplot() + 
  geom_ribbon(aes(x = time_min, ymin = median_y - sd_y, ymax = median_y + sd_y)) +
  facet_wrap(~ period, ncol = 1, scales = "free_x") +
  labs(title = "Team Width",
       subtitle = "GHA Attacking to the Right")
  
  
  
  