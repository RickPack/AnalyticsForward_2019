library(httr)
library(tidyverse)
library(jsonlite)
library(meetupr)
library(lubridate)
library(magick)
library(directlabels)



if (!github_file) {
  todaydt <- str_replace_all(as.character(Sys.Date()), "-","_")
  
  # Get the Yes RSVPs for a specified event
  # id_name is a descriptor of your choice
  # and id appears at the end of the Meetup.com URL
  # e.g., the number in
  # https://www.meetup.com/Research-Triangle-Analysts/
  # events/246678392/
  meetup_yes_RSVPs <- function(id_name, id){
    rladiesfrm <- meetupr::get_event_rsvps(meetupgrp_name, id) %>%
      dplyr::filter(response == "yes") %>%
      mutate(rsvp_yes_row = 1 + guests) %>% 
      mutate(dates_yes = date(ymd_hms(created)))
    
    df_content <-   rladiesfrm %>%
      group_by(dates_yes) %>%
      summarise(rsvp_yes_count = sum(rsvp_yes_row)) %>%
      ungroup()
    id_name      <- as.character(id_name)
    dates_yes    <- unique(df_content$dates_yes)
    rsvp_yes_count <- df_content$rsvp_yes_count
    # stringsAsFactors = FALSE to avoid warnings
    # about factor level differences when I bind_rows later
    df_out       <- data.frame(id_name, dates_yes, rsvp_yes_count, id,
                               stringsAsFactors = FALSE)
    invisible(df_out)
  }
  # manually extracted from URLs like
  # https://www.meetup.com/Research-Triangle-Analysts/events/246678392/
  AF19_id <-  '258165094'
  AF18_id  <- '246678392'
  AF17_id  <- '237118943'
  AF16_id  <- '228455037'
  AF15_id  <- '219885748'
  AF19_frm <- meetup_yes_RSVPs("AF19", AF19_id)
  AF18_frm <- meetup_yes_RSVPs("AF18", AF18_id)
  AF17_frm <- meetup_yes_RSVPs("AF17", AF17_id)
  AF16_frm <- meetup_yes_RSVPs("AF16", AF16_id)
  AF15_frm <- meetup_yes_RSVPs("AF15", AF15_id)
  
  date_fill <- function(df){
    df 
  }
  
  allAF_frm_rladies <- bind_rows(AF15_frm, AF16_frm, AF17_frm, AF18_frm, AF19_frm)
  
  allRTA_events_future <- get_events('Research-Triangle-Analysts')
  allRTA_events_past   <- get_events('Research-Triangle-Analysts', "past")
  allRTA_events        <- rbind(allRTA_events_future, allRTA_events_past)
  AF_events            <- allRTA_events[grepl("FORWARD", toupper(allRTA_events$name)),]
  # Do not include preparation meeting by frequent
  # high Analytics>Forward vote-earner, Mark Hutchinson
  AF_events            <- AF_events %>% dplyr::filter(!grepl("PREPARATION", toupper(name)))
  # local_date loads as a date column
  AF_events_dates_yes  <- AF_events %>% select(id, local_date, created) %>%
    # strip off time and only keep date
    mutate(created = str_sub(created, 1, 10)) %>%
    # renaming columns from Meetup API for easier identification
    rename(creation_date_per_AF_year = created,
           event_date = local_date) %>%
    group_by(id) %>%
    # seq function below needs from and to arguments to be date variables
    # so converting character to date with lubridate ymd function
    mutate(event_date = ymd(event_date),
           creation_date_per_AF_year = ymd(creation_date_per_AF_year)) %>%
    mutate(yes_year = lubridate::year(event_date))
  
  # revised until no NAs, added dates with tidyr::complete
  # will only contain non-NA values for group_by columns
  allAF_frm <- left_join(allAF_frm_rladies, AF_events_dates_yes) %>%
    group_by(id_name, event_date, creation_date_per_AF_year,
             id, yes_year) %>%
    mutate(max_date_per_AF_year = ymd(max(dates_yes))) %>%
    # https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
    # Notice use of unique because seq.Date needs one value for 'from' and 'to' arguments
    tidyr::complete(dates_yes = seq.Date(unique(creation_date_per_AF_year), 
                                         unique(max_date_per_AF_year), by = "day")) %>%
    replace_na(list(rsvp_yes_count = 0)) %>%
    mutate(dates_yes_cumsum = cumsum(rsvp_yes_count)) %>%
    ungroup() %>%
    # dropping so data frame can be provided to others and not cause questions about NAs
    # produced by spanning out dates with seq.Date
    select(-max_date_per_AF_year) %>%
    mutate(days_to_event = event_date - dates_yes) %>% 
    arrange(id_name, desc(days_to_event))
  
  
  # add days and month column as inspired by
  # StackOverflow user NicE on
  # https://stackoverflow.com/questions/28503262/using-lubridate-and-ggplot2-effectively-for-date-axis
  allAF_frm$yes_day   <- lubridate::day(allAF_frm$dates_yes)
  allAF_frm$yes_month <- lubridate::month(allAF_frm$dates_yes)
  allAF_frm$yes_year  <- lubridate::year(allAF_frm$dates_yes)
  allAF_frm$dates_yes_otheryear <- as.Date(
    format(
      # 2019 used in below line because future year
      # denotes false year imposed to homogenize
      # year for ggplot printing on same axes
      allAF_frm$dates_yes,"%d-%m-2020"),
    format = "%d-%m-%y")
  # Get the RSVP yes count as of the latest day of the most recent event
  # so years are ordered at farthest-right point for the year of 
  # greatest interest - the present year
  today_days_to_event      <- allAF_frm %>%
    dplyr::filter(yes_year == max(yes_year)) %>%
    mutate(min_days_event = min(days_to_event)) %>%
    distinct(min_days_event) %>%
    pull(min_days_event)
  
  today_relative_yes_count <- allAF_frm %>%
    group_by(yes_year) %>%
    dplyr::filter(days_to_event == today_days_to_event) %>%
    # relative to today
    rename(relative_today_cumsum = dates_yes_cumsum) %>%
    ungroup() %>%
    select(id, relative_today_cumsum)
  
  allAF_frm            <- allAF_frm %>%
    left_join(., today_relative_yes_count) %>%
    mutate(yes_year_factor = fct_reorder(as.factor(yes_year), relative_today_cumsum, 
                                         .desc = TRUE))
}

if (github_file) {
  allAF_frm <- readr::read_csv(github_filename) %>%
    mutate(yes_year_factor = fct_reorder(as.factor(yes_year), relative_today_cumsum, 
                                         .desc = TRUE))
  
  today_days_to_event      <- allAF_frm %>%
    dplyr::filter(yes_year == max(yes_year)) %>%
    mutate(min_days_event = min(days_to_event)) %>%
    distinct(min_days_event) %>%
    pull(min_days_event)
}

allAF_frm_weekday <- allAF_frm %>%
  mutate(yes_weekday = weekdays(ymd(dates_yes))) %>%
  group_by(yes_year, yes_weekday) %>%
  summarise(dates_yes_cumsum_week = sum(rsvp_yes_count)) %>%
  ungroup() %>%
  mutate(yes_weekday_factor  = factor(yes_weekday, levels = 
                                        c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday" ,"Saturday", "Sunday"),
                                      ordered = TRUE),
         # forcats::fct_rev used to reverse order 
         # (later year will come first)
         # so in stacked bar graph later year is on far-right
         yes_year_factor = fct_rev(factor(yes_year))) %>%
  select(-yes_year, -yes_weekday)

# same as above but exclude the last week, which is when
# registrations dramatically increase
# last day of last captured week is Saturday
allAF_frm_weekday_not_lastweek <- allAF_frm %>%
  dplyr::filter(days_to_event > 6) %>%
  mutate(yes_weekday = weekdays(ymd(dates_yes))) %>%
  group_by(yes_year, yes_weekday) %>%
  summarise(dates_yes_cumsum_week = sum(rsvp_yes_count)) %>%
  ungroup() %>%
  mutate(yes_weekday_factor  = factor(yes_weekday, levels = 
                                        c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday" ,"Saturday", "Sunday"),
                                      ordered = TRUE),
         # forcats::fct_rev used to reverse order 
         # (later year will come first)
         # so in stacked bar graph later year is on far-right
         yes_year_factor = fct_rev(factor(yes_year))) %>%
  select(-yes_year, -yes_weekday)

# same as above but only the week prior to Analytics>Forward,
# to help with a marketing plan for that week
# Last day of last captured week is Saturday
allAF_frm_weekday_penult_week <- allAF_frm %>%
  dplyr::filter(days_to_event > 6 & days_to_event < 14) %>%
  mutate(yes_weekday = weekdays(ymd(dates_yes))) %>%
  group_by(yes_year, yes_weekday) %>%
  summarise(dates_yes_cumsum_week = sum(rsvp_yes_count)) %>%
  ungroup() %>%
  mutate(yes_weekday_factor  = factor(yes_weekday, levels = 
                                        c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday" ,"Saturday", "Sunday"),
                                      ordered = TRUE),
         # forcats::fct_rev used to reverse order 
         # (later year will come first)
         # so in stacked bar graph later year is on far-right
         yes_year_factor = fct_rev(factor(yes_year))) %>%
  select(-yes_year, -yes_weekday)

if (!github_file) {
  write.csv(allAF_frm, paste0("C:/Users/rick2/Documents/Rick/AnalyticsForward/AnalyticsForward_", todaydt, "_", AMPM, ".csv"), row.names = FALSE)
  write.csv(allAF_frm_weekday, paste0("C:/Users/rick2/Documents/Rick/AnalyticsForward/AnalyticsForward_weekday_", todaydt, "_", AMPM, ".csv"), row.names = FALSE)
}

p1 <- 
  ggplot(data = allAF_frm,
         aes(x = dates_yes_otheryear,
             y = dates_yes_cumsum,
             colour = yes_year_factor)) +
  geom_line(size = 1) +
  scale_x_date(date_labels = "%b") +
  xlab("Month of year") +
  ylab("YES (will attend) RSVPs") +
  labs(colour = "Year") + 
  ggtitle(label = paste0("Research Triangle Analysts 'Analytics>Forward' Registrations as of\n",
                         Sys.Date()),
          subtitle = str_glue("Zillow Data Science (Kaggle) winner, Jordan Meyer", 
                              " keynoting March 9, 2019\n",
                              as.numeric(today_days_to_event), " days remaining\nGraph 1 of 5")) +
  directlabels::geom_dl(aes(label = yes_year), method = list("last.points",rot = -50)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, lineheight = .8, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(face = "bold.italic", color = "red", size = 16),
        axis.text.y = element_text(face = "bold.italic", color = "red", size = 16)) +
  labs(colour = "Year") + 
  # http://colorbrewer2.org/#type=qualitative&scheme=Set2&n=5
  scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")) +
  NULL

# annotate("text", x = ymd('2020-02-12'), y = 90,
#          label = "2019 registrations recently went flat",
#          size = 5)

p2 <- 
  ggplot(data = allAF_frm,
         aes(x = as.numeric(days_to_event),
             y = dates_yes_cumsum,
             colour = yes_year_factor)) +
  geom_line(size = 1) +
  xlab("Days to Event") +
  ylab("YES (will attend) RSVPs") +
  labs(colour = "Year") + 
  ggtitle(label = paste0("Research Triangle Analysts 'Analytics>Forward' as of\n",
                         Sys.Date(), "\nGraph 2 of 5"),
          subtitle = str_glue("Zillow Data Science (Kaggle) winner, Jordan Meyer", 
                              " keynoting March 9, 2019\n",
                              as.numeric(today_days_to_event), " days remaining")) +
  directlabels::geom_dl(aes(label = yes_year), method = list("last.points",rot = -50)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, lineheight = .8, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(face = "bold.italic", color = "red", size = 16),
        axis.text.y = element_text(face = "bold.italic", color = "red", size = 16)) +
  # http://colorbrewer2.org/#type=qualitative&scheme=Set2&n=5
  scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")) +
  scale_x_reverse() +
  # annotate("text", x = 30, y = 90,
  #          label = "2019 registrations recently went flat",
  #          size = 5) +
  NULL

p3 <-
  ggplot(data = allAF_frm_weekday,
         aes(y = dates_yes_cumsum_week,
             x = yes_weekday_factor,
             fill = yes_year_factor)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Yes RSVP Count") +
  xlab("Day of Week") +
  labs(fill = "Year") + 
  coord_flip() + 
  scale_fill_viridis_d() +
  ggtitle(label = paste0("Research Triangle Analysts 'Analytics>Forward' as of\n",
                         Sys.Date(), "\nGraph 3 of 5"),
          subtitle = str_glue("Zillow Data Science (Kaggle) winner, Jordan Meyer", 
                              " keynoting March 9, 2019\nAll weeks shown.\n",
                              as.numeric(today_days_to_event), " days remaining")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, lineheight = .8, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(face = "bold.italic", color = "red", size = 16),
        axis.text.y = element_text(face = "bold.italic", color = "red", size = 16))

p4 <-
  ggplot(data = allAF_frm_weekday_not_lastweek,
         aes(y = dates_yes_cumsum_week,
             x = yes_weekday_factor,
             fill = yes_year_factor)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Yes RSVP Count") +
  xlab("Day of Week") +
  labs(fill = "Year") + 
  coord_flip() + 
  scale_fill_viridis_d() +
  ggtitle(label = paste0("Research Triangle Analysts 'Analytics>Forward' as of\n",
                         Sys.Date(), "\nGraph 4 of 5"),
          subtitle = str_glue("Zillow Data Science (Kaggle) winner, Jordan Meyer", 
                              " keynoting March 9, 2019\nEXCLUDES the week of the event.\n",
                              as.numeric(today_days_to_event), " days remaining")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, lineheight = .8, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(face = "bold.italic", color = "red", size = 16),
        axis.text.y = element_text(face = "bold.italic", color = "red", size = 16))

p5 <-
  ggplot(data = allAF_frm_weekday_penult_week,
         aes(y = dates_yes_cumsum_week,
             x = yes_weekday_factor,
             fill = yes_year_factor)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Yes RSVP Count") +
  xlab("Day of Week") +
  labs(fill = "Year") + 
  coord_flip() + 
  scale_fill_viridis_d() +
  ggtitle(label = paste0("Research Triangle Analysts 'Analytics>Forward' as of\n",
                         Sys.Date(), "\nGraph 5 of 5"),
          subtitle = str_glue("Zillow Data Science (Kaggle) winner, Jordan Meyer", 
                              " keynoting March 9, 2019\nONLY the week prior to the event.\n",
                              as.numeric(today_days_to_event), " days remaining")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, lineheight = .8, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(face = "bold.italic", color = "red", size = 16),
        axis.text.y = element_text(face = "bold.italic", color = "red", size = 16))


ggsave(p1, file = paste0("C:/Users/rick2/Documents/Rick/AnalyticsForward/AF_",
                         todaydt, "_1", AMPM, ".png"), height = 5, width = 8)
ggsave(p2, file = paste0("C:/Users/rick2/Documents/Rick/AnalyticsForward/AF_",
                         todaydt, "_2", AMPM, ".png"), height = 5, width = 8)
ggsave(p3, file = paste0("C:/Users/rick2/Documents/Rick/AnalyticsForward/AF_",
                         todaydt, "_3", AMPM, ".png"), height = 5, width = 8)
ggsave(p4, file = paste0("C:/Users/rick2/Documents/Rick/AnalyticsForward/AF_",
                         todaydt, "_4", AMPM, ".png"), height = 5, width = 8)
ggsave(p5, file = paste0("C:/Users/rick2/Documents/Rick/AnalyticsForward/AF_",
                         todaydt, "_5", AMPM, ".png"), height = 5, width = 8)
# Stitch images for gif using Magick package
# Use 600 x 322 for LinkedIn
# Otherwise, 1200 x 720
# 
img <- image_graph(width = 1200, height = 720, res = 96)
print(p3)
print(p4)
print(p5)
print(p1)
print(p2)
dev.off()

animation <- image_animate(image_scale(img), fps = 0.25)
image_write(animation, paste0("C:/Users/rick2/Documents/Rick/AnalyticsForward/AF_",
                              todaydt, AMPM, "_animate.gif"))
dev.off()

img <- image_graph(width = 600, height = 322, res = 96)
print(p3)
print(p4)
print(p5)
print(p1)
print(p2)
dev.off()

animation <- image_animate(image_scale(img), fps = 0.25)
image_write(animation, paste0("C:/Users/rick2/Documents/Rick/AnalyticsForward/AF_",
                              todaydt, AMPM, "_LinkedIn_animate.gif"))

message("Total registrations")
allAF_frm %>% group_by(yes_year) %>% 
  dplyr::filter(dates_yes == max(dates_yes)) %>%
  select(id_name, yes_year, dates_yes_cumsum)