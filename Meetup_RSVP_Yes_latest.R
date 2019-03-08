# could use RVerbalExpressions to find other Meetups to analyze
extrafont::loadfonts(device = "win")
# extrafont::font_import("C:/Windows/Fonts/", pattern = "GIL_____")
library(httr)
library(tidyverse)
library(jsonlite)
library(meetupr)
library(lubridate)
library(magick)
library(directlabels)
library(highcharter)
library(htmlwidgets)
library(plotly)
library(ggforce)

Meetup_RSVP_Yes_Count <- function(save_to_folder = TRUE, 
                                  github_file = FALSE, 
                                  meetupgrp_name = 'Research-Triangle-Analysts',
                                  folder_save = 
                                   "C:/Users/rick2/Downloads/R/AnalyticsForward_2019/"){
## Notice need for raw.
github_filename1 <- stringr::str_glue(
  'https://raw.githubusercontent.com/RickPack/Analytics',
  'Forward_2019/master/AnalyticsForward_Registrations.csv')
github_filename2 <- stringr::str_glue(
  'https://raw.githubusercontent.com/RickPack/Analytics',
  'Forward_2019/master/AnalyticsForward_Reg_DayofWeek.csv')

relative_day_graph_helper <- function(){
  # Get the RSVP yes count as of the latest day of the most recent event
  # so years are ordered based on registrations on that day, each year
  # For example, if it's 2019 and 30 days remain until the event and
  # 2019 has 40 registrations, but 2018 had 50 on that day, 
  # 2019 will appear below 2018.
  
  max_yes_year <- max(allAF_frm$yes_year)
  
  today_days_to_event      <- allAF_frm %>%
    dplyr::filter(yes_year == max_yes_year) %>%
    mutate(min_days_event = min(days_to_event)) %>%
    distinct(min_days_event) %>%
    pull(min_days_event)
  
  today_relative_yes_count <- allAF_frm %>%
    select(yes_year, id, days_to_event, dates_yes_cumsum) %>% 
    group_by(yes_year) %>%
    dplyr::filter(days_to_event == today_days_to_event) %>%
    # relative to today
    rename(relative_today_cumsum = dates_yes_cumsum) %>%
    ungroup() %>%
    select(id, relative_today_cumsum)
  
  # Maximum number of registrations - for horizontal line on graph
  max_registrations <- max(allAF_frm$dates_yes_cumsum)
  
  rel_obj <- list(today_days_to_event, today_relative_yes_count, max_registrations,
                  max_yes_year)
}
##########################################
##   START of non-Github download code  ##     
##########################################
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
      mutate(dates_yes = date(ymd_hms(created)),
             id        = id)
      
    
    df_content <-   rladiesfrm %>%
      group_by(dates_yes) %>%
      summarise(rsvp_yes_count = sum(rsvp_yes_row)) %>%
      ungroup()
    id_name      <- as.character(id_name)
    dates_yes    <- unique(df_content$dates_yes)
    rsvp_yes_count <- df_content$rsvp_yes_count
    # stringsAsFactors = FALSE to avoid warnings
    # about factor level differences when I bind_rows later
    df_date_out       <- data.frame(id_name, dates_yes, rsvp_yes_count, id,
                               stringsAsFactors = FALSE)
    df_datetime_out   <- rladiesfrm %>% select(dates_yes, created, id) %>% 
                               rename(datetime_rsvp = created) 
    invisible(list(df_date_out, df_datetime_out))
  }
  # manually extracted from URLs like
  # https://www.meetup.com/Research-Triangle-Analysts/events/246678392/
  AF19_id <-  '258165094'
  AF18_id  <- '246678392'
  AF17_id  <- '237118943'
  AF16_id  <- '228455037'
  AF15_id  <- '219885748'
  
  AF19_lst <- meetup_yes_RSVPs("AF19", AF19_id)
  AF18_lst <- meetup_yes_RSVPs("AF18", AF18_id)
  AF17_lst <- meetup_yes_RSVPs("AF17", AF17_id)
  AF16_lst <- meetup_yes_RSVPs("AF16", AF16_id)
  AF15_lst <- meetup_yes_RSVPs("AF15", AF15_id)
  
  AF19_frm <- AF19_lst[[1]]
  AF18_frm <- AF18_lst[[1]]
  AF17_frm <- AF17_lst[[1]]
  AF16_frm <- AF16_lst[[1]]
  AF15_frm <- AF15_lst[[1]]
  
  AF19_dt <- AF19_lst[[2]]
  AF18_dt <- AF18_lst[[2]]
  AF17_dt <- AF17_lst[[2]]
  AF16_dt <- AF16_lst[[2]]
  AF15_dt <- AF15_lst[[2]]
  
  allAF_frm_rladies <- bind_rows(AF15_frm, AF16_frm, AF17_frm, AF18_frm, AF19_frm) %>% 
    group_by(id) %>% 
    mutate(creation_date_per_AF_year = min(dates_yes))
  
  allRTA_events_future <- get_events(meetupgrp_name)
  allRTA_events_past   <- get_events(meetupgrp_name, "past")
  allRTA_events        <- rbind(allRTA_events_future, allRTA_events_past)
  AF_events            <- allRTA_events[grepl("FORWARD", toupper(allRTA_events$name)),]
  # Do not include preparation meeting by frequent
  # high Analytics>Forward vote-earner, Mark Hutchinson
  AF_events            <- AF_events %>% dplyr::filter(!grepl("PREPARATION", toupper(name)))
  
  # local_date loads as a date column
  # created was 2019-01-17 for A>F 2019 but first RSVP shows as 2019-01-31 in Meetup
  # so not using
  AF_events_dates_yes  <- AF_events %>% select(id, name, local_date, created) %>%
    # strip off time and only keep date
    mutate(created = str_sub(created, 1, 10)) %>%
    # renaming columns from Meetup API for easier identification
    rename(false_creation_date_per_AF_year = created,
           event_date = local_date) %>%
    group_by(id) %>%
    # seq function below needs from and to arguments to be date variables
    # so converting character to date with lubridate ymd function
    mutate(event_date = ymd(event_date),
           yes_year = lubridate::year(event_date))
  
  allAF_frm_dt_all <- bind_rows(AF19_dt, AF18_dt, AF17_dt, AF16_dt, AF15_dt) %>% 
    left_join(., AF_events_dates_yes) %>%
    mutate(days_to_event = event_date - dates_yes)
  
  # revised until no NAs, added dates with tidyr::complete
  # will only contain non-NA values for group_by columns
  allAF_frm <- left_join(allAF_frm_rladies, AF_events_dates_yes) %>%
    group_by(id_name, event_date, false_creation_date_per_AF_year,
             creation_date_per_AF_year,
             id, name, yes_year) %>%
    mutate(max_date_per_AF_year = case_when(
             yes_year == year(ymd(todaydt)) ~ ymd(todaydt), 
             TRUE ~ max(ymd(dates_yes)))) %>%
    # https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
    # Notice use of unique because seq.Date needs one value for 'from' and 'to' arguments
    tidyr::complete(dates_yes = seq.Date(unique(creation_date_per_AF_year), 
                                         unique(max_date_per_AF_year),
                                            by = "day")) %>%
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
  
  rel_obj <- relative_day_graph_helper()
  today_days_to_event      <- rel_obj[[1]]
  today_relative_yes_count <- rel_obj[[2]]
  max_registrations        <- rel_obj[[3]]
  max_yes_year             <- rel_obj[[4]]
  
  
  allAF_frm            <- allAF_frm %>%
    left_join(., today_relative_yes_count) %>%
    mutate(yes_year_factor = fct_reorder(as.factor(yes_year), relative_today_cumsum, 
                                         .desc = TRUE))

allAF_frm_weekday <- allAF_frm %>%
  ## Notice that using weekdays required extra factor work
  mutate(yes_weekday_factor = wday(ymd(dates_yes), label = TRUE)) %>%
  group_by(yes_year, yes_weekday_factor) %>%
  summarise(rsvp_yes_cumsum_weekday = sum(rsvp_yes_count)) %>%
  ungroup() %>%
  # forcats::fct_rev used to reverse order 
  # (later year will come first)
  # so in stacked bar Chart later year is on far-right
  mutate(yes_year_factor = fct_rev(factor(yes_year))) %>%
  select(-yes_year)
if (save_to_folder) {
  write.csv(allAF_frm, paste0(folder_save, "AnalyticsForward_Registrations.csv"), row.names = FALSE)
  write.csv(allAF_frm_weekday, paste0(folder_save, "AnalyticsForward_Reg_DayofWeek.csv"), row.names = FALSE)
 }
}
##########################################
##   END of non-Github download code    ##     
##########################################

if (github_file) {
  raw_github1 <- RCurl::getURL(github_filename1)
  allAF_frm  <- read.csv(text = raw_github1) %>%
    rename(yes_year = yes_year_factor)
  raw_github2 <- RCurl::getURL(github_filename2)
  allAF_frm_weekday  <- read.csv(text = raw_github2) %>%
    mutate(yes_weekday_factor  = factor(yes_weekday_factor, levels = 
                                          c("Monday", "Tuesday", "Wednesday",
                                            "Thursday", "Friday" ,"Saturday", "Sunday"),
                                        ordered = TRUE),
           yes_year_factor = fct_rev(factor(yes_year_factor)))
  relative_day_graph_helper()
}

allAF_frm_dt_day <- allAF_frm_dt_all %>% 
  mutate(hour_rsvp = hour(datetime_rsvp),
         weekday_rsvp = wday(ymd_hms(datetime_rsvp), label = TRUE)) %>% 
  group_by(weekday_rsvp, hour_rsvp) %>% 
  summarise(dates_yes_cumsum = n())

weeks_until_event <- as.numeric(floor(today_days_to_event / 7))

allAF_frm_dt_day_currentweek <- allAF_frm_dt_all %>% 
  dplyr::filter(days_to_event <= 6 * (weeks_until_event + 1) + min(weeks_until_event, 1) &
                days_to_event >= 6 * (weeks_until_event + 1) + min(weeks_until_event, 1) - 6) %>% 
  mutate(hour_rsvp = hour(datetime_rsvp),
         weekday_rsvp = wday(ymd_hms(datetime_rsvp), label = TRUE)) %>% 
  group_by(weekday_rsvp, hour_rsvp, days_to_event) %>% 
  summarise(dates_yes_cumsum = n())
              
# same as above but exclude the final week, which is when
# registrations dramatically increase
# last day of last captured week is Saturday
allAF_frm_weekday_not_finalweek <- allAF_frm %>%
  dplyr::filter(days_to_event > 6) %>%
  mutate(yes_weekday = weekdays(ymd(dates_yes))) %>%
  group_by(yes_year, yes_weekday) %>%
  summarise(rsvp_yes_cumsum_weekday = sum(rsvp_yes_count)) %>%
  ungroup() %>%
  mutate(yes_weekday_factor  = factor(yes_weekday, levels = 
                                        c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday" ,"Saturday", "Sunday"),
                                      ordered = TRUE),
         # forcats::fct_rev used to reverse order 
         # (later year will come first)
         # so in stacked bar Chart later year is on far-right
         yes_year_factor = fct_rev(factor(yes_year))) %>%
  select(-yes_year, -yes_weekday)

# same as above but only the week prior to Analytics>Forward,
# to help with a marketing plan for that week
# Last day of last captured week is Saturday
allAF_frm_weekday_penult_week <- allAF_frm %>%
  dplyr::filter(days_to_event > 6 & days_to_event < 14) %>%
  mutate(yes_weekday = weekdays(ymd(dates_yes))) %>%
  group_by(yes_year, yes_weekday) %>%
  summarise(rsvp_yes_cumsum_weekday = sum(rsvp_yes_count)) %>%
  ungroup() %>%
  mutate(yes_weekday_factor  = factor(yes_weekday, levels = 
                                        c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday" ,"Saturday", "Sunday"),
                                      ordered = TRUE),
         # forcats::fct_rev used to reverse order 
         # (later year will come first)
         # so in stacked bar Chart later year is on far-right
         yes_year_factor = fct_rev(factor(yes_year))) %>%
  select(-yes_year, -yes_weekday)

allAF_frm_weekday_final_week <- allAF_frm %>%
  dplyr::filter(days_to_event < 7) %>%
  mutate(yes_weekday = weekdays(ymd(dates_yes))) %>%
  group_by(yes_year, yes_weekday) %>%
  summarise(rsvp_yes_cumsum_weekday = sum(rsvp_yes_count)) %>%
  ungroup() %>%
  mutate(yes_weekday_factor  = factor(yes_weekday, levels = 
                                        c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday" ,"Saturday", "Sunday"),
                                      ordered = TRUE),
         # forcats::fct_rev used to reverse order 
         # (later year will come first)
         # so in stacked bar Chart later year is on far-right
         yes_year_factor = fct_rev(factor(yes_year))) %>%
  select(-yes_year, -yes_weekday)

p1 <- 
  ggplot(data = allAF_frm,
         aes(x = dates_yes_otheryear,
             y = dates_yes_cumsum,
             colour = yes_year_factor)) +
  geom_line(size = 2) +
  ggforce::geom_mark_ellipse(aes(filter = yes_year_factor == max_yes_year, 
                                 colour = yes_year_factor, label = yes_year_factor),
                                 expand = unit(0.02, 'mm'),
                                 label.colour = "#fc8d62",
                                 label.fontsize = 16, label.fill = "black",
                                 label.buffer = unit(1, 'mm'),
                                 con.colour = "white", con.size = 0.3,
                                 con.type = "elbow") +
  ggforce::geom_link(aes(x = min(allAF_frm$dates_yes_otheryear),
                         xend = max(allAF_frm$dates_yes_otheryear),
                         y = max_registrations,
                         yend = max_registrations),
                         color = "white",
                         size = 1
                         ) +
  xlab("Date") +
  ylab("YES (will attend) RSVPs") +
  labs(colour = "Year") + 
  ggtitle(label = str_glue("Registrations for Research Triangle Analysts 'Analytics>Forward'\n",
                           "March 9, 2019 at Blue Cross and Blue Shield NC (Durham)"),
          subtitle = str_glue("Keynote by Jordan Meyer: $1M Zillow datascience (Kaggle) winner\n",
                              "Data as of ", as.character(Sys.time()), ": ",
                              as.numeric(today_days_to_event), " days remaining\nChart 1 of 7")) +
  directlabels::geom_dl(aes(label = yes_year), 
                        method = list("last.points", rot = -50, cex = 2)) +
  theme(plot.title = element_text(hjust = 0.5, color = '#EEEEEE',
                                  lineheight = .8, face = "bold",
                                  size = 26),
        plot.subtitle = element_text(hjust = 0.5, color = '#EEEEEE',
                                     size = 20),
        axis.text.x = element_text(face = "bold.italic", color = "#EEEEEE", size = 11),
        axis.text.y = element_text(face = "bold.italic", color = "#EEEEEE", size = 11),
        legend.position = "none",
        text = element_text(family = 'Gill Sans MT', size = 13, color = '#EEEEEE'),
        panel.background = element_rect(fill = '#333333'),
        plot.background = element_rect(fill = '#333333'),
        panel.grid = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  labs(colour = "Year") + 
  # http://colorbrewer2.org/#type=qualitative&scheme=Set2&n=5
  scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")) +
  geom_text(inherit.aes = FALSE, color = "white",
            aes(x = min(allAF_frm$dates_yes_otheryear) + 50, 
                y = max_registrations + 10,
                label = paste0("Record registrations = ", max_registrations)),
                size = 08) +
  NULL

# annotate("text", x = ymd('2020-02-12'), y = 90,
#          label = "2019 registrations recently went flat",
#          size = 5)

p2 <- 
  ggplot(data = allAF_frm,
         aes(x = as.numeric(days_to_event),
             y = dates_yes_cumsum,
             colour = yes_year_factor)) +
  geom_line(size = 2) +
  ggforce::geom_mark_ellipse(aes(filter = yes_year_factor == max_yes_year, 
                                 colour = yes_year_factor, label = yes_year_factor),
                             expand = unit(0.02, 'mm'),
                             label.colour = "#fc8d62",
                             label.fontsize = 16, label.fill = "black",
                             label.buffer = unit(1, 'mm'),
                             con.colour = "white", con.size = 0.3,
                             con.type = "elbow") +
  ggforce::geom_link(aes(x = min(allAF_frm$days_to_event),
                         xend = max(allAF_frm$days_to_event),
                         y = max_registrations,
                         yend = max_registrations),
                     color = "white",
                     size = 1
  ) +
  xlab("Days to Event") +
  ylab("YES (will attend) RSVPs") +
  labs(colour = "Year") + 
  ggtitle(label = str_glue("Registrations for Research Triangle Analysts 'Analytics>Forward'\n",
                           "March 9, 2019 at Blue Cross and Blue Shield NC (Durham)"),
          subtitle = str_glue("Keynote by Jordan Meyer: $1M Zillow datascience (Kaggle) winner\n",
                              "Data as of ", as.character(Sys.time()), ": ",
                              as.numeric(today_days_to_event), " days remaining\nChart 2 of 7")) +
  scale_x_reverse() +
  directlabels::geom_dl(aes(label = yes_year), 
                        method = list("last.points", rot = -50, cex = 2)) +
  theme(plot.title = element_text(hjust = 0.5, color = '#EEEEEE',
                                  lineheight = .8, face = "bold",
                                  size = 26),
        plot.subtitle = element_text(hjust = 0.5, color = '#EEEEEE',
                                     size = 20),
        axis.text.x = element_text(face = "bold.italic", color = "#EEEEEE", size = 11),
        axis.text.y = element_text(face = "bold.italic", color = "#EEEEEE", size = 11),
        legend.position = "none",
        text = element_text(family = 'Gill Sans MT', size = 13, color = '#EEEEEE'),
        panel.background = element_rect(fill = '#333333'),
        plot.background = element_rect(fill = '#333333'),
        panel.grid = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  labs(colour = "Year") + 
  # http://colorbrewer2.org/#type=qualitative&scheme=Set2&n=5
  scale_colour_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")) +
  geom_text(inherit.aes = FALSE, color = "white",
            aes(x = min(allAF_frm$days_to_event) + 10, 
                y = max_registrations + 10,
                label = paste0("Record registrations = ", max_registrations)),
            size = 08) +
  NULL

p3 <-
  ggplot(data = allAF_frm_weekday,
         aes(y = rsvp_yes_cumsum_weekday,
             x = yes_weekday_factor,
             fill = yes_year_factor)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Yes RSVP Count") +
  xlab("Day of Week") +
  labs(fill = "Year") + 
  coord_flip() + 
  scale_fill_viridis_d() +
  ggtitle(label = paste0("Research Triangle Analysts 'Analytics>Forward' as of\n",
                         Sys.Date(), "\nChart 3 of 7"),
          subtitle = str_glue("Zillow Data Science (Kaggle) winner, Jordan Meyer,", 
                              " keynoting March 9, 2019\nAll weeks shown.\n",
                              as.numeric(today_days_to_event), " days remaining")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, lineheight = .8, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(face = "bold.italic", color = "red", size = 16),
        axis.text.y = element_text(face = "bold.italic", color = "red", size = 16))
  
p4 <-
  ggplot(data = allAF_frm_weekday_not_finalweek,
         aes(y = rsvp_yes_cumsum_weekday,
             x = yes_weekday_factor,
             fill = yes_year_factor)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Yes RSVP Count") +
  xlab("Day of Week") +
  labs(fill = "Year") + 
  coord_flip() + 
  scale_fill_viridis_d() +
  ggtitle(label = paste0("Research Triangle Analysts 'Analytics>Forward' as of\n",
                         Sys.Date(), "\nChart 4 of 7"),
          subtitle = str_glue("Zillow Data Science (Kaggle) winner, Jordan Meyer,", 
                              " keynoting March 9, 2019\nEXCLUDES the week of the event.\n",
                              as.numeric(today_days_to_event), " days remaining")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, lineheight = .8, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(face = "bold.italic", color = "red", size = 16),
        axis.text.y = element_text(face = "bold.italic", color = "red", size = 16))

p5 <-
  ggplot(data = allAF_frm_weekday_final_week,
         aes(y = rsvp_yes_cumsum_weekday,
             x = yes_weekday_factor,
             fill = yes_year_factor)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Yes RSVP Count") +
  xlab("Day of Week") +
  labs(fill = "Year") + 
  coord_flip() + 
  scale_fill_viridis_d() +
  ggtitle(label = paste0("Research Triangle Analysts 'Analytics>Forward' as of\n",
                         Sys.Date(), "\nChart 5 of 7"),
          subtitle = str_glue("Zillow Data Science (Kaggle) winner, Jordan Meyer,", 
                              " keynoting March 9, 2019\nONLY the week of the event",
                              " (2 ppl in 2017 registered early Saturday AM).\n",
                              as.numeric(today_days_to_event), " days remaining")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, lineheight = .8, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(face = "bold.italic", color = "red", size = 16),
        axis.text.y = element_text(face = "bold.italic", color = "red", size = 16))

# from https://www.littlemissdata.com/blog/heatmaps
p6 <- ggplot(allAF_frm_dt_day,
             aes(hour_rsvp, weekday_rsvp)) + geom_tile(aes(fill = dates_yes_cumsum), 
                                                       colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "#66c2a5", high = "#a6d854") +  
  guides(fill = guide_legend(title = "Total\nRegistrations")) +
  theme_bw() + theme_minimal() + 
  ggtitle(label = str_glue("Registrations for Research Triangle Analysts 'Analytics>Forward'\n",
                           "March 9, 2019 at Blue Cross and Blue Shield NC (Durham)\n",
                           "Chart 6 of 7 (all years shown)")) +
  labs(x = "Registrations ('Yes' RSVPs) Per Hour", y = "Day of Week") +
  theme(plot.title = element_text(hjust = 0.5, color = '#EEEEEE',
                                  lineheight = .8, face = "bold",
                                  size = 26),
        plot.subtitle = element_text(hjust = 0.5, color = '#EEEEEE',
                                     size = 20),
        axis.text.x = element_text(face = "bold.italic", color = "#EEEEEE", size = 11),
        axis.text.y = element_text(face = "bold.italic", color = "#EEEEEE", size = 11),
        text = element_text(family = 'Gill Sans MT', size = 13, color = '#EEEEEE'),
        panel.background = element_rect(fill = '#333333'),
        plot.background = element_rect(fill = '#333333'),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# from https://www.littlemissdata.com/blog/heatmaps
p7 <- ggplot(allAF_frm_dt_day_currentweek, 
       aes(hour_rsvp, weekday_rsvp)) + geom_tile(aes(fill = dates_yes_cumsum), 
                                                 colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "#66c2a5", high = "#a6d854") +  
  guides(fill = guide_legend(title = "Total\nRegistrations")) +
  theme_bw() + theme_minimal() + 
  ggtitle(label = str_glue("Registrations for Research Triangle Analysts 'Analytics>Forward'\n",
                           "March 9, 2019 at Blue Cross and Blue Shield NC (Durham)\n",
                           "Chart 7 of 7 (all years shown)"),
          subtitle = str_glue("Depicting only when weeks until event = current weeks until event (", 
                              weeks_until_event, ")")) +
  labs(x = "Registrations ('Yes' RSVPs) Per Hour", y = "Day of Week") +
  theme(plot.title = element_text(hjust = 0.5, color = '#EEEEEE',
                                  lineheight = .8, face = "bold",
                                  size = 26),
        plot.subtitle = element_text(hjust = 0.5, color = '#EEEEEE',
                                     size = 20),
        axis.text.x = element_text(face = "bold.italic", color = "#EEEEEE", size = 11),
        axis.text.y = element_text(face = "bold.italic", color = "#EEEEEE", size = 11),
        text = element_text(family = 'Gill Sans MT', size = 13, color = '#EEEEEE'),
        panel.background = element_rect(fill = '#333333'),
        plot.background = element_rect(fill = '#333333'),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

grp_members <- get_members(meetupgrp_name)

# grp_dupes <- grp_members %>% group_by(name) %>% mutate(counter = n()) 
#                          %>% dplyr::filter(counter > 1) %>% arrange(name)
# grp_dupes %>% select(id, name, joined, city)

grp_members2 <- grp_members %>% 
  mutate(joined_date = ymd(str_sub(joined, 1, 10))) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  arrange(joined_date) %>% 
  mutate(member_count = row_number(),
         joined_year = year(joined_date),
         max_year    = max(joined_year),
         max_year_flag = case_when(
           joined_year == max_year ~ TRUE,
           TRUE ~ FALSE
         )) %>% 
  # a repeat found for some reason
  distinct(name, bio, joined_date, .keep_all = TRUE) %>% 
  left_join(., allAF_frm %>% 
                select(dates_yes, id, dates_yes_cumsum) %>% 
                dplyr::filter(dates_yes_cumsum > 0) %>% 
                mutate(AF_active = TRUE,
                       id = as.integer(id)) %>% 
                select(-dates_yes_cumsum), 
            by = "id") 
                        
grp_plot <- hchart(grp_members2, "line", 
                   hcaes(x = joined_date, y = member_count, 
                         name = name, bio = bio, joined = joined)) %>%
  hc_tooltip(pointFormat = ("<b>{point.name}</b><br/>Bio: {point.bio}<br/>Joined Date: {point.joined_date}")) %>% 
  hc_title(text = "'Research Triangle Analysts' (@RTPAnalysts) Meetup membership") %>%
  hc_subtitle(text = str_glue("Preparing for Analytics>Forward 2019: keynote by ",
                              "Jordan Meyer ", "(Kaggle->Zillow)")) %>%
  hc_credits(enabled = TRUE,
             text = "inspired by 2018 keynoter @dataandme [Mara Averick]",
             href = "http://rpubs.com/maraaverick/470388",
             style = list(
               fontSize = "14px")) %>% 
  hc_add_theme(hc_theme_flat(
    plotOptions = list(
      scatter = list(
        marker = list(radius = 4,
                      fillOpacity = 0.3)
      ))))  

grp_plot_latestyear <- hchart(grp_members2 %>% dplyr::filter(max_year_flag),
                              "line", 
                              hcaes(x = joined_date, y = member_count, 
                                    name = name, bio = bio, joined = joined)) %>%
  hc_tooltip(pointFormat = ("<b>{point.name}</b><br/>Bio: {point.bio}<br/>Joined Date: {point.joined_date}")) %>% 
  hc_title(text = "'Research Triangle Analysts' (@RTPAnalysts) Meetup membership\n[ONLY latest year shown]") %>%
  hc_subtitle(text = str_glue("Preparing for Analytics>Forward 2019: keynote by ",
                              "Jordan Meyer ", "(Kaggle->Zillow)")) %>%
  hc_credits(enabled = TRUE,
             text = "inspired by 2018 keynoter @dataandme [Mara Averick]",
             href = "http://rpubs.com/maraaverick/470388",
             style = list(
               fontSize = "14px")) %>% 
  hc_add_theme(hc_theme_flat(
    plotOptions = list(
      scatter = list(
        marker = list(radius = 4,
                      fillOpacity = 0.3)
      ))))         

# Thanks http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# grp_plot_geomcount <-
#   ggplot(grp_members2, aes(joined_date, member_count, shape = factor(AF_active))) +
#   ggtitle("Research Triangle Anlaysts [Meetup group] membership over time",
#           subtitle = "Impact of Active Analytics>Forward [can register] Event? ") +
#   geom_point(aes(colour = factor(AF_active)))

if (save_to_folder) {
  ggsave(p1, file = "af_2019-1.png", dpi = 72, width = 12, height = 5)
  ggsave(p2, file = "af_2019-2.png", dpi = 72, width = 12, height = 5)
  ggsave(p3, file = "af_2019-3.png", dpi = 72, width = 12, height = 5)
  ggsave(p4, file = "af_2019-4.png", dpi = 72, width = 12, height = 5)
  ggsave(p5, file = "af_2019-5.png", dpi = 72, width = 12, height = 5)
  ggsave(p6, file = "af_2019-6.png", dpi = 72, width = 12, height = 5)
  ggsave(p7, file = "af_2019-7.png", dpi = 72, width = 12, height = 5)
#  cowplot::save_plot("af_2019-1.png", p1)
#  Used Paint to change resolution to avoid truncation when animate used
  intro_image <- magick::image_read('Marketplace_Zillow_JordanMeyer_864x360.png', 
                                    strip = TRUE) 
  p1_image    <- image_read("af_2019-1.png") 
  p2_image    <- image_read("af_2019-2.png")
                 # image_annotate(str_glue("Will 2019 set a new record?"),
                 # size = 20, gravity = "northwest", color = "blue") %>% 
  p3_image    <- image_read("af_2019-3.png") 
  p4_image    <- image_read("af_2019-4.png") 
  p5_image    <- image_read("af_2019-5.png") 
  p6_image    <- image_read("af_2019-6.png") 
  p7_image    <- image_read("af_2019-7.png") 
  img <- c(intro_image, p1_image, p2_image, p3_image,
           p4_image, p5_image, p6_image, p7_image)
  animate1 <- image_animate(img, fps = 0.25)
  image_write(animate1, "AF_animate.gif")
  ## ggsave fails on higchart htmlwidget object
  saveWidget(grp_plot, file = paste0("RTAgrp.html"), 
             selfcontained = FALSE)
  saveWidget(grp_plot, file = paste0("RTAgrpLstYr.html"), 
             selfcontained = FALSE)
}
lst_out <- list(allAF_frm, allAF_frm_weekday, allAF_frm_weekday_not_finalweek,
                allAF_frm_dt_day, allAF_frm_dt_day_currentweek,
                allAF_frm_dt_all)
invisible(lst_out)
}