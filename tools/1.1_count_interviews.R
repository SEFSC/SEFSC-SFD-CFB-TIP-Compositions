# TABULATE INTERVIEWS AND INTEVIER DAYS
librarian::shelf(here, tidyverse)

tip_date <- max(
  as.numeric(gsub(".*?([0-9]+).RDS*", "\\1",
             list.files(here("data", "raw"),
                       pattern = "n_com_tip"))))

tip_file <- list.files(here("data", "raw"),
                       pattern = paste0("n_com_tip.+", tip_date))


n_tip <- readRDS(file = here("data", "raw", tip_file))

n_tip <- n_tip %>%
  mutate(ISL = case_when(STATE_LANDED == "PUERTO RICO" ~ "PR", 
                         STATE_LANDED == "VIRGIN ISLANDS" & COUNTY_LANDED %in% c("ST JOHN", "ST THOMAS") ~ "STT", 
                         STATE_LANDED == "VIRGIN ISLANDS" & COUNTY_LANDED == "ST CROIX" ~ "STX", 
                         TRUE ~ "NOT CODED"))

names(n_tip)

n1 <- n_tip %>% 
  filter(ISL != "NOT CODED",
         YEAR >= 1980, 
         YEAR < 2022) %>% 
  group_by(ISL, YEAR) %>% 
  summarize(interviews = n_distinct(ID))

n2 <- n_tip %>% 
  filter(ISL != "NOT CODED",
         YEAR >= 1980, 
         YEAR < 2022) %>% 
  select(ISL, YEAR, INTERVIEW_DATE, AGENT_USERNAME_ID) %>%
  group_by(ISL, YEAR) %>% 
  distinct() %>%
  summarize(interview_days = n())

# Interviews and interview days by island
interview_cnt <- n1 %>% 
  full_join(n2, by = c("ISL", "YEAR")) %>%
  mutate(int_per_day = round(interviews/interview_days,2))

write.csv(interview_cnt, 
          here("data", paste0("com_tip_year_isl_counts_", format(Sys.time(),'%Y%m%d'), ".csv")), 
          row.names = FALSE)
