# https://github.com/abchapman93/info_3700_spring_2021/blob/main/week_6_clinical_data/01-admin_demographic_data.ipynb

install.packages("RMySQL")
install.packages("getPass")
library(RMySQL)
library(tidyverse)
library(getPass)


conn <- DBI::dbConnect(MySQL(),
               user='uvu10919523', # Replace with your username
               password=getPass(), # This will pop up for you to enter your password
               dbname='mimic2',
               host='35.233.174.193'
               )
# First, query and load the two patient table
rslt <- dbSendQuery(conn, 'SELECT * FROM d_patients;')
d_patients <- tibble(fetch(rslt, n=-1))

rslt <- dbSendQuery(conn, 'SELECT * FROM demographic_detail;')
demographic_detail <- tibble(fetch(rslt, n=-1))

# Join them
# Join d_patients and demographic detail
d_patients %>%
    inner_join(demographic_detail, by = "subject_id")

# Compare number of male vs. female patients
d_patients %>%
    count(sex)

d_patients %>%
    ggplot(aes(x = sex)) +
    geom_bar()

# Reorder to be in descending order
d_patients %>%
    mutate(sex = replace_na(sex, "N/A")) %>%
    mutate(sex = sex %>% fct_infreq() ) %>%
    ggplot(aes(sex)) +
        geom_bar()

# Now in ascending order
d_patients %>%
    mutate(sex = replace_na(sex, "N/A")) %>%
    mutate(sex = sex %>% fct_infreq()  %>% fct_rev()) %>%
    ggplot(aes(sex)) +
    geom_bar()

# Analyze age at death
library(lubridate)

# Write a function to add the year of death based on DOB and DOD
add_age_at_death <- function(df) {
    return (df %>%
        mutate(dob = ymd_hms(dob), dod = ymd_hms(dod), 
               age_at_death = interval(dob, dod) / years(1)) # https://stackoverflow.com/questions/32312925/time-difference-in-years-with-lubridate
    )
}

# Five oldest and 5 youngest patients who died in hospital
d_patients %>%
    add_age_at_death %>%
    top_n(5, wt = age_at_death)
d_patients %>%
    add_age_at_death %>%
    top_n(-5, wt = age_at_death)

# Get summary statistics
d_patients %>%
    add_age_at_death %>%
    summarize(mean = mean(age_at_death), 
              sd = sd(age_at_death), 
              min = min(age_at_death), 
              max = max(age_at_death),
              median = median(age_at_death)
              )

# Now plot as a histogram
d_patients %>%
    add_age_at_death() %>%
    ggplot(aes(age_at_death)) +
        geom_histogram()

# Break it up by sex
d_patients %>%
    group_by(sex) %>%
    add_age_at_death %>%
    summarize(mean = mean(age_at_death), 
              sd = sd(age_at_death), 
              min = min(age_at_death), 
              max = max(age_at_death),
              median = median(age_at_death)
    )

d_patients %>%
    drop_na() %>% # Drop n/A values
    add_age_at_death() %>%
    ggplot(aes(age_at_death, fill = sex)) +
    geom_histogram() +
    facet_wrap(~sex)

d_patients %>%
    drop_na() %>%
    add_age_at_death() %>%
    ggplot(aes(y = age_at_death, x = sex)) +
    geom_boxplot()

# Now let's look at ethnicity
# demographic_detail detail is one row per admission, so will need to deduplicate
demographic_detail %>%
    distinct(subject_id, .keep_all = TRUE) %>%
    count(ethnicity_descr)

demographic_detail %>%
    distinct(subject_id, .keep_all = TRUE) %>%
    ggplot(aes(ethnicity_descr)) +
        geom_bar()

# Only keep top 5
demographic_detail %>%
    distinct(subject_id, .keep_all = TRUE) %>%
    group_by(ethnicity_descr) %>%
    summarize(count = n()) %>%
    top_n(5, wt = count) %>%
    mutate(ethnicity_descr = fct_reorder(ethnicity_descr, count, .desc = FALSE)) %>%
    ggplot(aes(y = ethnicity_descr, x = count)) +
        geom_bar(stat = "identity")

# Look at age of death by ethnicity
ethnicity_deaths <- d_patients %>%
    inner_join(demographic_detail %>%
                   distinct(subject_id, .keep_all = TRUE)) %>%
    select(subject_id, ethnicity_descr, dob, dod) %>%
    add_age_at_death()

head(ethnicity_deaths)

ethnicity_deaths %>%
    filter() %>%
    group_by(ethnicity_descr) %>%
    summarize(mean = mean(age_at_death), 
              sd = sd(age_at_death), 
              min = min(age_at_death), 
              max = max(age_at_death),
              median = median(age_at_death)
    )

ethnicity_deaths %>%
    ggplot(aes(age_at_death, fill = ethnicity_descr)) +
        geom_histogram() +
        facet_wrap(~ethnicity_descr)

# Faceted histogram with top 9
# First identify the 9 most common ethnicities
most_common <- ethnicity_deaths %>% 
    group_by(ethnicity_descr) %>% 
    summarize(cnt = n()) %>% 
    top_n(n = 8, wt = cnt) %>% 
    pull(ethnicity_descr)

# Now filter and plot
ethnicity_deaths %>%
    filter(ethnicity_descr %in% most_common) %>%
    mutate(ethnicity_descr = ethnicity_descr %>% fct_infreq()) %>%
    ggplot(aes(age_at_death, fill = ethnicity_descr)) +
        geom_histogram(show.legend = FALSE) +
        facet_wrap(~ethnicity_descr, nrow = 4, ncol = 2, scales = "free_y") +
        labs(title="Distribution of age at death by ethnicity")


