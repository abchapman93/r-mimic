install.packages("RMySQL")
install.packages("getPass")
library(RMySQL)
library(tidyverse)
library(getPass)


conn <- DBI::dbConnect(MySQL(),
               user='uvu10919523',
               password=getPass(),
               dbname='mimic2',
               host='35.233.174.193'
               )

rslt <- dbSendQuery(conn, 'SELECT * FROM d_patients;')
d_patients <- tibble(fetch(rslt, n=-1))

rslt <- dbSendQuery(conn, 'SELECT * FROM demographic_detail;')
demographic_detail <- tibble(fetch(rslt, n=-1))

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
