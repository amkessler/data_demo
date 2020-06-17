library(tidyverse)
library(janitor)
library(scales)
library(readxl)

#import the data
salaries <- read_excel("data/MLB2018.xlsx") %>% 
              clean_names()

#let's see what we have
salaries 


#which players get paid the most?
salaries %>% 
  arrange(desc(salary))

#which catchers got the most?
salaries %>% 
  filter(pos == "C") %>% 
  arrange(desc(salary))

#if we want to quickly find the same for another position?
#can just leave everything the same but switch the position letter
salaries %>% 
  filter(pos == "1B") %>% 
  arrange(desc(salary))


#Now how about a more complicated question...
#what if we wanted to know who the highest paid player was for EVERY position?
#In Excel this could require a lot of futzing around. But in R, check it out:

salaries %>%
  group_by(pos) %>%
  top_n(n = 1, wt = salary) 

#And if we wanted to know the top 5 for each? Just change one number above and re-run.

