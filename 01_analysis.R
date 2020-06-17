library(tidyverse)
library(janitor)
library(scales)
library(readxl)
library(writexl)

#import the data
salaries <- read_excel("data/MLB2018.xlsx") %>% 
              clean_names()

#let's see what we have
salaries 


#### ARRANGING ####

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


#### GROUPING ####

# What the total payroll paid out by each team?
salaries %>% 
  group_by(team) %>% 
  summarise(total_dollars = sum(salary)) %>% 
  arrange(desc(total_dollars))

# What the total paid in the league for each position?
salaries %>% 
  group_by(pos) %>%  # <--this is all that changed
  summarise(total_dollars = sum(salary)) %>% 
  arrange(desc(total_dollars))

# What about the median paid for each position?
salaries %>% 
  group_by(pos) %>% 
  summarise(average_paid = mean(salary)) %>% 
  arrange(desc(average_paid))

# And if you don't believe in the Designated Hitter?
salaries %>% 
  filter(pos != "DH") %>% 
  group_by(pos) %>% 
  summarise(average_paid = mean(salary)) %>% 
  arrange(desc(average_paid))

# Ah, much better.  :-)

### SAVING AND ITERATING ####

# let's say I want to save the results?
# we'll go back to the top team payrolls
# we can give it a name within R
teampayrolls <- salaries %>% 
  group_by(team) %>% 
  summarise(total_dollars = sum(salary)) %>% 
  arrange(desc(total_dollars))

teampayrolls

# now we can do all sorts of things with our new named slice of data

# we can filter based on it and do additional analysis
teampayrolls %>% 
  filter(total_dollars > 200000000)

# we can export it to a spreadsheet to share with others
write_xlsx(teampayrolls, "output/teampayrolls.xlsx")


# we can even pull that slice of data into charts we want to make
teampayrolls %>% 
  mutate(team = fct_reorder(team, total_dollars)) %>%
  # head(10) %>% #unccmment to limit t
  ggplot(aes(x = team, y = total_dollars)) +
  geom_col(fill = "lightblue") + theme_minimal() + coord_flip() +
  labs(title="Baseball Teams by Payroll (2018)",
       subtitle = "",
       caption = "",
       x ="", y = "") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.title=element_blank()) +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size = 8)) +
  scale_y_continuous(labels=dollar_format())
