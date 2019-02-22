library(tidyverse)
dis <- read.csv("Disability.csv") %>% 
  
group_by(ethnicity)%>% 
  summarise_all(funs(mean))

print(dis)