# ANALYSIS


# Set up ----

# Getting the renamed and transformed data
library(readxl)
scores_un_renamed <- read_xlsx("Data/scores_un_renamed.xlsx", sheet = 1)

# How are age groups distributed across regions? ----
library(ggplot2)
library(ggridges)
library(tidyverse)
age_groups_by_region <-  scores_un_renamed %>% 
    mutate(AGE_GROUP = factor(AGE_GROUP, levels=c('6-8', '9-11', '12-14', '15-18', '19+'))) %>% 
    group_by(AGE_GROUP, REGION) %>%
    summarise(n=n()) %>%
    ungroup() %>% 
    arrange(REGION, AGE_GROUP) 

# Plot this:

g <- age_groups_by_region %>% 
    ggplot(aes( x = AGE_GROUP, y = n, fill = AGE_GROUP)) +
    geom_col() +
    facet_wrap(~REGION)+
    ## Background
    theme_bw() +
    ## Colors
    scale_fill_viridis_d(option = "viridis", alpha = .8) +
    ## Set labels for x, y and the fill
    labs(title = "Distribution of Age per Region", 
         x = "Age Group", y = "", fill = "AGE_GROUP") 
g


# I may want to investigate the 5 bases !

base_tbl <- left_join(age_groups_by_region, regions)

base_tbl %>% 
    ggplot(aes( x = AGE_GROUP, y = n, fill = AGE_GROUP)) +
    geom_col() + #fill = "blue4"
    facet_wrap(~BASE)+
    ## Background
    theme_bw() +
    ## Colors
    scale_fill_viridis_d(option = "viridis", alpha = .8) +
    ## Set labels for x, y and the fill
    labs(x = "Age Group", y = "Number of children", fill = "AGE_GROUP") 

?facet_wrap

# How many of the children that received certification are already working in their field of studies ?

scores_un_renamed_19up %>% 
    select(received_certification, participated_tvet, is_working_field, is_working) %>% View()

scores_un_renamed_19up %>% 



# Impute data 

scores_un_renamed_19up_imputed <- scores_un_renamed_19up %>% 
    mutate(is_working_field = case_when(
        is_working == 0 ~ 0, 
        TRUE ~ is_working_field
    ))

scores_un_renamed_19up_imputed %>% View()

# The most vulnerable youth, who are not studying, working or on tvet

scores_un_renamed_19up_imputed
scores_un_renamed_19up_imputed %>% 
    group_by(not_s_w_t) %>% 
    summarise(n = n()) %>% 
    ungroup() 

scores_un_renamed_15_18 %>% 
    group_by(not_s_w_t) %>% 
    
    # Excluding the missing ones
    
    summarise(n = n()) %>% 
    
    ungroup() 
?summarise

# Pending items:

# There is a lot that seems to be possible to do in terms of quality of data (e.g. more data imputations, checking consistency of responses)


