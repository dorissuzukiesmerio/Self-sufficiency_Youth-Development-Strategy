# ANALYSIS


# Set up ----

# Getting the renamed and transformed data
library(readxl)
scores_un_renamed <- read_xlsx("Data/scores_un_renamed.xlsx", sheet = 1)

# Tables per age
# Ages 19+
scores_un_renamed_19up <- scores_un_renamed %>% 
    filter(AGE_GROUP == "19+") 
# Ages 15-18
scores_un_renamed_15_18 <- scores_un_renamed %>% 
    filter(AGE_GROUP == "15-18") 
# Ages 12-14
scores_un_renamed_12_14 <- scores_un_renamed %>% 
    filter(AGE_GROUP == "12-14") 
# Ages 9-11
scores_un_renamed_9_11 <- scores_un_renamed %>% 
    filter(AGE_GROUP == "9-11") 
# Ages 6-8
scores_un_renamed_6_8 <- scores_un_renamed %>% 
    filter(AGE_GROUP == "6-8") 

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

# How many of the children that participated in tvet and received certification are already working in their field of studies ?

scores_un_renamed_19up %>% 
    select(received_certification, participated_tvet, is_working_field, is_working) %>% View()


# scores_un_renamed_19up %>% 
scores_un_renamed_15_18 %>%      
    # Prepping the data
    select(not_s_w_t) %>%
    mutate(not_s_w_t = case_when(
        not_s_w_t == 1 ~ "Not Studying/Working/TVET",
        not_s_w_t == 0 ~ "Are Studying/Working/TVET",
        TRUE ~ "Data not collected"
    )) %>% 
    group_by(not_s_w_t) %>% 
    count() %>%
    mutate(proportion = n/2247) %>% 
    mutate(proportion_total = paste0(round(proportion * 100), "%")) %>%
    arrange(not_s_w_t) %>% 
    
    # ggplot
    ggplot(aes( x = not_s_w_t, y = n)) +
    geom_col(fill = "lightblue") + #fill = "blue4"
    # facet_wrap(~BASE)+
    ## Background
    theme_bw() +
    ## Set labels for x, y and the fill
    labs(x = "Vulnerable youth : 15-18 who are not studying, working or in TVET", y = "Number of youth", fill = "AGE_GROUP") +
    ggrepel::geom_text_repel(aes(label = proportion_total))

# scores_un_renamed_19up %>% 

scores_un_renamed_15_18 %>%    
    # Prepping the data
    select(not_s_w_t, REGION) %>%
    mutate(not_s_w_t = case_when(
        not_s_w_t == 1 ~ "No",
        not_s_w_t == 0 ~ "Yes",
        TRUE ~ "NA"
    )) %>% 

    group_by(not_s_w_t, REGION) %>% 
    count() %>% 
    mutate(proportion = n/997) %>% 
    mutate(proportion_total = paste0(round(proportion * 100), "%")) %>%
    
    arrange(not_s_w_t, REGION) %>% 
    
    # ggplot
    ggplot(aes( x = not_s_w_t, y = n)) +
    geom_col(fill = "lightblue") + #fill = "blue4"
    facet_wrap(~REGION)+
    ## Background
    theme_bw() +
    ## Set labels for x, y and the fill
    labs(x = "Vulnerable youth : 19+ who are not studying, working or in TVET", y = "Number of youth", fill = "AGE_GROUP") #+
    # ggrepel::geom_text_repel(aes(label = proportion_total))


regions <- read_xlsx("Data/panel_case_data_DS_II_2023.xlsx", sheet = "REGIONS & BASES")

scores_un_renamed_19up %>%
# scores_un_renamed_15_18 %>%    
    # Prepping the data
    left_join(regions) %>% 
    select(not_s_w_t, BASE) %>%
    mutate(not_s_w_t = case_when(
        not_s_w_t == 1 ~ "No",
        not_s_w_t == 0 ~ "Yes",
        TRUE ~ "NA"
    )) %>% 
    
    group_by(not_s_w_t, BASE) %>% 
    count() %>% 
    mutate(proportion = n/997) %>% 
    mutate(proportion_total = paste0(round(proportion * 100), "%")) %>%
    
    arrange(not_s_w_t, BASE) %>% 
    
    # ggplot
    ggplot(aes( x = not_s_w_t, y = n)) +
    geom_col(fill = "lightblue") + #fill = "blue4"
    facet_wrap(~BASE)+
    ## Background
    theme_bw() +
    ## Set labels for x, y and the fill
    labs(x = "Vulnerable youth : 19+ who are not studying, working or in TVET", y = "Number of youth", fill = "AGE_GROUP") #+
# ggrepel::geom_text_repel(aes(label = proportion_total))


# GENDER ----

# scores_un_renamed_19up %>% # Comment this line out when dealing with the other age group
scores_un_renamed_15_18 %>% # Comment this line out when dealing with the other age group
    
    ## Prepping the data:
    select(GENDER) %>% 
    group_by(GENDER) %>% 
    count() %>%
    mutate(proportion = n/2247) %>%  # Comment this line out when dealing with the other age group
    # mutate(proportion = n/977) %>% # Comment this line out when dealing with the other age group
    mutate(proportion_total = paste0(round(proportion * 100), "%")) %>%
    arrange(GENDER) %>% 
    
    ## Data Visualization:
    ggplot(aes( x = GENDER, y = n)) +
    geom_col(fill = "lightblue") + #fill = "blue4"
    # facet_wrap(~BASE)+
    ## Background
    theme_bw() +
    ## Set labels for x, y 
    labs(x = "15-18 BY GENDER", y = "Number of youth") +
    ggrepel::geom_text_repel(aes(label = proportion_total))

## Is gender breakout even per base? ----
# scores_un_renamed_19up %>%
scores_un_renamed_15_18 %>%
    # scores_un_renamed_15_18 %>%    
    # Prepping the data
    left_join(regions) %>% 
    select(GENDER, BASE) %>%
    group_by(GENDER, BASE) %>% 
    count() %>% 
    mutate(proportion = n/997) %>% 
    mutate(proportion_total = paste0(round(proportion * 100), "%")) %>%
    
    arrange(GENDER, BASE) %>% 
    
    # ggplot
    ggplot(aes( x = GENDER, y = n)) +
    geom_col(fill = "lightblue") + #fill = "blue4"
    facet_wrap(~BASE)+
    ## Background
    theme_bw() +
    ## Set labels for x, y and the fill
    labs(x = "15-18 per gender", y = "Number of youth") #+
# ggrepel::geom_text_repel(aes(label = proportion_total))
# Seems pretty even. Could test statistical significance, but...

## A few Outcomes per gender ----

# Obs: I could write a forloop or lapply to be more efficient

scores_un_renamed_15_18$is_working[is.na(scores_un_renamed_15_18$is_working)] <- 0
# scores_un_renamed_19up %>%
scores_un_renamed_15_18 %>%
    # Prepping the data
    select(GENDER, is_working) %>%
    group_by(GENDER, is_working) %>% 
    count() %>% 
    mutate(proportion = n/997) %>% 
    mutate(proportion_total = paste0(round(proportion * 100), "%")) %>%
    
    arrange(GENDER, is_working) %>% 
    
    # ggplot
    ggplot(aes( x = is_working, y = n)) +
    geom_col(fill = "lightblue") + #fill = "blue4"
    facet_wrap(~GENDER)+
    ## Background
    theme_bw() +
    ## Set labels for x, y and the fill
    # labs(x = "19+ who are working", y = "Number of youth") #+
    labs(x = "15-18 who are working", y = "Number of youth")


scores_un_renamed_19up %>%
    # scores_un_renamed_15_18 %>%
    # Prepping the data
    select(GENDER, not_s_w_t) %>%
    group_by(GENDER, not_s_w_t) %>% 
    count() %>% 
    mutate(proportion = n/997) %>% 
    mutate(proportion_total = paste0(round(proportion * 100), "%")) %>%
    
    arrange(GENDER, not_s_w_t) %>% 
    
    # ggplot
    ggplot(aes( x = not_s_w_t, y = n)) +
    geom_col(fill = "lightblue") + #fill = "blue4"
    facet_wrap(~GENDER)+
    ## Background
    theme_bw() +
    ## Set labels for x, y and the fill
    labs(x = "19+ who are not studying or working", y = "Number of youth") #+
# labs(x = "15-18 who are working", y = "Number of youth")

# ggrepel::geom_text_repel(aes(label = proportion_total))



# HOW ARE THE YOUTH IN WAKANDA - EMPLOYMENT ----

scores_un_renamed_19up %>% 
    rbind(scores_un_renamed_15_18) %>% 
    
    # Prepping the data
    select(is_working) %>%
    mutate(is_working = case_when(
        is_working == 1 ~ "Yes",
        is_working == 0 ~ "No",
        TRUE ~ "NA"
    )) %>% 
    
    group_by(is_working) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(proportion = n/(997+2247)) %>% 
    mutate(proportion_total = paste0(round(proportion * 100), "%")) %>%
    
    arrange(is_working) %>% 
    
    # ggplot
    ggplot(aes( x = is_working, y = n)) +
    geom_col(fill = "lightblue") + #fill = "blue4"
    # facet_wrap(~REGION)+
    ## Background
    theme_bw() +
    ## Set labels for x, y and the fill
    labs(x = "Youth who are working", y = "Number of youth") +
    ggrepel::geom_text_repel(aes(label = proportion_total))



# Impute data ----

# Through consistency (things that make sense):

# Logic says that if the person (19+) is "not working", that person is not "working in the field" either
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



# Pending items:

# There is a lot that seems to be possible to do in terms of quality of data (e.g. more data imputations, checking consistency of responses)


