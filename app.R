library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(lubridate)
library(readxl)

# Packages for Chart Creation
library(ggplot2)
library(scales)
library(plotly)

# Packages for Maps
library(sf)
library(leaflet)

source("psrc_palette.R")

wgs84 <- 4326
spn <- 2285 

psrc.zipcodes <- st_read(here('data', 'psrc_zipcodes.shp')) %>%
    st_transform(wgs84) %>%
    select(zipcode)

psrc.zipcodes.centroids <- psrc.zipcodes %>%
    st_transform(spn) %>%
    st_centroid() %>%
    st_transform(wgs84)

# Lists -------------------------------------------------------------------
county.lookup <- c("No Response" = 0,
                   "King County" = 1,
                   "Kitsap County" = 2,
                   "Pierce County" = 3,
                   "Snohomish County" = 4,
                   "Other" = 5)

county.names <- enframe(county.lookup)

age.lookup <- c("No Response" = 0,
                "Under 18" = 1,
                "18 to 64" = 2,
                "18 to 64" = 3,
                "18 to 64" = 4,
                "18 to 64" = 5,
                "18 to 64" = 6,
                "65 to 84" = 7,
                "65 to 84" = 8,
                "over 85" = 9)

age.names <- enframe(age.lookup)

gender.lookup <- c("No Response" = 0,
                   "Female" = 1,
                   "Male" = 2,
                   "Non-binary" = 3,
                   "No listed here" = 4)

gender.names <- enframe(gender.lookup)

income.lookup <- c("No Response" = 0,
                "Under $25k" = 1,
                "$25K to $50k" = 2,
                "$50k to $75k" = 3,
                "$75k to $100k" = 4,
                "$100k to $200k" = 5,
                "over $200k" = 6)

income.names <- enframe(income.lookup)

hhsize.lookup <- c("No Response" = 0,
                   "1 person" = 1,
                   "2 people" = 2,
                   "3 people" = 3,
                   "4 people" = 4,
                   "5 people" = 5,
                   "6 or more" = 6)

hhsize.names <- enframe(hhsize.lookup)

children.lookup <- c("No Response" = 0,
                     "No" = 1,
                     "Yes" = 2)

children.names <- enframe(children.lookup)

elderly.lookup <- c("No Response" = 0,
                    "No" = 1,
                    "Yes" = 2)

elderly.names <- enframe(elderly.lookup)

disability.lookup <- c("No Response" = 0,
                       "No" = 1,
                       "Yes" = 2)

disability.names <- enframe(disability.lookup)

veteran.lookup <- c("No Response" = 0,
                    "No" = 1,
                    "Yes" = 2)

veteran.names <- enframe(veteran.lookup)

language.lookup <- c("English" = 0,
                   "Chinese" = 1,
                   "Korean" = 2,
                   "Russian" = 3,
                   "Spanish" = 4,
                   "Tagalog" = 5,
                   "Vietnamese" = 6,
                   "Other" = 7)

language.names <- enframe(language.lookup)

q1.lookup <- c( "Working from home" = 1,
                "Working outside the home" = 2,
                "Combination" = 3,
                "Student" = 4,
                "Unemployed" = 5,
                "Retired" = 6,
                "Unable to work" = 7,
                "Not working" = 8,
                "Other" = 9)


q1.names <- enframe(q1.lookup)

q3.lookup <- c( "Not at all supportive" = 1,
                "Somewhat unsupportive" = 2,
                "Neutral" = 3,
                "Somewhat supportive" = 4,
                "Very supportive" = 5)

q3.names <- enframe(q3.lookup)

q12.lookup <- c( "Very poor" = 1,
                "Poor" = 2,
                "Average" = 3,
                "Good" = 4,
                "Excellent" = 5,
                "Don't know" = 6)

q12.names <- enframe(q12.lookup)

q16.lookup <- c("Better information" =  " Better information (how to use transit, trip planning, real-time arrival information, etc.)",
                "Easier to access" = " Easier to access (closer to my home or places I go, more parking at transit centers or park & rides, accommodations for people with disabilities, etc.)",
                "Easier to travel with people or belongings" = " Easier to travel with people or belongings (children, bikes, groceries, etc.)",
                "Extended service" = " Extended service (longer hours throughout the week, more weekend service, etc.)",
                "Not planning to use" = " I do not plan to use transit after COVID-19",
                "Improved safety features" = " Improved safety features (on board, at stops or stations, trip to/from stops/stations, etc.)",
                "More affordable" = " More affordable",
                "More comfortable" = " More comfortable (on board, at stops or stations, trip to/from stops/stations, etc.)",
                "On-time" = " On-time arrivals and departures",
                "Other" = " Other (please specify)",
                "Shorter trip time" = " Shorter trip time (more direct service, shorter wait times, less time on board, etc.)")

q16 <- "Please select the top three things that would motivate you to use public transit more often when COVID-19 is no longer a serious threat to public health."
q16.names <- enframe(q16.lookup)
q16.clean <- "Please select the top three \\(3 things that would motivate you to use public transit more often when COVID-19 is no longer a serious threat to public health.\\) "
q16.order <- c("Other","Not planning to use","Easier to travel with people or belongings","Better information","More affordable",
               "Extended service","On-time","Easier to access","More comfortable","Improved safety features","Shorter trip time")

q17.lookup <- c("Reliable neighborhood transit" =  " Reliable, well-connected transit service for local neighborhoods",
                "Complete Bike/Ped facilities" = " Complete network for bicyclists and pedestrians",
                "Reliable roads" = " Reliable, well-maintained roads and highways",
                "High speed rail" = " High speed rail connecting to places in and outside the region",
                "More Fast Ferries" = " More direct and faster ferry options",
                "Fast/secure deliveries" = " Deliveries are fast and secure",
                "Expanded air travel" = " Expanded air travel connecting to places in and outside the region",
                "Expanded transit to major destinations" = " Expanded, faster transit service between major destinations across the region",
                "Other" = " Other (please specify)",
                "Widespread charging stations" = " Widespread electric vehicle charging stations")

q17 <- "When you think about a complete and reliable transportation system in the future, what are your priorities?"
q17.names <- enframe(q17.lookup)
q17.clean <- "The central Puget Sound region continues to grow and create more demands on the transportation system. When you think about a complete and reliable transportation system in the future, what are your priorities\\? Please select your top three \\(3 priorities.\\) "
q17.order <- c("Other","Fast/secure deliveries", "Expanded air travel", "Widespread charging stations", "Reliable roads", "Complete Bike/Ped facilities",
               "More Fast Ferries", "High speed rail", "Expanded transit to major destinations", "Reliable neighborhood transit")

q36 <- "In addition to difficulties from COVID-19, do you or does anyone in your household experience any of the following challenges to using specialized transportation services?"
q36.lookup <- c("Lack of information" =  "Lack of information about available services",
                "Language Barrier" = "Lack of spoken and/or written language assistance",
                "Span of Service" = "Lack of services at times when they are needed",
                "Cost" = "High cost of services",
                "Travel Time" = "Longer travel times compared to other transportation options (driving in a personal vehicle, transit, etc.)",
                "Wait Times" = "Long wait times when transferring between services",
                "Difficult to Transfer" = "Difficulty transferring between services due to eligibility requirements",
                "Difficult getting to things" = "Difficulty getting to medical appointments, pharmacies, and grocery stores",
                "No Challenges" = "No challenges to using specialized transportation services",
                "Other" = "Other (please specify)")

q36.names <- enframe(q36.lookup)
q36.clean <- " In addition to difficulties from COVID-19, do you or does anyone in your household experience any of the following challenges to using specialized transportation services \\(check all that may apply\\?\\) "
q36.order <- c("Other","No Challenges","Difficult getting to things","Difficult to Transfer","Wait Times",
               "Travel Time","Cost","Span of Service","Language Barrier","Lack of information")
low.income <- c("Under $25k","$25K to $50k")
moderate.income <- c("$50k to $75k", "$75k to $100k")
high.income <- c("$100k to $200k","over $200k")


# Functions ---------------------------------------------------------------
create.bar.charts <- function (c.data) {

    g <-  ggplotly(ggplot(data = c.data,
                      aes(x = name, 
                          y = total, 
                          fill = name,
                          text = paste0(prettyNum(round(total, 0), big.mark = ","), " of ", prettyNum(round(total.responses, 0), big.mark = ","), " responses","<br> (",
                                        round((total/total.responses)*100,0),"% of total reponses)"))) +
                   geom_col(
                       color = "black",
                       alpha = 1.0,
                       position = "dodge") +
                      coord_flip() +
                   scale_fill_manual(values= psrc.colors) +
                   labs(x = NULL, y = NULL) +
                   theme(plot.title = element_text(size = 10, face = 'bold'),
                         axis.text.x = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.line = element_blank(),
                         panel.background = element_blank(),
                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         legend.position = "none",
                         legend.title = element_blank()),
               tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25))

    return(g)
}

summarize.question.by.race <- function(c.data=rtp.data, q, d, n) {
    
    # Filter data 
    temp <- c.data %>% filter(question_number == q & response_date <= d & response > 0) %>% select(race, response) %>% mutate(mutate(across(response, as.numeric)))
    
    # Add names and consolidate race to white & bipoc
    temp <- left_join(temp, n, by=c("response"="value")) %>% 
        filter(race != "No Response") %>%
        mutate(race = case_when(
            race == "White" ~ "White",
            race != "White" ~ "BIPOC")) %>%
        mutate(response=1)
    
    total.non.white <- temp %>% filter(race=="BIPOC") %>% select(response) %>% pull() %>% sum()   
    total.white <- temp %>% filter(race=="White") %>% select(response) %>% pull() %>% sum()   
    
    df <- temp %>% select(race, name,response) %>%
        group_by(race,name) %>%
        summarize(total = sum(response)) %>%
        rename(responses = total) %>%
        mutate(total = total.white) %>%
        mutate(total = case_when(
            race == "White" ~ total.white,
            race != "White" ~ total.non.white)) %>%
        mutate(share=responses/total) %>%
        select(-total) %>%
        rename(total=responses)
    
    g <-  ggplotly(ggplot(data = df,
                          aes(x = name, 
                              y = share, 
                              fill = race,
                              text = paste0(prettyNum(round(share*100, 0), big.mark = ","), "%"))) +
                       geom_col(
                           color = "black",
                           alpha = 1.0,
                           position = "dodge") +
                       scale_x_discrete(labels = function(x) str_wrap(x, width = 48)) +
                       coord_flip() +
                       labs(x = NULL, y = NULL) +
                       scale_fill_manual(values= psrc.colors) +
                       theme(plot.title = element_text(size = 10, face = 'bold'),
                             axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.line = element_blank(),
                             panel.background = element_blank(),
                             panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                             panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                             panel.grid.major.x = element_blank(),
                             panel.grid.minor.x = element_blank(),
                             legend.position = "bottom",
                             legend.title = element_blank()),
                   tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25))
    
    return(g)
    
}

summarize.question.by.income <- function(c.data=rtp.data, q, d, n) {
    
    # Filter data 
    temp <- c.data %>% filter(question_number == q & response_date <= d & response > 0) %>% select(income, response) %>% 
        mutate(mutate(across(response, as.numeric)))
        
    # Add names and consolidate race to white & bipoc
    temp <- left_join(temp, n, by=c("response"="value")) %>% 
        filter(income != "No Response") %>%
        mutate(income = case_when(
            income %in% low.income ~ "Low",
            income %in% moderate.income ~ "Moderate",
            income %in% high.income ~ "Upper")) %>%
        mutate(response=1)
    
    total.lowinc <- temp %>% filter(income=="Low") %>% select(response) %>% pull() %>% sum()   
    total.midinc <- temp %>% filter(income=="Moderate") %>% select(response) %>% pull() %>% sum()
    total.uppinc <- temp %>% filter(income=="Upper") %>% select(response) %>% pull() %>% sum()   
    
    df <- temp %>% select(income, name, response) %>%
        group_by(income,name) %>%
        summarize(total = sum(response)) %>%
        rename(responses = total) %>%
        mutate(total = case_when(
            income == "Low" ~ total.lowinc,
            income == "Moderate" ~ total.midinc,
            income == "Upper" ~ total.uppinc)) %>%
        mutate(share=responses/total) %>%
        select(-total) %>%
        rename(total=responses)
    
    g <-  ggplotly(ggplot(data = df,
                          aes(x = name, 
                              y = share, 
                              fill = income,
                              text = paste0(prettyNum(round(share*100, 0), big.mark = ","), "%"))) +
                       geom_col(
                           color = "black",
                           alpha = 1.0,
                           position = "dodge") +
                       scale_x_discrete(labels = function(x) str_wrap(x, width = 48)) +
                       coord_flip() +
                       labs(x = NULL, y = NULL) +
                       scale_fill_manual(values= psrc.colors) +
                       theme(plot.title = element_text(size = 10, face = 'bold'),
                             axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.line = element_blank(),
                             panel.background = element_blank(),
                             panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                             panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                             panel.grid.major.x = element_blank(),
                             panel.grid.minor.x = element_blank(),
                             legend.position = "bottom",
                             legend.title = element_blank()),
                   tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0, y = 0))
    
    return(g)
    
}

summarize.multi.question.by.race <- function(c.data=rtp.data, q, d, n) {
    
    temp <- c.data %>% 
        filter(question_number == q & response_date <= d & response > 0) %>% 
        separate(question, c("question", "sub-question"), "\\?") %>%
        select(race, `sub-question`, response) %>% 
        mutate(mutate(across(response, as.numeric))) %>%
        mutate(race = case_when(
            race == "White" ~ "White",
            race != "White" ~ "BIPOC")) %>%
        mutate(count=1) %>%
        group_by(race,`sub-question`) %>%
        summarize(total_response = sum(response), total_count = sum(count)) %>%
        mutate(average = total_response / total_count)
    
    g <-  ggplotly(ggplot(data = temp,
                          aes(x = `sub-question`, 
                              y = average, 
                              fill = race,
                              text = paste0(prettyNum(round(average, 2), big.mark = ",")))) +
                       geom_col(
                           color = "black",
                           alpha = 1.0,
                           position=position_dodge(0.5)) +
                       scale_x_discrete(labels = function(x) str_wrap(x, width = 48)) +
                       coord_flip() +
                       ylim(0,5) +
                       labs(x = NULL, y = NULL) +
                       scale_fill_manual(values= psrc.colors) +
                       theme(plot.title = element_text(size = 10, face = 'bold'),
                             axis.line = element_blank(),
                             panel.background = element_blank(),
                             panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                             panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                             panel.grid.major.x = element_blank(),
                             panel.grid.minor.x = element_blank(),
                             legend.position = "bottom",
                             legend.title = element_blank()),
                   tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25))
    
    return(g)
}

summarize.preference.question.by.race <- function(c.data=rtp.data, q, d, t, n, o) {

    temp <- c.data %>% 
        filter(question_number == q & response_date <= d) %>% 
        mutate(question = gsub(t, "", question)) %>%
        filter(response == 1) %>%
        filter(race != "No Response") %>%
        mutate(race = case_when(
            race == "White" ~ "White",
            race != "White" ~ "BIPOC")) %>%
        mutate(response=1) %>%
        select(race, question, response)

    temp <- left_join(temp, n, by=c("question"="value"))

    total.non.white <- temp %>% filter(race=="BIPOC") %>% select(response) %>% pull() %>% sum()   
    total.white <- temp %>% filter(race=="White") %>% select(response) %>% pull() %>% sum()   

    df <- temp %>% select(race, name, response) %>%
        group_by(race, name) %>%
        summarize(total = sum(response)) %>%
        rename(responses = total) %>%
        mutate(total = total.white) %>%
        mutate(total = case_when(
            race == "White" ~ total.white,
            race != "White" ~ total.non.white)) %>%
        mutate(share=responses/total) %>%
        select(-total) %>%
        rename(total=responses)
    
    df$name <- factor(df$name, levels=o)

    g <-  ggplotly(ggplot(data = df,
                      aes(x = name, 
                          y = share, 
                          fill = race,
                          text = paste0(prettyNum(round(share*100, 0), big.mark = ","), "%"))) +
                   geom_col(
                       color = "black",
                       alpha = 1.0,
                       position = "dodge") +
                   scale_x_discrete(labels = function(x) str_wrap(x, width = 48)) +
                   coord_flip() +
                   labs(x = NULL, y = NULL) +
                   scale_fill_manual(values= psrc.colors) +
                   theme(plot.title = element_text(size = 10, face = 'bold'),
                         axis.text.x = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.line = element_blank(),
                         panel.background = element_blank(),
                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         legend.position = "bottom",
                         legend.title = element_blank()),
               tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0, y = 0))

    return(g)
    
}

summarize.preference.question.by.income <- function(c.data=rtp.data, q, d, t, n, o) {
    
    temp <- c.data %>% 
        filter(question_number == q & response_date <= d) %>% 
        mutate(question = gsub(t, "", question)) %>%
        filter(response == 1) %>%
        filter(income != "No Response") %>%
        mutate(income = case_when(
            income %in% low.income ~ "Low",
            income %in% moderate.income ~ "Moderate",
            income %in% high.income ~ "Upper")) %>%
        mutate(response=1) %>%
        select(income, question, response)
    
    temp <- left_join(temp, n, by=c("question"="value"))
    
    total.lowinc <- temp %>% filter(income=="Low") %>% select(response) %>% pull() %>% sum()   
    total.midinc <- temp %>% filter(income=="Moderate") %>% select(response) %>% pull() %>% sum()
    total.uppinc <- temp %>% filter(income=="Upper") %>% select(response) %>% pull() %>% sum()   

    df <- temp %>% select(income, name, response) %>%
        group_by(income,name) %>%
        summarize(total = sum(response)) %>%
        rename(responses = total) %>%
        mutate(total = case_when(
            income == "Low" ~ total.lowinc,
            income == "Moderate" ~ total.midinc,
            income == "Upper" ~ total.uppinc)) %>%
        mutate(share=responses/total) %>%
        select(-total) %>%
        rename(total=responses)
    
    df$name <- factor(df$name, levels=o)
    
    g <-  ggplotly(ggplot(data = df,
                          aes(x = name, 
                              y = share, 
                              fill = income,
                              text = paste0(prettyNum(round(share*100, 0), big.mark = ","), "%"))) +
                       geom_col(
                           color = "black",
                           alpha = 1.0,
                           position = "dodge") +
                       scale_x_discrete(labels = function(x) str_wrap(x, width = 48)) +
                       coord_flip() +
                       labs(x = NULL, y = NULL) +
                       scale_fill_manual(values= psrc.colors) +
                       theme(plot.title = element_text(size = 10, face = 'bold'),
                             axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.line = element_blank(),
                             panel.background = element_blank(),
                             panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                             panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                             panel.grid.major.x = element_blank(),
                             panel.grid.minor.x = element_blank(),
                             legend.position = "bottom",
                             legend.title = element_blank()),
                   tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0, y = 0))
    
    return(g)
    
}

create.zipcode.map <- function(c.data, d) {
    
    # Get response by zipcode
    response.by.zip <- c.data %>% 
        filter(question_number == "Q1" & response_date <= d) %>% 
        select(zipcode) %>% 
        drop_na(zipcode) %>%
        mutate(response=1)
    
    zipcode.point.response <- left_join(psrc.zipcodes.centroids, response.by.zip, by=c("zipcode")) %>%
        mutate(response = replace_na(response, 0))
    
    icons <- awesomeIcons(
        icon = 'user',
        iconColor = 'black',
        library = 'fa'
    )
    
    m <- leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addLayersControl(baseGroups = c("Base Map"),
                         overlayGroups = c("Zipcode","Responses"),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        
        addEasyButton(easyButton(
            icon="fa-globe", title="Region",
            onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
        
        addPolygons(data=psrc.zipcodes,
                    fillOpacity = 0.0,
                    opacity = 1.0,
                    weight = 1,
                    color = "#91268F",
                    dashArray = "1",
                    group = "Zipcode") %>%
        
        addAwesomeMarkers(data=zipcode.point.response, 
                          icon = icons, 
                          clusterOptions = markerClusterOptions(),
                          group="Responses")
    
    return(m)
    
}

# Process and Clean Survey Data -------------------------------------------

rtp.data <- read_excel(here('data', '1. PSRC Future of Transportation Survey .xlsx'), sheet="codified")

# Details for each respondent
county <- rtp.data %>% select(`Response ID`,county=contains("Q18")) 
county <- left_join(county,county.names,by=c("county"="value")) %>% select(-county) %>% rename(county=name)
rtp.data <- left_join(rtp.data,county,by=c("Response ID"))

zipcode <- rtp.data %>% select(`Response ID`,zipcode=contains("Q19"))
rtp.data <- left_join(rtp.data,zipcode,by=c("Response ID"))

age <- rtp.data %>% select(`Response ID`,age=contains("Q20")) 
age <- left_join(age,age.names,by=c("age"="value")) %>% select(-age) %>% rename(age=name)
rtp.data <- left_join(rtp.data,age,by=c("Response ID"))

gender <- rtp.data %>% select(`Response ID`,gender=contains("Q21")) 
gender <- left_join(gender,gender.names,by=c("gender"="value")) %>% select(-gender) %>% rename(gender=name)
rtp.data <- left_join(rtp.data,gender,by=c("Response ID"))

race <- rtp.data %>% select(`Response ID`, contains("Q22")) %>% mutate(`Two or more` = rowSums(.[2:9])) %>%
    mutate(race="") %>%
    mutate(race = case_when(
        .[[2]] == 1 & .[[10]] <2 ~ "American Indian or Alaska Native",
        .[[3]] == 1 & .[[10]] <2 ~ "Asian or Asian American",
        .[[4]] == 1 & .[[10]] <2 ~ "Black or African American",
        .[[5]] == 1 & .[[10]] <2 ~ "Hispanic or Latinx",
        .[[6]] == 1 & .[[10]] <2 ~ "Middle Eastern or North African",
        .[[7]] == 1 & .[[10]] <2 ~ "Native Hawaiian or Pacific Islander",
        .[[8]] == 1 & .[[10]] <2 ~ "White",
        .[[9]] == 1 & .[[10]] <2 ~ "Other",
        .[[10]] >= 2 ~ "Two or more races",
        .[[10]] == 0 ~ "No Response")) %>%
    select(`Response ID`,race)
rtp.data <- left_join(rtp.data,race,by=c("Response ID"))

hhsize <- rtp.data %>% select(`Response ID`,hhsize=contains("Q23")) 
hhsize <- left_join(hhsize,hhsize.names,by=c("hhsize"="value")) %>% select(-hhsize) %>% rename(hhsize=name)
rtp.data <- left_join(rtp.data,hhsize,by=c("Response ID"))

income <- rtp.data %>% select(`Response ID`,income=contains("Q24")) 
income <- left_join(income,income.names,by=c("income"="value")) %>% select(-income) %>% rename(income=name)
rtp.data <- left_join(rtp.data,income,by=c("Response ID"))

children <- rtp.data %>% select(`Response ID`,children=contains("Q25")) 
children <- left_join(children,children.names,by=c("children"="value")) %>% select(-children) %>% rename(children=name)
rtp.data <- left_join(rtp.data,children,by=c("Response ID"))

elderly <- rtp.data %>% select(`Response ID`,elderly=contains("Q26")) 
elderly <- left_join(elderly,elderly.names,by=c("elderly"="value")) %>% select(-elderly) %>% rename(elderly=name)
rtp.data <- left_join(rtp.data,elderly,by=c("Response ID"))

disability <- rtp.data %>% select(`Response ID`,disability=contains("Q27")) 
disability <- left_join(disability,disability.names,by=c("disability"="value")) %>% select(-disability) %>% rename(disability=name)
rtp.data <- left_join(rtp.data,disability,by=c("Response ID"))

veteran <- rtp.data %>% select(`Response ID`,veteran=contains("Q28")) 
veteran <- left_join(veteran,veteran.names,by=c("veteran"="value")) %>% select(-veteran) %>% rename(veteran=name)
rtp.data <- left_join(rtp.data,veteran,by=c("Response ID"))

language <- rtp.data %>% select(`Response ID`,language=contains("Q30"))
language <- left_join(language,language.names,by=c("language"="value")) %>% select(-language) %>% rename(language=name)
rtp.data <- left_join(rtp.data,language,by=c("Response ID"))

# Remove personal information and extra columns
rtp.data <- rtp.data %>%
    select(-c(2:39)) %>%
    select(!(cols=contains("Q18"))) %>% select(!(cols=contains("Q19"))) %>% select(!(cols=contains("Q20"))) %>%
    select(!(cols=contains("Q21"))) %>% select(!(cols=contains("Q22"))) %>% select(!(cols=contains("Q23"))) %>%
    select(!(cols=contains("Q24"))) %>% select(!(cols=contains("Q25"))) %>% select(!(cols=contains("Q26"))) %>%  
    select(!(cols=contains("Q27"))) %>% select(!(cols=contains("Q28"))) %>% select(!(cols=contains("Q29"))) %>%  
    select(!(cols=contains("Q30"))) %>% select(!(cols=contains("Q37"))) %>% select(!(cols=contains("Q38"))) %>%
    select(!(cols=contains("Q39"))) %>% select(!(cols=contains("Q40"))) %>% select(!(cols=contains("Q41")))

# Remove temporary data
rm(age, age.names, children, children.names, county, county.names, disability, disability.names, elderly, elderly.names, 
   gender, gender.names, hhsize, hhsize.names, income, income.names, language, language.names, veteran, veteran.names, race, zipcode)

# Transform to long form
rtp.data <- rtp.data %>%
    mutate(across(contains("Q"), as.character)) %>%
    pivot_longer(cols=contains("Q"), names_to="question", values_to="response") %>%
    mutate(question_number = question) %>%
    mutate(question_number = str_extract(question_number, "[Q](..)"), question_number = str_trim(question_number), question = str_replace(question, "[Q](..)", "")) %>%
    mutate(question = str_replace(question,"\\(",""),question = str_replace(question,"\\)","")) %>%
    mutate(question = gsub("<p>", "", question), question = gsub("</p>", "", question)) %>%
    mutate(question = gsub("<strong>", "", question), question = gsub("</strong>", "", question), question = gsub("&nbsp;", "", question)) %>%
    rename(response_date = `Responded at`) %>%
    mutate(response_date=as_date(response_date))

first.date <- rtp.data %>% select(response_date) %>% pull() %>% min()
total.responses <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date) %>% pull() %>% length()

# Tables filtered to Response Type ----------------------------------------
county <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, county) %>% mutate(response = 1) %>% rename(name=county)
county$name <- factor(county$name, levels=c("No Response","Other","Snohomish County","Pierce County","Kitsap County","King County"))

race <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, race) %>% mutate(response = 1) %>% rename(name=race)
race$name <- factor(race$name, levels=c("No Response","White","Other","Two or more races",
                                        "Native Hawaiian or Pacific Islander","Middle Eastern or North African","Hispanic or Latinx",
                                        "Asian or Asian American","American Indian or Alaska Native","Black or African American"))

income <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, income) %>% mutate(response = 1) %>% rename(name=income)
income$name <- factor(income$name, levels=c("No Response","over $200k","$100k to $200k","$75k to $100k","$50k to $75k","$25K to $50k","Under $25k"))

age <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, age) %>% mutate(response = 1) %>% rename(name=age)
age$name <- factor(age$name, levels=c("No Response","over 85","65 to 84","18 to 64","Under 18"))

size <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, hhsize) %>% mutate(response = 1) %>% rename(name=hhsize)
size$name <- factor(size$name, levels=c("No Response","6 or more","5 people","4 people","3 people","2 people","1 person"))

disabled <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, disability) %>% mutate(response = 1) %>% rename(name=disability)
children <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, children) %>% mutate(response = 1) %>% rename(name=children)

rtp.logo <- here('data',"Regional Transportation Plan Logo_3.jpg")
psrc.logo <- here('data',"psrc-logo.png")

# User Interface for Dashboard --------------------------------------------
ui <- dashboardPage(skin = "black", title = "PSRC RTP Online Survey",
    dashboardHeader(title = imageOutput("psrc_logo")),
    
    dashboardSidebar(
        sidebarMenu(
            br(),
            imageOutput("rtp_logo", height="100px"),
            br(),
            menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Work Status", tabName = "survey-q1", icon = icon("briefcase")),
            menuItem("Work from Home", tabName = "wfh", icon = icon("house-user")),
            menuItem("Infrastructure", tabName = "infrastructure", icon = icon("road")),
            menuItem("Transit Preference", tabName = "transit-preference", icon = icon("bus")),
            menuItem("Transportation Priority", tabName = "transportationpriority", icon = icon("train")),
            menuItem("Specialized Transportation", tabName = "special-needs", icon = icon("blind")),
            sliderInput("surveydates",
                        "Dates:",
                        min = as.Date(first.date,"%Y-%m-%d"),
                        max = as.Date(today()),
                        value=as.Date(today()),
                        timeFormat="%Y-%m-%d")
        )
    ),
    
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    
                    fluidRow(
                        infoBoxOutput("dateBox"),
                        infoBoxOutput("progressBox"),
                        infoBoxOutput("pocBox")
                    ),
                    
                    fluidRow(
                        infoBoxOutput("incomeBox"),
                        infoBoxOutput("childrenBox"),
                        infoBoxOutput("disabledBox")
                    ),

                    fluidRow(
                        column(width = 6, h2("Response by County")),
                        column(width = 6, h2("Response by Race/Ethnicity"))),
                    fluidRow(
                        column(width = 6, plotlyOutput("countychart")),
                        column(width = 6, plotlyOutput("racechart"))),
                    fluidRow(
                        column(width = 6, h2("Response by Income")),
                        column(width = 6, h2("Response by Household Size"))),
                    fluidRow(
                        column(width = 6, plotlyOutput("incomechart")),
                        column(width = 6, plotlyOutput("sizechart"))),
                    fluidRow(
                        column(width = 6, h2("Response by Age")),
                        column(width = 6, h2("Response by Zipcode"))),
                    fluidRow(
                        column(width = 6, plotlyOutput("agechart")),
                        column(width = 6, leafletOutput("zipcodemap")))
            ),
            
            # Second tab content
            tabItem(tabName = "survey-q1",
                    fluidRow(h4(textOutput("workstatustext"))),
                    fluidRow(box(title = "By Race / Ethnicity", solidHeader = TRUE, status = "primary", plotlyOutput("q1racechart"), width = 6),
                             box(title = "By Income", solidHeader = TRUE, status = "success", plotlyOutput("q1incomechart"), width = 6))
                    
            ),
            
            # Transit Preference
            tabItem(tabName = "transit-preference",
                    fluidRow(h4(textOutput("transitpreftext"))),
                    fluidRow(box(title = "By Race / Ethnicity", solidHeader = TRUE, status = "primary", plotlyOutput("q16racechart"), width = 6),
                             box(title = "By Income", solidHeader = TRUE, status = "success", plotlyOutput("q16incomechart"), width = 6))
                    
            ),
            
            # Special Needs
            tabItem(tabName = "special-needs",
                    fluidRow(h4(textOutput("specneedspreftext"))),
                    fluidRow(box(title = "By Race / Ethnicity", solidHeader = TRUE, status = "primary", plotlyOutput("q36racechart"), width = 6),
                             box(title = "By Income", solidHeader = TRUE, status = "success", plotlyOutput("q36incomechart"), width = 6))
                    
            ),
            
            # Infrastructure
            tabItem(tabName = "infrastructure",
                    fluidRow(h4(textOutput("infrastructuretext"))),
                    fluidRow(box(title = "Near Home", solidHeader = TRUE, status = "primary", plotlyOutput("q13chart"), width = 12)),
                    fluidRow(box(title = "Near Work", solidHeader = TRUE, status = "success", plotlyOutput("q12chart"), width = 12))
            ),
            
            # Future Transportation Priority
            tabItem(tabName = "transportationpriority",
                    fluidRow(h4(textOutput("transprioritytext"))),
                    fluidRow(box(title = "By Race / Ethnicity", solidHeader = TRUE, status = "primary", plotlyOutput("q17racechart"), width = 6),
                             box(title = "By Income", solidHeader = TRUE, status = "success", plotlyOutput("q17incomechart"), width = 6))
            ),
            
            # Work from Home tab content
            tabItem(tabName = "wfh",
                    fluidRow(h4(textOutput("wfhtext"))),
                    fluidRow(box(title = "Now by Race/Ethnicity", solidHeader = TRUE, status = "primary", plotlyOutput("wfhnowracechart"), width = 6),
                             box(title = "After COVID-19 by Race/Ethnicity", solidHeader = TRUE, status = "success", plotlyOutput("wfhaftracechart"), width = 6)),
                    fluidRow(box(title = "Now by Income", solidHeader = TRUE, status = "primary", plotlyOutput("wfhnowincomechart"), width = 6),
                             box(title = "After COVID-19 by Income", solidHeader = TRUE, status = "success", plotlyOutput("wfhaftincomechart"), width = 6))
                    
            )
            
        ) # end of tab items for main body
        
    ) #end of dashboard body
)

#

# Server functions for Dashboard ------------------------------------------

server <- function(input, output) {
    
    output$rtp_logo <- renderImage({
        return(list(src = rtp.logo, contentType = "image/jpg",alt = "Alignment", width = "95%", style = "padding-left: 10px"))
    }, deleteFile = FALSE)
    
    output$psrc_logo <- renderImage({
        return(list(src = psrc.logo, contentType = "image/png",alt = "Alignment", width = "75%"))
    }, deleteFile = FALSE)
    
    output$workstatustext <- renderText({
        paste0(rtp.data %>% filter(question_number == "Q1") %>% select(question) %>% pull() %>% unique())
    })
    
    output$wfhtext <- renderText({
        paste0(rtp.data %>% filter(question_number == "Q3") %>% select(question) %>% mutate(question = gsub(" now", "", question)) %>% pull() %>% unique())
    })
    
    output$infrastructuretext <- renderText({
        paste0(rtp.data %>% filter(question_number == "Q13") %>% separate(question, c("question", "sub-question"), "\\?") %>% select(question) %>% mutate(question = gsub(" in the area where you live", ":", question)) %>% pull() %>% unique())
    })
    
    output$transitpreftext <- renderText({
        paste0(q16)
    })
    
    output$specneedspreftext <- renderText({
        paste0(q36)
    })
    
    output$transprioritytext <- renderText({
        paste0(q17)
    })

    output$dateBox <- renderInfoBox({
        
        infoBox(
            "Selected Date", paste0(months(input$surveydates), " ", day(input$surveydates), ", ", year(input$surveydates)), icon = icon("calendar-alt"),
            color = "blue"
        )
    })

    output$progressBox <- renderInfoBox({
        
        responses <- county %>% filter(response_date <= input$surveydates) %>%  select(response_date) %>% pull() %>% length()
        
        infoBox(
            "Total Responses", paste0(responses), icon = icon("users"),
            color = "purple"
        )
    })
    
    output$pocBox <- renderInfoBox({
        
        responses <- county %>% filter(response_date <= input$surveydates) %>%  select(response_date) %>% pull() %>% length()
        
        poc.responses <- race %>% 
            filter(response_date <= input$surveydates, name != "White", name != "No Response") %>%  
            select(response_date) %>% pull() %>% length()
        
        share <- round((poc.responses/responses)*100,0)
        
        infoBox(
            "BIPOC", paste0(poc.responses, " (",share,"% of all responses)"), icon = icon("users"),
            color = "yellow"
        )
    })
    
    output$disabledBox <- renderInfoBox({
        
        responses <- county %>% filter(response_date <= input$surveydates) %>%  select(response_date) %>% pull() %>% length()
        
        disabled.responses <- disabled %>% 
            filter(response_date <= input$surveydates, name == "Yes") %>%  
            select(response_date) %>% pull() %>% length()
        
        share <- round((disabled.responses/responses)*100,0)
        
        infoBox(
            "People with a Disability", paste0(disabled.responses, " (",share,"% of all responses)"), icon = icon("blind"),
            color = "fuchsia"
        )
    })
 
    output$childrenBox <- renderInfoBox({
        
        responses <- county %>% filter(response_date <= input$surveydates) %>%  select(response_date) %>% pull() %>% length()
        
        children.responses <- children %>% 
            filter(response_date <= input$surveydates, name == "Yes") %>%  
            select(response_date) %>% pull() %>% length()
        
        share <- round((children.responses/responses)*100,0)
        
        infoBox(
            "People with Kids", paste0(children.responses, " (",share,"% of all responses)"), icon = icon("child"),
            color = "maroon"
        )
    })

    output$incomeBox <- renderInfoBox({
        
        responses <- county %>% filter(response_date <= input$surveydates) %>%  select(response_date) %>% pull() %>% length()
        
        income.responses <- income %>% 
            filter(response_date <= input$surveydates, name %in% c("Under $25k","$25K to $50k")) %>%  
            select(response_date) %>% pull() %>% length()
        
        share <- round((income.responses/responses)*100,0)
        
        infoBox(
            "People with Lower Incomes", paste0(income.responses, " (",share,"% of all responses)"), icon = icon("money-bill-alt"),
            color = "green"
        )
    })
    
    countyslider <- reactive({
        
        df <- county %>% 
            filter(response_date <= input$surveydates) %>% 
            select(name,response) %>%
            group_by(name) %>%
            summarize(total = sum(response))
    })

    raceslider <- reactive({
        
        df <- race %>% 
            filter(response_date <= input$surveydates) %>% 
            select(name,response) %>%
            group_by(name) %>%
            summarize(total = sum(response))
    })
 
    incomeslider <- reactive({
        
        df <- income %>% 
            filter(response_date <= input$surveydates) %>% 
            select(name,response) %>%
            group_by(name) %>%
            summarize(total = sum(response))
    })
 
    sizeslider <- reactive({
        
        df <- size %>% 
            filter(response_date <= input$surveydates) %>% 
            select(name,response) %>%
            group_by(name) %>%
            summarize(total = sum(response))
    }) 
    
    ageslider <- reactive({
        
        df <- age %>% 
            filter(response_date <= input$surveydates) %>% 
            select(name,response) %>%
            group_by(name) %>%
            summarize(total = sum(response))
    })
    
    output$countychart <- renderPlotly({create.bar.charts(countyslider())})
    output$racechart <- renderPlotly({create.bar.charts(raceslider())})
    output$incomechart <- renderPlotly({create.bar.charts(incomeslider())})
    output$sizechart <- renderPlotly({create.bar.charts(sizeslider())})
    output$agechart <- renderPlotly({create.bar.charts(ageslider())})
    
    output$q1racechart <- renderPlotly({summarize.question.by.race(rtp.data, q="Q1", d=input$surveydates, n=q1.names)})
    output$q1incomechart <- renderPlotly({summarize.question.by.income(rtp.data, q="Q1", d=input$surveydates, n=q1.names)})
    
    output$wfhnowracechart <- renderPlotly({summarize.question.by.race(q="Q3", d=input$surveydates, n=q3.names)})
    output$wfhnowincomechart <- renderPlotly({summarize.question.by.income(q="Q3", d=input$surveydates, n=q3.names)})
    
    output$wfhaftracechart <- renderPlotly({summarize.question.by.race(q="Q4", d=input$surveydates, n=q3.names)})
    output$wfhaftincomechart <- renderPlotly({summarize.question.by.income(q="Q4", d=input$surveydates, n=q3.names)})
    
    output$q12chart <- renderPlotly({summarize.multi.question.by.race(q="Q12", d=input$surveydates, n=q12.names)})
    output$q13chart <- renderPlotly({summarize.multi.question.by.race(q="Q13", d=input$surveydates, n=q12.names)})
    
    output$q16racechart <- renderPlotly({summarize.preference.question.by.race(q="Q16", d=input$surveydates, n=q16.names, t=q16.clean, o=q16.order)})
    output$q16incomechart <- renderPlotly({summarize.preference.question.by.income(q="Q16", d=input$surveydates, n=q16.names, t=q16.clean, o=q16.order)})

    output$q17racechart <- renderPlotly({summarize.preference.question.by.race(q="Q17", d=input$surveydates, n=q17.names, t=q17.clean, o=q17.order)})
    output$q17incomechart <- renderPlotly({summarize.preference.question.by.income(q="Q17", d=input$surveydates, n=q17.names, t=q17.clean, o=q17.order)})
    
    output$q36racechart <- renderPlotly({summarize.preference.question.by.race(q="Q36", d=input$surveydates, n=q36.names, t=q36.clean, o=q36.order)})
    output$q36incomechart <- renderPlotly({summarize.preference.question.by.income(q="Q36", d=input$surveydates, n=q36.names, t=q36.clean, o=q36.order)})
    
    output$zipcodemap <- renderLeaflet({create.zipcode.map(c.data=rtp.data, d=input$surveydates)})
    
}

shinyApp(ui, server)
