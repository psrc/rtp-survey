library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(here)
library(lubridate)

# Packages for Chart Creation
library(ggplot2)
library(scales)
library(plotly)

# Packages for Maps
library(sf)
library(leaflet)

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
                "18 to 24" = 2,
                "25 to 34" = 3,
                "35 to 44" = 4,
                "45 to 54" = 5,
                "55 to 64" = 6,
                "65 to 74" = 7,
                "75 to 84" = 8,
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

q1.lookup <- c( "Working from home (employed full or part-time, self-employed)  " = 1,
                "Working outside the home (employed full or part-time, self-employed)  " = 2,
                "Some combination of working from home or outside the home " = 3,
                "Student (full or part-time)" = 4,
                "Unemployed or furloughed" = 5,
                "Retired" = 6,
                "Unable to work and not looking for employment (for example, due to disability or caregiver role)" = 7,
                "Not working for pay or not looking for employment" = 8,
                "Other (please tell us more below)" = 9)

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

psrc.colors <- list("People of Color" = "#AD5CAB",
                    "White" = "#E3C9E3")


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
    
    # Add names and consolidate race to white & people of color
    temp <- left_join(temp, n, by=c("response"="value")) %>% 
        filter(race != "No Response") %>%
        mutate(race = case_when(
            race == "White" ~ "White",
            race != "White" ~ "People of Color")) %>%
        mutate(response=1)
    
    total.non.white <- temp %>% filter(race=="People of Color") %>% select(response) %>% pull() %>% sum()   
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

summarize.multi.question.by.race <- function(c.data=rtp.data, q, d, n) {
    
    temp <- c.data %>% 
        filter(question_number == q & response_date <= d & response > 0) %>% 
        separate(question, c("question", "sub-question"), "\\?") %>%
        select(race, `sub-question`, response) %>% 
        mutate(mutate(across(response, as.numeric))) %>%
        mutate(race = case_when(
            race == "White" ~ "White",
            race != "White" ~ "People of Color")) %>%
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

# Process and Clean Survey Data -------------------------------------------
rtp.data <- data.table::fread(here('data', 'RTPsurvey.csv')) %>% as_tibble()

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
    mutate(mutate(across(contains("Q"), as.character))) %>%
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
race <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, race) %>% mutate(response = 1) %>% rename(name=race)
income <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, income) %>% mutate(response = 1) %>% rename(name=income)
size <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, hhsize) %>% mutate(response = 1) %>% rename(name=hhsize)
disabled <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, disability) %>% mutate(response = 1) %>% rename(name=disability)
children <- rtp.data %>% filter(question_number == "Q1") %>% select(response_date, children) %>% mutate(response = 1) %>% rename(name=children)

question_choices <- c("Q1")


# User Interface for Dashboard --------------------------------------------
ui <- dashboardPage(skin = "purple", title = "RTP Survey",
    dashboardHeader(title = "2022-2050 RTP Survey",
                    titleWidth = '20%'),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Work Status", tabName = "survey-q1", icon = icon("th")),
            menuItem("Work from Home - Now", tabName = "survey-q3", icon = icon("th")),
            menuItem("Work from Home - Later", tabName = "survey-q4", icon = icon("th")),
            menuItem("Infrastructure near Home", tabName = "survey-q13", icon = icon("th")),
            menuItem("Infrastructure near Work", tabName = "survey-q12", icon = icon("th")),
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
                        column(width = 6, h2("Survey Response by County")),
                        column(width = 6, h2("Survey Response by Race/Ethnicity"))),
                    fluidRow(
                        column(width = 6, plotlyOutput("countychart")),
                        column(width = 6, plotlyOutput("racechart"))),
                    fluidRow(
                        column(width = 6, h2("Survey Response by Income")),
                        column(width = 6, h2("Survey Response by Household Size"))),
                    fluidRow(
                        column(width = 6, plotlyOutput("incomechart")),
                        column(width = 6, plotlyOutput("sizechart"))),
            ),
            
            # Second tab content
            tabItem(tabName = "survey-q1",
                    fluidRow(
                        column(width = 3, selectInput("QuestionNumber","Select the Survey Question:",question_choices)),
                        column(width = 9, "")),
                    fluidRow(h2(textOutput("question"))),
                    fluidRow(
                        column(width=10, plotlyOutput("questionchart")))
                    
            ),
            
            # Infrastructure Near Home tab content
            tabItem(tabName = "survey-q13",
                    fluidRow(h3(textOutput("q13text"))),
                    fluidRow(
                        column(width=10, plotlyOutput("q13chart", width = "100%", height = "300%")))
                    
            ),
            
            # Infrastructure Near Work tab content
            tabItem(tabName = "survey-q12",
                    fluidRow(h3(textOutput("q12text"))),
                    fluidRow(
                        column(width=10, plotlyOutput("q12chart", width = "100%", height = "300%")))
                    
            ),
            
            # Work from Home Now tab content
            tabItem(tabName = "survey-q3",
                    fluidRow(h3(textOutput("q3text"))),
                    fluidRow(
                        column(width=10, plotlyOutput("q3chart", width = "100%", height = "300%")))
                    
            ),
            
            # Work from Home Future tab content
            tabItem(tabName = "survey-q4",
                    fluidRow(h3(textOutput("q4text"))),
                    fluidRow(
                        column(width=10, plotlyOutput("q4chart", width = "100%", height = "300%")))
                    
            )
            
        ) # end of tab items for main body
        
    ) #end of dashboard body
)

#

# Server functions for Dashboard ------------------------------------------

server <- function(input, output) {
    
    output$question <- renderText({
        paste0(rtp.data %>% filter(question_number == input$QuestionNumber) %>% select(question) %>% pull() %>% unique())
    })
    
    output$q3text <- renderText({
        paste0(rtp.data %>% filter(question_number == "Q3") %>% select(question) %>% pull() %>% unique())
    })
    
    output$q4text <- renderText({
        paste0(rtp.data %>% filter(question_number == "Q4") %>% select(question) %>% pull() %>% unique())
    })
    
    output$q13text <- renderText({
        paste0(rtp.data %>% filter(question_number == "Q13") %>% separate(question, c("question", "sub-question"), "\\?") %>% select(question) %>% pull() %>% unique())
    })
    
    output$q12text <- renderText({
        paste0(rtp.data %>% filter(question_number == "Q12") %>% separate(question, c("question", "sub-question"), "\\?") %>% select(question) %>% pull() %>% unique())
    })
    
    output$dateBox <- renderInfoBox({
        
        infoBox(
            "Selected Date", paste0(input$surveydates), icon = icon("calendar-alt"),
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
            "People of Color", paste0(poc.responses, " (",share,"% of all responses)"), icon = icon("users"),
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
    
    output$countychart <- renderPlotly({create.bar.charts(countyslider())})
    output$racechart <- renderPlotly({create.bar.charts(raceslider())})
    output$incomechart <- renderPlotly({create.bar.charts(incomeslider())})
    output$sizechart <- renderPlotly({create.bar.charts(sizeslider())})
    
    output$questionchart <- renderPlotly({summarize.question.by.race(rtp.data, q=input$QuestionNumber, d=input$surveydates, n=q1.names)})
    output$q3chart <- renderPlotly({summarize.question.by.race(q="Q3", d=input$surveydates, n=q3.names)})
    output$q4chart <- renderPlotly({summarize.question.by.race(q="Q4", d=input$surveydates, n=q3.names)})
    output$q12chart <- renderPlotly({summarize.multi.question.by.race(q="Q12", d=input$surveydates, n=q12.names)})
    output$q13chart <- renderPlotly({summarize.multi.question.by.race(q="Q13", d=input$surveydates, n=q12.names)})
   
}

shinyApp(ui, server)
