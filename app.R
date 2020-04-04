
if(!require(xml2)) install.packages(xml2, repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages(rvest, repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages(readxl, repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages(plyr, repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages(dplyr, repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages(tidyverse, repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages(stringr, repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages(jsonlite, repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages(leaflet, repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages(maps, repos = "http://cran.us.r-project.org")
if(!require(mapdata)) install.packages(mapdata, repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages(shiny, repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages(shinydashboard, repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages(plotly, repos = "http://cran.us.r-project.org")


library(xml2)
library(rvest)
library(readxl)
library(stringr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(leaflet)
library(maps)
library(mapdata)
library(plyr)
library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)

#Load only Once
long_lat_data <- read_csv("datasets/lat_long_country.csv")
dt<-read_csv(paste0(getwd(),"/datasets/time_series_covid19_confirmed_global.csv"))
dt_death<-read_csv(paste0(getwd(),"/datasets/time_series_covid19_deaths_global.csv"))

header <- dashboardHeader(
  title = "Covid19 - Live Tracker"
)

sidebar <- dashboardSidebar(
  sidebarMenu(class='sidebar',
    menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
    menuItem("Today", icon = icon("th"), tabName = "Today"),
    menuItem("Map", icon = icon("map"), tabName = "Map"),
    menuItem("Progression", icon = icon("chart-bar"), tabName = "Progression"),
    menuItem("Incremental Change", icon = icon("calendar"), tabName = "WeeklyChange")
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #tags$link(rel = "stylesheet", type = "text/css", href = "material-dashboard.css")
  ),
  fluidPage(
    fluidRow(
      column(width = 12,
        tabItems(
         tabItem(tabName = "Overview",
                 fluidRow(
                   id="glance_info_boxes",
                   infoBox("Total Cases", textOutput("Total_cases") , icon = icon("briefcase-medical"), width = 3),
                   infoBox("Total Recovered", textOutput("Total_recovered"), icon = icon("thumbs-up"), width = 3),
                   infoBox("Total Death", textOutput("Total_death"), icon = icon("dizzy"), width = 3),
                   infoBox("Recovered(%)", textOutput("Total_recovered_percent"), icon = icon("vial"), width = 3)
                 ),
                 fluidRow(
                   id="world_data" ,
                   box(DT::dataTableOutput("full_table") ,solidHeader = T, height = NULL, width = 12)
                 ),
                 fluidRow(
                   id="reference" ,
                   box(title = "Scraping from worldometers.info/coronavirus/ and Hopkins dataset", solidHeader = T, height = NULL, width = 12)
                 )
                 
         ),
         
         tabItem(tabName = "Today",
           fluidRow(
             id = "today_info_boxes",
             h2("Today"),
             infoBox("Max New Case In", textOutput("max_country_new_case") , icon = icon("credit-card"), width = 3),
             infoBox("New Cases", textOutput("max_country_new_case_count"), icon = icon("briefcase-medical"), width = 3),
             infoBox("Max New Death In", textOutput("max_country_new_death"), icon = icon("credit-card"), width = 3),
             infoBox("Max Death", textOutput("max_country_new_death_count"), icon = icon("dizzy"), width = 3)
           ),
           fluidRow(
             id = "select_country",
             selectInput("countryselect",
                         multiple = F,selectize = T, choices = NULL, label = "Select"),
             infoBox("Country", textOutput("selectedCountryName"), icon = icon("credit-card"), width = 4),
             infoBox("Deaths", textOutput("selectedCountryNewDeathCount"), icon = icon("dizzy"), width = 4),
             infoBox("New Cases", textOutput("selectedCountryNewCasesCount"), icon = icon("briefcase-medical"), width = 4)
           )
         ),
         tabItem(tabName = "Map",
          fluidRow(
            id="map_view",
            h2("Click/Tap on the bubble"),
            leafletOutput("cases_map")
          )
         ),
         tabItem(tabName = "Progression",
          fluidRow(
            column(width = 12,
            selectInput("countrySelectProgress",
                        multiple = T,selectize = T, choices = NULL, label = "Select")
          )),
          fluidRow(
            column(width = 6,
              id="progress_line",
              plotlyOutput("plot_by_coutry_total_cases")
            ),
            column(width = 6,
               id="progress_line_death",
               div(class='bottom-margin'),
               plotlyOutput("plot_by_coutry_total_death")
            )
          )     
         ),
         tabItem(
           tabName = "WeeklyChange",
           fluidRow(
             column(
               width = 6,
               selectInput("countrySelectCycle",
                           multiple = F,selectize = T, choices = NULL, label = "Select")
             ),
             column(width = 6,
                    radioButtons(inputId="timecycle", label = "Cycle", 
                                 choices = c("Weekly"="Weekly", "Monthly"="Monthly"),
                                 selected = "Weekly")
           ),
           fluidRow(
             column(width = 12,
                    id = "weekly_chage",
                    plotlyOutput("plot_by_change")
              )
           )
         )
       )
      )
    )
  )
)
)

ui <- dashboardPage(header, sidebar, body)

ui1 <- fluidPage(
  
  # Application title
  titlePanel("Covid"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(),
    # Show a plot of the generated distribution
    mainPanel(h2(textOutput("max_country")))
  )
)

# Define server logic
server <- function(input, output, session) {
  url_to_scrap <- 'https://www.worldometers.info/coronavirus/'
  #url_to_scarp_locations <- 'https://developers.google.com/public-data/docs/canonical/countries_csv'
  content <- read_html(url_to_scrap)
  #content_locations <- read_html(url_to_scarp_locations)
  #loc_data <- html_node(content_locations, "table") %>% html_table(header=NA)
  main_table <- html_node(content, '#main_table_countries_today')
  # Get and Clean data
  records <- html_table(x = main_table, header = NA)
  records_to_show_table <- records
  records$TotalCases <-  as.integer(gsub("\\,", "", records$TotalCases))
  records$NewCases <-  (gsub("\\,", "", records$NewCases))
  records$NewDeaths <-  as.integer(gsub("\\+", "", records$NewDeaths))
  records$TotalDeaths <-  as.integer(gsub("\\,", "", records$TotalDeaths))
  records$NewCases <-  as.integer(gsub("\\+", "", records$NewCases))
  records$TotalRecovered <-  as.integer(gsub("\\,", "", records$TotalRecovered))
  
  #Cleansing the data
  records <- select(records, -7:-12)
  records[is.na(records)] <- 0
  records <- records[!(records$`Country,Other` == 'World' | records$`Country,Other` == 'Total:' ),]
  names(records)[names(records)=="Country,Other"] <- "Country"
  rownames(records) <- NULL
  
  #Get the location data
  #orig_records <- records
  orig_records <- records %>% left_join(long_lat_data, by=c("Country"="Country_Name_To_Use"))
  
  #Map
  mypalette <- colorBin( palette="YlOrRd", domain=orig_records$TotalCases, na.color="transparent")
  
  # Prepare the text for the tooltip:
  mytext <- paste0(
    "<div ", "style=\"font-size:15px\">",
    "<strong>",
    "Country: ", "</strong>", records$Country, "<br/>",
    "<strong>",
    "Total Cases: ", "</strong>", "<span style=\"color : #A27B72;\">",formatC
    (records$TotalCases, format = "d", big.mark = ","), "</span>","<br/>",
    "<strong>",
    "Total Death: ", "</strong>", "<span style=\" color: red\">",formatC(records$TotalDeaths, format = "d", big.mark = ","), "</span>","<br/>",
    "<strong>",
    "Total Recovered: ", "</strong>", "<span style=\"color:green\">",formatC(records$TotalRecovered, format = "d", big.mark = ","),"</span>","<br/></div>",
    sep="")%>%
    lapply(htmltools::HTML)
  
  # Final Map
  output$cases_map <- renderLeaflet({leaflet(orig_records) %>% 
    addTiles() %>% setView(lng = 0, lat = 10,zoom = 1.5) %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addCircleMarkers(~Longitude, ~Latitude, 
                     fillColor = ~mypalette(TotalCases), 
                     fillOpacity = 0.7, 
                     color="white",
                     radius=~ifelse(sqrt(TotalCases) > 30, sqrt(TotalCases)/10, sqrt(TotalCases)), 
                     stroke=FALSE, 
                     popup = mytext, 
                     labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), 
                                                  textsize = "13px", direction = "auto")) %>% 
    addLegend( pal=mypalette, 
               values=~TotalCases, 
               opacity=0.9, 
               title = "Total Cases", position = "topright" )})
  
  #Progressions
  gathered_dt <- gather(dt, "Date", "Total Confirmed Cases", -1)
  temp_dt <- gathered_dt[!duplicated(gathered_dt),]
  total_confirm_cases_by_day <- temp_dt %>% group_by(`Country/Region`, Date) %>% summarise(Total_Cases = sum(`Total Confirmed Cases`))
  total_confirm_cases_by_day$Date <- mdy(total_confirm_cases_by_day$Date)
  total_confirm_cases_by_day <- total_confirm_cases_by_day %>% mutate(Weekly = (week(Date)), Wday = wday(Date, label = T), Monthly = month(Date, label=T))
  total_confirm_cases_by_day$Weekly <- as.factor(total_confirm_cases_by_day$Weekly)
  total_confirm_cases_by_day$Wday <- as.factor(total_confirm_cases_by_day$Wday)
  total_confirm_cases_by_day$Monthly <- as.factor(total_confirm_cases_by_day$Monthly)
  #total_confirm_cases_by_day %>% View()
  #Death
  gathered_death_dt <- gather(dt_death, "Date", "Total Confirmed Death", -1)
  temp_death_dt <- gathered_death_dt[!duplicated(gathered_death_dt),]
  total_death_cases_by_day <- temp_death_dt %>% group_by(`Country/Region`, Date) %>% summarise(Total_Death = sum(`Total Confirmed Death`))
  total_death_cases_by_day$Date <- mdy(total_death_cases_by_day$Date)
  total_death_cases_by_day <- total_death_cases_by_day %>% mutate(week = (week(Date)-3), wday = wday(Date, label = T), Month = month(Date, label=F))
  total_death_cases_by_day$week <- as.factor(total_death_cases_by_day$week)
  total_death_cases_by_day$wday <- as.factor(total_death_cases_by_day$wday)
  total_death_cases_by_day$Month <- as.factor(total_death_cases_by_day$Month)
  
  #Country with Maximum Case Today
  temp_df <- records %>% filter(NewCases == max(NewCases))
  country_name_max_case_today <- temp_df$Country
  count_of_max_case <- temp_df$NewCases
  temp_df_death <- records %>% filter(NewDeaths == max(NewDeaths))
  country_name_max_death <- temp_df_death$Country 
  count_of_new_death <- unique(temp_df_death$NewDeaths)
  
  #Cylce
  
  
  #All the values
  total_cases <- formatC(sum(records$TotalCases), format = "d", big.mark = ",")
  total_death <- formatC(sum(records$TotalDeaths), format = "d", big.mark = ",")
  total_recovered <- formatC(sum(records$TotalRecovered), format = "d", big.mark = ",")
  total_reco_percent <- paste((round(sum(records$TotalRecovered)/sum(records$TotalCases),digits=4))*100, "%")
  
  output$max_country_new_case <- renderText(formatC(country_name_max_case_today, format = "d", big.mark = ","))
  output$max_country_new_case_count <- renderText(formatC(count_of_max_case, format = "d", big.mark = ","))
  output$max_country_new_death <- renderText(formatC(country_name_max_death, format = "d", big.mark = ","))
  output$max_country_new_death_count <- renderText(formatC(count_of_new_death, format = "d", big.mark = ","))
  
  output$Total_cases <- renderText(total_cases)
  output$Total_death <- renderText(total_death)
  output$Total_recovered <- renderText(total_recovered)
  output$Total_recovered_percent <- renderText(total_reco_percent)
  output$full_table <- DT::renderDataTable({records}, 
                            style="bootstrap4",
                            rownames = FALSE, 
                            options = list(
                            paging = T,
                            responsive = T,
                            scrollX = T,
                            order = list(list(1, 'desc')))
  )
  
  #Reactive Elements
  
  countryVar = reactive({
    mydata = records$Country
  })

  data <- reactive({
    filter(records, Country == input$countryselect)
  })
  
  data_for_selected_country_p <- reactive({
    filter(total_confirm_cases_by_day, `Country/Region` %in% input$countrySelectProgress)
  })
  
  data_for_selected_country_dea <- reactive({
    filter(total_death_cases_by_day, `Country/Region` %in% input$countrySelectProgress)
  })
  
  data_for_cycle <- reactive({
    t <- total_confirm_cases_by_day %>% 
      filter(`Country/Region` == input$countrySelectCycle) %>%
      group_by_('Date', input$timecycle) %>%
      summarize(totalcase = sum(Total_Cases))
    g <- t %>% group_by_(input$timecycle) %>% summarize(tot = max(totalcase)) %>%
      mutate(d = tot -lag(tot), numb = row_number(tot))
    g[1,3] = g[1,2]
    g
  })
  
  #Observe  Elements
  observe({
    updateSelectInput(session,
                      inputId = "countryselect",
                      label = "Choose a country",
                      selected = "India",
                      choices = countryVar()
    )})
  
  observe({
    updateSelectInput(session,
                      inputId = "countrySelectProgress",
                      label = "Choose Countries",
                      selected = "India",
                      choices = countryVar()
    )})
  
  observe({
    updateSelectInput(session,
                      inputId = "countrySelectCycle",
                      label = "Choose Country",
                      selected = "India",
                      choices = countryVar()
    )})
  
  #Styling Elements
  
  plot_title_font_family <- list(family = 'Source Sans Pro,sans-serif', color='rgba(6, 6, 6, 0.45)')
  plot_title_x_y <- data.frame(x = .5, y = .95)
  plot_x_y_labels <- list(family = 'Source Sans Pro,sans-serif', color='rgba(6, 6, 6, 0.45)')

  
  
  #Output Elements
  
  output$plot_by_coutry_total_death <- renderPlotly({
    p_d <- data_for_selected_country_dea() %>%
      plot_ly(x=~Date,y=~Total_Death, color = data_for_selected_country_dea()$`Country/Region`) %>% 
      add_lines(line=list(width=2), text = c(''), hoverinfo = 'text', showlegend = F) %>% 
      add_markers() %>%
      layout(
        annotations = list(x = 1.25, y = -0.1, text = "*Updates - Daily", 
                           showarrow = F, xref='paper', yref='paper', 
                           xanchor='right', yanchor='auto', xshift=10, yshift=0,
                           font=list(size=10, color="#c9c9c9")),
        title = list(text = paste0('Number of Deaths'), 
                     font = plot_title_font_family,
                     x = plot_title_x_y[['x']],
                     xanchor= 'center',
                     yanchor= 'top',
                     y = plot_title_x_y[['y']]),
        autosize = T,
        xaxis = list(type = 'date', title = list(text=''), 
                     tickfont = plot_x_y_labels,
                     zeroline = FALSE,
                     showgrid = FALSE),
        yaxis = list(title = list(text = paste0('Count'), 
                                  font = plot_title_font_family),
                                  tickfont = plot_x_y_labels,
                                  showgrid = FALSE),
                     showlegend = T
      )
    p_d
  })
  
  output$plot_by_coutry_total_cases <- renderPlotly({
    p <- data_for_selected_country_p() %>%
            plot_ly(x=~Date,y=~Total_Cases, color = data_for_selected_country_p()$`Country/Region`) %>% 
              add_lines(line=list(width=2), text = c(''), hoverinfo = 'text', showlegend = F) %>% 
              add_markers() %>%
                layout(
                  annotations = list(x = 1.30, y = -0.1, text = "*Updates - Daily", 
                                     showarrow = F, xref='paper', yref='paper', 
                                     xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                     font=list(size=10, color="#c9c9c9")),
                  title = list(text = paste0('Number of Cases'), 
                               font = plot_title_font_family,
                               x = plot_title_x_y[['x']],
                               y = plot_title_x_y[['y']],
                               xanchor= 'center',
                               yanchor= 'top',
                               pad = 10),
                  autosize = T,
                  xaxis = list(type = 'date', title = "", tickfont = plot_x_y_labels, showgrid = FALSE, zeroline = 0, showline = FALSE),
                  yaxis = list(title = list(text = paste0('Count (Log Scale)'), 
                                            font = plot_title_font_family), type="log", 
                                            tickfont = plot_x_y_labels, showgrid = FALSE),
                  showlegend = T
                )
    p
  })
  
  output$plot_by_change <- renderPlotly({
    x_to_draw <- if(input$timecycle == 'Weekly')
                    "numb"
                else if(input$timecycle == 'Monthly')
                  "Monthly"
    print(x_to_draw)
    fig_cycle <- plot_ly(data = data_for_cycle(), x = ~get(x_to_draw), y = ~d, 
                         type = 'waterfall',
                         connector = list(line = list(color= "rgb(63, 63, 63)"))) %>%
      layout(
        annotations = list(x = 1, y = -0.1, text = "*Updates - Daily", 
                          showarrow = F, xref='paper', yref='paper', 
                          xanchor='right', yanchor='auto', xshift=0, yshift=0,
                          font=list(size=10, color="#c9c9c9")),
        title = list(text = 'Incremental Analysis',
                     font = plot_title_font_family,
                     x = plot_title_x_y[['x']],
                     y = plot_title_x_y[['y']]),
        xaxis = list(
          pad = 10,
          tickfont = plot_x_y_labels,
          title = list(
            text = paste0(
            if(input$timecycle=='Weekly')
              "Week"
            else
              "Month"
          ),
          font = plot_title_font_family)
        ),
        yaxis = list(
          tickfont = plot_x_y_labels,
          title = list(text = "Count",
                       font = plot_title_font_family),
          pad = 10
        )
      )
    fig_cycle
  })
  
  
  output$selectedCountryName <- renderText({
    paste(data()$Country)
  })
  output$selectedCountryNewDeathCount <- renderText({
    paste(formatC(data()$NewDeaths, format = "d", big.mark = ","))
  })
  output$selectedCountryNewCasesCount <- renderText({
    paste(formatC(data()$NewCases, format = "d", big.mark = ","))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
