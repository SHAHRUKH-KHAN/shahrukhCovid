
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

header <- dashboardHeader(
  title = "Covid19 - Live Tracker"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
    menuItem("Today", icon = icon("th"), tabName = "Today"),
    menuItem("Map", icon = icon("map"), tabName = "Map"),
    menuItem("Progression", icon = icon("chart-bar"), tabName = "Progression")
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #tags$link(rel = "stylesheet", type = "text/css", href = "fontawesome.css")
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
                   box(title = "World's Data", DT::dataTableOutput("full_table") ,solidHeader = T, height = NULL, width = 12)
                 ),
                 fluidRow(
                   id="reference" ,
                   box(title = "Scraping from worldometers.info/coronavirus/", solidHeader = T, height = NULL, width = 12)
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
            id="progress_line",
            h2("Total Cases Until Yesterday"),
            selectInput("countrySelectProgress",
                        multiple = T,selectize = T, choices = NULL, label = "Select"),
            plotlyOutput("plot_by_coutry_total_cases")
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
  records$TotalCases <-  as.integer(gsub("\\,", "", records$TotalCases))
  records$NewCases <-  (gsub("\\,", "", records$NewCases))
  records$NewDeaths <-  as.integer(gsub("\\+", "", records$NewDeaths))
  records$TotalDeaths <-  as.integer(gsub("\\,", "", records$TotalDeaths))
  records$NewCases <-  as.integer(gsub("\\+", "", records$NewCases))
  records$TotalRecovered <-  as.integer(gsub("\\,", "", records$TotalRecovered))
  
  #Cleansing the data
  records <- select(records, -10,-7:-9)
  records[is.na(records)] <- 0
  records <- records[-nrow(records),]
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
  
  
  #Country with Maximum Case Today
  temp_df <- records %>% filter(NewCases == max(NewCases))
  country_name_max_case_today <- temp_df$Country
  count_of_max_case <- temp_df$NewCases
  temp_df_death <- records %>% filter(NewDeaths == max(NewDeaths))
  country_name_max_death <- temp_df_death$Country 
  count_of_new_death <- unique(temp_df_death$NewDeaths)
  
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
  output$full_table <- DT::renderDataTable({records}, rownames = FALSE, options = list(
    paging = T,
    responsive = T,
    scrollX = T,
    order = list(list(1, 'desc')))
  )
  
  countryVar = reactive({
    mydata = records$Country
  })
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
  
  data <- reactive({
    filter(records, Country == input$countryselect)
  })
  
  data_for_selected_country_p <- reactive({
    filter(total_confirm_cases_by_day, `Country/Region` %in% input$countrySelectProgress)
  })
  
  output$plot_by_coutry_total_cases <- renderPlotly({
    p <- data_for_selected_country_p() %>%
            plot_ly(x=~Date,y=~Total_Cases, color = data_for_selected_country_p()$`Country/Region`) %>% 
              add_lines(line=list(width=2)) %>% add_markers() %>%
                layout(
                  autosize = T,
                  xaxis = list(type = 'date', title = ""),
                  yaxis = list(title = "Frequency (Log Scale)", type="log"),
                  showlegend = T
                )
    #for(countr in data_for_country()$Country) {
     # p <- p %>% add_trace(y=~Total_Cases, name = countr)
    #}
    p
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


