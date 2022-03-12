library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(DT)
library(shinyjs)
library(Rcpp)
library(leaflet)
library(leaflet.extras)
library(stringr)

Data1 <- read.csv("segmentaa.csv")
Data2 <- read.csv("segmentab.csv")
Data3 <- read.csv("segmentac.csv")
Data4 <- read.csv("segmentad.csv")
Data5 <- read.csv("segmentae.csv")
Data6 <- read.csv("segmentaf.csv")
Data7 <- read.csv("segmentag.csv")
Data8 <- read.csv("segmentah.csv")

LatLon <- read.csv("CTALatLon.csv")

Data1$newDate <- mdy(Data1$date)
Data2$newDate <- mdy(Data2$date)
Data3$newDate <- mdy(Data3$date)
Data4$newDate <- mdy(Data4$date)
Data5$newDate <- mdy(Data5$date)
Data6$newDate <- mdy(Data6$date)
Data7$newDate <- mdy(Data7$date)
Data8$newDate <- mdy(Data8$date)
Data1$station_id <- Data1$station_id
colnames(Data1)<- c('station_id','stationname','date', 'daytype', 'rides', 'newDate')

Data1Merged <- merge(Data1, LatLon, by = "station_id")
Data2Merged <- merge(Data2, LatLon, by = "station_id")
Data3Merged <- merge(Data3, LatLon, by = "station_id")
Data4Merged <- merge(Data4, LatLon, by = "station_id")
Data5Merged <- merge(Data5, LatLon, by = "station_id")
Data6Merged <- merge(Data6, LatLon, by = "station_id")
Data7Merged <- merge(Data7, LatLon, by = "station_id")
Data8Merged <- merge(Data8, LatLon, by = "station_id")

Data1Merged$Year <- year(mdy(Data1Merged$date))
Data2Merged$Year <- year(mdy(Data2Merged$date))
Data3Merged$Year <- year(mdy(Data3Merged$date))
Data4Merged$Year <- year(mdy(Data4Merged$date))
Data5Merged$Year <- year(mdy(Data5Merged$date))
Data6Merged$Year <- year(mdy(Data6Merged$date))
Data7Merged$Year <- year(mdy(Data7Merged$date))
Data8Merged$Year <- year(mdy(Data8Merged$date))


if(Data1Merged$RED == TRUE){
  Data1Merged$Line = "Red"
}else if(Data1Merged$BLUE == TRUE){
  Data1Merged$Line = "Blue"
}else if(Data1Merged$BRN == TRUE){
  Data1Merged$Line = "Brown"
}else if(Data1Merged$G == TRUE){
  Data1Merged$Line = "Green"
}else if(Data1Merged$O == TRUE){
  Data1Merged$Line = "Orange"
}else if(Data1Merged$Pnk == TRUE){
  Data1Merged$Line = "Pink"
}else if(Data1Merged$P == TRUE){
  Data1Merged$Line = "Purple"
}else if(Data1Merged$Y == TRUE){
  Data1Merged$Line = "Yellow"
}

if(Data2Merged$RED == TRUE){
  Data2Merged$Line = "Red"
}else if(Data2Merged$BLUE == TRUE){
  Data2Merged$Line = "Blue"
}else if(Data2Merged$BRN == TRUE){
  Data2Merged$Line = "Brown"
}else if(Data2Merged$G == TRUE){
  Data2Merged$Line = "Green"
}else if(Data2Merged$O == TRUE){
  Data2Merged$Line = "Orange"
}else if(Data2Merged$Pnk == TRUE){
  Data2Merged$Line = "Pink"
}else if(Data2Merged$P == TRUE){
  Data2Merged$Line = "Purple"
}else if(Data2Merged$Y == TRUE){
  Data2Merged$Line = "Yellow"
}


if(Data3Merged$RED == TRUE){
  Data3Merged$Line = "Red"
}else if(Data3Merged$BLUE == TRUE){
  Data3Merged$Line = "Blue"
}else if(Data3Merged$BRN == TRUE){
  Data3Merged$Line = "Brown"
}else if(Data3Merged$G == TRUE){
  Data3Merged$Line = "Green"
}else if(Data3Merged$O == TRUE){
  Data3Merged$Line = "Orange"
}else if(Data3Merged$Pnk == TRUE){
  Data3Merged$Line = "Pink"
}else if(Data3Merged$P == TRUE){
  Data3Merged$Line = "Purple"
}else if(Data3Merged$Y == TRUE){
  Data3Merged$Line = "Yellow"
}


if(Data4Merged$RED == TRUE){
  Data4Merged$Line = "Red"
}else if(Data4Merged$BLUE == TRUE){
  Data4Merged$Line = "Blue"
}else if(Data4Merged$BRN == TRUE){
  Data4Merged$Line = "Brown"
}else if(Data4Merged$G == TRUE){
  Data4Merged$Line = "Green"
}else if(Data4Merged$O == TRUE){
  Data4Merged$Line = "Orange"
}else if(Data4Merged$Pnk == TRUE){
  Data4Merged$Line = "Pink"
}else if(Data4Merged$P == TRUE){
  Data4Merged$Line = "Purple"
}else if(Data4Merged$Y == TRUE){
  Data4Merged$Line = "Yellow"
}


if(Data5Merged$RED == TRUE){
  Data5Merged$Line = "Red"
}else if(Data5Merged$BLUE == TRUE){
  Data5Merged$Line = "Blue"
}else if(Data5Merged$BRN == TRUE){
  Data5Merged$Line = "Brown"
}else if(Data5Merged$G == TRUE){
  Data5Merged$Line = "Green"
}else if(Data5Merged$O == TRUE){
  Data5Merged$Line = "Orange"
}else if(Data5Merged$Pnk == TRUE){
  Data5Merged$Line = "Pink"
}else if(Data5Merged$P == TRUE){
  Data5Merged$Line = "Purple"
}else if(Data5Merged$Y == TRUE){
  Data5Merged$Line = "Yellow"
}


if(Data6Merged$RED == TRUE){
  Data6Merged$Line = "Red"
}else if(Data6Merged$BLUE == TRUE){
  Data6Merged$Line = "Blue"
}else if(Data6Merged$BRN == TRUE){
  Data6Merged$Line = "Brown"
}else if(Data6Merged$G == TRUE){
  Data6Merged$Line = "Green"
}else if(Data6Merged$O == TRUE){
  Data6Merged$Line = "Orange"
}else if(Data6Merged$Pnk == TRUE){
  Data6Merged$Line = "Pink"
}else if(Data6Merged$P == TRUE){
  Data6Merged$Line = "Purple"
}else if(Data6Merged$Y == TRUE){
  Data6Merged$Line = "Yellow"
}


if(Data7Merged$RED == TRUE){
  Data7Merged$Line = "Red"
}else if(Data7Merged$BLUE == TRUE){
  Data7Merged$Line = "Blue"
}else if(Data7Merged$BRN == TRUE){
  Data7Merged$Line = "Brown"
}else if(Data7Merged$G == TRUE){
  Data7Merged$Line = "Green"
}else if(Data7Merged$O == TRUE){
  Data7Merged$Line = "Orange"
}else if(Data7Merged$Pnk == TRUE){
  Data7Merged$Line = "Pink"
}else if(Data7Merged$P == TRUE){
  Data7Merged$Line = "Purple"
}else if(Data7Merged$Y == TRUE){
  Data7Merged$Line = "Yellow"
}

if(Data8Merged$RED == TRUE){
  Data8Merged$Line = "Red"
}else if(Data8Merged$BLUE == TRUE){
  Data8Merged$Line = "Blue"
}else if(Data8Merged$BRN == TRUE){
  Data8Merged$Line = "Brown"
}else if(Data8Merged$G == TRUE){
  Data8Merged$Line = "Green"
}else if(Data8Merged$O == TRUE){
  Data8Merged$Line = "Orange"
}else if(Data8Merged$Pnk == TRUE){
  Data8Merged$Line = "Pink"
}else if(Data8Merged$P == TRUE){
  Data8Merged$Line = "Purple"
}else if(Data8Merged$Y == TRUE){
  Data8Merged$Line = "Yellow"
}


if(LatLon$RED == TRUE){
  LatLon$Line = "Red"
}else if(LatLon$BLUE == TRUE){
  LatLon$Line = "Blue"
}else if(LatLon$BRN == TRUE){
  LatLon$Line = "Brown"
}else if(LatLon$G == TRUE){
  LatLon$Line = "Green"
}else if(LatLon$O == TRUE){
  LatLon$Line = "Orange"
}else if(LatLon$Pnk == TRUE){
  LatLon$Line = "Pink"
}else if(LatLon$P == TRUE){
  LatLon$Line = "Purple"
}else if(LatLon$Y == TRUE){
  LatLon$Line = "Yellow"
}

styles <- c("First Style", "Second Style", "Third Style")
order <- c("Alphabetical", "Low-High")
years<-c(2001:2021)
stops <- vector()
for (i in colnames(LatLon)){
  stops <- c(stops, LatLon[i]$STATION_NAME)
}
DataFirstDay <- subset(Data8Merged, newDate == "2021-08-23")

ui <- dashboardPage(
  #Title for the application
  dashboardHeader(title = "CS 424 Project 2"),
  #Components on sidebar to switch from tab to tab
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
    sidebarMenu(
      #First set of plots and tables that the user sees
      menuItem("", tabName = "default", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("Dashboard", tabName = "dashboard", icon = NULL),
      menuItem("Map Visualization", tabName = "maps", icon = NULL),
      menuItem("Bar Chart Visualization", tabName = "bars", icon = NULL),
      menuItem("Compare Plots", tabName = "comparePlots", icon = NULL),
      menuItem("About", tabName = "about", icon = NULL)
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "about",
        strong(h1("About this application")),
        h3("This interface allows users to look at CTA data over the past couple of decades for each CTA stop in the form of bar charts, tables, and maps.
        The data for this application is provided by Chicago Data portal. You may access it using this link: 
        https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f")
      ),
      tabItem(
        tabName = "default",
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        box(title = "All Entries on August 8th 2021", solidHeader = TRUE, status = "primary", width = 20,
            plotOutput("dashboardTabTable", height = 300),
        ),
        leafletOutput("dashboardTabMap")
      ),      tabItem(
        tabName = "dashboard",
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        box(title = "All Entries on August 8th 2021", solidHeader = TRUE, status = "primary", width = 20,
            plotOutput("dashboardTabTable2", height = 300),
        ),
        leafletOutput("dashboardTabMap2")
      ),
      tabItem(
        tabName = "maps",
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        selectInput("Style", "Select the style", styles, selected = "First Style"), 
        dateInput("inputDate1", "Date:", value = "2020-03-12"),
        actionButton("prev1", "<-"),
        actionButton("next1", "->"),
        leafletOutput("mapTabMap")
      ),
      tabItem(
        tabName = "bars",
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        selectInput("Order", "Select the order", order, selected = "Alphabetical"), 
        dateInput("inputDate2", "Date:", value = "2020-03-12"),
        actionButton("prev2", "<-"),
        actionButton("next2", "->"),
        box(title = "", solidHeader = TRUE, status = "primary", width = 20,
            plotOutput("chartTabChart", height = 300),
        ),
        box( title = "", solidHeader = TRUE, status = "primary", width = 20,
             DT::dataTableOutput("barChartEntriesTable", height = 300)
        )
      ),
      tabItem(
        tabName = "comparePlots",
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        column(1,
               #Spacer Column 
        ),
        column(3,
               fluidRow(
                 fluidRow(
                   #The dropdown menu to allow the user to select the year and stop they want to visualize for the left-most graphs
                   box(
                     selectInput("YearOne", "Select the year to visualize", years, selected = 2021), 
                     selectInput("StopOne", "Select the stop to visualize", unique(stops), selected = "Pulaski")
                     ),
                   box(width = 5,
                       #The checkboxes allow the user to pick which visualizations they want to see
                       p("Choose what data"),
                       checkboxInput("toggleDayEntriesOne", "Every Day of the Year", value = TRUE),
                       checkboxInput("toggleMonthEntriesOne", "Every Month of the Year", value = TRUE),
                       checkboxInput("toggleDayOfTheWeekEntriesOne", "Every Day of the Week for the Year", value = TRUE),
                   ),
                 ),
                 fluidRow(
                   id = "DayEntriesBoxOne",
                   box(status = "primary", width = 20,
                       plotOutput("DayEntriesOne", height = 300),
                   )
                 ),
                 fluidRow(
                   id = "MonthEntriesBoxOne",
                   box(status = "primary", width = 20,
                       plotOutput("MonthEntriesOne", height = 300),
                   )
                 ),
                 #This row has the bar graph for every single entry split in days of the week in a user specified year at a user specified stop (left-most)
                 fluidRow(
                   id = "DayOfTheWeekEntriesBoxOne",
                   box(status = "primary", width = 20,
                       plotOutput("DayOfTheWeekEntriesOne", height = 300),
                   )
                 ),
               ), 
        ),
        column(3,
               #Spacer Column 
        ),
        column(3,
               fluidRow(
                 fluidRow(
                   box(
                     #The dropdown menu to allow the user to select the year and stop they want to see the data for the left-most tables
                     selectInput("YearOneTable", "Select the year to visualize", years, selected = 2021), 
                     selectInput("StopOneTable", "Select the stop to visualize", unique(stops), selected = "Pulaski")
                   ),
                   box(
                     width = 5,
                     #The checkboxes allow the user to pick which tables they want to see
                     p("Choose what data"),
                     checkboxInput("toggleDayEntriesTableOne", "Every Day of the Year", value = TRUE),
                     checkboxInput("toggleMonthEntriesTableOne", "Every Month of the Year", value = TRUE),
                     checkboxInput("toggleDayOfTheWeekTableEntriesOne", "Every Day of the Week for the Year", value = TRUE),
                   ),
                 ),
                 #This row has the table for every single entry in a user specified year at the user specified stop (left-most)
                 fluidRow(
                   id = "DayEntriesBoxTableOne",
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("DayEntriesTableOne", height = 300)
                   )
                 ),
                 #This row has the table for every single entry split in months in a user specified year at a user specified stop (left-most)
                 fluidRow(
                   id = "MonthEntriesBoxTableOne",
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("MonthEntriesTableOne", height = 300)
                   )
                 ),
                 #This row has the table for every single entry split in days of the week in a user specified year at a user specified stop (left-most)
                 fluidRow(
                   id = "DayOfTheWeekEntriesBoxTableOne",
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("DayOfTheWeekEntriesTableOne", height = 300)
                   )
                 ),
               )
        ),
        column(1,
               #Spacer Column 
        ),
      )
      )
    )
)
server <- function(input, output, session) {
  observeEvent(input$prev1,{
    date <- as.Date(input$inputDate1)
    updateDateInput(session,"inputDate1",value = date-1, min= "2001-01-01",max = "2021-11-30")
  })
  observeEvent(input$next1,{
    date <- as.Date(input$inputDate1)
    updateDateInput(session,"inputDate1",value = date+1, min= "2001-01-01",max = "2021-11-30")
  })
  observeEvent(input$prev2,{
    date <- as.Date(input$inputDate2)
    updateDateInput(session,"inputDate2",value = date-1, min= "2001-01-01",max = "2021-11-30")
  })
  observeEvent(input$next2,{
    date <- as.Date(input$inputDate2)
    updateDateInput(session,"inputDate2",value = date+1, min= "2001-01-01",max = "2021-11-30")
  })
  
  observeEvent(input$toggleDayEntriesOne, {
    #Source for ShinyJS show/hide: https://rdrr.io/cran/shinyjs/man/visibilityFuncs.html
    if(input$toggleDayEntriesOne == TRUE){
      shinyjs::show("DayEntriesBoxOne")
    }
    else{
      shinyjs::hide("DayEntriesBoxOne")
    }
  })
  
  observeEvent(input$toggleMonthEntriesOne, {
    if(input$toggleMonthEntriesOne == TRUE){
      shinyjs::show("MonthEntriesBoxOne")
    }
    else{
      shinyjs::hide("MonthEntriesBoxOne")
    }
  })
  
  observeEvent(input$toggleDayOfTheWeekEntriesOne, {
    if(input$toggleDayOfTheWeekEntriesOne == TRUE){
      shinyjs::show("DayOfTheWeekEntriesBoxOne")
    }
    else{
      shinyjs::hide("DayOfTheWeekEntriesBoxOne")
    }
  })
  
  observeEvent(input$toggleDayEntriesTableOne, {
    if(input$toggleDayEntriesTableOne == TRUE){
      shinyjs::show("DayEntriesBoxTableOne")
    }
    else{
      shinyjs::hide("DayEntriesBoxTableOne")
    }
  })
  
  observeEvent(input$toggleMonthEntriesTableOne, {
    if(input$toggleMonthEntriesTableOne == TRUE){
      shinyjs::show("MonthEntriesBoxTableOne")
    }
    else{
      shinyjs::hide("MonthEntriesBoxTableOne")
    }
  })
  
  observeEvent(input$toggleDayOfTheWeekTableEntriesOne, {
    if(input$toggleDayOfTheWeekTableEntriesOne == TRUE){
      shinyjs::show("DayOfTheWeekEntriesBoxTableOne")
    }
    else{
      shinyjs::hide("DayOfTheWeekEntriesBoxTableOne")
    }
  })
  
  
  stopReactiveOne <- reactive({
    do.call("rbind", list(subset(Data1Merged, Data1Merged$STATION_NAME == input$StopOne), subset(Data2Merged,Data2Merged$STATION_NAME == input$StopOne), subset(Data3Merged,Data3Merged$STATION_NAME == input$StopOne), subset(Data4Merged,Data4Merged$STATION_NAME == input$StopOne), subset(Data5Merged, Data5Merged$STATION_NAME == input$StopOne), subset(Data6Merged, Data6Merged$STATION_NAME == input$StopOne), subset(Data7Merged, Data7Merged$STATION_NAME == input$StopOne), subset(Data8Merged, Data8Merged$STATION_NAME == input$StopOne))) 
  })
  
  stopReactiveOneTable <- reactive({
    do.call("rbind", list(subset(Data1Merged, Data1Merged$STATION_NAME == input$StopOneTable & as.numeric(Data1Merged$Year) == as.numeric(input$YearOneTable)), subset(Data2Merged,Data2Merged$STATION_NAME == input$StopOneTable& as.numeric(Data2Merged$Year) == as.numeric(input$YearOneTable)), subset(Data3Merged,Data3Merged$STATION_NAME == input$StopOneTable& as.numeric(Data3Merged$Year) == as.numeric(input$YearOneTable)), subset(Data4Merged,Data4Merged$STATION_NAME == input$StopOneTable& as.numeric(Data4Merged$Year) == as.numeric(input$YearOneTable)), subset(Data5Merged, Data5Merged$STATION_NAME == input$StopOneTable& as.numeric(Data5Merged$Year) == as.numeric(input$YearOneTable)), subset(Data6Merged, Data6Merged$STATION_NAME == input$StopOneTable& as.numeric(Data6Merged$Year) == as.numeric(input$YearOneTable)), subset(Data7Merged, Data7Merged$STATION_NAME == input$StopOneTable& as.numeric(Data7Merged$Year) == as.numeric(input$YearOneTable)), subset(Data8Merged, Data8Merged$STATION_NAME == input$StopOneTable& as.numeric(Data8Merged$Year) == as.numeric(input$YearOneTable)))) 
  })
  
  yearReactive <- reactive({
    input$YearOne
  })
  
  yearReactiveTable <- reactive({
    input$YearOneTable
  })
  
  styleReactive <- reactive({
    if(input$Style == "First Style"){
      Style = providers$CartoDB.Positron
    }else if(input$Style == "Second Style"){
      Style = providers$Esri.WorldTopoMap
    }
    else if(input$Style == "Third Style"){
      Style = providers$Esri.NatGeoWorldMap
    }
  })
  
  orderReactive <- reactive({
    if(input$Order == "Low-High"){
      Order = "Low-High"
    }
    else{
      Order = "Alphabetical"
    }
  })
  
  dateReactive <- reactive({
    do.call("rbind", list(subset(Data1Merged, newDate == input$inputDate1), subset(Data2Merged, newDate == input$inputDate1), subset(Data3Merged, newDate == input$inputDate1), subset(Data4Merged, newDate == input$inputDate1), subset(Data5Merged, newDate == input$inputDate1), subset(Data6Merged, newDate == input$inputDate1), subset(Data7Merged, newDate == input$inputDate1), subset(Data8Merged, newDate == input$inputDate1)))
  })
  
  dateReactive2 <- reactive({
    do.call("rbind", list(subset(Data1Merged, newDate == input$inputDate2), subset(Data2Merged, newDate == input$inputDate2), subset(Data3Merged, newDate == input$inputDate2), subset(Data4Merged, newDate == input$inputDate2), subset(Data5Merged, newDate == input$inputDate2), subset(Data6Merged, newDate == input$inputDate2), subset(Data7Merged, newDate == input$inputDate2), subset(Data8Merged, newDate == input$inputDate2)))
  })

  dateReactiveTable <- reactive({
    do.call("rbind", list(subset(Data1Merged, newDate == input$inputDate2), subset(Data2Merged, newDate == input$inputDate2), subset(Data3Merged, newDate == input$inputDate2), subset(Data4Merged, newDate == input$inputDate2), subset(Data5Merged, newDate == input$inputDate2), subset(Data6Merged, newDate == input$inputDate2), subset(Data7Merged, newDate == input$inputDate2), subset(Data8Merged, newDate == input$inputDate2)))
  })

  
  output$dashboardTabTable <- renderPlot({
    ggplot(DataFirstDay, aes(x = stationname, y = rides)) + geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle=90)) + labs(y= "Entries", x = "Stations")
  })
  output$dashboardTabTable2 <- renderPlot({
    ggplot(DataFirstDay, aes(x = stationname, y = rides)) + geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle=90)) + labs(y= "Entries", x = "Stations")
  })
  
  output$chartTabChart <- renderPlot({
    dates <- dateReactive2()
    order <- orderReactive()
    if(order == "Low-High"){
      ggplot(dates, aes(reorder(stationname, rides), rides)) + geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle=90)) + labs(y= "Entries", x = "Stations")
    }else{
      ggplot(dates, aes(x = stationname, y = rides)) + geom_bar(stat="identity", fill="steelblue") + theme(axis.text.x = element_text(angle=90)) + labs(y= "Entries", x = "Stations")
    }
  })
  
  output$barChartEntriesTable <- DT::renderDataTable(
    DT::datatable(
      {
        table <- dateReactiveTable()[c(9,5)]
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  output$dashboardTabMap <- renderLeaflet({
    leaflet(DataFirstDay) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addCircleMarkers(lng=DataFirstDay$Lon, lat=DataFirstDay$Lat, popup=paste("Station: ", DataFirstDay$STATION_NAME, "\nEntries: ", DataFirstDay$rides)) %>%
      addResetMapButton() %>%
      setView(-87.63245, 41.88425, zoom = 10)
  })
  output$dashboardTabMap2 <- renderLeaflet({
    leaflet(DataFirstDay) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addCircleMarkers(lng=DataFirstDay$Lon, lat=DataFirstDay$Lat, popup=paste("Station: ", DataFirstDay$STATION_NAME, "\nEntries: ", DataFirstDay$rides)) %>%
      addResetMapButton() %>%
      setView(-87.63245, 41.88425, zoom = 10)
  })
  output$mapTabMap <- renderLeaflet({
    style <- styleReactive()
    date <- dateReactive()
    leaflet(date) %>%
      addProviderTiles(style) %>%
      addCircleMarkers(lng=date$Lon, lat=date$Lat, popup=paste("Station: ", date$STATION_NAME, "\nEntries: ", date$rides)) %>%
      addResetMapButton() %>%
      setView(-87.63245, 41.88425, zoom = 10)
  })

  output$DayEntriesOne <- renderPlot({
    stop <- stopReactiveOne()
    year <- yearReactive()
    test <- subset(stop, as.numeric(stop$Year) == as.numeric(year))
    ggplot(test, aes(x = newDate, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "STATION_NAME"], "Daily Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Date")
  })
  output$MonthEntriesOne <- renderPlot({
    stop <- stopReactiveOne()
    year <- yearReactive()
    test <- subset(stop, as.numeric(stop$Year) == as.numeric(year))
    orderOfMonths <- factor(months(test$newDate), level = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
    ggplot(test, aes(x = orderOfMonths, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "STATION_NAME"], "Monthly Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Month")
  })
  output$DayOfTheWeekEntriesOne <- renderPlot({
    stop <- stopReactiveOne()
    year <- yearReactive()
    test <- subset(stop, as.numeric(stop$Year) == as.numeric(year))
    orderOfDays <- factor(weekdays(test$newDate), level = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    ggplot(test, aes(x = orderOfDays, y = rides)) + geom_bar(stat="identity", fill="steelblue") + labs(title = paste(stop[1, "STATION_NAME"], "Day of the Week Entries")) + theme(plot.title = element_text(hjust = 1, size = 10)) + labs(y= "Entries", x = "Day of The Week")
  })
  output$DayEntriesTableOne <- DT::renderDataTable(
    DT::datatable(
      {
        #taking out the specific columns (newDate and rides)
        stop <- stopReactiveOneTable()[c(6,5)]
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE,lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Date", "Entries")
    )
  )
  
  output$MonthEntriesTableOne <- DT::renderDataTable({
    DT::datatable(
      {
        #using the reactive data to create a new dataframe that tallies up the total number of entries per month
        stop <- stopReactiveOneTable()
        Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        #Source: https://www.statology.org/r-sum-column-with-condition/
        Entries <- c(sum(stop[which(months(stop$newDate)=="January"), 5]),sum(stop[which(months(stop$newDate)=="February"), 5]),sum(stop[which(months(stop$newDate)=="March"), 5]),sum(stop[which(months(stop$newDate)=="April"), 5]),sum(stop[which(months(stop$newDate)=="May"), 5]),sum(stop[which(months(stop$newDate)=="June"), 5]),sum(stop[which(months(stop$newDate)=="July"), 5]),sum(stop[which(months(stop$newDate)=="August"), 5]),sum(stop[which(months(stop$newDate)=="September"), 5]),sum(stop[which(months(stop$newDate)=="October"), 5]),sum(stop[which(months(stop$newDate)=="November"), 5]),sum(stop[which(months(stop$newDate)=="December"), 5]))
        df <- data.frame(Month, Entries)
        as.data.frame(df)
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE 
    )
  })
  
  output$DayOfTheWeekEntriesTableOne <- DT::renderDataTable(
    DT::datatable(
      {
        #using the reactive data to create a new dataframe that tallies up the total number of entries per day of the week
        stop <- stopReactiveOneTable()
        DayOfTheWeek <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
        #Source: https://www.statology.org/r-sum-column-with-condition/
        Entries <- c(sum(stop[which(weekdays(stop$newDate)=="Monday"), 5]),sum(stop[which(weekdays(stop$newDate)=="Tuesday"), 5]),sum(stop[which(weekdays(stop$newDate)=="Wednesday"), 5]),sum(stop[which(weekdays(stop$newDate)=="Thursday"), 5]),sum(stop[which(weekdays(stop$newDate)=="Friday"), 5]),sum(stop[which(weekdays(stop$newDate)=="Saturday"), 5]),sum(stop[which(weekdays(stop$newDate)=="Sunday"), 5]))
        df <- data.frame(DayOfTheWeek, Entries)
        as.data.frame(df)
      },
      #disabled searching, changing length, and displaying row names
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Day of the Week", "Entries")  
    )
  )
  
}
shinyApp(ui, server)
