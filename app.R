library(shiny)
library(waiter)
library(slider)
library(leaflet)
library(tidyverse)
library(highcharter)
library(shinyMobile)
library(RColorBrewer)
library(leaflet.extras)
library(shinycssloaders)

rm(list = ls())

# download.file('https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_500k.json', 'us_county_geojson.json')
# county_geojson <- rgdal::readOGR('us_county_geojson.json')
# saveRDS(county_geojson, 'county_geojson.rds')
covid_counties <- read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv', stringsAsFactors = F) %>% mutate(date = as.Date(date)) %>% 
  mutate(fips = as.character(fips),
         fips = ifelse(nchar(fips) == 4, paste0(0, fips), fips),
         fips = ifelse(county == 'New York City', 36061, fips)) %>% 
  filter(county != 'Unknown')
covid_states <- read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv', stringsAsFactors = F) %>% mutate(date = as.Date(date))
county_geojson <- readRDS('county_geojson.rds')

state_fips_lookup <- covid_states %>% 
  select(state, fips) %>% 
  unique() %>% 
  mutate(fips = ifelse(nchar(fips) == 1, paste0(0, fips), fips))

doubling_time <- function(growth_rate) {
  log(2) / log(1 + growth_rate)
}

county_calc <- covid_counties %>% 
  group_by(county, state, fips) %>% 
  arrange(date) %>% 
  filter(cases >= lag(cases), deaths >= lag(deaths)) %>% 
  filter(max(cases) >= 10) %>% 
  mutate(days_since_10th_case = as.numeric(date - min(date[cases >= 10])),
         log_cases = log(cases)) %>% 
  mutate(daily_growth_rate = (cases - lag(cases, 1)) / lag(cases, 1),
         growth_rate_rolling_average = slide_dbl(daily_growth_rate, mean, .before = 3, .complete = F),
         days_to_double = doubling_time(growth_rate_rolling_average)) %>% 
  ungroup()

county_double_days <- county_calc %>% 
  group_by(county, state, fips) %>% 
  filter(date == max(date)) %>% 
  arrange(state, county) %>% 
  filter(!is.na(days_to_double), cases >= 10, days_to_double != Inf) %>% 
  select(county, state, days_to_double, fips) %>% 
  ungroup()

county_calc %>% 
  filter(fips == 50027)

pal <- colorNumeric('RdYlGn', county_double_days$days_to_double[!is.na(county_double_days$days_to_double) & county_double_days$days_to_double != Inf], na.color = 'transparent')

county_double_days$color <- pal(county_double_days$days_to_double)

county_geojson$fips <- str_replace_all(county_geojson$GEO_ID, '0500000US', '')

# county_geojson$deaths <- map(unique(county_geojson$fips), function(x) {
#   df <- covid_counties %>% 
#     filter(fips == x) 
#   
#   if (nrow(df) > 0) {
#     deaths <- df %>% 
#       filter(date == max(date)) %>% 
#       pull(deaths)
#   } else {
#     deaths <- 0
#   }
#   return(deaths)
# }) %>% unlist()

county_geojson$cases <- map(unique(county_geojson$fips), function(x) {
  df <- covid_counties %>% 
    filter(fips == x) 
  
  if (nrow(df) > 0) {
    cases <- df %>% 
      filter(date == max(date)) %>% 
      pull(cases)
  } else {
    cases <- 0
  }
  return(cases)
}) %>% unlist()

county_geojson$days_to_double <- map(unique(county_geojson$fips), function(x) {
  df <- county_double_days %>% 
    filter(fips == x) 
  
  if (nrow(df) > 0) {
    if (df$days_to_double != Inf) {
      days_to_double <- df$days_to_double 
    } else {
      days_to_double <- NA
    }
  } else {
    days_to_double <- NA
  }
  return(days_to_double)
}) %>% unlist()


ui <- f7Page(
  title = 'COVID-19 US County Dashboard',
  init = f7Init(theme = "dark"),
  f7SingleLayout(
    navbar = f7Navbar(
      title = 'COVID-19 Dashboard',
      hairline = FALSE,
      shadow = TRUE,
      bigger = TRUE
    ),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
    use_waiter(),
    waiter_show_on_load(html = div(spin_loaders(42), HTML('<h5>Pulling latest data from The New York Times.<br>This may take a minute.</h5>'))),
    uiOutput('ui_layout'),
    f7Row(
      f7Col(
        f7Card(
          title = '',
          uiOutput('notes_ui')
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  req(nrow(covid_counties) > 0)
  
  output$ui_layout <- renderUI({
    req(input$deviceInfo)
    if (input$deviceInfo$desktop) {
      ui <- f7Row(
        f7Col(
          f7Card(
            title = '',
            uiOutput('state_title'),
            withSpinner(leafletOutput('county_map'), type = 3, color = '#71eeb8', color.background = '#1c1c1d'),
          )
        ), 
        f7Col(
          f7Card(
            title = '',
            uiOutput('state_title_chart'),
            withSpinner(highchartOutput('county_chart'), type = 3, color = '#71eeb8', color.background = '#1c1c1d')
          )
        )
      )
    } else {
      ui <- f7Row(
        f7Col(
          f7Card(
            title = '',
            uiOutput('state_title'),
            leafletOutput('county_map'),
          ),
          f7Card(
            title = '',
            uiOutput('state_title_chart'),
            highchartOutput('county_chart')
          )
        )
      )
    }
    return(ui)
  })
  
  
  county_data <- reactive({
    req(input$state)
    waiter_hide()
    county_calc %>% 
      filter(days_since_10th_case >= 0, state == input$state) %>% 
      rowwise() %>% 
      mutate(tooltip = htmltools::HTML(
        paste0('<b><span style="font-size:14px">', county, '</span></b><br>',
               format(date, '%m/%d'), ' - ', days_since_10th_case, ' days since 10th case<br><br>',
               '<span style="font-size:14px"><b>', prettyNum(cases, big.mark = ','), '</b> total cases<br>',
               'Doubling every <b>', round(days_to_double, 1), '</b> days</span>'
        )
      )) %>% 
      ungroup()
  })
  
  output$state_title <- renderUI({
    div(style = 'display:inline-block',
        div(style = 'display:inline-block;vertical-align:middle;', h2('COVID-19 case doubling rate by county:')),
        div(style = 'display:inline-block;vertical-align:middle;margin-bottom:7px;font-size:20px !important;', f7Select('state', '', choices = sort(unique(county_calc$state[!county_calc$state %in% c('Guam', 'Virgin Islands')])), selected = 'New York'))
    )
  })
  
  output$state_title_chart <- renderUI({
    div(style = 'display:inline-block',
        div(style = 'display:inline-block;vertical-align:middle;', h2(paste0('COVID-19 cases by county (log scale) over time: ', input$state)))
    )
  })
  
  output$county_chart <- renderHighchart({
    county_data() %>% 
      hchart(hcaes(x = days_since_10th_case, y = log_cases, group = county), type = 'line') %>% 
      hc_tooltip(formatter = JS('function() {return this.point.tooltip;}'), useHTML = TRUE) %>%
      hc_xAxis(title = list(text = 'Days since 10th case', style = list(fontSize = '18px', color = '#fff')), allowDecimals = FALSE, labels = list(style = list(fontSize = '16px', color = '#fff'))) %>% 
      hc_yAxis(title = list(text = NA), labels = list(formatter = JS("function() {return Math.round(Math.pow(Math.E, this.value)).toLocaleString('en')}"), style = list(fontSize = '16px', color = '#fff'))) %>% 
      hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>% 
      hc_colors(county_double_days$color[county_double_days$county %in% county_data()$county & county_double_days$state %in% county_data()$state]) %>%
      hc_legend()
  })
  
  output$county_map <- renderLeaflet({
    
    selected_state_fips <- state_fips_lookup$fips[state_fips_lookup$state == input$state]
    # selected_state_fips <- state_fips_lookup$fips[state_fips_lookup$state == 'Washington']
    
    this_state <- county_geojson[county_geojson$STATE == selected_state_fips,]
    
    leaflet(this_state, options = leafletOptions(
      zoomControl = FALSE,
      dragging = FALSE,
      attributionControl = FALSE)) %>%
      addPolygons(smoothFactor = 0.5, fillOpacity = 1,
                  color = 'grey', weight = 1,
                  fillColor = ~pal(days_to_double),
                  label = map(paste0('<b>', this_state$NAME, "</b><br>", 
                                     '<b>', prettyNum(this_state$cases, big.mark = ','), '</b>', ' total cases<br>',
                                     ifelse(!is.na(this_state$days_to_double), 
                                            paste0('Cases doubling every ', ifelse(round(this_state$days_to_double, 1) == 1, '', paste0('<b>', round(this_state$days_to_double, 1), '</b>')), ifelse(round(this_state$days_to_double, 1) == 1, ' <b>day</b>', ' days')), 'Not enough data to<br>calculate doubling rate')
                  ), HTML),
                  labelOptions = labelOptions(textsize = "15px"),
                  highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = county_double_days$days_to_double, opacity = 1,
                labFormat = labelFormat(transform = function(x) round(x, 1)),
                title = HTML('Days until<br>cases double'), position = 'topleft') %>% 
      setMapWidgetStyle(list(background = "#1c1c1d"))
  })
  
  output$notes_ui <- renderUI({
    req(input$deviceInfo)
    if (input$deviceInfo$desktop) {
      ui <- HTML(
        paste0(
          'Case doubling rate is calculated based on a three day rolling average of daily case growth rates.
            <br>Only counties with at least 10 cases are included.
            <br>Note that some data comes from "Unknown" counties and is labelled as such. Also note that cases from all of New York City are labelled as New York County (Manhattan).
            <br>Data from <a href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html" target="_blank" class="link external">The New York Times</a>, last updated ',
          format(max(covid_counties$date), '%B %d, %Y'), '. See <a href="https://github.com/charlie86/covid-dashboard" target="_blank" class="link external">GitHub</a> for code. Developed by <a href="https://www.thompsonanalytics.com/" target="_blank" class="link external">Charlie Thompson</a>.'
        )
      )
    } else {
      ui <- HTML(
        paste0(
          'Case doubling rate is calculated based on a three day rolling average of daily case growth rates.
            <br>Only counties with at least 10 cases are included.
            <br>Note that some data comes from "Unknown" counties and is labelled as such.
            <br>Also note that cases from all of New York City are labelled as New York County (Manhattan).
            <br>Data from <a href="https://github.com/nytimes/covid-19-data" target="_blank" class="link external">The New York Times</a>, last updated ', format(max(covid_counties$date), '%B %d, %Y'), 
          '. <br>See <a href="https://github.com/charlie86/covid-dashboard" target="_blank" class="link external">GitHub</a> for code. 
          <br>Developed by <a href="https://www.thompsonanalytics.com/" target="_blank" class="link external">Charlie Thompson</a>.'
        )
      )
    }
    return(ui)
  })
  
}

shinyApp(ui, server)