library(furrr)
library(shiny)
library(waiter)
library(slider)
library(leaflet)
library(tidyverse)
library(highcharter)
library(shinyMobile)
library(RColorBrewer)
library(shinymaterial)
library(leaflet.extras)
plan(multiprocess)

rm(list = ls())

# download.file('https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_500k.json', 'us_county_geojson.json')
# county_geojson <- rgdal::readOGR('us_county_geojson.json')
# saveRDS(county_geojson, 'county_geojson.rds')
covid_counties <- read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv', stringsAsFactors = F) %>% mutate(date = as.Date(date)) %>% 
  mutate(fips = as.character(fips),
         fips = ifelse(nchar(fips) == 4, paste0(0, fips), fips))
covid_states <- read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv', stringsAsFactors = F) %>% mutate(date = as.Date(date))
county_geojson <- readRDS('county_geojson.rds')

state_fips_lookup <- covid_states %>% 
  select(state, fips) %>% 
  unique() %>% 
  mutate(fips = ifelse(nchar(fips) == 1, paste0(0, fips), fips))

doubling_time <- function(growth_rate) {
  log(2) / log(1 + growth_rate)
}

case_tooltip <- "function() {
       var points = this.points;
            var pointsLength = points.length;
            var tooltipMarkup = pointsLength ? '<span style=\"font-size: 10px\">' + points[0].key + ' days since 10th case</span><br/>' : '';
            var index;
            var y_value_exp;

            for(index = 0; index < pointsLength; index += 1) {
              y_value_exp = Math.round(Math.pow(Math.E, points[index].y)).toLocaleString('en');
              tooltipMarkup += '<span style=\"color:' + points[index].series.color + '\">\u25CF</span> ' + points[index].series.name + ': <b>' + y_value_exp  + ' cases</b><br/>';
            }

            return tooltipMarkup;
}"

death_tooltip <- "function() {
       var points = this.points;
            var pointsLength = points.length;
            var tooltipMarkup = pointsLength ? '<span style=\"font-size: 10px\">' + points[0].key + ' days since 10th death</span><br/>' : '';
            var index;
            var y_value_exp;

            for(index = 0; index < pointsLength; index += 1) {
              y_value_exp = Math.round(Math.pow(Math.E, this.y)).toLocaleString('en');
              tooltipMarkup += '<span style=\"color:' + this.series.color + '\">\u25CF</span> ' + this.series.name + ': <b>' + y_value_exp + ' deaths</b><br/>';
            }

            return tooltipMarkup;
}"

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

# View(county_calc %>% filter(county == 'New York City'))

county_double_days <- county_calc %>% 
  group_by(county, state, fips) %>% 
  filter(date == max(date)) %>% 
  arrange(state, county) %>% 
  filter(!is.na(days_to_double), cases >= 10, days_to_double != Inf) %>% 
  select(county, state, days_to_double, fips) %>% 
  ungroup()

pal <- colorNumeric('RdYlGn', county_double_days$days_to_double[!is.na(county_double_days$days_to_double) & county_double_days$days_to_double != Inf], na.color = 'transparent')

county_double_days$color <- pal(county_double_days$days_to_double)

county_geojson$fips <- str_replace_all(county_geojson$GEO_ID, '0500000US', '')

county_geojson$deaths <- future_map(unique(county_geojson$fips), function(x) {
  df <- covid_counties %>% 
    filter(fips == x) 
  
  if (nrow(df) > 0) {
    deaths <- df %>% 
      filter(date == max(date)) %>% 
      pull(deaths)
  } else {
    deaths <- 0
  }
  return(deaths)
}) %>% unlist()

county_geojson$cases <- future_map(unique(county_geojson$fips), function(x) {
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
  title = 'COVID US County Dashboard',
  init = f7Init(theme = "light"),
  f7SingleLayout(
    navbar = f7Navbar(
      title = 'COVID US County Dashboard',
      hairline = FALSE,
      shadow = TRUE,
      bigger = TRUE
    ),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
    use_waiter(),
    waiter_show_on_load(html = div(spin_loaders(42), h5('Initializing app'))),
    f7Row(
      f7Col(
        f7Card(
          title = '',
          uiOutput('state_title'),
          leafletOutput('county_map'),
        )
      ), 
      f7Col(
        f7Card(
          title = '',
          uiOutput('state_title_chart'),
          highchartOutput('county_chart')
        )
      )
    ),
    f7Row(
      f7Col(
        f7Card(
          title = '', 
            HTML(
          paste0(
            'Only counties with at least 10 cases are included. Doubling rate is calculated based on a three day rolling average of daily case growth rates.
            <br>Data from <a href="https://github.com/nytimes/covid-19-data">The New York Times</a>, last updated ',
            format(max(covid_counties$date), '%B %d, %Y'), '. See <a href="https://github.com/charlie86/covid-dashboard">GitHub</a> for code.'
            )
          )
        )
      )
    )
    # useShinyalert(),
    # uiOutput('inputs'),
    # uiOutput('charts')
  )
)

# ui <- f7Page(title = 'COVID stats',
#                     material_row(
#                       material_column(width = 3, 
#                                       material_card(title = '',
#                                         'hi'
#                                       )
#                       ),
#                       material_column(width = 9,
#                                       material_card(
#                                         title = uiOutput('state_title'),
#                                         HTML('Green indicates a flatter curve; red reflects a steeper growth rate. Only counties with at least 10 cases are included. Data from <a href="https://github.com/nytimes/covid-19-data" target="_blank">The New York Times</a>.'),
#                                         highchartOutput('county_chart', height = '600px')
#                                       ))
#                     )
# )

server <- function(input, output, session) {
  
  req(nrow(covid_counties) > 0)
  
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
        div(style = 'display:inline-block;vertical-align:middle;', h2('COVID-19 total cases by county:')),
        div(style = 'display:inline-block;vertical-align:middle;margin-bottom:7px;font-size:20px !important;', f7Select('state', '', choices = sort(unique(county_calc$state)), selected = 'New York'))
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
      hc_xAxis(title = list(text = 'Days since 10th case', style = list(fontSize = '18px')), allowDecimals = FALSE, labels = list(style = list(fontSize = '16px'))) %>% 
      hc_yAxis(title = list(text = 'Cases (log scale)', style = list(fontSize = '18px')), labels = list(formatter = JS("function() {return Math.round(Math.pow(Math.E, this.value)).toLocaleString('en')}"), style = list(fontSize = '16px'))) %>% 
      hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>% 
      hc_colors(county_double_days$color[county_double_days$county %in% county_data()$county & county_double_days$state %in% county_data()$state]) %>%
      hc_legend()
  })
  
  output$county_map <- renderLeaflet({
    
    selected_state_fips <- state_fips_lookup$fips[state_fips_lookup$state == input$state]
    
    this_state <- county_geojson[county_geojson$STATE == selected_state_fips,]
    
    leaflet(this_state) %>%
      addPolygons(smoothFactor = 0.5, fillOpacity = 1,
                  color = 'grey', weight = 1,
                  fillColor = ~pal(days_to_double),
                  label = map(paste0('<b>', this_state$NAME, "</b>: <br>", 
                                     '<b>', prettyNum(this_state$cases, big.mark = ','), '</b>', ' total cases<br>',
                                     ifelse(!is.na(this_state$days_to_double), 
                                            paste0('Cases doubling every ', ifelse(round(this_state$days_to_double, 1) == 1, '', paste0('<b>', round(this_state$days_to_double, 1), '</b>')), ifelse(round(this_state$days_to_double, 1) == 1, ' <b>day</b>', ' days')), 'Not enough data to<br>calculate doubling rate')
                  ), HTML),
                  labelOptions = labelOptions(textsize = "15px"),
                  highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = county_double_days$days_to_double, opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(x, 1)),
                title = 'Days until cases double') %>% 
      setMapWidgetStyle(list(background = "white"))
  })
  
  # state_plot_df <- covid_states %>% 
  #   group_by(fips) %>% 
  #   filter(cases >= lag(cases), deaths >= lag(deaths)) %>%
  #   filter(max(deaths) >= 10) %>% 
  #   mutate(days_since_10th_death = as.numeric(date - min(date[deaths >= 10])),
  #          log_deaths = log(deaths)) %>% 
  #   ungroup() %>% 
  #   left_join(state_lookup, by = c('state' = 'state.name')) %>% 
  #   group_by(fips) %>% 
  #   mutate(daily_growth_rate = (deaths - lag(deaths, 1)) / lag(deaths, 1),
  #          growth_rate_rolling_average = slide_dbl(daily_growth_rate, mean, .before = 3, .complete = F),
  #          days_to_double = doubling_time(growth_rate_rolling_average))
  # 
  # states_double_days <- state_plot_df %>% 
  #   group_by(fips) %>% 
  #   filter(date == max(date)) %>% 
  #   arrange(state) %>% 
  #   filter(!is.na(days_to_double)) %>% 
  #   select(state, fips, days_to_double)
  # 
  # state_plot_df %>% 
  #   inner_join(select(states_double_days, fips), by = 'fips') %>% 
  #   filter(days_since_10th_death >= 0) %>% 
  #   rowwise() %>% 
  #   mutate(tooltip = htmltools::HTML(
  #     paste0('<b><span style="font-size:14px">', state, '</span></b><br>',
  #            format(date, '%m/%d'), ' - ', days_since_10th_death, ' days since 10th death<br><br>',
  #            '<span style="font-size:14px"><b>', deaths, '</b> total deaths<br>',
  #            'Doubling every <b>', round(days_to_double, 1), '</b> days</span>'
  #     )
  #   )) %>% 
  #   ungroup() %>% 
  #   hchart(hcaes(x = days_since_10th_death, y = log_deaths, group = state), type = 'line') %>% 
  #   hc_tooltip(formatter = JS('function() {return this.point.tooltip;}'), useHTML = TRUE) %>%
  #   hc_xAxis(title = list(text = 'Days since 10th death'), allowDecimals = FALSE) %>% 
  #   hc_yAxis(title = list(text = 'Deaths (log scale)'), labels = list(formatter = JS("function() {return Math.round(Math.pow(Math.E, this.value))}"))) %>% 
  #   hc_plotOptions(line = list(marker = list(enabled = F))) %>% 
  #   hc_add_theme(hc_theme_smpl()) %>% 
  #   hc_colors(colorize(states_double_days$days_to_double, brewer.pal(3, 'RdYlGn'))) %>%
  #   hc_title(text = 'COVID-19 deaths by US state') %>% 
  #   hc_subtitle(text = htmltools::HTML('Green indicates a flatter curve; red reflects a steeper growth rate. Only states with at least 10 deaths are included. Data from <a href="https://github.com/nytimes/covid-19-data" target="_blank">The New York Times</a>.')) %>% 
  #   hc_legend()
  
  
}

shinyApp(ui, server)