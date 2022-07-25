#####################################
## Stop and search in London boroughs
## 'Your Map' - StopWatch
## Thiago R. Oliveira
## 2022


# load necessary packages
library(shiny)
library(leaflet)
library(readxl)
library(readODS)
library(sf)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(ggplot2)
library(kableExtra)

## PREAMBLE

# Define colour palette
pal <- colorBin(palette = "Reds", 
                domain = c(0:2000))

# Read shapefile: London boroughs
boroughs <- st_read('data/statistical-gis-boundaries-london 2/ESRI/London_Borough_Excluding_MHW.shp') %>%
  st_transform(crs = 4326) %>%
  mutate(NAME = factor(NAME))

# read stop and search data: december 2017 - october 2021
load('data/ss_dec17oct21.RData')

# read stop and search data and link with boroughs shapefile
data <- ss_dec17oct21 %>%
    mutate(month = format(Date, format = "%Y-%m"),
           Legislation = as.factor(Legislation),
           ethnicity = case_when(
               Self.defined.ethnicity %in% c('Asian/Asian British - Any other Asian background',
                                             'Asian/Asian British - Pakistani',
                                             'Asian/Asian British - Bangladeshi',
                                             'Asian/Asian British - Indian') ~ "Asian (or Asian British)",
               Self.defined.ethnicity %in% c('Black/African/Caribbean/Black British - African',
                                             'Black/African/Caribbean/Black British - Any other Black/African/Caribbean background',
                                             'Black/African/Caribbean/Black British - Caribbean') ~ 'Black (or Black British)',
               Self.defined.ethnicity %in% c('Mixed/Multiple ethnic groups - Any other Mixed/Multiple ethnic background',
                                             'Mixed/Multiple ethnic groups - White and Asian',
                                             'Mixed/Multiple ethnic groups - White and Black African',
                                             'Mixed/Multiple ethnic groups - White and Black Caribbean') ~ 'Mixed',
               Self.defined.ethnicity %in% c('Asian/Asian British - Chinese',
                                             'Other ethnic group - Any other ethnic group',
                                             'Other ethnic group - Not stated') ~ 'Chinese or Other Ethnic Group',
               Self.defined.ethnicity %in% c('White - Any other White background',
                                             'White - English/Welsh/Scottish/Northern Irish/British',
                                             'White - Irish') ~ 'White',
               Self.defined.ethnicity %in% "" | is.na(Self.defined.ethnicity) ~ 'Not Stated / Unknown'
           )) %>%
    drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(boroughs)) %>%
  st_join(boroughs %>% select(NAME, geometry)) %>%
  st_drop_geometry()

# read data: ethnic profile by boroughs (2011 census)
ethnicity_by_boroughs <- read_ods('data/Ethnicity by London borough 2011 census.ods', sheet = 2) %>%
    rename(borough = "") %>%
    dplyr::filter(borough != 'London' & borough != 'Inner London' & borough != 'Outer London') %>%
    mutate(borough = factor(borough))


### APP
## Define UI

ui <- bootstrapPage(
    fluidPage(
        
        # Define title
        h1("Stop and search in London boroughs"),
        
        # First output: map
        leafletOutput("mymap"),
        
        # Second output: 
        sidebarLayout(
            # Left hand side: Panel to select legislation, date, and borough
            sidebarPanel(
                selectInput("select.legislation", "Which legislation would like to visualise?",
                            choices = levels(data$Legislation),
                            selected = "Police and Criminal Evidence Act 1984 (section 1)"),
                dateRangeInput("select.date", "Select date range", start = min(data$Date), end = max(data$Date),
                              min = min(data$Date), max = max(data$Date)),
                selectInput("select.borough", "Which borough would you like to visualise?",
                            choices = levels(boroughs$NAME)),
                textOutput("my.borough")
            ),
            # Right hand side: plot time series of stop and search count
            mainPanel(
                plotOutput("myplotF",
                           hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                uiOutput("hover_info")
            )
        ),
        
        # Third output:
        sidebarLayout(
            # Left hand side: text about ethnic disparities
            sidebarPanel(
                textOutput("text.disparities")
            ),
            # Right hand side: table comparing S&S and ethnic profile  
            mainPanel(
                tableOutput("table.disparities")
            )
        ),
        
        # Fourth output
        sidebarLayout(
            # Left hand side: ethnic disparities in London
            sidebarPanel(
                plotOutput("barplot.london")
            ),
            # Right hand side: ethnic disparities in borough
            mainPanel(
                plotOutput("barplot.borough", width = "85%")
            )
        ),
        
        # Fifth output
        sidebarLayout(
          # Left hand side: odds ratios in London
          sidebarPanel(
            plotOutput("oddsratios.london")
          ),
          # Right hand side: odds ratios in borough
          mainPanel(
            plotOutput("oddsratios.borough", width = "85%")
          )
        )
        
    )
)


## DEFINE SERVER

server <- function(input, output, session) {
  
    # define new data: filter based on user input
    new.data <- reactive({
        req(input$select.legislation, input$select.date)
        data %>%
          filter(Legislation == input$select.legislation) %>%
          filter(Date >= input$select.date[1] & Date <= input$select.date[2]) %>%
          count(NAME, name = "stops") %>%
          drop_na()
    })

    #  define map
    output$mymap <- renderLeaflet({

        boroughs_stops <- boroughs %>%
          mutate(stops = new.data()$stops)
        
        pal <- colorBin(palette = "Reds", 
                        domain = c(0:max(boroughs_stops$stops, na.rm = T)))
        
        leaflet(boroughs_stops) %>% 
            addProviderTiles("Stamen.TonerBackground") %>% 
            addPolygons(color = "red",
                        label = ~NAME,
                        fillColor = ~pal(boroughs_stops$stops),
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 1,
                        layerId = ~NAME) %>%
            addLegend("topright",
                      pal = pal,
                      values = ~stops,
                      title = paste(input$select.legislation),
                      opacity = 1) #%>%
    })
    
    # info provided by user: select police force area through clicking on map
    click <- observe({
        my.click <- input$mymap_shape_click
        
        updateSelectInput(session,
                          inputId = "select.borough",
                          label = "Which borough would you like to visualise?",
                          choices = levels(boroughs$NAME),
                          selected = my.click$id)
    })
    
    # prepare data for time series plot
    dataplot <- reactive({
        req(input$select.legislation, input$select.borough)
        data %>%
          filter(Legislation == input$select.legislation)
    })
    
    # define time series plot
    output$myplotF <- renderPlot({
            event <- input$mymap_shape_click$id
            
            this.borough <- 
                dataplot() %>% 
                filter(NAME == input$select.borough) %>%
                group_by(month) %>%
                summarise(n_stops = n())
            
            dataplot_london <- 
                dataplot() %>%
                group_by(month) %>%
                summarise(n_stops = round(n()/33,2)) %>%
                as.data.frame()
            
            myplotF <- ggplot(this.borough, aes(y = value, x = month, group = 1)) + 
                geom_point(aes(y = n_stops, colour = input$select.borough)) + geom_line(aes(y = n_stops, colour = input$select.borough)) +
                geom_point(data = dataplot_london, aes(y = n_stops, colour = "London (average)")) + geom_line(data = dataplot_london, aes(y = n_stops, colour = "London (average)")) +
                ylim(0,pmax(this.borough$n_stops, dataplot_london$n_stops, na.rm = T) %>% max()) +
                labs(y = paste(input$select.legislation), x = "", title = paste(input$select.legislation, "searches in London (average) and in", input$select.borough)) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                      panel.background = element_blank(), axis.line = element_line(colour = "black"),
                      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                      plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 14)) + 
                scale_color_manual(values = c("black", "red"),
                                   breaks = c("London (average)", input$select.borough),
                                   labels = c("London (average)", input$select.borough)) + 
                guides(color = guide_legend(title = ""))
            
            myplotF
    })
    
    output$hover_info <- renderUI({
      hover <- input[["plot_hover"]]
      if(is.null(hover)) return(NULL)
      
      this.borough <- 
        dataplot() %>% 
        #filter(borough == event) %>%
        filter(NAME == input$select.borough) %>%
        group_by(month) %>%
        summarise(n_stops = n())
      
      point <- nearPoints(this.borough, 
                          hover, threshold = 5, maxpoints = 1)
      if(nrow(point) == 0) return(NULL)
      
      left_px <- hover$coords_css$x
      top_px  <- hover$coords_css$y
      
      style <- paste0(
        "position:absolute; z-index:100; pointer-events:none; ", 
        "background-color: rgba(245, 245, 245, 0.85); ",
        "left:", left_px, 
        "px; top:", top_px, "px;"
      )
      
      # tooltip created as wellPanel
      tooltip <- paste0(
        "<b> Number of searches: </b>",     point[["n_stops"]],     "<br/>"
      )
      wellPanel(
        style = style, p(HTML(tooltip))
      )
    })
            
    # prepare data for texts
    data.n <- reactive({
        req(input$select.legislation, input$select.date, input$select.borough)
        data %>%
            filter(Legislation == input$select.legislation) %>%
            filter(Date >= input$select.date[1] & Date <= input$select.date[2])
            
    })
    
    # output: text about ethnic disparities
    output$my.borough <- renderText({
        paste(data.n() %>% filter(NAME == input$select.borough) %>% nrow(), "stops based on the", input$select.legislation, "legislation were recorded in the borough of", 
              input$select.borough, "between", input$select.date[1], "and", input$select.date[2], ". This is",
              if_else(data.n() %>% filter(NAME == input$select.borough) %>% nrow() > (data.n() %>% nrow() / 33), paste("above"), paste("below")), "the average number of",
              input$select.legislation, "searches in London in this period, which is", round(nrow(data.n()) / 33, 2), ".")
    })
    
    output$text.disparities <- renderText({
        paste("Considering all ", input$select.legislation, " searches in ", input$select.borough, " between ", input$select.date[1], " and ", input$select.date[2], 
              ", ", 
              round((data.n() %>% filter(NAME == input$select.borough & ethnicity == "Black (or Black British)") %>% nrow() / data.n() %>% filter(NAME == input$select.borough) %>% nrow()) * 100, 2), 
              "% were against people self-defined as Black (or Black British), while ", 
              round((data.n() %>% filter(NAME == input$select.borough & ethnicity == "White") %>% nrow() / data.n() %>% filter(NAME == input$select.borough) %>% nrow()) * 100, 2),
              "% were against people self-defined as White. According to Census 2011, ", 
              round(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Black or Black British'] / ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total'] * 100, 2),
              "% of the population living in ", input$select.borough, " are Black or Black British, whereas ",
              round(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'White'] / ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total'] * 100, 2),
              "% are White.",
              sep = "")
    })
    
    # table: ethnic disparities
    output$table.disparities <- function(){
        this.brgh <- 
            data.n() %>% 
            filter(NAME  == input$select.borough) %>%
            mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black (or Black British)', 'Mixed', 'Asian (or Asian British)', 'Chinese or Other Ethnic Group', 'Not Stated / Unknown')))
        this.brgh %>%
            group_by(ethnicity, .drop = FALSE) %>%
            summarise(n = n(),
                      prop = (n() / nrow(this.brgh)) * 100,
                      .groups = "drop") %>%
            as.data.frame() %>%
            mutate(pop = c(round(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'White'] / 
                                     ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total'] * 100, 2),
                           round(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Black or Black British'] / 
                                     ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total'] * 100, 2),
                           round(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Mixed'] / 
                                     ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total'] * 100, 2),
                           round(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Asian or Asian British'] / 
                                     ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total'] * 100, 2),
                           round(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Chinese or Other Ethnic Group'] / 
                                     ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total'] * 100, 2),
                           0)
            ) %>%
            kbl("html", digits = 2, col.names = c('Self-defined ethnic group', 
                                                  paste('Number of searches in', input$select.borough), 
                                                  paste('Percentage of searches in', input$select.borough, '(%)'),
                                                  paste('Ethnic composition in', input$select.borough, '(%)')
                                                  )) %>%
            kable_styling(bootstrap_options = c("striped", "hover"))
    }
    
    # barplot: London
    output$barplot.london <- renderPlot({
        ethn.london <- 
            data.n() %>%
            group_by(ethnicity) %>%
            summarise(prop = n() / nrow(data.n())) %>%
            mutate(type = 'stops') %>%
            bind_rows(
                tibble(ethnicity = c('Asian (or Asian British)', 'Black (or Black British)', 'Chinese or Other Ethnic Group',
                                     'Mixed', 'Not Stated / Unknown', 'White'),
                       prop = c(
                           sum(ethnicity_by_boroughs[, 'Asian or Asian British']) / sum(ethnicity_by_boroughs[, 'Total']),
                           sum(ethnicity_by_boroughs[, 'Black or Black British']) / sum(ethnicity_by_boroughs[, 'Total']),
                           sum(ethnicity_by_boroughs[, 'Chinese or Other Ethnic Group']) / sum(ethnicity_by_boroughs[, 'Total']),
                           sum(ethnicity_by_boroughs[, 'Mixed']) / sum(ethnicity_by_boroughs[, 'Total']),
                           0,
                           sum(ethnicity_by_boroughs[, 'White']) / sum(ethnicity_by_boroughs[, 'Total'])),
                       type = 'pop'
                )
            ) %>%
            mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black (or Black British)', 'Mixed', 'Asian (or Asian British)', 'Chinese or Other Ethnic Group', 'Not Stated / Unknown')))
        barplot.london <- ggplot(ethn.london, aes(y = prop, fill = type, x = ethnicity)) + 
            geom_bar(position="dodge", stat="identity") +
            labs(y = "", x = "", title = paste('Ethnic disparities in London', '(', input$select.legislation, ')')) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
                  legend.text = element_text(size = 10),
                  plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) + 
            scale_fill_manual(values = c("black", "red"),
                              labels = c("Ethnic composition in London", paste(input$select.legislation, "\n searches in London"))) + 
            guides(fill = guide_legend(title = ""))
        barplot.london
    })
    
    # barplot: borough
    output$barplot.borough <- renderPlot({
        ethn.borough <- 
            data.n() %>%
            filter(NAME  == input$select.borough) %>%
            group_by(ethnicity) %>%
            summarise(prop = n() / nrow(data.n() %>% 
                                          filter(NAME == input$select.borough))) %>%
            mutate(type = 'stops') %>%
            bind_rows(
                tibble(ethnicity = c('Asian (or Asian British)', 'Black (or Black British)', 'Chinese or Other Ethnic Group',
                                     'Mixed', 'Not Stated / Unknown', 'White'),
                       prop = c(
                           sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Asian or Asian British']) / sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total']),
                           sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Black or Black British']) / sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total']),
                           sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Chinese or Other Ethnic Group']) / sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total']),
                           sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Mixed']) / sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total']),
                           0,
                           sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'White']) / sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Total'])),
                       type = 'pop'
                )
            ) %>%
            mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black (or Black British)', 'Mixed', 'Asian (or Asian British)', 'Chinese or Other Ethnic Group', 'Not Stated / Unknown')))
        barplot.borough <- ggplot(ethn.borough, aes(y = prop, fill = type, x = ethnicity)) + 
            geom_bar(position="dodge", stat="identity") +
            labs(y = "", x = "", title = paste('Ethnic disparities in', input$select.borough, '(', input$select.legislation, ')')) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
                  legend.text = element_text(size=15),
                  plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 14)) + 
            scale_fill_manual(values = c("black", "red"),
                              labels = c(paste("Ethnic composition in", input$select.borough), 
                                         paste(input$select.legislation, "\n searches in", input$select.borough))) + 
            guides(fill = guide_legend(title = ""))
        barplot.borough
    })
    
    # odds ratios: london
    output$oddsratios.london <- renderPlot({
      
      odds.london <-
        tibble(
          ethnicity = c('White', 'Black (or Black British)', 'Mixed', 'Asian (or Asian British)', 'Chinese or Other Ethnic Group') %>% 
            factor(levels = c('White', 'Black (or Black British)', 'Mixed', 'Asian (or Asian British)', 'Chinese or Other Ethnic Group')),
          odds = c(
            1,
            
            round(
              (data.n() %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'Black (or Black British)') %>% pull(n) / 
                 sum(ethnicity_by_boroughs[, "Black or Black British"])) /
                (data.n() %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'White') %>% pull(n) / 
                   sum(ethnicity_by_boroughs[, 'White'])),
              2),
            
            round(
              (data.n() %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'Mixed') %>% pull(n) / 
                 sum(ethnicity_by_boroughs[, 'Mixed'])) /
                (data.n() %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'White') %>% pull(n) / 
                   sum(ethnicity_by_boroughs[, 'White'])),
              2),
            
            round(
              (data.n() %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'Asian (or Asian British)') %>% pull(n) / 
                 sum(ethnicity_by_boroughs[, 'Asian or Asian British'])) /
                (data.n() %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'White') %>% pull(n) / 
                   sum(ethnicity_by_boroughs[, 'White'])),
              2),
            
            round(
              (data.n() %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'Chinese or Other Ethnic Group') %>% pull(n) / 
                 sum(ethnicity_by_boroughs[, 'Chinese or Other Ethnic Group'])) /
                (data.n() %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'White') %>% pull(n) / 
                   sum(ethnicity_by_boroughs[, 'White'])),
              2)
            ),
          white = c(T, rep(F, 4))
        )
      
      barplot.odds.london <- ggplot(odds.london, aes(y = odds, x = ethnicity, fill = white)) + 
        geom_bar(position = "dodge", stat = "identity", width = .25) +
        geom_text(aes(label = odds), vjust = -.75, colour = "red", size = 5) +
        ylim(0,12) +
        labs(y = "", x = "", title = paste('ODDS RATIOS: Ethnic disparities in London (', input$select.legislation, ')', sep = "")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
              legend.text = element_text(size = 10), legend.position = "None",
              plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
        scale_x_discrete(breaks = c('White', 'Black (or Black British)', 'Mixed', 'Asian (or Asian British)', 'Chinese or Other Ethnic Group', 'Not Stated / Unknown')) + 
        scale_fill_manual(values = c("black", "red"),
                          labels = c("", ""))
      
      barplot.odds.london
          
    })
    
    # odds ratios: borough
    output$oddsratios.borough <- renderPlot({
      
      odds.borough <-
        tibble(
          ethnicity = c('White', 'Black (or Black British)', 'Mixed', 'Asian (or Asian British)', 'Chinese or Other Ethnic Group') %>% 
            factor(levels = c('White', 'Black (or Black British)', 'Mixed', 'Asian (or Asian British)', 'Chinese or Other Ethnic Group')),
          odds = c(
            1,
            
            round(
              (data.n() %>% filter(NAME == input$select.borough) %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'Black (or Black British)') %>% pull(n) / 
                 sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Black or Black British'])) /
                (data.n() %>% filter(NAME == input$select.borough) %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'White') %>% pull(n) / 
                   sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'White'])),
              2),
            
            round(
              (data.n() %>% filter(NAME == input$select.borough) %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'Mixed') %>% pull(n) / 
                 sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Mixed'])) /
                (data.n() %>% filter(NAME == input$select.borough) %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'White') %>% pull(n) / 
                   sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'White'])),
              2),
            
            round(
              (data.n() %>% filter(NAME == input$select.borough) %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'Asian (or Asian British)') %>% pull(n) / 
                 sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Asian or Asian British'])) /
                (data.n() %>% filter(NAME == input$select.borough) %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'White') %>% pull(n) / 
                   sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'White'])),
              2),
            
            round(
              (data.n() %>% filter(NAME == input$select.borough) %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'Chinese or Other Ethnic Group') %>% pull(n) / 
                 sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'Chinese or Other Ethnic Group'])) /
                (data.n() %>% filter(NAME == input$select.borough) %>% group_by(ethnicity) %>% summarise(n = n()) %>% filter(ethnicity == 'White') %>% pull(n) / 
                   sum(ethnicity_by_boroughs[ethnicity_by_boroughs$borough == input$select.borough, 'White'])),
              2)
          ),
          white = c(T, rep(F, 4))
        )
      
      barplot.odds.borough <- ggplot(odds.borough, aes(y = odds, x = ethnicity, fill = white)) + 
        geom_bar(position = "dodge", stat = "identity", width = .45) +
        ylim(0,12) +
        geom_text(aes(label = odds), vjust = -.75, colour = "red", size = 7.5) +
        labs(y = "", x = "", title = paste('ODDS RATIOS: Ethnic disparities in ', input$select.borough, ' (', input$select.legislation, ')', sep = "")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
              legend.text = element_text(size = 10), legend.position = "None",
              plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 14)) +
        scale_x_discrete(breaks = c('White', 'Black (or Black British)', 'Mixed', 'Asian (or Asian British)', 'Chinese or Other Ethnic Group', 'Not Stated / Unknown')) + 
        scale_fill_manual(values = c("black", "red"),
                          labels = c("", ""))
      
      barplot.odds.borough
      
    })
    
}


shinyApp(ui, server)
