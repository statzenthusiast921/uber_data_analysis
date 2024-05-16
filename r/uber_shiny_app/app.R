#----- Load libraries
message('Loading packages...')
suppressPackageStartupMessages({
  library(lubridate)
  library(dplyr)
  library(leaflet)
  library(stringr)
  library(ggplot2)
  library(tidyr)
  library(wordcloud)
  library(RColorBrewer)
  library(wordcloud2)
  library(tm)
  library(SemNetCleaner)
  library(padr)
  library(geosphere)
  library(jvamisc)
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(plotly)
  library(timetk)
})

#----- Load and clean data
source('~/uber_data_analysis/r/load_and_clean_data_script.R')


ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title="Uber Data Analysis"),
    dashboardSidebar(
      sliderInput(
        inputId = "year_slider", 
        label = "Choose a year:",
        min = 2016, 
        max = 2023,
        value = 2023,
        step = 1,
        sep=""
      ),
      sidebarMenu(
        menuItem(
          "Uber Eats",
          tabName = "uber_eats",
          icon =icon('utensils')
        ),
        menuItem(
          "Uber Rides",
          tabName = "uber_rides",
          icon = icon('car')
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "uber_eats",
          #----- Row 0
          fluidRow(
            align = 'center',
            valueBoxOutput("vbox1",width=4),
            valueBoxOutput("vbox2",width=4),
            valueBoxOutput("vbox3",width=4)
          ),
          
          
          #----- Row 1
          fluidRow(
            align="center",
            column(
              width=6,
              DTOutput("price_table", height = '500px')
            ),
            column(
              width = 6,
              plotlyOutput("top_restaurants_chart", height = '250px'),
              plotOutput("word_cloud", height = '250px', width = '100%')
              
            )
          ),
          #----- Row 2
          fluidRow(
            align = 'center',
            column(
              width = 12,
              plotlyOutput("when_order", height = '180px')
            )
          )
        ),
        tabItem(
          tabName = "uber_rides",
          fluidRow(
            align = 'center',
            valueBoxOutput("vbox4",width=4),
            valueBoxOutput("vbox5",width=4),
            valueBoxOutput("vbox6",width=4)
          ),
          fluidRow(
            align="center",
            column(
              width=6,
              leafletOutput("location_map", height = '350px')
            ),
            column(
              width = 6,
              plotlyOutput("time_freq_ride_plot", height = '350px')
              
            ),
            column(
              width = 12,
              plotlyOutput("time_of_pickup_plot", height = '200px')
              
            )
          ))
    
      
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  

  
  food_summary_tbl <- reactive({
    
    order_df <- orders_cleaned |>
      filter(Order.Status == "COMPLETED") |>
      select(Order.Time, Order.Price) |>
      distinct() |>
      mutate(
        first_of_month = lubridate::floor_date(as.Date(Order.Time),'month')
      ) |>
      #filter(lubridate::year(first_of_month)==2018) |>
      mutate(
        year = lubridate::year(Order.Time),
        month = lubridate::month(Order.Time)
      ) |>
      group_by(first_of_month) |>
      summarise(avg_spent_order = round(mean(Order.Price, na.rm=TRUE),2),
                total_spent_order = round(sum(Order.Price, na.rm=TRUE),2),
                num_order = n()) |>
      pad_by_time(
        .date_var = first_of_month,
        .by = 'month',
        .pad_value = 0,
        .start_date = '2016-09-01',
        .end_date = '2023-12-01'
      ) |>
      ungroup() |>
      mutate(month = lubridate::month(first_of_month)) |>
      filter(lubridate::year(first_of_month)==input$year_slider) |>
      select(-c(first_of_month)) |>
      relocate(month, .before = avg_spent_order) |>
      rename(
        Month = month,
        `Avg Spent` = avg_spent_order,
        `Total Spent` = total_spent_order,
        `# Orders` = num_order
      )
    
    return(order_df)
  })
  
  #----- Value Boxes
  output$vbox1 <- renderValueBox({
    valueBox(
      value = paste0(
        '$',
        formatC(mean(food_summary_tbl()$`Avg Spent`),format="d",big.mark = ",")
      ),
      subtitle = paste0("Avg Spent in ", input$year_slider),
      color = "light-blue"
    )
  })
  
  output$vbox2 <- renderValueBox({
    valueBox(
      value = 
        paste0(
          '$',
          formatC(sum(food_summary_tbl()$`Total Spent`),format="d",big.mark = ",")
        ),
      subtitle = paste0("Total Spent in ", input$year_slider),
      color = "light-blue"
    )
  })
  
  output$vbox3 <- renderValueBox({
    valueBox(value = formatC(sum(food_summary_tbl()$`# Orders`),format="d",big.mark = ","),
             subtitle = paste0("# Orders in ", input$year_slider),
             color = "light-blue"
    )
  })
  
  #----- Price tables
  
  output$price_table <-renderDT({
    
    dtable = datatable(
      food_summary_tbl(), 
      rownames=TRUE, 
      options = list(pageLength=20,lengthChange = FALSE, dom='t')
    ) |>
      formatString(c(2,3), prefix = "$")
    
    colRamp = colorRamp(c("white","red"))
    data_columns <- c('Avg Spent', 'Total Spent', '# Orders')
    
    for(column in data_columns){
      x = na.omit(food_summary_tbl()[[column]])
      brks = quantile(x, probs = seq(.05, .95, .01))
      RGB = colRamp(c(0, (brks-min(x))/(max(x)-min(x))))
      clrs = apply(RGB, 1, function(rgb){
        sprintf("rgb(%s)", toString(round(rgb,0)))
      })
      dtable = dtable %>% 
        formatStyle(column, backgroundColor = styleInterval(brks, clrs))
    }
    
    dtable
  })
  
  #------ Make the cumulative restaurant data that can be filtered
  
  
  
  rest_plot_rank <- reactive({
    
    rest_plot_df <- eats_df |>
      filter(Order.Status=="COMPLETED") |>
      select(Date,Restaurant.Name) |>
      mutate(
        year = lubridate::year(Date),
        month = lubridate::month(Date)
      ) |>
      distinct() |>
      group_by(Restaurant.Name, year) |>
      summarise(
        Count = n()
      ) |>
      filter(year == input$year_slider) |>
      arrange(desc(Count))

    
    rest_plot_df <- rest_plot_df[1:5,]
    
    return(rest_plot_df)
    })
  
  
  
  #----- Bar Chart for Restaurants
  
  output$top_restaurants_chart <- renderPlotly({
    
    plot1 <- ggplot(
      data = rest_plot_rank(), aes(
        x = reorder(Restaurant.Name,-Count), 
        y = Count, 
        text = c(paste0('Restaurant Name: ', Restaurant.Name))
      )) +
      geom_bar(stat = "identity", fill = 'skyblue') +
      xlab('') + ylab('# Orders') +
      theme(
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank()
      )
    
    ggplotly(plot1, tooltip = c('text'))
    
  })
  
  
  #----- Word Clouds for Orders
  
  word_cloud_df <- reactive({
    #----- Create a word cloud of ordered items
    orders_filtered <- orders_cleaned |>
      mutate(cleaned_item_name = Item.Name) |>
      filter(lubridate::year(Date) == input$year_slider)
    
    #Step 1: Remove weird characters
    orders_filtered <- orders_filtered |>
      mutate(
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\-', ' '),
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\/', ''),
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\~', ''),
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\&', ''),
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\.', ''),
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\_', ''),
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\*', ''),
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\+', ''),
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\|', ''),
        cleaned_item_name = stringr::str_replace(cleaned_item_name,'\\$', '')
        
      )
    
    
    #Step 2: Transform plural words into singular
    orders_filtered <- orders_filtered |>
      mutate(
        cleaned_item_name = singularize(cleaned_item_name)
      )
    
    
    text <- orders_filtered$cleaned_item_name
    # Create a corpus  
    docs <- Corpus(VectorSource(text))
    
    docs <- docs |>
      tm_map(removeNumbers) |>
      tm_map(removePunctuation) |>
      tm_map(stripWhitespace)
    
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    
    return(df)
    
  })
  
  output$word_cloud <- renderPlot({
    
    par(mar = c(0, 0, 0, 0))  # Adjust the margins as needed
    
    set.seed(123) # for reproducibility 
    wordcloud(
      words = word_cloud_df()$word,
      freq = word_cloud_df()$freq, 
      min.freq = 1,
      max.words=200, 
      random.order=FALSE, 
      rot.per=0.35,  
      colors=brewer.pal(8, "Dark2"),
      scale = c(3,1),
      mar = c(0.5, 0.5, 0.5, 0.5)
    )
  })
  
  
  
  #----- Orders by Hour
  hour_df <- reactive({
    filtered_eats <- eats_df |>
      filter(lubridate::year(Date)==input$year_slider) |>
      #filter(lubridate::year(Date)==2016) |>
      select(-c(Item.Name,Customizations,Item.Price,Order.Price)) |>
      distinct() |>
      group_by(Hr) |>
      summarise(count = n()) |>
      pad_int(
        by = 'Hr',
        start_val = 0,
        end_val = 23,
        step = 1
      ) |>
      mutate(
        count2 = ifelse(is.na(count),0,count)
      ) |>
      select(-c(count)) |>
      rename(count = count2)
    
  })
  
  output$when_order <- renderPlotly({
    
    
    plot2 <- ggplot(data = hour_df(), aes(x = Hr, y = count)) +
      geom_line() + geom_point() +
      xlab('Hour') + ylab('# Orders')
    
    ggplotly(plot2)
    
  })
  
#---------- Uber Rides plots and tables ----------#
  
  #----- Value Boxes
  distance_df <- reactive({
    rides <- rides |>
      filter(year==input$year_slider)
    
    return(rides)
      
  })
  

  output$vbox4 <- renderValueBox({
    valueBox(
      value = paste0(round(mean(distance_df()$dist_miles),1), ' miles'),
      subtitle = paste0("Avg distance in ", input$year_slider),
      color = "light-blue"
    )
  })
  
  

  output$vbox5 <- renderValueBox({
    valueBox(
      value = paste0(round(mean(distance_df()$TripMin),1), ' Min'),
      subtitle = paste0("Avg duration in ", input$year_slider),
      color = "light-blue"
    )
  })
  
  
  output$vbox6 <- renderValueBox({
    valueBox(
      value = dim(distance_df())[1],
      subtitle = paste0("# rides in ", input$year_slider),
      color = "light-blue"
    )
  })

  
  
  output$location_map <- renderLeaflet({
    
    leaflet(distance_df() |>filter(Trip.or.Order.Status == "COMPLETED")) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~Begin.Trip.Lng, 
        lat = ~Begin.Trip.Lat, 
        popup = ~City
      )
  })
  

      
  
  output$time_freq_ride_plot <- renderPlotly({

    p <- ggplot(
      data = distance_df() |> group_by(TripMin_Cat) |> summarise(Count = n()), aes(
        x = reorder(TripMin_Cat,-Count),
        y = Count,
        text = c(paste0('Time: ', TripMin_Cat))
      )) +
      geom_bar(stat = "identity", fill = 'skyblue') +
      xlab('Time') + ylab('# Rides') +
      theme(
        #axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)
      )

    ggplotly(p, tooltip = c('text'))


  })
  
  output$time_of_pickup_plot <- renderPlotly({
    line_plot <- ggplot(data = distance_df() |> 
                          group_by(Hr_Pickup) |> 
                          summarise(Count = n()) |> 
                          pad_int(
                            by = 'Hr_Pickup',
                            start_val = 0,
                            end_val = 23,
                            step = 1) |>
                          mutate(Count = replace_na(Count,0)), 
                        aes(x = Hr_Pickup, y = Count)) +
      geom_line() + geom_point() +
      xlab('Hour') + ylab('# Rides')
    
    ggplotly(line_plot)
  })

  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
