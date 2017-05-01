library(shiny)
library(ggvis)
library(reshape2)
library(plotly)
library(GGally)

cat('\014')
#setwd('~/workspace/MSAN_viz/hw3/')
fb <- read.csv('dataset_Facebook.csv',sep = ';')
types <- levels(fb$Type)
y_axis <- c("Lifetime.Post.Total.Reach","Lifetime.Post.Total.Impressions","Lifetime.Engaged.Users","Lifetime.Post.Consumers","Lifetime.Post.Consumptions")
x_axis <- c("Post.Month","Post.Weekday")

ui <- fluidPage(
  headerPanel('Facebook data'),
  sidebarPanel(
    conditionalPanel(condition = "input.conditionedPanels == 1",
    radioButtons("type", "Type", choices = types))
  ),
  mainPanel(
    tabsetPanel(id = 'conditionedPanels',
      tabPanel("user engaged", plotlyOutput("hist1"),value = 1),
      tabPanel("scatterplot matrix", plotlyOutput("hist2"), value = 2) ,
      tabPanel("Parallel Coordinates Plot", plotlyOutput("hist3"),value = 3))
  )
)


server <- function(input, output) {
  select_type <- reactive({fb[fb$Type==input$type,]})
  output$hist1 <- renderPlotly({
    plot_ly(data = select_type(),x = ~share,y = ~like,type = 'scatter',mode = 'markers',size = ~Lifetime.Engaged.Users,
            text = ~paste('Users: ', Lifetime.Engaged.Users))
  })
  output$hist2 <- renderPlotly({
    df_tmp <- subset(fb,select = c("Lifetime.Post.Total.Impressions","Lifetime.Engaged.Users","Lifetime.Post.Consumers","Lifetime.Post.Consumptions"))
    colnames(df_tmp) <- c("Impressions","Users","Consumers","Consumptions")
    ggplotly(ggpairs(df_tmp))
  })
  output$hist3 <- renderPlotly({
    df_par <- fb[c("Category","Post.Month", "Post.Weekday",
                 "Post.Hour", 
                 "Type")]
    ggplotly(ggparcoord(df_par, 1:4, groupColumn= "Type"))
    
  })
 
}


shinyApp(ui = ui, server = server)
