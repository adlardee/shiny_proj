############################################# GLOBAL FILE BELOW ####################################

#setwd("C:/Users/adlar/Desktop/shiny_proj")
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(DT)
library(leaflet)
library(googleVis)
library(corrplot)
library(ggcorrplot)


happiness = read.csv(file = "happiness2017.csv")

#remove row names
rownames(happiness) = NULL

#create variable with colnames as choice
choice = colnames(happiness)[-1]

#this is used for the correlation plot (plot 6)
happiness5 = select(happiness, 'Happiness.Score', 'Economy..GDP.per.Capita.','Family', 'Health..Life.Expectancy.', 'Freedom', 'Generosity', 'Trust..Government.Corruption.', 'Dystopia.Residual')


##################################################### UI.R #########################################


library(shinydashboard)

ui <- fluidPage(shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Country Happiness 2017'),
    dashboardSidebar(
      sidebarUserPanel(
        name = 'By Eric Adlard',
        subtitle = '',
        image = 'happiness.png'
      ),
      sidebarMenu(
        menuItem("Map", tabName = "map", icon = icon("map")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("Graphs", tabName = "graphs", icon = icon("bar-chart")),
        menuItem("About", tabName = "about", icon = icon("info"))
      ),
      selectizeInput("selected",
                     "Select Item to Display",
                     choice)
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "map",
                fluidRow(box(title = "What is this map showing me and how do I use it?",
                             footer = "The below map is an interactive map to look at the overall Country Happiness 
                             ranking as well as the characterisics that make up its overall ranking. The map is 
                             shaded from red to green with dark green values indicating a higher score and dark red 
                             indicating a low score for the characteristic in 'Select Item to Display' in the menu 
                             bar on the left. Those characteristics are Happiness.Rank, Economy..GDP.per.Capita., 
                             Family, Health..Life.Expectancy., Freedom, Generosity, Trust..Government.Corruption, 
                             and Dystopia.Residual", width=12)),
                # gvisGeoChart
                fluidRow(box(htmlOutput("map"),width = 12)
                         )),
        tabItem(tabName = "data",
                # datatable
                fluidRow(box(DT::dataTableOutput("table"), width = 12))),
        tabItem(tabName = "about",
                fluidRow(box(title = "The World Happiness Report",
                             footer = "The World Happiness Report is a landmark survey of the state of global happiness.
                             The first report was published in 2012, the second in 2013, the third in 2015, and the 
                             fourth in the 2016 Update. The World Happiness 2017, which ranks 155 countries by their 
                             happiness levels, was released at the United Nations at an event celebrating International 
                             Day of Happiness on March 20th. The report continues to gain global recognition as 
                             governments, organizations and civil society increasingly use happiness indicators to 
                             inform their policy-making decisions. Leading experts across fields - economics, psychology,
                             survey analysis, national statistics, health, public policy and more - describe how 
                             measurements of well-being can be used effectively to assess the progress of nations. 
                             The reports review the state of happiness in the world today and show how the new science 
                             of happiness explains personal and national variations in happiness.", width=12)),
                fluidRow(box(title = "How are the scores created?",
                             footer = "The happiness scores and rankings use data from the Gallup World Poll. 
                             The scores are based on answers to the main life evaluation question asked in the poll. 
                             This question, known as the Cantril ladder, asks respondents to think of a ladder with 
                             the best possible life for them being a 10 and the worst possible life being a 0 and to 
                             rate their own current lives on that scale. The scores are from nationally 
                             representative samples for the years 2013-2016 and use the Gallup weights to make the 
                             estimates representative. The columns following the happiness score estimate the extent 
                             to which each of six factors - economic production, social support, life expectancy, 
                             freedom, absence of corruption, and generosity - contribute to making life evaluations 
                             higher in each country than they are in Dystopia, a hypothetical country that has 
                             values equal to the world's lowest national averages for each of the six factors. 
                             They have no impact on the total score reported for each country, but they do explain 
                             why some countries rank higher than others.", width=12)),
                fluidRow(box(title = "What is Dystopia?",
                             footer = "Dystopia is an imaginary country that has the world's least-happy people. 
                             The purpose in establishing Dystopia is to have a benchmark against which all countries 
                             can be favorably compared (no country performs more poorly than Dystopia) in terms of 
                             each of the six key variables, thus allowing each sub-bar to be of positive width. 
                             The lowest scores observed for the six key variables, therefore, characterize Dystopia. 
                             Since life would be very unpleasant in a country with the world's lowest incomes, 
                             lowest life expectancy, lowest generosity, most corruption, least freedom and least 
                             social support, it is referred to as 'Dystopia', in contrast to Utopia.", width=12)),
                fluidRow(box(title = "What are the residuals?",
                             footer = "The residuals, or unexplained components, differ for each country, reflecting the 
                             extent to which the six variables either over- or under-explain average 2014-2016 life 
                             evaluations. These residuals have an average value of approximately zero over the whole set 
                             of countries.", width=12)),
                fluidRow(box(title = "What are the key drivers of happiness in each country?",
                             footer = "The following columns: GDP per Capita, Family, Life Expectancy, Freedom, 
                             Generosity, Trust Government Corruption describe the extent to which these factors 
                             contribute in evaluating the happiness in each country. The Dystopia Residual metric 
                             actually is the Dystopia Happiness Score(1.85) + the Residual value or the unexplained 
                             value for each country as stated in the previous answer.If you add all these factors 
                             up, you get the happiness score", width=12))),
        tabItem(tabName = "graphs",
                # graphs
                fluidRow(column(6, plotOutput("plot1")),
                         column(6, plotOutput("plot2"))
                ),
                fluidRow(column(6, plotOutput("plot3")),
                         column(6, plotOutput("plot4"))
                ),
                fluidRow(column(6, plotOutput("plot5")),
                         column(6, plotOutput("plot6"))
                )
    )
  )
))))


################################### SERVER.R ####################################################


server <- function(input, output, session) {

  #Google Vis Map on Map tab
  output$map <- renderGvis({
    gvisGeoChart(happiness, "Country", input$selected,
                 options=list(region="world", displayMode="regions", 
                              width="auto", height="auto", colorAxis = "{colors: [ '#e31b23','#00853f']}"))
    # using width="auto" and height="auto" to automatically adjust the map size
  })
  
  #Data output on Data tab
  output$table <- DT::renderDataTable({
    datatable(happiness, rownames=FALSE) %>% 
      formatStyle(input$selected,  
                  background="skyblue", fontWeight='bold')
    # Highlight selected column using formatStyle
  })
  output$plot1<-renderPlot({
    ggplot(happiness, aes(x = Happiness.Score, y = Economy..GDP.per.Capita., colour= Region)) + 
      ggtitle("Scatterplot of Happiness vs Economy by regions") + geom_point(size = 4)
  })
  output$plot2<-renderPlot({
    ggplot(happiness, aes(x = Happiness.Score, y = Health..Life.Expectancy., colour= Region)) + 
      ggtitle("Scatterplot of Happiness vs Health / Life Expectancy by regions") + geom_point(size = 4)
  })
  output$plot3<-renderPlot({
    ggplot(happiness, aes(x = Happiness.Score, y = Freedom, colour= Region)) + 
      ggtitle("Scatterplot of Happiness vs Freedom by regions") + geom_point(size = 4)
  })
  output$plot4<-renderPlot({
    ggplot(happiness, aes(x = Happiness.Score, y = Generosity, colour= Region)) + 
      ggtitle("Scatterplot of Happiness vs Generosity by regions") + geom_point(size = 4)
  })
  output$plot5<-renderPlot({
    ggplot(happiness, aes(x = Region, y = Happiness.Score, colour= Region, srt = 45)) + 
      ggtitle("Boxplot of Happiness Score by regions") + geom_boxplot() + theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  output$plot6<-renderPlot({
    corr = round(cor(happiness5), 1)
    ggcorrplot(corr, method = "circle") + ggtitle("Correlation graph among variables")
  })
}
shinyApp(ui, server)
