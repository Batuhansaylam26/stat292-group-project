#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with  Shiny here:
#
#    http://shiny.rstudio.com/
#


library(scatterplot3d) 
library(shiny)
library(leaflet)
library(magrittr)
library(rworldmap)
library(rgeos)
library(rworldmap)
library(rgdal)
library(readxl)
library(ggplot2)
library(countrycode)
library(plotly)
library(shinythemes)
library(dplyr)
library(ggpubr)
library(shinyWidgets)
library(kableExtra)
library(GGally)
X2020_Statistical_Annex_Table_4 <- read_excel("2020_Statistical_Annex_Table_4.xlsx", 
                                              col_types = c("numeric", "text", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric"))


mydata = X2020_Statistical_Annex_Table_4
mydata = as.data.frame(mydata)

mydata = mydata[,-1]
mydata = na.omit(mydata)
library(dplyr)

female = mydata[,c(1,2,3,4,6,8,10,12)]

male = mydata[,c(1,2,3,5,7,9,11,13)]

female$isfemale = 1

male$isfemale = 0

names(male)

names(male) = c("Country",
                "Gender_DI_value",
                "Gender_DI_group",
                "Human_DI",
                "Life_Expectancy",
                "Expected_Years_of_Schooling",
                "Mean_Years_of_Schooling",
                "Gross_National_Income_Per_Capita",
                "isfemale") 


names(female) = c("Country",
                  "Gender_DI_value",
                  "Gender_DI_group",
                  "Human_DI",
                  "Life_Expectancy",
                  "Expected_Years_of_Schooling",
                  "Mean_Years_of_Schooling",
                  "Gross_National_Income_Per_Capita",
                  "isfemale")
male$HD_Groups = ifelse(male$Human_DI > 0.9, "Very High",
                        ifelse(male$Human_DI > 0.76, "High",
                               ifelse(male$Human_DI > 0.67, "Medium",
                                      "Low")))

female$HD_Groups = ifelse(female$Human_DI > 0.88, "Very High",
                          ifelse(female$Human_DI > 0.73, "High",
                                 ifelse(female$Human_DI > 0.56, "Medium",
                                        "Low")))
test = rbind(male, female)
test$isfemale = as.factor(test$isfemale)

test1 = rbind(male, female)
test1$Gender_DI_group=as.factor(test1$Gender_DI_group)
test1$isfemale = as.factor(test1$isfemale)
test1$continent <- countrycode(sourcevar = test1[, "Country"],
                              origin = "country.name",
                              destination = "continent")
test1$HD_Groups = as.factor(test1$HD_Groups)
test2=subset(test1,test1$isfemale==0)
test3=subset(test1,test1$isfemale==1)

ui <- bootstrapPage(
  tags$style("
        #controls {
          background-color: #ddd;
          opacity: 0.5;
        }
        #controls:hover{
          opacity: 1;
        }
               "),
  navbarPage(theme = shinytheme("cerulean"), collapsible = TRUE,
  title="Exploratory Analysis of GNI",
  tabPanel("Data Preview",
           tabsetPanel(
             tabPanel("Data",
                      DT::dataTableOutput('ex4')
             ),
             tabPanel("Summary",
                      tableOutput("sum")),
             tabPanel("Correlation Plot",
                      plotOutput("plotting")
             )
           )
  ),
  tabPanel("Map",
           leafletOutput("myMap", width = '100%', height = '100vh'),
           absolutePanel(id = "controls", class = "panel panel-default",
                         top = 75, left = 55, width = 200, fixed=TRUE,
                         draggable = TRUE, height = "auto",
                         h4("Description of App"),
                         p("App explains the Estimated GNI Per Capita
                           in the world with respect to different variables. And
                           Demonstrates the GNI for variety of categories such as
                           continent, gender, HDI. The app gives anylsis to inform
                           users about GNI. Also, the app predicts the expected GNI for
                           different values of mean schooling years.")
                         
                         )
           ),
  tabPanel("Linear Association",
           sidebarLayout(
             sidebarPanel(
               selectInput("select", "Continent:",
                           c("Africa",
                             "Americas",
                             "Asia" ,
                             "Europe",
                             "Oceania",
                             "World"), 
                           selected = "World"),
               p("In this scatter plots, we will focus on linear relationship between mean years of schooling  and their estimated GNI per capita"),
               numericInput("num", label = h3("Mean years of schooling:"), value = 1),
               h4("The estimated GNI per capita:"),
               verbatimTextOutput("value")
             ),
             mainPanel(
               plotOutput("plot1"),
               tableOutput("summary5")
               )
           )
  ),
  tabPanel("GNI per Capita~X with Factors",
    sidebarLayout(
      sidebarPanel(
        h4("Choose the variable which horizontal line represents: "),
        selectInput("select1", "Variable :",
                    choices=c("GDI value"="Gender_DI_value",
                      "GDI groups"= "Gender_DI_group",
                      "HDI value"="Human_DI",
                      "Life expectancy"="Life_Expectancy",
                      "Expecter years of schooling"="Expected_Years_of_Schooling",
                      "Mean years of Schooling"="Mean_Years_of_Schooling"
                      ), 
                    selected = "Gender_DI_value"),
        selectInput("select2","Choose factor:",choices = c("Gender"="isfemale","HDI groups"="HDI_groups","continent","GDI gruops"="Gender_DI_group"),selected = "isfemale"),
        h4("HDI groups:"),
        p("Each country classifies according to its HDI value. A value above 0.800 is classified as very high, between 0.700 and 0.799 as high, between 0.550 and 0.699 as medium, and below 0.550 as low."),
        h4("Gender:"),
        p("In the scatter plot, the color ID representing males as the dots is 0 and females are 1.")
        
        
        ),
      
      mainPanel(
        plotOutput("plot3"))
      

      )
    ),
  tabPanel("Marginal Density",
           hr("HDI-GNI"),
           p("Each country classified according to its HDI group."),
           p("Marginal density graphs show us the marginal densities of continents according to lines."),
           sliderTextInput("slidering1","Choose HDI group:",choices = c("Low","Medium","High","Very High")),
           column(6,
                  plotOutput("Male1")),
           column(6,
                  plotOutput("Female1")),
             
           column(6,
                  p("In the graph of male, each dot represents a country, with the colors representing the continents,the horizontal position showing the HDI values of men  and vertical position showing GNI per capita of men")),
           column(6,
                  p("In the graph of female, each dot represents a country, with the colors representing the continents, the horizontal position showing the HDI values of women  and vertical position showing GNI per capita of women")),
           
             
           ),


           ))
  


  
  

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  test4=subset(test2,test2$HD_Groups=="Medium")
  test5=subset(test3,test3$HD_Groups=="Medium")
  test6=subset(test2,test2$HD_Groups=="Low")
  test7=subset(test3,test3$HD_Groups=="Low")
  test8=subset(test2,test2$HD_Groups=="High")
  test9=subset(test3,test3$HD_Groups=="High")
  test10=subset(test2,test2$HD_Groups=="Very High")
  test11=subset(test3,test3$HD_Groups=="Very High")
  output$sum=function(){
    summary(test) %>%
      kbl(caption = "Five Number Summary") %>%
      kable_classic(full_width = F, html_font = "Cambria")
  }
  output$Male1=renderPlot({
    if(req(input$slidering1)=="Low"){
      ggscatterhist(
        test6, x = "Human_DI", y = "Gross_National_Income_Per_Capita",
        color = "continent", size = 3, alpha = 0.6,
        palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
        margin.params = list(fill = "continent", color = "black", size = 0.2)
      )}else if(req(input$slidering1)=="Medium"){
        ggscatterhist(
          test4, x = "Human_DI", y = "Gross_National_Income_Per_Capita",
          color = "continent", size = 3, alpha = 0.6,
          palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          margin.params = list(fill = "continent", color = "black", size = 0.2)
        )
        
      }else if(req(input$slidering1)=="High"){
        ggscatterhist(
          test8, x = "Human_DI", y = "Gross_National_Income_Per_Capita",
          color = "continent", size = 3, alpha = 0.6,
          palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          margin.params = list(fill = "continent", color = "black", size = 0.2)
        )
      }else if(req(input$slidering1)=="Very High"){
        ggscatterhist(
          test10, x = "Human_DI", y = "Gross_National_Income_Per_Capita",
          color = "continent", size = 3, alpha = 0.6,
          palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          margin.params = list(fill = "continent", color = "black", size = 0.2)
        )
      }
    
    }
  )
  output$Female1=renderPlot({
    if(req(input$slidering1)=="Low"){
      ggscatterhist(
        test7, x = "Human_DI", y = "Gross_National_Income_Per_Capita",
        color = "continent", size = 3, alpha = 0.6,
        palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
        margin.params = list(fill = "continent", color = "black", size = 0.2)
      )}else if(req(input$slidering1)=="Medium"){
        ggscatterhist(
          test5, x = "Human_DI", y = "Gross_National_Income_Per_Capita",
          color = "continent", size = 3, alpha = 0.6,
          palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          margin.params = list(fill = "continent", color = "black", size = 0.2)
        )
        
      }else if(req(input$slidering1)=="High"){
        ggscatterhist(
          test9, x = "Human_DI", y = "Gross_National_Income_Per_Capita",
          color = "continent", size = 3, alpha = 0.6,
          palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          margin.params = list(fill = "continent", color = "black", size = 0.2)
        )
      }else if(req(input$slidering1)=="Very High"){
        ggscatterhist(
          test11, x = "Human_DI", y = "Gross_National_Income_Per_Capita",
          color = "continent", size = 3, alpha = 0.6,
          palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          margin.params = list(fill = "continent", color = "black", size = 0.2)
        )
      }
    
  }
    
  )
  
  # get world map
  wmap <- getMap(resolution="high")
  worldmap=map_data("world")
  # get centroids
  centroids <- gCentroid(wmap, byid=TRUE)
  
  # get a data.frame with centroids
  df <- as.data.frame(centroids)
  countries <- readOGR("https://rstudio.github.io/leaflet/json/countries.geojson")
  t=cbind(df,rownames(df))
  names(t)[3]="Country"
  t[(t$Country=="United States of America"),]["Country"]="United States"
  t[(t$Country=="Russia"),]["Country"]="Russian Federation"
  k=merge(X2020_Statistical_Annex_Table_4,t,by="Country")
  k=na.omit(k)
  test$continent=countrycode(sourcevar = test[, "Country"],
                             origin = "country.name",
                             destination = "continent")
  mm=leaflet() %>% addProviderTiles(providers$CartoDB.Voyager ) %>%
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Gender Development Index Value (2019)`)*(10), 
                     fillOpacity = 0.7, color = "red",group = "GDI Value") %>% 
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Human Development Index Female (value) (2019)` )*(10), 
                     fillOpacity = 0.7, color = "purple",group = "HDI Female value") %>%   
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Human Development Index Male (value) (2019)` )*(10), 
                     fillOpacity = 0.7, color = "green",group = "HDI Male value") %>% 
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Expected years of schooling Female (years) (2019)`)/(2), 
                     fillOpacity = 0.7, color = "goldenrod",group ="years of schooling Female" ) %>% 
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Expected years of schooling Male (years) (2019)`)/(2), 
                     fillOpacity = 0.7, color = "cornflowerblue",group = "years of schooling Male") %>% 
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Life expectancy at birth Female (years) (2019)` )/(10), 
                     fillOpacity = 0.7, color = "yellow",group = "Life expectancy at birth Female" ) %>% 
    
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Life expectancy at birth Male (years) (2019)` )/(10), 
                     fillOpacity = 0.7, color = "pink",group = "Life expectancy at birth Male" ) %>% 
    
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Mean years of schooling Female (years) (2019)` )*(2/3), 
                     fillOpacity = 0.7, color = "orange",group ="Mean years of schooling Female" ) %>% 
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Mean years of schooling Male (years) (2019)` )*(2/3), 
                     fillOpacity = 0.7, color = "darkgreen",group ="Mean years of schooling Male" ) %>% 
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Estimated  gross national income per capita Female (2017 PPP $) (2019)` )/(5000), 
                     fillOpacity = 0.7, color = "palevioletred",group = "gross national income per capita Female") %>% 
    addCircleMarkers(data = k, lat = ~ y, lng = ~ x, weight = 0.1, radius = ~(k$`Estimated  gross national income per capita Male (2017 PPP $) (2019)` )/(5000), 
                     fillOpacity = 0.7, color = "darkred",group = "gross national income per capita Male" ) %>% 
    addLayersControl(
      position = "bottomright",
      overlayGroups =c("GDI Value","HDI Female value","HDI Male value","years of schooling Female",
                       "years of schooling Male","Life expectancy at birth Female","Life expectancy at birth Male",
                       "Mean years of schooling Female","Mean years of schooling Male","gross national income per capita Female",
                       "gross national income per capita Male"),
      options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("HDI Female value","HDI Male value","years of schooling Female",
                "years of schooling Male","Life expectancy at birth Female","Life expectancy at birth Male",
                "Mean years of schooling Female","Mean years of schooling Male","gross national income per capita Female",
                "gross national income per capita Male")) 
  
  
  output$myMap=renderLeaflet({
    mm
  })
  output$ex4 <- DT::renderDataTable(
    DT::datatable(X2020_Statistical_Annex_Table_4, options = list(searching = FALSE))
  )
  k$continent <- countrycode(sourcevar = k[, "Country"],
                             origin = "country.name",
                             destination = "continent")
  output$plot1=renderPlot({
    if(input$select=="World"){
      model <- lm(Gross_National_Income_Per_Capita ~Mean_Years_of_Schooling,data = test)
      x <- data.frame("Mean_Years_of_Schooling"= seq(min(na.omit(test$Mean_Years_of_Schooling)), max(na.omit(test$Mean_Years_of_Schooling)), length = length(test)))
      names(x)[1]="Mean_Years_of_Schooling"
      ci.band <- predict(model, newdata = x,
                         interval="confidence",level=0.95)
      pi.band <- predict(model, newdata = x,
                         interval="prediction",level=0.95)
      
      ci.band=as.data.frame(ci.band )
      pi.band=as.data.frame(pi.band)
      plot1=test %>% 
        ggplot(aes(x = Mean_Years_of_Schooling, y = Gross_National_Income_Per_Capita)) +
        geom_point(color="blue")+coord_cartesian(xlim=c(min(test$Mean_Years_of_Schooling),max(test$Mean_Years_of_Schooling)),ylim=c(min(test$Gross_National_Income_Per_Capita),max(test$Gross_National_Income_Per_Capita))) + geom_smooth(method = "lm", col = "green" )
      plot1=plot1+geom_line(x,mapping=aes(x=x$Mean_Years_of_Schooling,y=ci.band$lwr ),colour="purple")
      plot1=plot1+geom_line(x,mapping=aes(x=x$Mean_Years_of_Schooling,y=ci.band$upr ),colour="purple")
      plot1=plot1+geom_line(x,mapping=aes(x=x$Mean_Years_of_Schooling,y=pi.band$lwr ),colour="orange")
      plot1=plot1+geom_line(x,mapping=aes(x=x$Mean_Years_of_Schooling,y=pi.band$upr ),colour="orange")
      plot1+theme_classic()
      
    }else {
      t=subset(test,test$continent==input$select)
      modeling <- lm(Gross_National_Income_Per_Capita ~Mean_Years_of_Schooling,data = t)
      x <- data.frame("Mean_Years_of_Schooling"= seq(min(na.omit(t$Mean_Years_of_Schooling)), max(na.omit(t$Mean_Years_of_Schooling)), length = length(t)))
      names(x)[1]="Mean_Years_of_Schooling"
      ci.band <- predict(modeling, newdata = x,
                         interval="confidence",level=0.95)
      pi.band <- predict(modeling, newdata = x,
                         interval="prediction",level=0.95)
      
      ci.band=as.data.frame(ci.band )
      pi.band=as.data.frame(pi.band)
      plot1=t %>% 
        ggplot(aes(x = Mean_Years_of_Schooling, y = Gross_National_Income_Per_Capita)) +
        geom_point(color="blue")+coord_cartesian(xlim=c(min(t$Mean_Years_of_Schooling),max(t$Mean_Years_of_Schooling)),ylim=c(min(t$Gross_National_Income_Per_Capita),max(t$Gross_National_Income_Per_Capita))) + geom_smooth(method = "lm", col = "green" )
      plot1=plot1+geom_line(x,mapping=aes(x=x$Mean_Years_of_Schooling,y=ci.band$lwr ),colour="purple")
      plot1=plot1+geom_line(x,mapping=aes(x=x$Mean_Years_of_Schooling,y=ci.band$upr ),colour="purple")
      plot1=plot1+geom_line(x,mapping=aes(x=x$Mean_Years_of_Schooling,y=pi.band$lwr ),colour="orange")
      plot1=plot1+geom_line(x,mapping=aes(x=x$Mean_Years_of_Schooling,y=pi.band$upr ),colour="orange")
      plot1+theme_classic()
      
    }
    })

  output$summary5=function(){
    
    if(input$select=="World"){
      summary(model)$coefficients %>%
        kbl(caption = "Model Coefficient") %>%
        kable_classic(full_width = F, html_font = "Cambria")
    }else {
      t=subset(test,test$continent==input$select)
      modeling <- lm(Gross_National_Income_Per_Capita ~Mean_Years_of_Schooling,data = t)
      summary(modeling)$coefficients %>%
        kbl(caption = "Model Coefficient") %>%
        kable_classic(full_width = F, html_font = "Cambria")
    }
  }
  output$value=renderPrint({
    if(input$select=="World"){
      as.numeric(model$coefficients[1] + (input$num)*model$coefficients[2])
    }else {
      t=subset(test,test$continent==input$select)
      modeling <- lm(Gross_National_Income_Per_Capita ~Mean_Years_of_Schooling,data = t)
      as.numeric(modeling$coefficients[1] + (input$num)*modeling$coefficients[2])
    }
    
  })

  
  output$plot3=renderPlot({
    mydata = X2020_Statistical_Annex_Table_4
    mydata = as.data.frame(mydata)
    
    mydata = mydata[,-1]
    mydata = na.omit(mydata)
    library(dplyr)
    
    female = mydata[,c(1,2,3,4,6,8,10,12)]
    
    male = mydata[,c(1,2,3,5,7,9,11,13)]
    
    female$isfemale = 1
    
    male$isfemale = 0
    
    names(male)
    
    names(male) = c("Country",
                    "Gender_DI_value",
                    "Gender_DI_group",
                    "Human_DI",
                    "Life_Expectancy",
                    "Expected_Years_of_Schooling",
                    "Mean_Years_of_Schooling",
                    "Gross_National_Income_Per_Capita",
                    "isfemale") 
    
    
    names(female) = c("Country",
                      "Gender_DI_value",
                      "Gender_DI_group",
                      "Human_DI",
                      "Life_Expectancy",
                      "Expected_Years_of_Schooling",
                      "Mean_Years_of_Schooling",
                      "Gross_National_Income_Per_Capita",
                      "isfemale")
    male$HD_Groups = ifelse(male$Human_DI > 0.9, "Very High",
                            ifelse(male$Human_DI > 0.76, "High",
                                   ifelse(male$Human_DI > 0.67, "Medium",
                                          "Low")))
    
    female$HD_Groups = ifelse(female$Human_DI > 0.88, "Very High",
                              ifelse(female$Human_DI > 0.73, "High",
                                     ifelse(female$Human_DI > 0.56, "Medium",
                                            "Low")))
    
    test = rbind(male, female)
    test$Gender_DI_group=as.factor(test$Gender_DI_group)
    test$isfemale = as.factor(test$isfemale)
    test$continent <- countrycode(sourcevar = test[, "Country"],
                               origin = "country.name",
                               destination = "continent")
    test$HD_Groups = as.factor(test$HD_Groups)
    
    library(ggplot2)
    plot3=ggplot(test, aes(x = get(input$select1), y = Gross_National_Income_Per_Capita))
      if(input$select2=="HDI_groups"){
        colors <- c("Very High" = "#D9717D", "High" = "#4DB6D0", "Medium" = "#BECA55", "Low" = 77)
        plot3+geom_point(aes(colour = HD_Groups)) + 
          scale_color_manual(values = colors) +
          xlab("Variable X") +
          ylab("Variable Y")  +
          geom_smooth(se = F, data = filter(test, HD_Groups == "Very High"), colour = "#D9717D")+
          geom_smooth(se = F, data = filter(test, HD_Groups == "High"), colour = "#4DB6D0")+
          geom_smooth(se = F, data = filter(test, HD_Groups == "Medium"), colour = "#BECA55")+
          geom_smooth(se = F, data = filter(test, HD_Groups == "Low"), colour = 77)+
          theme(axis.line = element_line(colour = "black",
                                         size = 0.24))+theme_classic()}
      else if(input$select2 == "isfemale"){
        plot3+ geom_point(aes(colour = get(input$select2))) + 
          scale_color_discrete(input$select2) +
          xlab("Variable X") +
          ylab("Variable Y")  +
          geom_smooth(se = F, data = male, colour = "#F8766D")+
          geom_smooth(se = F, data = female, colour = "#619CFF")+
          theme(axis.line = element_line(colour = "black",
                                         size = 0.24))+theme_classic()
      }else if(input$select2=="continent"){
        plot3+geom_point(aes(colour = get(input$select2)))+
          scale_color_discrete(input$select2) +
          xlab("Variable X") +
          ylab("Variable Y")  +
          geom_smooth(se = F, data = filter(test, continent == "Africa"), colour = "#D9717D")+
          geom_smooth(se = F, data = filter(test, continent == "Americas"), colour = "#4DB6D0")+
          geom_smooth(se = F, data = filter(test, continent == "Asia"), colour = "#BECA55")+
          geom_smooth(se = F, data = filter(test, continent == "Europe"), colour = 77)+
          geom_smooth(se = F, data = filter(test, continent == "Ocenia"), colour = 77)+
          theme(axis.line = element_line(colour = "black",
                                         size = 0.24))+theme_classic()
      }else if(input$select2=="Gender_DI_group"){
        colors <- c("1" = "#D9717D", "2" = "#4DB6D0", "3" = "#BECA55", "4" = "purple", "5" = "black")
        plot3+geom_point(aes(colour = get(input$select2)))+
          scale_color_discrete(input$select2) +
          xlab("Variable X") +
          ylab("Variable Y")  +
          geom_smooth(se = F, data = filter(test, Gender_DI_group == 1), colour = "#D9717D")+
          geom_smooth(se = F, data = filter(test, Gender_DI_group == 2), colour = "#4DB6D0")+
          geom_smooth(se = F, data = filter(test, Gender_DI_group == 3), colour = "#BECA55")+
          geom_smooth(se = F, data = filter(test, Gender_DI_group == 4), colour = "purple")+
          geom_smooth(se = F, data = filter(test, Gender_DI_group == 5), colour = "black")+
          theme(axis.line = element_line(colour = "black",
                                         size = 0.24))+theme_classic()
      }
    
    
    
 
  })
  output$plotting=renderPlot({
    ggpairs(select_if(test, is.numeric),lower = list(
      mapping = aes(color = 12)
    ),diag = list(mapping = aes(color = "red")))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)



#Group name:Safaera
#Batuhan SAYLAM 2429264
#Zarrin Yusibova 2490043
#Nebih þahin 2429298
#Berkay türetken 2502433
#Süleyman buðra gülsoy 2283109

#References:
#Data Source: https://hdr.undp.org/gender-development-index#/indicies/GDI
#
