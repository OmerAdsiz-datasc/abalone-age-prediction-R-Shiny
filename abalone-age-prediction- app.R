data1<-read.table("C:/Users/adsiz/Desktop/NotLonely.final.project/abalone.txt",header = F,sep = ",") 
colnames(data1)<-c("Sex","Length","Diameter","Height","Whole_weight","Shucked_weight","Viscera_weight","Shell_weight","Rings")
library(magrittr)
attach(data1)
library(shiny)
library(shinythemes)
library(Hmisc)

# Erkan Akbaba - 1915289
# ??mer Ads??z - 2290443
# Rukiye Esma ??zcan - 2146264
# Selendeniz KD1zD1lD1rmak - 2146207

ui <- fluidPage(
    shinythemes::themeSelector(), #Theme selector.
    # <--- Add this somewhere in the UI
    navbarPage(
        
        "FIND MY ABALONE'S AGE",
        tabPanel("INTRODUCTION",icon=icon("atom"),
                 
                 mainPanel(
                     
                     tabsetPanel(type = "tab",
                                 tabPanel("Abstract",   
                                          h3("GROUP MEMBERS"),
                                          
                                          p("Erkan Akbaba - 1915289"),
                                          p("Omer Adsiz - 2290443"),
                                          p("Rukiye Esma Ozcan - 2146264"),
                                          p("Selendeniz Kizilirmak - 2146207"),
                                          h3("AIM OF THE APP"),
                                          p("The Application aims three basic steps. 
               Explaining data , calculation of Abalone's age 
               and the relationships between variables. To explain the first step, 
                 the App represents two different parts which are Data Summary 
                 and Number of Observations in the Navigation  bar. To explain the 
                 second step, the App simulates us a age calculator. To explain the 
                 final step, the App supplies some graphs, plot and models."),
                                          p("In addition, the App provides
               video to the users in order to make the research more comprehensible."),
                                          
                                          h3(p("DATA DESCRIPTION")),
                                          p("This study has been done in Department of Computer Science, 
                 University of Tasmania by Sam Waugh on December, 1995. The study 
                 focused on predicting the age of abalone from physical measurements. 
                 The age of  abalone is determining by cutting its shell through the cone 
                 by staining it and after this process, number of rings are starting ounting through 
                 a microscope. Since it is a time consuming and tedious process, researches are 
                 looking for a easy and quick solution which is using other measurements of Abalone 
                 to predict its age."),
                                          p("The study based on 8 different attributes which are Sex, Length, Diameter, Height 
                 and 4 kinds of weights and 4177 instances. They gives information about its ring 
                 which will give us information about its age. In addition, there is no missing 
                 attribute value."),
                                          
                                          url <- a("Data Source", 
                                                   href="https://archive.ics.uci.edu/ml/datasets/Abalone")),
                                 tabPanel("What are Abalone?",
                                          br(),
                                          p("Abalone are basically known as marine snails. 
                                            They are single shelled snails with a large muscular foot 
                                            in order to hold rocks or reefs. Abalone are attached to their shell, 
                                            which is the most popular and beautiful part of Abalone. 
                                            If they are removed from their shell, without any damage, they can 
                                            keep alive but having their shell is significantly important for their 
                                            defense in their habitat.Their habitat is Coastal Waters. 
                                            There are 60-100 species known globally. 
                                            They are important ecologically, economically and culturally. 
                                            The most known usage of Abalone is jewelry production. 
                                            Finally and unfortunately, some species of Abalone are endangered."),
                                          br(),
                                          br(),
                                          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/glvDFOsEmhA" frameborder="0" allow="accelerometer; 
                              autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                     )
                     
                 ) 
                 
        ), # Navbar 1, tabPanel
        
        tabPanel("DATA SUMMARY",icon = icon("list-alt"),
                 mainPanel(tabsetPanel(type = "tab",
                                       tabPanel("Variable Types", verbatimTextOutput("str")),
                                       tabPanel("Summary Statistics", verbatimTextOutput("summary")),
                                       tabPanel("Describe Data", verbatimTextOutput("describe"))
                                       
                                       #Descriptive statistics panel. You can see variable types, summary or describe of data.
                 ))
        ), 
        tabPanel("DATA",icon = icon("table"),
                 
                 sidebarPanel(
                     numericInput("obs",
                                  label =  "Number of Observations to view:",
                                  min = 1,
                                  max = 4177,
                                  value = 6)
                     
                     
                 ), # sidebarPanel
                 
                 mainPanel(
                     tableOutput("view")
                     
                     #DATA view panel.
                     
                     
                 ) # mainPanel
                 
        ), # Navbar 1, tabPanel
        tabPanel("AGE CALCULATOR", icon=icon("calculator"),
                 titlePanel("Age of Abolone"),
                 h4("You can approximately calculate the Abolone's Age by entering their selected values."),
                 sidebarLayout(
                     
                     sidebarPanel(
                         
                         
                         numericInput("Diameter", "Diameter (mm)", value = 0.4079,step = 0.01),
                         numericInput("Height", "Height (mm)", value = 0.1395,step = 0.01),
                         numericInput("ShuckedWeight", "Shucked Weight (gr)", value = 0.3594,step =0.01 ),
                         numericInput("ShellWeight", "Shell Weight (gr)", value = 0.2388,step = 0.01),
                         
                         actionButton("Cal" , "Calculate")
                     ),
                     
                     mainPanel(
                         br(),
                         textOutput("age_out"),
                         br(),
                         br(),
                         br(),
                         img(src = "pics.png", height = 200, width = 400)
                         #Age calculation panel, used some variable to calculate age of abalone.
                     )
                 )
                 
        ),
        tabPanel("PLOTS",icon = icon("bar-chart-o"),sidebarLayout(
            sidebarPanel(
                
                h3("BoxPlot"),
                selectInput("Var","Select Variable",
                            choices =c("Length","Diameter","Height","Whole_weight",
                                       "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                
                
                checkboxInput("outliers", "Show outliers", TRUE),
                h3("Histogram"),
                selectInput("Var2","Select Variable",
                            choices =c("Length","Diameter","Height","Whole_weight",
                                       "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                sliderInput(inputId = "bins",
                            label = "Number of bins for Histogram:",
                            min = 1,
                            max = 50,
                            value = 30),
                h3("ScatterPlot"),
                selectInput("Var3","Select First Variable",
                            choices =c("Length","Diameter","Height","Whole_weight",
                                       "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                
                selectInput("Var4","Select Second Variable",
                            choices =c("Length","Diameter","Height","Whole_weight",
                                       "Shucked_weight","Viscera_weight","Shell_weight","Rings"))
                
                
            ),
            mainPanel(plotOutput("sexPlot"),br(),plotOutput("hist"),br(),
                      plotOutput("scatter"),br(),
                      p(h5("Correlation between selected two variables:")),
                      verbatimTextOutput("corr"),br(),p(h4("Pearson Correlation")),
                      p("The Pearson correlation coefficient, r, can take a range of values from +1 to -1. 
                        A value of 0 indicates that there is no association between the two variables. 
                        A value greater than 0 indicates a positive association; 
                        that is, as the value of one variable increases, 
                        so does the value of the other variable.
                        A value less than 0 indicates a negative association; 
                        that is, as the value of one variable increases, 
                        the value of the other variable decreases. "))
            
            #Plots panel, you can see three different types of plots and some options to use.
        )),
        
        
        tabPanel("MODEL",p(h4("VARIABLES")),icon=icon("cog", lib = "glyphicon"),
                 sidebarLayout(sidebarPanel(
                     p("Notes: If you select all the predictor variables the same, 
                     it becomes a simple linear regression model. 
                     However, you can make multiple linear regressions 
                     by selecting as many different predictors as you want.
                     For example, if you want to use three predictors, you must choose first
                     five predictor variables the same."),
                     selectInput("Var5","Dependent Variable",
                                 choices =c("Length","Diameter","Height","Whole_weight",
                                            "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                     
                     selectInput("Var6","First Predictor Variable",
                                 choices =c("Length","Diameter","Height","Whole_weight",
                                            "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                     selectInput("Var7","Second Predictor Variable",
                                 choices =c("Length","Diameter","Height","Whole_weight",
                                            "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                     
                     selectInput("Var8","Third Predictor Variable",
                                 choices =c("Length","Diameter","Height","Whole_weight",
                                            "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                     
                     selectInput("Var9","Fourth Predictor Variable",
                                 choices =c("Length","Diameter","Height","Whole_weight",
                                            "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                     
                     selectInput("Var10","Fifth Predictor Variable",
                                 choices =c("Length","Diameter","Height","Whole_weight",
                                            "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                     selectInput("Var11","Sixth Predictor Variable",
                                 choices =c("Length","Diameter","Height","Whole_weight",
                                            "Shucked_weight","Viscera_weight","Shell_weight","Rings")),
                     
                     selectInput("Var12","Seventh Predictor Variable",
                                 choices =c("Length","Diameter","Height","Whole_weight",
                                            "Shucked_weight","Viscera_weight","Shell_weight","Rings"))
                 ),mainPanel(p(h4("MODEL SUMMARY")),
                             verbatimTextOutput("model"),
                             p(h4("CONFIDENCE INTERVAL")),verbatimTextOutput("confint"),
                             p(h4("RESIDUAL STANDARD ERROR(RSE or Sigma)")),verbatimTextOutput("error"),
                             p(h4("PLOTS OF MODEL")),plotOutput("modelplot")) ) ),
        tabPanel("REFERENCES", icon=icon("sign-out"),
                 h2("REFERENCES"),
                 url <- a("(n.d.). Retrieved from https://archive.ics.uci.edu/ml/datasets/Abalone",href= "https://archive.ics.uci.edu/ml/datasets/Abalone"),
                 br(),  
                 url <- a("Abalone Calculator. (n.d.). Retrieved from http://abalone.floatingeye.net/",href= "http://abalone.floatingeye.net/"),
                 br(),
                 url <- a("(n.d.). Retrieved from https://rstudio.github.io/shinythemes/",href= "https://rstudio.github.io/shinythemes/"),
                 br(),
                 url <- a("Gallery. (n.d.). Retrieved from https://shiny.rstudio.com/gallery/",href= "https://shiny.rstudio.com/gallery/"),
                 br(),
                 url <- a("Abalone Introduction. (n.d.). Retrieved from https://www.marinebio.net/marinescience/06future/abintro.htm",
                          href= "https://www.marinebio.net/marinescience/06future/abintro.htm"),
                 
                 h2("PHOTO CREDITS"),
                 url <- a("Photo Source", 
                          href="https://www.thefishsociety.co.uk/abalone.html"),
                 br(),
                 url <- a("Video Source", 
                          href="https://www.youtube.com/watch?v=EB8ecdalbDE")
        )
        
        
    ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
    
    output$summary <- renderPrint({
        summary(data1)
        #Summary of data.
    })
    
    output$str <- renderPrint({
        str(data1)
        #Types of variables.
    })
    output$describe<-renderPrint({
        describe(data1)
        #Describe data.
    })
    
    
    output$view <- renderTable({head(data1, n = input$obs)})
    #Number of observation to view.
    
    
    
    age<- reactive({
        
        2.744048 + 12.059487*input$Diameter + 11.531655*input$Height - 11.777583*input$ShuckedWeight + 20.667310*input$ShellWeight
    })
    
    output$age_out <- renderText({
        
        input$Cal
        
        isolate(paste("Age of Abalone:" , age() ))
        #Age calculation.
    })
    output$hist<-renderPlot({
        bins <- seq(min(data1[,input$Var2]), max(data1[,input$Var2]), length.out = input$bins + 1)
        #Bins option.
        
        hist(data1[,input$Var2],breaks = bins,col = "lightblue",border = "white",
             main = "Histogram of Selected Variable",xlab = "Selected Variable")
        #Histogram of selected variable.
    })
    
    
    output$scatter<-renderPlot({
        plot(data1[,input$Var3], data1[,input$Var4],main="Scatter Plot of Selected variables",
             col=c("red","blue")) 
        #Scatterplot of selected variables.
    })
    output$corr<-renderPrint({
        a<-cor(data1[,input$Var3], data1[,input$Var4])
        print(a)
        ifelse(a==1,"Perfect positive relationship",
               ifelse(a==0,"No relationship",
                      ifelse(a < 0,"Negative relationship","Positive relationship")))
        
    })
    
    formulaText <- reactive({
        paste(input$Var,"~Sex")
    })
    
    output$sexPlot <- renderPlot({
        boxplot(main="Boxplot of Selected Variable~Sex",as.formula(formulaText()),
                data = data1,
                outline = input$outliers,
                col =c("red","lightblue","orange"), pch = 19) #Boxplot of selected variable.
        #You can choose outliers option. Default shows outliers.
        
    })
    formulamodel <- reactive({
        paste(input$Var5,"~",input$Var6,"+",input$Var7,"+",input$Var8,"+",input$Var9,
              "+",input$Var10,"+",input$Var11,"+",input$Var12)
    })
    output$model <- renderPrint({
        
        model <- lm(as.formula(formulamodel()))
        print(summary(model))
    })
    output$confint<-renderPrint({
        model <- lm(as.formula(formulamodel()))
        
        confint(model)
    })
    output$error<-renderPrint({
        model <- lm(as.formula(formulamodel()))
        
        sigma(model)/mean(Diameter)
        
    })
    output$modelplot<-renderPlot({
        model <- lm(as.formula(formulamodel()))
        
        par(mfrow=c(2,2))
        
        plot(model)
    })
    
} # servers


# Create Shiny object
shinyApp(ui = ui, server = server)
