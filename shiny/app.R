#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(usmap)
library(shinyWidgets)
library(ggcorrplot)
library(usmap)

# Read in all my data from RDS

obesity_region <- readRDS("data/obesity_region.RDS")
joined_cor <- readRDS("data/joined_cor.RDS")
bootstrap_joined <- readRDS("data/bootstrap_joined.RDS")
joined <- readRDS("data/joined.RDS")
food_poverty_mean <- readRDS("data/food_poverty_mean.RDS")
food_sum <- readRDS("data/food_sum.RDS")
food_lia <- readRDS("data/food_lia.RDS")

# ui code

# I want a navigation bar at the top, so I created a navbarPage. Then I wanted 4
# different tab panels in the navigation bar, meaning An Introduction,
# Variables, Findings, and About. I also included the title of my project in the
# navbarPage.

ui <- navbarPage(
    
    title = "Obesity and Food Access in the US",
    
    theme = shinytheme("cosmo"),
    
    tabPanel("An Introduction",
        fluidPage(
            
            br(),
            br(),

            # Outputted the title image that I wanted for the application. Then
            # I used fluidRow to create a nice layout for the introduction page
            # and used columns to choose where I wanted the text to go. In then
            # put in the text that I wanted to tell the audience. I changed the
            # colors and size of the text using style.
            
            imageOutput("obesity_image", width = "100%", height = "100%"),
            
            br(),
            
            fluidRow(column(2), column(8,
                                       
                     # I used h to create a heading and p to create a new
                     # paragraph. tag$b makes the text bold, and br() means a
                     # break.
                                       
                     h1(tags$b("Obesity and Food Insecurity in the United States"), align = "center", style = "color: darkred"),
                     
                     p(tags$b("Insights on America's Alarming Obesity Rates and an Analysis on Food Insecurity"), align = "center"),
                     
                     br(),
                     
                     p("Although one of the wealthiest countries in the world, the United States has one of the highest obesity rates
                  of any developed country. In 2016, according to the", a("Central Intelligence Agency", href = 
                  "https://www.cia.gov/library/publications/the-world-factbook/rankorder/2228rank.html"), ", the US saw 36.20% of its 
                  adult population classified as obese, ranking #12 worldwide. Granted, the US is an incredibly large and diverse country. 
                  Diets and quantity of physical activity vary region to region, state to state. Thus, it is important to look at the
                  variation in the percentage of population that is obese across different states and different regions."),
                     
                     br(),
                     
                     # By using style, I can change colors of the text.
                     
                     h3("Obesity", style = "color: darkred"),
                     
                     p("Obesity is defined as excessive or abnormal fat accumulation that may impair health, according to the", a("World
                  Health Organization", href = "https://www.who.int/news-room/fact-sheets/detail/obesity-and-overweight"), ". Body Mass Index is one way to determine obesity status; it is calculated by dividng a person's 
                  weight in kilograms by the square of their height in meters. A BMI equal to or greater than 30 is considered obese."),
                     
                     br(),
                     
                     p("Because those who are obese are more susceptible to severe illnesses, I wanted to explore different 
                       factors that could affect the percentage of obesity in a population. Some of these variables include, of
                       course, age, gender, and the region in which you live. However, I was also inspired by the unfortunate situation
                       that COVID19 has presented us with: food shortages. I wondered if having easy access to healthy and reliable
                       food supplies had any correlation with obesity in the US. After all, if you cannot easily reach healthy food options, it 
                       is possible that you would resort to cheap fast food or unhealthy diets. I was also curious to see if food insecurity was different
                       across different states and regions of the country, and how much it could be impacting obesity."),
                     
                     br(),
                     
                     h3("Food Insecurity", style = "color: darkred"),
                     
                     p("Currently, about 1 in 9 people in the US are suffering from food insecurity. According to", a("Feeding America", href =
                     "https://www.feedingamerica.org/hunger-in-america/food-insecurity"), ", food insecurity is a household's inability to provide 
                     enough food for every person to live an active, healthy life. It is a method of measuring hunger in America, but 
                     there are often serious health consequences that come with food insecurity, especially for children and for seniors, 
                     who are often stuck between choosing to feed themselves and paying for medical care. Food insecurity can be caused by economic reasons, 
                     such as a lack of money to buy groceries. However, there are infrastructural causes as well, like living in a food 
                     desert, where there is either a lack in supplies of or difficulty in reaching fresh and healthy foods. This second definition of
                     food insecurity was one of the main variables in this analysis. Because poverty seems to be associated with food
                     insecurity, I looked at this variable as well."),
                     
                     br(),
                     
                     h3("Analysis", style = "color: darkred"),
                     
                     p("There are two main variables that I examined, as aforementioned: obesity and food access. Under the Variables 
                       in Question tab, you will find my analysis on both. The Obesity tab explores obesity across the US over time. It also looks at 
                       how obesity differs across regions in the country. Within this tab, you can find a comparison between obesity and poverty as well.
                       The Food Access tab looks at food access across different states in the US. Lastly, the Findings tab looks at the correlations
                       between variables and regression models for predicting mean percentage of obesity in a population."),
                     
                     br(),
                     br(),
                     br()
)))),
    
# I also want there to be a drop down menu for my variables in the navigation
# bar, and I can do that using navbarMenu. Within the obesity variable section,
# I added different tabs, like "The US," "Poverty," and "Regions" using
# tabsetPanel. fluidRow created a nice layout for the page. I used imageOutput
# again to display my obesity in the US map.

    navbarMenu("Variables in Question",
               tabPanel("Obesity",
                    tabsetPanel(
                        tabPanel("The United States",
                            
                        fluidRow(
                            column(3),
                            
                            column(6,
                                   
                                br(),
                                
                                h2("Obesity Across the United States Over Time", style = "color: darkred; text-align: center"),
                                
                                br(),
                                
                                 p("How does the percentage of the population classified as obese or overweight change over time?
                                    The graphic below suggests that the percentage increases over time, looking at years from
                                    2011-2018. Where does most of the change occur?", style = "text-align: center")),

                                ),
                        
                        fluidRow(
                            
                             imageOutput("obesity_usmap")
                        )),
                        
               tabPanel("Regions",
                        
                        br(),
                        br(),
                       
                        # Using sidebarLayout allows me to create a sidebarPanel
                        # and a mainPanel, which is the graph I wanted. I allow
                        # the user to choose which region of the graph they want
                        # to see by selectInput, which corresponds in the server
                        # portion of Shiny later.
                         
                        sidebarLayout(
                            sidebarPanel(column(4),
                                         
                                h3("Obesity Across Individual Regions", style = "color: darkred"),          
                        
                                p("How does the percentage of the population classified as obese or overweight differ across the
                                regions of America over time? The graph to the right suggests that all regions have seen an
                                increase in mean percentages, with relatively steep increases each year, looking at years from
                                2011-2018. Which region has the steepest slope?"),
                                
                                br(),
                                br(),
                                
                                selectInput("region", tags$b("Choose a region:"),
                                            choices = c("West", "South", "Northeast", "Midwest"),
                                            multiple = FALSE)
                               
                               ),
                            
                            mainPanel( 
                                imageOutput("obesity_region")
                                )
                        ),
               
                        br(),
                        br(),
                        br(),
                        
                        # # Using sidebarLayout allows me to create a
                        # sidebarPanel and a mainPanel, which is the graph I
                        # wanted.
                        
                        sidebarLayout(
                          sidebarPanel(column(4),
                                       
                                h3("Obesity Across Regions Comparatively", style = "color: darkred"),          
                                       
                                br(),
                                
                                p("It may be helpful to see how the change in the mean percentage of the population 
                                  classified as obese or overweight overtime differs region to region. This is easier
                                  to compare when the graphs are shown side-by-side. It appears the South and the Midwest
                                  are increasing the fastest, and have also started off the highest as well.")),
                          
                          mainPanel(
                              imageOutput("obesity_region_facet")
                          )
                        )
                ),
               
               tabPanel("Poverty",
                        
                br(),
                br(),
                
                # Using sidebarLayout allows me to create a sidebarPanel and a
                # mainPanel, which is the graph I wanted. I allow the user to
                # choose which region of the graph they want to see by
                # selectInput, which corresponds in the server portion of Shiny
                # later.
                
                sidebarLayout(
                    sidebarPanel(column(3),
                        
                        h3("Poverty in the US and Obesity", style = "color: indigo"),
                        
                        br(),
                        
                        p("I was curious to see how the poverty rate might be related to obesity rates, especially when considering
                          the ruralness of the area in which the subject is living. Upon graphing the two variables, it appears
                          that there does seem to be a slight positive relationship between the two. The relationship is stronger
                          in more rural settings than urban settings. The definition of rural and urban are defined according the 2010 US
                          Census Bureau's definition. However, it is necessary to look at the correlation between the two variables
                          to determine if there is a strong relationship."),
                        
                        br(),
                        
                        selectInput("urban", tags$b("Ruralness:"),
                                    choices = c("Urban" = "urban", 
                                                "Rural" = "rural"),
                                    multiple = FALSE)
                        
                    ),
                    
                    mainPanel(
                        plotOutput("poverty_obesity_plot")
                    )
                ),
                
                      # I wanted another graph below the previous one, so I
                      # created another sidebarPanel layout below the one on
                      # top.
                
                       sidebarPanel(
                           
                           h3("Poverty in the US as a Whole", style = "color: indigo"),
                           
                           br(),
                           
                           p("Given the map on the US's increasing obesity rates overtime (under the The United States tab), I thought 
                           it would be interesting to create a map of the US's poverty rates as well and compare the both of them. 
                           Looking at the two maps together, it is interesting to see which states have darker colors on both, meaning
                           higher poverty and obesity rates. Which states have similarities in the obesity rate map and the poverty rate map?")
                           
                       ),
                       
                       mainPanel(
                           plotOutput("poverty_plot"),

                           br(),
                           br(),
                           
                   )
               )
               )),
               
                tabPanel("Food Access",
                     
                    # I used tabsetPanel again to create tabs within the US tab.
                    # fluidRow allows me to create a nice layout for my page.
                    # The column function sets the text into the different grid
                    # layouts of fluidRow to help me organize my text.
                             
                    tabsetPanel(
                        tabPanel("The United States",
                                 
                                 br(),
                                 
                                 fluidRow(
                                     column(3),
                                     
                                     column(6,
                                            
                                            br(),
                                            
                                            h2("Food Insecurity in the US", style = "color: darkgreen; text-align: center"),
                                            
                                            br(),
                                            
                                            p("Given the map on the US's increasing obesity rates overtime (under the The United States tab), 
                                       I thought it would be interesting to compare a map that examines the US's food insecurities as well. This map
                                       gives a broad overview of what food insecurity looks like in the United States.
                                       Looking at the two maps together, it is interesting to see which states have darker colors on both, meaning
                                       higher food insecurity and obesity rates. Which states have similarities in the obesity rate map and the 
                                       food insecurity map?"
                                 )),
                        
                             fluidRow(
                                imageOutput("lia_usmap")
                        )
                        )),
                        
                        tabPanel("States",
                                 
                                 br(),
                                 br(),
                                 
                                 # Using sidebarLayout allows me to create a sidebarPanel and a mainPanel, which is the graph I wanted. 
                                 
                                 sidebarLayout(
                                     sidebarPanel(column(3),
                                                  
                                                  h3("Food Insecurity Across States", style = "color: darkgreen"),
                                                  
                                                  br(),
                                                  
                                                  p("I also wanted to look at food insecurity specifically across different states. 
                                                    Food insecurity seems the most prominent in the Southern region, and the least
                                                    prominent in the Northeast region. This is sensible, as the South often has more
                                                    rural areas than does the Northeast"),
                                                  
                                     ),
                                     
                                     mainPanel(
                                         plotOutput("lia_states")
                                     )
                                 )     
                                 ),
                        
                        tabPanel("Poverty",
                                 
                                 br(),
                                 br(),
                                 
                        # Using sidebarLayout allows me to create a sidebarPanel
                        # and a mainPanel, which is the graph I wanted. Then I
                        # used selectInput to allow the user to choose if they
                        # want to see urban or rural on the graph, which
                        # corresponds with the server code later on.
                            
                             sidebarLayout(
                                 sidebarPanel(column(3),
                                              
                                              h3("Poverty in the US and Food Insecurity", style = "color: darkgreen"),
                                              
                                              br(),
                                              
                                              p("I was curious how poverty affects food insecurity as well,
                                              and how that depends on the ruralness of the area you live in.
                                              It appears from the differences in the urban and rural graphs that 
                                              being impoverished in a rural area means a stronger positive relationship with having
                                              low food access, more so than being impoverished in an urban area, though both
                                              have positive relationships. This is logical, as having less income means a lower
                                              ability to buy healthier foods. Furthermore, living in an urban area means a better 
                                              public transportation system, so even if you do not own a car, there are still ways to
                                              go to large grocery stores, whereas the same cannot be said for rural areas."),
                                              
                                              br(),
                                              
                                              p(tags$em("When choosing the different ruralness options, the graph may take quite a while to 
                                                        load. Thank you for your patience.")),
                                              
                                              br(),
                                              
                                              selectInput("urban", tags$b("Ruralness:"),
                                                          choices = c("Urban" = "urban", 
                                                                      "Rural" = "rural"),
                                                          multiple = FALSE)
                                              
                                 ),
                                 
                                 mainPanel(
                                     plotOutput("poverty_lia_plot")
                                 )
                             )
                             )    
                            
                                 )
                        
                        )
                    
),
    
# I created my last tab panel (aside from the About page) using tabPanel and
# named it Findings. I want tabs within this tab, so I used tabsetPanel to
# create the tabs "Corrleation," "Variables," and "Model."

    tabPanel("Findings",
             tabsetPanel(
                 tabPanel("Correlation",
                          
                          br(),
                          br(),
                          
                  # Using sidebarLayout allows me to create a sidebarPanel and a
                  # mainPanel, which is the graph I wanted.
                  
                  sidebarLayout(
                      
                      sidebarPanel(
                                   
                                h3("Correlation Between Obesity and Different Variables", style = "color: darkred"),
                                
                                br(),
                            
                                p("I was curious to see whether or not a status of low income and low food access
                                  was related to obesity status. For a preliminary examination, I looked at the correlation
                                  between the status of low income and low food access and obesity. However, the
                                  correlation is < 0.1, meaning the correlation is very low, as is the correlation
                                  between just the status of low food access and obesity. Thus, I decided to look at whether
                                  or not poverty plays a role in obesity. Although it is slightly higher, > 0.1, the 
                                  correlation is still fairly low. It turns out, then, that food insecurity does not seem
                                  to affect obesity rates. Nevertheless, it is clear that poverty rates are 
                                  correlated with having low income and low food access, as well as low food access.")),
                            
                        mainPanel(column(2),
                                 plotOutput("corr")
                  )
                  )       
                         ),
                    
                  tabPanel("Variables",
                              
                        br(),
                        br(),
                        
                        # Using sidebarLayout allows me to create a sidebarPanel
                        # and a mainPanel, which is the graph I wanted. I wanted
                        # to create a drop down menu, and I was able to do so by
                        # using selectInput to allow the user to choose which
                        # variables they wanted to look at.
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                h3("How Different Demographic Variables Affect Obesity and Food Access", style = "color: darkred"),
                                
                                br(),
                                
                                p("Because the variables I was curious to examine turn out to be unrelated, I decided to
                              examine other variables in relation to obesity rates while continuing
                              to examine other variables that may affect the status of food insecurity in the graph to
                              the right. It appears differences in age and gender see the biggest difference in mean obesity, 
                              but not ruralness. However, differences in living location see the biggest difference in 
                              status of food insecurity the most, whereas age and gender do not."),
                                
                                br(),
                                
                                selectInput("yvariable", tags$b("Outcome Variable:"),
                                            choices = c("Mean Percent Obese" = "mean_perc_obese", 
                                                        "Mean Percent Low Income and Low Food Access" = "perc_lia", 
                                                        "Mean Percent Low Food Access" = "perc_la")),

                                selectInput("demographic", tags$b("Demographic Variable:"),
                                            choices = c("Age" = "age", 
                                                        "Gender" = "gender", 
                                                        "Race" = "race", 
                                                        "Ruralness" = "urban")),
                            ),
                            
                            mainPanel(
                                
                                plotOutput("demographic")
                            
                        )
                        )
                    ),
                 
                    tabPanel("Model",
                        
                        br(),
                        br(),
                        
                        # Using fluidPage gives the page a new layout. Using
                        # sidebarLayout allows me to create a \ sidebarPanel and
                        # a mainPanel, which is the graph I wanted.
                        
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(
                                  
                                  br(),
                                  
                                  h3("Regression Models", style = "color: darkred; text-align: left"),
                                  
                                  br(),
                                  
                                  p("In order to create a regression model, I bootstrapped my data, replicating it several times until
                                    there were 50,000 observations. This allowed me to increase my sample size as well as increase 
                                    randomness to better reflect the overall population. I then used a linear regression model to 
                                    predict the effect of difference variables on obesity rates."),
                                  
                                  br(),
                                  
                                  h4("Outcomes", style = "color: darkred"),
                                  
                                  p("From the regression models, as well as the correlation plot from the Correlation tab, I discovered
                                  that my two variables of interest (obesity and food insecurity) were very weakly related. Moving from
                                  food secure (meaning not low income and low food access) to food insecure only changed the average obesity
                                  rate by less than 1% of the population. The same goes for the low food access defintion of 
                                  food insecurity. In addition, povery rates also seem not to correlate much with obesity rates either. Instead,
                                  obesity rates seem to be related to age and gender the most. It is difficult to determine a causal relationship
                                  based on the following analysis, as there are many other confounding variables that could be addressed,
                                  such as food prices for healthy foods or physical activity."),
                                  
                                  br(),
                                  
                                  h4("How to read a linear regression model with no interaction", style = "color: darkred"),
                                  
                                  p("The variable column indicates the variable of interest. In some cases, the variable
                                    will have different levels associated with it. For example, for the Gender variable, there
                                    are 2 categories: genderFemale and genderMale. The (Intercept) portion of the table,
                                    usually the first row in the table, depicts the data for the baseline level, typically
                                    taking the first level alphabetically. In this case, it is the genderFemale level. If there
                                    are no levels, then in Intercept value generally represents when the variable is equal to 0."),
                                  
                                  br(),
                                  
                                  p("The Estimate column gives an estimate for how the dependent variable may change with a 
                                    1 unit increases in the independent variable, also known as the slope of the regression, for 
                                    numerical independent variables. In this case, for a categorical variable, the estimate for genderFemale 
                                    shows that, for females in a population, there is an average of 28% obesity. For genderMale, the estimate
                                    value is an offset from the baseline variable. So, for genderMale, there is a 6% higher average of obesity
                                    in a population than for genderFemale, or an average of 34%."),
                                  
                                  br(),
                                  
                                  p("The Lower Bound and Upper Bound columns yield the confidence interval. This tells us how confident we are
                                    that the true value of whatever parameter we are estimating falls within the given upper and lower bound ranges.
                                    For example, for genderFemale, we are 95% confident that the true average percentage of obesity in a population 
                                    lies within the range of 28.1 and 28.3%.")),
                                  
                            # My gt regression tables are saved as html files,
                            # so I needed to use includeHTML to read in the file
                            # into Shiny.
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Age",
                                             br(),
                                             br(),
                                        includeHTML("plots/age_gt.html"),
                                             br(),
                                             br(),
                                        plotOutput("lm_age")),
                                    tabPanel("Gender",
                                             br(),
                                             br(),
                                        includeHTML("plots/gender_gt.html"),
                                             br(),
                                             br(),
                                        plotOutput("lm_gender")),
                                    tabPanel("Race",
                                             br(),
                                             br(),
                                        includeHTML("plots/race_gt.html"),
                                             br(),
                                             br(),
                                        plotOutput("lm_race")),
                                    tabPanel("Region",
                                             br(),
                                             br(),
                                        includeHTML("plots/region_gt.html"),
                                             br(),
                                             br(),
                                        plotOutput("lm_region")),
                                    tabPanel("Ruralness",
                                             br(),
                                             br(),
                                        includeHTML("plots/urban_gt.html"),
                                             br(),
                                             br(),
                                        plotOutput("lm_ruralness")),
                                    tabPanel("Low Income and Low Food Access",
                                             br(),
                                             br(),
                                        includeHTML("plots/lia_gt.html")),
                                    tabPanel("Low Food Access",
                                             br(),
                                             br(),
                                        includeHTML("plots/la_gt.html")),
                                    tabPanel("Poverty",
                                             br(),
                                             br(),
                                        includeHTML("plots/poverty_gt.html"))
                                         ))))
                             
                             )
                 )
             
             ),

    # To create a drop down menu in the navigation bar, I used navbarMenu. Then
    # I used tabPanel to label the menu options, with the options being "The
    # Data" and the page about me. To include hyperlinks in the text, I used
    # certain formating like a() and the function href =.
    
    navbarMenu("About",
               tabPanel("The Data",
                        
                        column(10,
                        
                        h2("Sourcing the Data", style = "color: darkred"),
                        
                        br(),
                        
                        p("The data I have used in this analysis are from the United States Department of Agriculture (USDA), the 
                        Center for Disease Control (CDC) and from the 2010 United States Census Bureau. The obesity data is obtained 
                        from the CDC's ", 
                        a("Behavioral Risk Factor Surveillance System", href = 
                        "https://chronicdata.cdc.gov/Nutrition-Physical-Activity-and-Obesity/Nutrition-Physical-Activity-and-Obesity-Behavioral/hn4x-zwk7/data"),
                        ". The first few visualizations include obesity data for 2011-2018. The food insecurity data is obtained from the", 
                        a("USDA", 
                        href = "https://www.ers.usda.gov/data-products/food-access-research-atlas/"),
                        " , which drew its analysis from the 2010 US Census Bureau survey.")),
                        
                        br(),
                        
                        column(10,
                        
                        h2("Analyzing the Data", style = "color: darkred"),
                        
                        br(),
                        
                        p("To analyze the obesity data with the food insecurity data,
                        I had to join the two datasets together. Because the obesity data only contained data from 2011 onward, and
                          the poverty dataset only contained data from 2010, I decided to subset for 2011 observations only from
                          the obesity dataset, as that was the closest year to 2010, so differences in obesity obesrvations between 2011 and in what
                          would have been the obesrvations for 2010 would be reduced. Thus, for visualisations including the poverty variable, 
                          and variables involving food insecurity, the data used is for 2011 obesity data compared to the 2010 US Census Bureau survey 
                          on food insecurity."),
                        
                        p("For more information on how I cleaned, tidied, and analyzed the data, please refer to my",
                          a("GitHub", href = "https://github.com/rjphan/ms_8"),
                          " account."),
                        
                        br(),
                        
                        h2("Resources and Ways to Help", style = "color: darkred"),
                        
                        br(),
                        
                        p("To look for more information on food insecurity, please visit", a("Feeding America", 
                        href = "https://www.feedingamerica.org/"), " and the", a("USDA",
                        href = "https://www.ers.usda.gov/topics/food-nutrition-assistance/food-security-in-the-us/key-statistics-graphics.aspx"),
                        ". Please also consider donating to local food banks. You can find the one nearest you using Feeding America's", 
                        a("Food Pantry Finder", href = "https://www.feedingamerica.org/find-your-local-foodbank"), ". 
                        Both non-perishable foods, personal care items, and monetary donations are generally accepted."),
                        
                        p("To look for more information on obesity, please visit the", a("Center for Disease Control and Prevention", 
                        href = "https://www.cdc.gov/obesity/index.html"), " or the", a("World Health Organization", href =
                        "https://www.who.int/news-room/fact-sheets/detail/obesity-and-overweight"), ". The CDC also has tips on how to",
                        a("minimize the risk of obesity", href = "https://www.cdc.gov/obesity/strategies/index.html"), ".")
                        
               )),
               
               # Using tags$em allowed me to make the text italicized.
               
               tabPanel("The Author",
                        
                        column(10,
                        
                        h2("Contact", style = "color: darkred"),
                        
                        br(),
                        
                        p("My name is Rachel Phan, and I am a Harvard Undergraduate (Class of 2021) studying environmental science
                          and public policy, with a focus on urbanization and real estate. I am also curious about business, marketing, and 
                          food systems, which is why I thought it was be interesting to analyze data on food access. I hope my project helped
                          to identify some social issues dealing with food and nutritional health."),
                        
                        p("You can see more of my work at my", a("GitHub", href = "https://github.com/rjphan"), " account. Please feel free
                        to also contact me at my email:", a("rphan@college.harvard.edu", href = "mailto: rphan@college.harvard.edu"), ". I would be happy to talk about this project or anything
                          else!"),
                        
                        br(),
                        
                        p(tags$em("Project by Rachel Phan, '21, for GOV1005: Data Science, Spring 2020"))
                        
                        )
               )
    )
)
    

# Server code

server <- function(input, output) {
    
    # Load in image for top of app for Introductio page. I decide the height and
    # width of the image, and set deleteFile = FALSE so my image wouldn't get
    # deleted from my files once the app runs. I set it equal to TRUE by mistake
    # once and it was a hassle. renderImage allows you to render image files.
    
    output$obesity_image <- renderImage({
        list(src = './images/obesity.image.jpg', 
             height = 300,
             width = 600,
             style = "display: block; margin-left: auto; margin-right: auto;")}, 
        deleteFile = FALSE)
    
    output$obesity_usmap <- renderImage({
        list(src = "./plots/obesity_mean_usmap.gif",
             contentType = 'image/gif',
             height = 650,
             width = 650,
             style = "display: block; margin-left: auto; margin-right: auto;")}, 
        deleteFile = FALSE)
    
    # Now I needed to render some plots. I did this using renderPlot. It is
    # basically the same code as in an RMD file. The only thing I had to do was
    # set the data argument for ggplot instead of piping data like usual. The
    # name after output$ is what you should call in the ui portion of Shiny to
    # get the same grah or image.
    
    output$obesity_region <- renderPlot({
        obesity_region <- obesity_region %>% 
        filter(!is.na(region)) %>% 
        filter(region == input$region)
        
        ggplot(data = obesity_region, aes(x = year, y = mean_perc)) +
            geom_jitter(aes(alpha = 0.1), show.legend = FALSE) +
            theme_minimal() +
            labs(title = "Percent of Population Classified as Obese or Overweight \n in US Over Time by Region",
                 caption = "Data from Center for Disease Control",
                 y = "Mean Percent of Population",
                 x = "Year") +
            geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "dark red") +
            theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank())
    })
    
    # Next I needed to output a bunch of different plots and images, so I used
    # renderImage and also renderPlot. Set filter(urban == input$urban) allows
    # the user from the ui portion of Shiny to choose the factor level to filter
    # my data by, which is why input$urban works. I then graphed the data and
    # set a best fit line.
    
    output$obesity_region_facet <- renderImage({
        list(src = './plots/obesity_region_plot.png', 
             height = 600,
             width = 650,
             style = "display: block; margin-left: auto; margin-right: auto;")}, 
        deleteFile = FALSE)
    
    output$poverty_obesity_plot <- renderPlot({
        bootstrap_joined <- bootstrap_joined %>% 
            filter(urban == input$urban)
        
        ggplot(data = bootstrap_joined, aes(x = mean_poverty, y = mean_perc_obese)) +
            geom_jitter(color = "purple") +
            theme_minimal() +
            theme(panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank()) +
            labs(title = "Poverty Rate on Obesity Rate",
                 caption = "Data from US Department of Agriculture, 2010 Census Bureau, \n and Center for Disease Control",
                 x = "Mean Poverty Rate",
                 y = "Mean Obesity Rate") +
            geom_smooth(method = "lm", color = "blue")
    })
    
    # I used renderPlot and renderImage to render a couple graphs. I plotted
    # poverty on a US map, and then low income and low food access on the US
    # map.
    
    output$poverty_plot <- renderPlot({
        plot_usmap(data = food_poverty_mean, values = "mean_poverty", 
                   color = "white", labels = FALSE) +
            theme(legend.position = "right",
                  plot.title = element_text(hjust = 0.5)) + 
            scale_fill_continuous(low = "white", high = "blue", name = "Mean Poverty Rate") +
            labs(title = "Poverty Rate in the US",
                 caption = "Data from US Department of Agriculture and 2010 Census Bureau") +
            theme_void()
    })
    
    output$lia_usmap <- renderImage({
        list(src = './plots/lia_usmap.png', 
             height = 650,
             width = 650,
             style = "display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE)
    
    # I then made the bar plot for the low income and low food access variable.
    
    output$lia_states <- renderPlot({
        food_lia <- food_lia %>% 
            group_by(region)
        
        ggplot(data = food_lia, aes(y = reorder(state, perc), x = perc, fill = region)) +
            geom_col() +
            theme_minimal() +
            theme(title = element_text(size = 15),
                  axis.text.y = element_text(hjust = 1, size = 13),
                  panel.grid.major.y = element_line(color = "white"),
                  panel.grid.minor.y = element_line(color = "white"),
                  plot.subtitle = element_text(size = 12)) +
            labs(title = "Percent of Counties with a Lack of Food Access and Low Income Population",
                 subtitle = "Lack of Food Access Defined by 33% of the Population Living More Than 1 Mile (Urban) 
                 Or 10 Miles (Rural) Away from Supermarket, Supercenter, or Grocery Store Who are Low Income",
                 caption = "Data from US Department of Agriculture and 2010 Census Bureau",
                 x = "Percent of Counties",
                 y = "State",
                 fill = "Region") +
            geom_text(aes(label = perc), hjust = -0.3, size = 4)
        
    },
    width = 940,
    height = 750)
    
    # I created a graph where you can choose the amount of ruralness using
    # input$urban. I used geom_jitter because some points overlapped with each
    # other.
    
    output$poverty_lia_plot <- renderPlot({
        bootstrap_joined <- bootstrap_joined %>% 
            filter(urban == input$urban)
        
        ggplot(data = bootstrap_joined, aes(x = mean_poverty, y = perc_lia)) +
            geom_jitter(color = "dark green") +
            theme_minimal() +
            theme(panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank()) +
            labs(title = "Relationship Between Poverty Rate and Low Income and Low Food Access",
                 caption = "Data from US Department of Agriculture and 2010 Census Bureau",
                 x = "Mean Poverty Rate",
                 y = "Percent of Population Classified as \n Having Low Income and Low Food Access") +
            geom_smooth(method = "lm", color = "green")
    })
    
    # I created a correlation plot between variables using ggcorrplot.
    
    output$corr <- renderPlot({
        ggcorrplot(joined_cor, 
                   type = "upper", 
                   colors = c("blue", "white", "dark red"),
                   lab = TRUE,
                   lab_size = 4,
                   lab_col = "black",
                   ggtheme = theme_void) +
            labs(title = "Correlations Between Obesity, \n Poverty, and Food Access")  + 
            theme(plot.title = element_text(hjust = 0.5, size = 15),
                  legend.position = "none")
    },
    width = 500,
    height = 500)
    
    # I want to create a plot where you can choose the yvariable and demographic
    # (xvariable) to look, so I used both input$demographic and input$yvariable.
    # However, when I use the inputs, the graph doesn't factor the categorical
    # variables, even though I specifically mutated the variables to be factors.
    # I realized this is because, if I just use aes(x =, y =), R will read
    # whatever I set x and y equal to as a single vector with one observation.
    # Thus, I need to use aes_string to factor the categorical variables and
    # make them reactive. Using ifelse and paste allows me to print labels that
    # are based on the user input.
    
    output$demographic <- renderPlot({
        bootstrap_joined <- bootstrap_joined %>%
        mutate(age = as.factor(age),
               gender = as.factor(gender),
               race = as.factor(race),
               urban = as.factor(urban)) %>% 
            drop_na(input$demographic)

        new_xaxis_label <- if(input$demographic == "age"){
            print("Age")
        } else if(input$demographic == "gender"){
            print("Gender")
        } else if(input$demographic == "race"){
            print("Race")
        } else if(input$demographic == "urban"){
            print("Ruralness")
        }

        new_yaxis_label <- if(input$yvariable == "perc_lia"){
            print("Mean % of Population Classified as Low Income and Low Food Access")
        } else if(input$yvariable == "perc_la"){
            print("Mean % of Population Classified as Low Food Access")
        } else if(input$yvariable == "mean_perc_obese"){
            print("Mean % of Population Classified as Obese or Overweight")
        }
        
        new_legend_label <- if(input$demographic == "age"){
            print("Age")
        } else if(input$demographic == "gender"){
            print("Gender")
        } else if(input$demographic == "race"){
            print("Race")
        } else if(input$demographic == "urban"){
            print("Ruralness")
        }
        
        ggplot(data = bootstrap_joined, aes_string(x = input$demographic,
                                                   y = input$yvariable,
                                                   color = input$demographic)) +
            geom_jitter() +
            theme_minimal() +
            theme(panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text.x = element_text(angle = 15, hjust = 1)) +
            labs(title = paste("Relationship Between ", new_xaxis_label, " and ", new_yaxis_label, "", sep = ""),
                 caption = "Data from US Department of Agriculture and 2010 Census Bureau",
                 color = new_legend_label,
                 x = new_xaxis_label,
                 y = new_yaxis_label)
        
        
    })
    
    # To better visualize the regression than just through gt tables, 
    # I made a plot to show the predictions. Since each estimate is 
    # an offset from the baseline variable, I added the baseline 
    # estimate to each of the other levels in the variable. However
    # I could only make visualizations for the categorical data,
    # since I only had one point for the numerical data. I had 
    # tried mapping the linear model onto the whole dataset, 
    # but because of the nature of the data, it yielded an error
    # that my data was a double but it could only take a single.
    # Thus, I decided not to pursue visualizations for the 
    # numerical variables.
    
    # I created a visualization for age by first using tidy. Then,
    # I wanted to add the baseline estimate, so I made that an object 
    # to add later. I mutated a new column called Estimate that added
    # on the baseline estimate to the other levels of the variable.
    # I also wanted to rename some specific observations, like the
    # (Intercept) term, which wouldn't make sense to viewers,
    # and I did so using an arrow and calling the specific cell the
    # observation was in. Then I used str_sub to get rid of the 
    # prefixes in each variable. str_sub tells R which character position 
    # to start the name in. For example, for age, the position I want
    # the names to start in is 4, so  that gets rid of the "age" prefix,
    # which has 3 characters and thus occupies positions 1-3. Then I
    # used ggplot to make a scatterplot.
    
    output$lm_age <- renderPlot({
            lm_age <- lm(mean_perc_obese ~ age, data = bootstrap_joined) %>% 
                tidy()
            
            baseline <- lm_age %>% 
                filter(term == "(Intercept)") %>% 
                select(estimate) %>% 
                pull() %>% 
                round(digits = 2)
            
            lm_age2 <- lm_age %>% 
            mutate("Estimate" = case_when(
                term == "(Intercept)" ~ estimate,
                term != "(Intercept)" ~ estimate + baseline
            ))
            
            lm_age2[1, "term"] <- "age18-24"
            lm_age2$term <- str_sub(lm_age2$term, 4)
        
            ggplot(data = lm_age2, aes(x = term, y = Estimate, color = term)) +
            geom_point(aes(size = 20), show.legend = FALSE) +
            theme_minimal() +
            theme(axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14)) +    
            labs(title = "Predicted Mean Percentage of Population Classified as Obese Based on Age",
                 caption = "Data from USDA, 2010 Census Bureau, and CDC",
                 x = "Age",
                 y = "Mean Obesity Rate")
    })
    
    # I created a visualization for gender by first using tidy. Then,
    # I wanted to add the baseline estimate, so I made that an object 
    # to add later. I mutated a new column called Estimate that added
    # on the baseline estimate to the other levels of the variable.
    # I also wanted to rename some specific observations, like the
    # (Intercept) term, which wouldn't make sense to viewers,
    # and I did so using an arrow and calling the specific cell the
    # observation was in. Then I used str_sub to get rid of the 
    # prefixes in each variable. str_sub tells R which character position 
    # to start the name in. For example, for gender, the position I want
    # the names to start in is 7, so  that gets rid of the "gender" prefix,
    # which has 6 characters and thus occupies positions 1-6. Then I
    # used ggplot to make a scatterplot.
    
    output$lm_gender <- renderPlot({
        lm_gender <- lm(mean_perc_obese ~ gender, data = bootstrap_joined) %>% 
            tidy()

        baseline <- lm_gender %>%
            filter(term == "(Intercept)") %>%
            select(estimate) %>%
            pull() %>%
            round(digits = 2)

        lm_gender2 <- lm_gender %>%
            mutate("Estimate" = case_when(
                term == "(Intercept)" ~ estimate,
                term != "(Intercept)" ~ estimate + baseline
            ))

        lm_gender2[1, "term"] <- "genderFemale"
        lm_gender2$term <- str_sub(lm_gender2$term, 7)

        ggplot(data = lm_gender2, aes(x = term, y = Estimate, color = term)) +
            geom_point(aes(size = 20), show.legend = FALSE) +
            theme_minimal() +
            theme(axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14)) +
            labs(title = "Predicted Mean Percentage of Population Classified as Obese Based on Gender",
                 caption = "Data from USDA, 2010 Census Bureau, and CDC",
                 x = "Gender",
                 y = "Mean Obesity Rate")
    })
    
    # I created a visualization for race by first using tidy. Then,
    # I wanted to add the baseline estimate, so I made that an object 
    # to add later. I mutated a new column called Estimate that added
    # on the baseline estimate to the other levels of the variable.
    # I also wanted to rename some specific observations, like the
    # (Intercept) term, which wouldn't make sense to viewers,
    # and I did so using an arrow and calling the specific cell the
    # observation was in. Then I used str_sub to get rid of the 
    # prefixes in each variable. str_sub tells R which character position 
    # to start the name in. For example, for race, the position I want
    # the names to start in is 5, so  that gets rid of the "race" prefix,
    # which has 4 characters and thus occupies positions 1-4. Then I
    # used ggplot to make a scatterplot.
    
    output$lm_race <- renderPlot({
        lm_race <- lm(mean_perc_obese ~ race, data = bootstrap_joined) %>% 
            tidy()
        
        baseline <- lm_race %>%
            filter(term == "(Intercept)") %>%
            select(estimate) %>%
            pull() %>%
            round(digits = 2)
        
        lm_race2 <- lm_race %>%
            mutate("Estimate" = case_when(
                term == "(Intercept)" ~ estimate,
                term != "(Intercept)" ~ estimate + baseline
            ))
        
        lm_race2[1, "term"] <- "    2 or more races"
        lm_race2$term <- str_sub(lm_race2$term, 5)
        
        ggplot(data = lm_race2, aes(x = term, y = Estimate, color = term)) +
            geom_point(aes(size = 20), show.legend = FALSE) +
            theme_minimal() +
            theme(axis.text.x = element_text(size = 12, angle = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14)) +
            labs(title = "Predicted Mean Percentage of Population Classified as Obese Based on Race",
                 caption = "Data from USDA, 2010 Census Bureau, and CDC",
                 x = "Race",
                 y = "Mean Obesity Rate")
    })
    
    # I created a visualization for region by first using tidy. Then,
    # I wanted to add the baseline estimate, so I made that an object 
    # to add later. I mutated a new column called Estimate that added
    # on the baseline estimate to the other levels of the variable.
    # I also wanted to rename some specific observations, like the
    # (Intercept) term, which wouldn't make sense to viewers,
    # and I did so using an arrow and calling the specific cell the
    # observation was in. Then I used str_sub to get rid of the 
    # prefixes in each variable. str_sub tells R which character position 
    # to start the name in. For example, for region, the position I want
    # the names to start in is 7, so  that gets rid of the "region" prefix,
    # which has 6 characters and thus occupies positions 1-6. Then I
    # used ggplot to make a scatterplot.
    
    output$lm_region <- renderPlot({
        lm_region <- lm(mean_perc_obese ~ region, data = bootstrap_joined) %>% 
            tidy()
        
        baseline <- lm_region %>%
            filter(term == "(Intercept)") %>%
            select(estimate) %>%
            pull() %>%
            round(digits = 2)
        
        lm_region2 <- lm_region %>%
            mutate("Estimate" = case_when(
                term == "(Intercept)" ~ estimate,
                term != "(Intercept)" ~ estimate + baseline
            ))
        
        lm_region2[1, "term"] <- "regionMidwest"
        lm_region2$term <- str_sub(lm_region2$term, 7)
        
        ggplot(data = lm_region2, aes(x = term, y = Estimate, color = term)) +
            geom_point(aes(size = 20), show.legend = FALSE) +
            theme_minimal() +
            theme(axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14)) +
            labs(title = "Predicted Mean Percentage of Population Classified as Obese Based on Region",
                 caption = "Data from USDA, 2010 Census Bureau, and CDC",
                 x = "Region",
                 y = "Mean Obesity Rate")
    })
    
    # I created a visualization for urban by first using tidy. Then,
    # I wanted to add the baseline estimate, so I made that an object 
    # to add later. I mutated a new column called Estimate that added
    # on the baseline estimate to the other levels of the variable.
    # I also wanted to rename some specific observations, like the
    # (Intercept) term, which wouldn't make sense to viewers,
    # and I did so using an arrow and calling the specific cell the
    # observation was in. Then I used str_sub to get rid of the 
    # prefixes in each variable. str_sub tells R which character position 
    # to start the name in. For example, for urban, the position I want
    # the names to start in is 6, so  that gets rid of the "urban" prefix,
    # which has 5 characters and thus occupies positions 1-5. Then I
    # used ggplot to make a scatterplot.
    
    output$lm_ruralness <- renderPlot({
        lm_urban <- lm(mean_perc_obese ~ urban, data = bootstrap_joined) %>% 
            tidy()
        
        baseline <- lm_urban %>%
            filter(term == "(Intercept)") %>%
            select(estimate) %>%
            pull() %>%
            round(digits = 2)
        
        lm_urban2 <- lm_urban %>%
            mutate("Estimate" = case_when(
                term == "(Intercept)" ~ estimate,
                term != "(Intercept)" ~ estimate + baseline
            ))
        
        lm_urban2[1, "term"] <- "urbanRural"
        lm_urban2[2, "term"] <- "urbanUrban"
        lm_urban2$term <- str_sub(lm_urban2$term, 6)
        
        ggplot(data = lm_urban2, aes(x = term, y = Estimate, color = term)) +
            geom_point(aes(size = 20), show.legend = FALSE) +
            theme_minimal() +
            theme(axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14)) +
            labs(title = "Predicted Mean Percentage of Population Classified as Obese Based on Ruralness",
                 caption = "Data from USDA, 2010 Census Bureau, and CDC",
                 x = "Ruralness",
                 y = "Mean Obesity Rate")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# To Do Could make predictor graphic based on your demographic like Steph's
# project. Could add little icons in the navbar tab using icon.


