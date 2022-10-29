library(shinydashboard)
library(shiny)
library(highcharter)

dashboardPage(skin = "blue",
              
              dashboardHeader(title = "Human Resource Analysis",titleWidth = 300),
              dashboardSidebar(
                sidebarMenu(
                  menuItem(
                    text = "Overview", 
                    tabName = "menu_1", 
                    icon = icon("globe")), 
                  
                  menuItem(text = "Employee",
                           tabName = "menu_2",
                           icon = icon("user")),
                  
                  menuItem(text = "Manager",
                           tabName = "menu_3",
                           icon = icon("user")),
                  menuItem(text = "Data",
                           tabName = "menu_4",
                           icon = icon("table")),
                  menuItem(text = "GitHub", 
                           icon = icon("github"), 
                           href = "https://github.com/rafiff23/HumanResourceShinyWebApp")
                )
              ),
              dashboardBody(id = 'test',
                            tags$style('#test {
                             background-color: #FFF8EA;
              }'),
                            
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                ),
                
                tabItems(
                  # --------- Page 1 : Resource
                  tabItem(
                    tabName = "menu_1", 
                    
                    fluidPage(
                      h2(tags$b("Human Resource")),
                      br(),
                      div(style = "text-align:justify", 
                          p("Human resources is the set of people who make up the workforce of an organization, business sector, industry, or economy.", 
                            "A narrower concept is human capital, the knowledge and skills which the individuals command.",
                            "Similar terms include manpower, labor, personnel, associates or simply: people."),
                          br()
                      )
                    ),
                    fluidPage(
                      box(width = 4,height = 425,
                          style = 'font-size:12px;',
                          h3("First Page"),
                          div(style = "text-align:justify",
                              p("First page is about the distribution data of Human Resource, with 3 aspects that include in this page :
                                Recruitment Source, Engagement Survey, and Special Project Count"),
                              h3("Second Page"),
                              p("Second Page you can see that how the salary is effecting the employee satisfaction
                                and their performance."),
                                h3("Third  Page"),
                              p("the third page I provide a visualization of the manager's performance to his subordinates"),
                                h3("Fourth Page"),
                              p("The Last Page is the DataFrame I use for this dashboard", br(),
                                "Data Source :",
                                a(href = "https://www.kaggle.com/datasets/rhuebner/human-resources-data-set",
                                  "Kaggle")),
                              
                          ),
                          background = "navy"
                      ),
              
                                        
                    box(
                    highchartOutput(outputId = "hcontainer"),
                    width="8") 
                    ),
                    fluidPage(
                      box(
                        highchartOutput(outputId = "hcontainer6"),
                        width = "9"
                      ),
                      box(
                      selectInput("rect",label= h4("Select Source :"),choices = df_clean$RecruitmentSource, selected = "Google Search", multiple = T),
                      br(),
                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),
                      sliderInput(inputId = "rectr", label = "Project Count :", 0,9,5, animate = T,ticks = T),
                      br(),
                      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
                      sliderInput(inputId = "rectrr", label = "Engagement Survey :", 0,5,3, animate = T, ticks = T),
                      width = 3,
                      height = 425,
                      background = "light-blue",
                      solidHeader = T)
                    )
                   ),
                  
                  # --------- Page 2 : Employee
                  tabItem(
                    tabName = "menu_2",
                    
                    # --------- INPUT
                    fluidRow(
                      box(
                        width = 12,
                        
                        # selecInput() adalah fungsi untuk memilih salah satu category youtube
                        sliderInput(inputId = "range", label = "Salary",0,250000,100000, animate = T, width = 1800)
                      )
                    ),
                    fluidRow(
                       box(
                         highchartOutput(outputId = "hcontainer2"),
                         height = 400
                       ),
                      box(
                        highchartOutput(outputId = "hcontainer3"),
                        height = 400
                      ),
                      box(
                        highchartOutput(outputId = "hcontainer5"),
                        height = 500
                      ),
                      fluidRow(
                        valueBox("8", 
                                 "Total Unique Department", 
                                 icon = icon("city"),
                                 color = "maroon",
                                 width = 6),
                        valueBox("31", 
                                 "Total Unique Position", 
                                 icon = icon("users"),
                                 color = "aqua",
                                 width = 6),
                        valueBox("IT/IS", 
                                 "Most Paid Departments by Average besides Executive Office", 
                                 icon = icon("chart-line"),
                                 color = "maroon",
                                 width = 6),
                        valueBox("CIO", 
                                 "Most Paid Position by Average besides Executive Office", 
                                 icon = icon("user"),
                                 color = "aqua",
                                 width = 6)
                      )
                    )
                    
                  ),
                  
                  tabItem(
                    tabName = "menu_3",
                    fluidRow(
                    box(
                      selectInput("name",label= h4("Select Manager Name :"),choices = df_clean$ManagerName, selected = 1),
                      width = 12
                    ),
                    box(
                      highchartOutput(outputId = "hcontainer7"),
                      width = 12,
                      collapsible = T
                    ),
                    box(
                      highchartOutput(outputId = "hcontainer8"),
                      width = 4
                    ),
                    box(
                      highchartOutput(outputId = "hcontainer9"),
                      width = 4
                    ),
                    box(
                      valueBoxOutput(width = 12,outputId = "Box3"),
                      valueBoxOutput(width = 12,outputId = "Box1"),
                      valueBoxOutput(width = 12,outputId = "Box2"),
                      width = 4,
                      height = 425,
                    )
                  )
                  ),
                  
                  # --------- page 4 : data
                  tabItem(
                    tabName = "menu_4",
                    DT::dataTableOutput(outputId = "hcontainer4")
                  )
                  
                )
                
              )
)