shinyUI(

    div(id = "canvas",
        navbarPage(strong("Street Trees Census",style = "color:green"),
                   theme = "styles.css",
            # 1. Intro tab
            tabPanel("Introduction",
                mainPanel(
                  width = 12,
                  h1("Project: 2015 NYC Street Trees Census"),
                  h4("An RShiny app detecting tree species and problems in NYC"),
                  h2("Background:"),
                  p("Trees are vital. They give us oxygen, store carbon and give life to whole world's wildlife. People love to live and work in greens. Therefore, trees would become an more vital component of urban life."),
                  br(),
                  h3("Project Summary:"),
                  p("Our project analyzes and visualizes street trees information regarding health and illness conditions for different species of trees in New York City. The tree data were obtained from NYC Open Data portal. Our group also compared street tree's distribution in NYC betweem year 2005 and year 2015.We created a Shiny App to assist users to explore our findings in four main tabs: Map, Comparison, Future Ideas and Data."),
                  h4("-Map: Users can pinpoint any location in New York City by zipcode and choose problems and species of trees they interested. Then total tree numbers, species proportion, health proportion, guard condition and sidewalk condition will be presented on the right output section."),
                  h4("-Data: Both year 2015 and year 2005's data contain over 500000 trees' information we used to analyze and to apply algorithm."),
                  h4("-Target Users: For urban construction group or parks and recreation department to improve urban greening in the future.")            
                    
                    ),
                # footer
                div(class = "footer","Applied Data Science")
            ),
            
            # 2. MAP tab
            tabPanel("Map",
                div(class = "outer",
                    # leaflet map
                    leafletOutput("map",width = "100%", height = "1000px"),
                    
                    # Control Panel
                    absolutePanel(id = "controls",class = "panel panel-default", 
                                  fixed = TRUE, draggable = TRUE,
                                  top = 120, left = 20, right = "auto", bottom = "auto", 
                                  width = 250, height = "auto",
                                  
                                  #overview of trees coverage of NYC
                                  h3("Overview"),
                                  h5(strong("Tree Coverage:")),
                                  checkboxInput("click_heatmap","heat map",value = FALSE),
                                  # select the species
                                  selectInput("type", label = "Species of tree:",
                                              choices = append(as.character(unique(data$spc)),"None",0),selected = NULL,multiple = FALSE
                                  ),
                                  # check the problem
                                  radioButtons("enable_heatmap", "Tree problem heatmap:",
                                                     choices = list("Root Problem","Trunk Problem","Branch Problem"),
                                               selected = character(0)
                                  ),
                                  
                                  #By Zipcode
                                  h3("By ZIPCODE"),
                                  # select the zipcode
                                  h4("Step 1"),
                                  selectInput("zipcode", label = "Please select the ZIPCODE :", 
                                              choices =  c("None",sort(as.character(unique(tree_zip$zip)))),selected = NULL,multiple = FALSE
                                  ),
                                  h5("Pie Charts are shown on the right."),
                                  div(),
                                  h4("Step 2"),
                                  checkboxGroupInput("enable_markers", "Add Markers for:",
                                                     choices = c("Root Problem","Trunk Problem","Branch Problem")
                                  )
                    ),
                    
                    # Output Panel
                    absolutePanel(id = "controls", class = "panel panel-default", fixed= TRUE, draggable = TRUE,
                                  top = 120, left = "auto", right = 20, bottom = "auto", width = 320, height = "auto",
                                  #overview of trees coverage of NYC
                                  h3("Outputs"),
                                  #By Zipcode
                                  h4("By ZIPCODE"),
                                  #By Zipcode
                                  p(textOutput("ziparea")),
                                  p(textOutput("total")),
                                  plotlyOutput("spc_pie", height="180"),
                                  plotlyOutput("health_pie",height = "180"),
                                  plotlyOutput("guard_pie",height = "180"),
                                  plotlyOutput("sidewalk_pie",height = "180")
                                 
                    )              
                )
            ),
            # 3. Comparison tab
            tabPanel("Comparison",
                     div(class = "outer2",
                         leafletOutput("map1",width = "100%", height = "1000px"),
                         absolutePanel(id = "controls",class = "panel panel-default", 
                                       fixed = TRUE, draggable = TRUE,
                                       top = 120, left = 20, right = "auto", bottom = "auto", 
                                       width = 250, height = "auto",
                                       
                                       #overview of trees coverage of NYC
                                       h3("Tree Coverage:"),
                                       h4(strong("Please select both Regions and Year.")),
                                       radioButtons("enable_regions", "Regions:",
                                                          choices = c("Zipcodes","Boroughs"),
                                                    selected = character(0)
                                       ),
                                       radioButtons("comparison_heatmap", "Year:",
                                                    choices = list("2005","2015"),
                                                    selected = character(0)
                                       )
                         ),
                         absolutePanel(id = "controls", class = "panel panel-default", fixed= TRUE, draggable = TRUE,
                                       top = 120, left = "auto", right = 20, bottom = "auto", width = 400, height = "auto",
                                       #By Zipcode
                                       h4("By ZIPCODE"),
                                       #By Zipcode
                                       selectInput("zipcode2", label = "Please select the ZIPCODE :", 
                                                   choices =  c("None",sort(as.character(unique(tree_zip$zip)))),selected = NULL,multiple = FALSE
                                       ),
                                       p(textOutput("total1")),
                                       plotlyOutput("spc_pie1", height="300"),
                                       p(textOutput("total2")),
                                       plotlyOutput("spc_pie2", height="300")
                         )
                     )       
            ),
            # 5. Next Steps / Future Ideas
            tabPanel("Next Steps / Future Ideas",
                     mainPanel
                     (
                       width = 12,
                       h3("Predictive Model for Trees"),
                       p("We can track a given tree ID or a specific tree's health condition through prior year tree dataset and recent year dataset. Moreover, as we have the information for that specific tree, for example: if it was located on a sidewalk or whether it had branch/trunk/root problem, we can make a predictive model to anticipate if this tree has a high possibility of dying. This model can help relevant departments or volunteer groups to take necessary precautions and to prevent future deaths."),
                       br(),
                       h3("Analyzing Trees after Grouping By Similarities"),
                       p(" The tree census datasets give us information about each tree's species. Then we can use this message to create another column 'Category'. For 'Category', we can put tree with similar features in the same category. But it would be hard to group trees since it require a vast knowledge in botany.  However, If we can resolve this issue, we would create a new map showing trees with same category and it would develop another interesting and detailed map view.")
                     ),
                     # footer
                     div(class = "footer","Applied Data Science")
            ),
            # 4. Data tab
            tabPanel("Data",
                     
                     div(width = 12,
                         
                         h1("Tree Data (2015)"),
                         br(),
                         dataTableOutput('table')
                     ),
                     
                     div(class="footer", "Applied Data Science")
            )

         )
        )
    )

