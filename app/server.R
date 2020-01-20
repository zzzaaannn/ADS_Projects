shinyServer(function(input, output,session) {
  # 2. map tab
  output$map <- renderLeaflet({
    # default map, base layer
    m <- leaflet(nyc_zipcode) %>%
      addTiles() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.983,40.7639,zoom = 10)
    
  })
  
  output$map1 <- renderLeaflet({
    # default map, base layer
    m <- leaflet() %>%
      addTiles() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.983,40.7639,zoom = 10) 
  })
  


  # Overview:
  ##checkbox for heatmap
  observeEvent(input$click_heatmap,{
    ## when the box is checked, show the heatmap 
    if(input$click_heatmap ==TRUE) leafletProxy("map")%>%
      addPolygons(data = nyc_zipcode,
                  popup = paste0("<strong>Zipcode: </strong>", 
                                 nyc_zipcode$postalCode,
                                 "<br><strong>Number of Trees: </strong>",
                                 nyc_zipcode$value.x),
                  stroke = T, weight=1,
                  fillOpacity = 0.95,
                  color = ~pal(nyc_zipcode$value.x),
                  highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  group = "number_of_trees")%>%
      ## the legend of color
      addLegend(pal = pal,group = "number_of_trees", values = nyc_zipcode$value.x, opacity = 1) %>%
      showGroup("number_of_trees") 
    ## when the box is unchecked, show the base map
    else{leafletProxy("map") %>% hideGroup("number_of_trees") %>% clearControls()}
  })
  ## species: 
  spe <- reactive({
    data %>% filter(spc == input$type) %>% select(zip,lat,lng) %>%
      mutate(zip = as.character(zip)) %>% 
      group_by(zip) %>% summarise(value = n()) %>% left_join(zipcode) %>% select(-c("city","state"))
  })
  observeEvent(input$type,{
    leafletProxy("map") %>%
      clearGroup("type")
    leafletProxy("map") %>% 
      addMarkers(data = spe(),group = "type",icon = list(iconUrl = "icon/tree.png",iconSize=c(15,15)))
  })
  
  ## heatmap for problem: 
  
  observeEvent(input$enable_heatmap,{
    
    if("Root Problem" %in% input$enable_heatmap) {leafletProxy("map") %>% clearGroup("type") %>% clearControls() %>% 
      addPolygons(data = nyc_zipcode,
                  popup = paste0("<strong>Zipcode: </strong>", 
                                 nyc_zipcode$postalCode,
                                 "<br><strong>Number of Trees having root problem: </strong>",
                                 nyc_zipcode$value.root),
                  stroke = T, weight=1,
                  fillOpacity = 0.95,
                  color = ~pal_root(nyc_zipcode$value.root),
                  highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  group = "root")%>%
      ## the legend of color
      showGroup("root") %>% addLegend(pal = pal_root,group = "root", values = nyc_zipcode$value.root, opacity = 1)}
    # ## when the box is unchecked, show the base map
    # else{leafletProxy("map") %>% hideGroup("root") %>% clearControls()}
    
    if("Branch Problem" %in% input$enable_heatmap) {leafletProxy("map") %>% clearGroup("type") %>% clearControls() %>% 
      addPolygons(data = nyc_zipcode,
                  popup = paste0("<strong>Zipcode: </strong>", 
                                 nyc_zipcode$postalCode,
                                 "<br><strong>Number of Trees having branch problem: </strong>",
                                 nyc_zipcode$value.branch),
                  stroke = T, weight=1,
                  fillOpacity = 0.95,
                  color = ~pal_branch(nyc_zipcode$value.branch),
                  highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  group = "branch")%>%
      ## the legend of color
      showGroup("branch") %>%  addLegend(pal = pal_branch,group = "branch", values = nyc_zipcode$value.branch, opacity = 1)}
    # ## when the box is unchecked, show the base map
    # else{leafletProxy("map") %>% hideGroup("branch") %>% clearControls()}
    
    if("Trunk Problem" %in% input$enable_heatmap) {leafletProxy("map") %>% clearGroup("type") %>% clearControls() %>% 
      addPolygons(data = nyc_zipcode,
                  popup = paste0("<strong>Zipcode: </strong>", 
                                 nyc_zipcode$postalCode,
                                 "<br><strong>Number of Trees having trunk problem: </strong>",
                                 nyc_zipcode$value.trunk),
                  stroke = T, weight=1,
                  fillOpacity = 0.95,
                  color = ~pal_trunk(nyc_zipcode$value.trunk),
                  highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  group = "trunk")%>%
      ## the legend of color
      showGroup("trunk") %>%  addLegend(pal = pal_trunk,group = "trunk", values = nyc_zipcode$value.trunk, opacity = 1)}
    # ## when the box is unchecked, show the base map
    # else{leafletProxy("map") %>% hideGroup("trunk") %>% clearControls()}
  })
  
  observeEvent(input$zipcode,{
    updateRadioButtons(session,"enable_heatmap",choices  = list("Root Problem","Trunk Problem","Branch Problem"),selected = character(0))
  })
  
  
  observeEvent(input$zipcode,{
    if (nchar(input$zipcode) != 0) {leafletProxy("map") %>% 
      clearGroup("number_of_trees") %>% 
      clearGroup("type") %>% 
      clearGroup("markers_root") %>% 
      clearGroup("markers_branch") %>%
      clearGroup("markers_trunk") %>% 
      clearGroup("root") %>%
      clearGroup("branch") %>% 
      clearGroup("trunk") %>% clearControls()} 
  })
  
  #By ZIPCODE:
  ## problem
  observeEvent(input$enable_markers, {
    df_root <- problem %>% filter(root == 1) %>% 
      select(c("problems","health","spc_common","steward","guards","sidewalk","zipcode","lat","lng")) %>%
      mutate(zip = as.character(zipcode)) %>% filter(zip==input$zipcode)
    df_branch <- problem %>% filter(branch == 1) %>% 
      select(c("problems","health","spc_common","steward","guards","sidewalk","zipcode","lat","lng")) %>%
      mutate(zip = as.character(zipcode)) %>% filter(zip==input$zipcode)
    df_trunk <- problem %>% filter(trunk == 1) %>% 
      select(c("problems","health","spc_common","steward","guards","sidewalk","zipcode","lat","lng")) %>%
      mutate(zip = as.character(zipcode)) %>% filter(zip==input$zipcode)
    
    if("Root Problem" %in% input$enable_markers) leafletProxy("map",data = df_branch) %>% clearControls() %>% 
      addMarkers(lat = ~lat,lng = ~lng,
                 group ="markers_root",popup = paste0("<strong>Zipcode: </strong>", 
                                                      df_root$zip,
                                                      "<br><strong>Species: </strong>", 
                                                      df_root$spc_common,
                                                      "<br><strong>Health: </strong>", 
                                                      df_root$health,
                                                      "<br><strong>Steward: </strong>", 
                                                      df_root$steward,
                                                      "<br><strong>Guards: </strong>", 
                                                      df_root$guards,
                                                      "<br><strong>Sidewalk: </strong>", 
                                                      df_root$sidewalk,
                                                      "<br><strong>Problems: </strong>", 
                                                      df_root$problems,
                                                      "<br><strong>Latitude: </strong>", 
                                                      df_root$lat,
                                                      "<br><strong>Longitude: </strong>", 
                                                      df_root$lng),
                 icon = list(iconUrl = "icon/root.png",iconSize=c(15,15)))%>% 
      showGroup("markers_root")
    else{leafletProxy("map2") %>% hideGroup("markers_root")}
    
    if("Branch Problem" %in% input$enable_markers) leafletProxy("map",data = df_branch) %>% clearControls() %>% 
      addMarkers(lat = ~lat,lng = ~lng,
                 group ="markers_branch",popup = paste0("<strong>Zipcode: </strong>", 
                                                        df_branch$zip,
                                                        "<br><strong>Species: </strong>", 
                                                        df_branch$spc_common,
                                                        "<br><strong>Health: </strong>", 
                                                        df_branch$health,
                                                        "<br><strong>Steward: </strong>", 
                                                        df_branch$steward,
                                                        "<br><strong>Guards: </strong>", 
                                                        df_branch$guards,
                                                        "<br><strong>Sidewalk: </strong>", 
                                                        df_branch$sidewalk,
                                                        "<br><strong>Problems: </strong>", 
                                                        df_branch$problems,
                                                        "<br><strong>Latitude: </strong>", 
                                                        df_branch$lat,
                                                        "<br><strong>Longitude: </strong>", 
                                                        df_branch$lng),
                 icon = list(iconUrl = "icon/branch.png",iconSize=c(15,15)))%>% showGroup("markers_branch")
    else{leafletProxy("map2") %>% hideGroup("markers_branch")}
    
    if("Trunk Problem" %in% input$enable_markers) leafletProxy("map",data = df_trunk) %>% clearControls() %>% 
      addMarkers(lat = ~lat,lng = ~lng,
                 group ="markers_trunk",popup = paste0("<strong>Zipcode: </strong>", 
                                                       df_trunk$zip,
                                                       "<br><strong>Species: </strong>", 
                                                       df_trunk$spc_common,
                                                       "<br><strong>Health: </strong>", 
                                                       df_trunk$health,
                                                       "<br><strong>Steward: </strong>", 
                                                       df_trunk$steward,
                                                       "<br><strong>Guards: </strong>", 
                                                       df_trunk$guards,
                                                       "<br><strong>Sidewalk: </strong>", 
                                                       df_trunk$sidewalk,
                                                       "<br><strong>Problems: </strong>", 
                                                       df_trunk$problems,
                                                       "<br><strong>Latitude: </strong>", 
                                                       df_trunk$lat,
                                                       "<br><strong>Longitude: </strong>", 
                                                       df_trunk$lng),
                 icon = list(iconUrl = "icon/trunk.png",iconSize=c(15,15)))%>% showGroup("markers_trunk")
    
    else{leafletProxy("map") %>% hideGroup("markers_trunk")}
    
  })
  
  
  # Output Panel
  output$ziparea = renderText(paste("Zipcode: ",input$zipcode))
  output$total = renderText(paste("Number of trees in this zip: ",
                                  nrow(data[data$zip==(as.numeric(input$zipcode)),])))
  
  output$spc_pie = renderPlotly({
    df1 = data[data$zip==as.numeric(input$zipcode),]
    a1 = aggregate(df1$tree_id,list(df1$spc),length)
    col1 = rainbow(nrow(a1))
    plot_ly(labels=a1[,1],values=a1[,2], type = "pie",
            marker=list(colors=col1),textinfo = "none") %>%
      layout(title = paste("Species proportion"),showlegend=F,
             xaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F),
             yaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F))
  })
  
  output$health_pie = renderPlotly({
    df1 = data[data$zip==as.numeric(input$zipcode),]
    a1 = aggregate(df1$tree_id,list(df1$health),length)
    col1 = rainbow(nrow(a1))
    plot_ly(labels=a1[,1], values=a1[,2], type = "pie",
            marker=list(colors=col1)) %>%
      layout(title = paste("Health proportion"),showlegend=F,
             xaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F),
             yaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F))
  })
  
  output$guard_pie = renderPlotly({
    df1 = data[data$zip==as.numeric(input$zipcode),]
    a1 = aggregate(df1$tree_id,list(df1$guard),length)
    col1 = rainbow(nrow(a1))
    plot_ly(labels=a1[,1], values=a1[,2], type = "pie",
            marker=list(colors=col1)) %>%
      layout(title = paste("Guard proportion"),showlegend=F,
             xaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F),
             yaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F))
  })
  
  output$sidewalk_pie = renderPlotly({
    df1 = data[data$zip==as.numeric(input$zipcode),]
    a1 = aggregate(df1$tree_id,list(df1$side),length)
    col1 = rainbow(nrow(a1))
    plot_ly(labels=a1[,1], values=a1[,2], type = "pie",
            marker=list(colors=col1)) %>%
      layout(title = paste("Sidewalk proportion"),showlegend=F,
             xaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F),
             yaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F))
  })
  
  output$table = renderDataTable(tree_data2, options = list(pageLength = 10, lengthMenu = list(c(10))))
  
  
  # 3. comparison tab
  observeEvent({input$enable_regions
    input$comparison_heatmap},{
      if("Zipcodes" %in% input$enable_regions & "2005" %in% input$comparison_heatmap) leafletProxy("map1") %>% clearControls()  %>% 
        addPolygons(data = nyc_zipcode,
                    popup = paste0("<strong>Zipcode: </strong>", 
                                   nyc_zipcode$postalCode,
                                   "<br><strong>Number of Trees:",
                                   nyc_zipcode$value.y),
                    stroke = T, weight=1,
                    fillOpacity = 0.95,
                    color = ~pal05(nyc_zipcode$value.y),
                    highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,bringToFront = TRUE),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    group = "number_of_trees05")%>%
        showGroup("number_of_trees05") %>% 
        addLegend(pal = pal05,group = "number_of_trees05", values = nyc_zipcode$value.y, opacity = 1)
     # else{leafletProxy("map1") %>% hideGroup("number_of_trees05") %>% clearControls()}
      
      if("Zipcodes" %in% input$enable_regions & "2015" %in% input$comparison_heatmap) leafletProxy("map1") %>% clearControls()  %>% 
        addPolygons(data = nyc_zipcode,
                    popup = paste0("<strong>Zipcode: </strong>", 
                                    nyc_zipcode$postalCode,
                                    "<br><strong>Number of Trees:",
                                    nyc_zipcode$value.x),
                    stroke = T, weight=1,
                    fillOpacity = 0.95,
                    color = ~pal_boro(nyc_zipcode$value.x),
                    highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,bringToFront = TRUE),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    group = "number_of_trees")%>%
        showGroup("number_of_trees") %>% 
        addLegend(pal = pal_boro,group = "number_of_trees", values = nyc_zipcode$value.x, opacity = 1) 
      #else{leafletProxy("map1") %>% hideGroup("number_of_trees") %>% clearControls()}
      
      if("Boroughs" %in% input$enable_regions & "2005" %in% input$comparison_heatmap) leafletProxy("map1") %>% clearControls()  %>% 
        addPolygons(data = nyc_boroughs,popup = paste0("<strong>Borough: </strong>", 
                                                       nyc_boroughs$boro_name,
                                                       "<br><strong>Numbers of Trees: </strong>", 
                                                       nyc_boroughs$value.y),
                    stroke = T, weight=1,
                    fillOpacity = 0.95,
                    color = ~pal05_boro(nyc_boroughs$value.y),
                    highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,bringToFront = TRUE),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    group = "number_of_trees05_boro")%>%
        showGroup("number_of_trees05_boro") %>% 
        addLegend(pal = pal05_boro,group = "number_of_trees05_boro", values = nyc_boroughs$value.y, opacity = 1)
     # else{leafletProxy("map1") %>% hideGroup("number_of_trees05_boro") %>% clearControls()}
      
      if("Boroughs" %in% input$enable_regions & "2015" %in% input$comparison_heatmap) leafletProxy("map1") %>% clearControls()  %>% 
        addPolygons(data = nyc_boroughs,
                    popup = paste0("<strong>Borough: </strong>", 
                                           nyc_boroughs$boro_name,
                                           "<br><strong>Numbers of Trees: </strong>", 
                                           nyc_boroughs$value.x),
                    stroke = T, weight=1,
                    fillOpacity = 0.95,
                    color = ~pal_boro(nyc_boroughs$value.x),
                    highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,bringToFront = TRUE),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    group = "number_of_trees_boro")%>%
        showGroup("number_of_trees_boro") %>% 
        addLegend(pal = pal_boro,group = "number_of_trees_boro", values = nyc_boroughs$value.x, opacity = 1) 
      #else{leafletProxy("map1") %>% hideGroup("number_of_trees_boro") %>% clearControls()}
      
    })
  
  output$spc_pie1 = renderPlotly({
    tt <- tree %>% filter(zipcode == input$zipcode2) %>% group_by(spc_common,boroname) %>% tally()
    plot_ly(labels = tt$spc_common,parents = tt$boroname, values = tt$n, type = "sunburst") %>%
      layout(title = paste("Species proportion in 2015"),showlegend=F,
             xaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F),
             yaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F))
  })
  
  output$spc_pie2 = renderPlotly({
    tt <- tree05 %>% filter(zipcode == input$zipcode2) %>% group_by(spc_common,boroname) %>% tally()
    plot_ly(labels = tt$spc_common,parents = tt$boroname, values = tt$n, type = "sunburst") %>%
      layout(title = paste("Species proportion in 2005"),showlegend=F,
             xaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F),
             yaxis=list(showgrid=F,zeroline=F,showline=F,autotick=T,ticks='',showticklabels=F))
  })
})
