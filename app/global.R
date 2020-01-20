library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(zipcode)
library(plotly)

# original data
load("tree.RData")
# change the name of latitute and longitute
colnames(tree)[39:40] <- c("lat","lng")
tree$zipcode[tree$zipcode ==83] <- "00083"
# select the variables we may need and save it as tree_data
tree_data <- tree %>% select(c("problems","status","health","spc_common",
                          "steward","guards","sidewalk",
                          "address","zipcode","zip_city",
                          "boroname","root_stone","root_grate","root_other",
                          "trnk_wire","trnk_light","trnk_other",
                          "brnch_ligh","brnch_shoe","brnch_othe","lat","lng"))
tree_data2 = tree %>% select(c("status","health","spc_common",
                                    "steward","guards","sidewalk",
                                    "zipcode","zip_city",
                                    "boroname","lat","lng"))

# species data save it as data
load("tree_data.RData")
data$health = tree$health
data$guard = tree$guards
data$side = tree$sidewalk
colnames(data)[4:5] = c("lat","lng")
e = data$spc[631]
data = data[data$spc != e,]   ## remove empty values
data = data[order(data$spc),] ## order the spc names  


# Base map
## Shapefiles for adding polygons of regions in NYC
nyc_zipcode <- readOGR("nyc-zip-code-tabulation-areas-polygons.geojson","nyc-zip-code-tabulation-areas-polygons")
nyc_zipcode$postalCode <- as.factor(nyc_zipcode$postalCode)
# Heatmap: Tree coverage dataset 
treeCountsGroupedByZipCode <- tree_data %>% group_by(zipcode) %>% tally()
treeCountsGroupedByZipCode <- as.data.frame(treeCountsGroupedByZipCode)
colnames(treeCountsGroupedByZipCode) <- c("ZIPCODE", "value")


# find zipcode data that have the lat, log of each zipcode
data("zipcode")
tree_zip <- tree_data %>% select(zipcode) %>% mutate(zip = as.character(zipcode))

# problem data set save it as problem
problem <- tree_data %>% 
  select(c("health","spc_common","steward","guards","sidewalk","zipcode","root_stone","root_grate","root_other",
           "trnk_wire","trnk_light","trnk_other",
           "brnch_ligh","brnch_shoe","brnch_othe",
           "lat","lng","problems")) %>% 
  mutate(root = ifelse(root_stone == "Yes",1,
                       ifelse(root_grate =="Yes",1,
                              ifelse(root_other =="Yes",1,0))),
         trunk = ifelse(trnk_wire == "Yes",1,
                        ifelse(trnk_light =="Yes",1,
                               ifelse(trnk_other =="Yes",1,0))),
         branch = ifelse(brnch_ligh == "Yes",1,
                         ifelse(brnch_shoe =="Yes",1,
                                ifelse(brnch_othe =="Yes",1,0))))

# problem: root data
markers_root <- problem %>% filter(root == 1) %>% 
  select(zipcode,lat,lng) %>%
  mutate(zip = as.character(zipcode)) %>% 
  group_by(zip) %>% summarise(value.root = n()) %>% left_join(zipcode) %>% select(-c("city","state"))

markers_root$zip <- as.factor(markers_root$zip)
# problem: trunk data
markers_trunk <- problem %>% filter(trunk == 1) %>% 
  select(zipcode,lat,lng) %>%
  mutate(zip = as.character(zipcode)) %>% 
  group_by(zip) %>% summarise(value.trunk = n()) %>% left_join(zipcode) %>% select(-c("city","state"))

markers_trunk$zip <- as.factor(markers_trunk$zip)
# problem: branch data
markers_branch <- problem %>% filter(branch == 1) %>% 
  select(zipcode,lat,lng) %>%
  mutate(zip = as.character(zipcode)) %>% 
  group_by(zip) %>% summarise(value.branch = n()) %>% left_join(zipcode) %>% select(-c("city","state"))

markers_branch$zip <- as.factor(markers_branch$zip)

nyc_zipcode@data <- left_join(nyc_zipcode@data,markers_root,by = c("postalCode" = "zip"))
nyc_zipcode@data <- left_join(nyc_zipcode@data,markers_trunk,by = c("postalCode" = "zip"))
nyc_zipcode@data <- left_join(nyc_zipcode@data,markers_branch,by = c("postalCode" = "zip"))

# Heatmap: for coloring
pal <- colorNumeric(
  palette = "Greens",
  domain = nyc_zipcode$value.x)
pal_root <- colorNumeric(
  palette = "Blues",
  domain = nyc_zipcode$value.root)
pal_branch <- colorNumeric(
  palette = "Oranges",
  domain = nyc_zipcode$value.branch)
pal_trunk <- colorNumeric(
  palette = "Purples",
  domain = nyc_zipcode$value.trunk)


load("tree05.RData")
tree05_data <- tree05[tree05$zipcode !=0,]
tree05_data <- tree05_data %>% select(c("spc_common","zipcode","boroname","latitude","longitude"))

tree05CountsGroupedByZipCode <- tree05_data %>% group_by(zipcode) %>% tally()
tree05CountsGroupedByZipCode <- as.data.frame(tree05CountsGroupedByZipCode)
colnames(tree05CountsGroupedByZipCode) <- c("ZIPCODE", "value")
pal05 <- colorNumeric(
  palette = "Greens",
  domain = nyc_zipcode$value.y)

treeCountsGroupedByZipCode$ZIPCODE <- as.factor(treeCountsGroupedByZipCode$ZIPCODE)
tree05CountsGroupedByZipCode$ZIPCODE <- as.factor(tree05CountsGroupedByZipCode$ZIPCODE)

nyc_zipcode$postalCode <- as.factor(nyc_zipcode$postalCode)
nyc_zipcode@data <- left_join(nyc_zipcode@data,treeCountsGroupedByZipCode,by = c("postalCode" = "ZIPCODE"))
nyc_zipcode@data<- left_join(nyc_zipcode@data,tree05CountsGroupedByZipCode,by = c("postalCode" = "ZIPCODE"))



nyc_boroughs <- readOGR("BoroughBoundaries.geojson","BoroughBoundaries")

treeCountsGroupedByboroname <- tree_data %>% group_by(boroname) %>% tally()
treeCountsGroupedByboroname <- as.data.frame(treeCountsGroupedByboroname)
colnames(treeCountsGroupedByboroname) <- c("boro", "value")
pal_boro <- colorNumeric(
  palette = "Greens",
  domain = nyc_boroughs$value.x)

tree05CountsGroupedByboroname <- tree05_data %>% group_by(boroname) %>% tally()
tree05CountsGroupedByboroname <- as.data.frame(tree05CountsGroupedByboroname)
levels(tree05CountsGroupedByboroname$boro)[levels(tree05CountsGroupedByboroname$boro) == "5"] <- "Staten Island"
colnames(tree05CountsGroupedByboroname) <- c("boro1", "value","boro")
tree05CountsGroupedByboroname <- tree05CountsGroupedByboroname %>% select(c("boro","value"))
pal05_boro <- colorNumeric(
  palette = "Greens",
  domain = nyc_boroughs$value.y)

nyc_boroughs$boro_name <- as.factor(nyc_boroughs$boro_name)
nyc_boroughs@data <- left_join(nyc_boroughs@data,treeCountsGroupedByboroname,by = c("boro_name" = "boro"))
nyc_boroughs@data <- left_join(nyc_boroughs@data,tree05CountsGroupedByboroname,by = c("boro_name" = "boro"))

tree05_data$spc_common = tolower(tree05_data$spc_common)


tt <- tree %>% filter(zipcode == "10024") %>% group_by(spc_common,boroname) %>% tally()
plot_ly(labels = tt$spc_common,parents = tt$boroname, values = tt$n, type = "sunburst")

tt2 <- tree %>% filter(boroname == "Manhattan") %>% 
  group_by(spc_common,zipcode) %>% tally() %>%
  filter(spc_common != "") %>% arrange(desc(n))
p <- plot_ly(labels = tt2[1:30,"zipcode"],parents = tt2[1:30,"spc_common"], values = tt2[1:30,"n"], type = "sunburst")
