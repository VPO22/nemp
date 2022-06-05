require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
require(shinyWidgets)
require(tidyverse)
require(patchwork)
require(sf)
require(magrittr)
require(tmap)
require(leaflet)
require(plotly)
require(metR)
require(tmap)
require(DT)
require(highcharter)
require(leaflet)
require(rgeoda)

# color codes
require(scales)
cols <- brewer_pal(type = "div")(8)
mycolor4 =  gradient_n_pal(cols)(seq(0, 1, length.out = 30)) %>% rev()
waste.colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")



# Data ---------------------
## Districts ----------------
districts = st_read("data/spatial/Districts and TC as 2020.shp", 
                    quiet = TRUE) %>% 
  janitor::clean_names() %>% 
  select(region = region_nam, lga = new_dist20) %>% 
  st_make_valid()%>% 
  filter(!region %in% c("Mjini Magharibi", "Kaskazini Pemba", "Kaskazini Unguja", "Kusini Pemba", "Kusini Unguja"))

## Regions of Tanzania ------
regions = st_read("data/spatial/regions_tz_poly.shp", quiet = TRUE) %>% 
  st_transform(crs = 4326) %>% janitor::clean_names() %>% 
  select(region = tz_regiona)

regions = regions%>% 
  mutate(region = if_else(region == "Dar  Es Salaam", "Dar Es Salaam", region),
         region = str_to_title(region)) 

regions.znz = c("Kaskazini", "Kaskazini Pemba", "Kusini", "Kusini Pemba", "Mjini Magharibi")

## LAND DEGRADATION----

land.degradation = st_read("data/spatial/Tanzania_land_degradation.shp", 
                           quiet = TRUE) 

land.degradation.three = land.degradation %>% 
  filter(!severity == "0") %>% 
  mutate(severity = if_else(severity ==  "Not degraded", "Low", severity),
         severity = if_else(severity ==  "Light degraded", "Low", severity),
         severity = if_else(severity ==  "Moderate degraded", "Moderate", severity),
         severity = if_else(severity ==  "Severe degraded", "High", severity),
         severity = if_else(severity ==  "Very severe degraded", "High", severity)) %>% 
  select(severity)


degraded.region.sf = st_read("data/spatial/region_degraded.shp", quiet = TRUE)

degraded.region.area = degraded.region.sf %>% 
  mutate(area = st_area(geometry) %>% as.numeric(), area_ha = area / 10000) %>% 
  st_drop_geometry()  %>% 
  group_by(region, severity) %>% 
  summarise(total = sum(area_ha)) %>% 
  ungroup() %>% 
  arrange(desc(total)) %>% 
  mutate(across(is.numeric, round, 0))

### -----relative values derived -----

degraded.region = read_csv("data/spatial/area_degraded_region.csv")

pct.degraded.region = read_csv("data/spatial/pct_degraded_region.csv")


pct.degraded.region.long = pct.degraded.region %>% 
  pivot_longer(cols = Low:High, names_to = "severity", values_to = "percent") 

regions.tz = regions %>% 
  filter(!region %in% regions.znz) %>% 
  mutate(area = st_area(geometry) %>% as.numeric()) %>% 
  filter(area > 1000000) %>% 
  select(-area)

region.tz.degraded.sf = regions.tz %>% 
  left_join(pct.degraded.region.long)

mkoa.degraded.area = degraded.region %>% 
  pivot_longer(cols = Low:High, names_to = "severity", values_to = "area_km2")  %>% 
  mutate(across(is.numeric, round, 0))

region.names = mkoa.degraded.area %>% distinct(region) %>% pull(region)

## Waste management -------------

waste = readxl::read_excel("data/spatial/Waste management-hussein.xlsx", sheet = 1) %>% 
  janitor::clean_names() %>% 
  select(-1) %>% 
  mutate(percent_collection = percent_collection * 100, across(is.numeric, round, 2),
         lga = if_else(lga == "Ilala MC", "Dar-es-Salaam CC", lga)) %>% 
  rename(region = region_name) %>% 
  filter(!region %in% c("Mjini Magharibi", "Kaskazini Pemba", "Kaskazini Unguja", "Kusini Pemba", "Kusini Unguja"))



waste = waste  %>% 
  separate(col = lga, into = c("district", "code"), sep = " ", remove = FALSE) %>% 
  select(-district_name, -environmental_challenge)

districts = districts %>% 
  mutate(lga = if_else(lga == "Dar es Salaam CC", "Dar-es-salaam CC", lga))

districts.waste = districts %>% 
  left_join(waste) 


districts.waste %>% st_drop_geometry() %>%  dplyr::filter(code == "CC")


## Invasive Species ------
ias = readxl::read_excel("data/spatial/invasive.xlsx", sheet = 1) %>% 
  janitor::clean_names()

spp = ias %>% 
  distinct(invasive_species) %>% pull()




regions.ias = regions %>% 
  filter(!region %in% regions.znz) %>% 
  left_join(ias)




mikoa = ias %>% distinct(region) %>% pull()


intensity = ias %>% 
  group_by(region) %>% 
  summarise(n = n()) %>% 
  mutate(percentage = n/sum(n) *100) %>% 
  ungroup() %>% 
  mutate(label = paste(region, "\n", round(percentage, 1), "%")) %>% 
  arrange(desc(n))


regeion.inv.intensity = regions %>% 
  left_join(intensity) 



## climate data -----

change = readxl::read_excel("data/spatial/Climate data - changes 2010-2040.xlsx") %>% 
  janitor::clean_names()  %>% 
  pivot_longer(cols = temperature:water_runoff) %>% 
  rename(region = regions)

change.var = change %>% arrange(name) %>% distinct(name) %>% pull()

legend.title = c("Aridity (%)" , "Precipitation (%)",  "Soils moisture (%)",   "Temperature (C)",     "Water discharge (%)" ,"Water runoff (%)")

change.regions = regions %>% 
  left_join(change) %>% 
  filter(!region %in% c("Kaskazini", "Kaskazini Pemba", "Kusini", "Kusini Pemba", "Mjini Magharibi"))

## bushfire ------

bushfire = st_read("data/spatial/bushfire_poly_2019.shp", quiet = TRUE) %>% 
  janitor::clean_names() 


## deforestation new ----
regions.deforested.foresta =   st_read("data/spatial/forest_deforested_regions.shp", quiet = TRUE)
region.name = regions %>% st_drop_geometry() %>% distinct(region)%>% pull(region)


forest.deforested = st_read("data/spatial/forest_deforested_valid_topology.shp", quiet = TRUE)

## remove invalid geometry from the dataset
forest.deforested.valid = forest.deforested %>% 
  st_make_valid() %>% 
  mutate(valid = st_is_valid(geometry)) %>% 
  filter(valid == TRUE)


## agricultura expansion ------
agri = readxl::read_excel("data/spatial/Land degradation _Factors 1.xlsx", 
                          sheet = "Agric land") %>% 
  janitor::clean_names() %>% select(region, pct_change)

change.class = agri %>% 
  mutate(change = case_when(pct_change <= 0 ~ "Below 0%",
                            pct_change > 0 & pct_change <= 50 ~ "0-50%",
                            pct_change > 50 & pct_change <= 100 ~ "51-100%",
                            pct_change > 100 ~ "Above 100%",
                            is.na(pct_change)~"No Data")) 


regions.expand.agric = regions %>% 
  left_join(change.class)

## trash data ------
trash.poly = st_read("data/spatial/trash_areas_poly.shp")


## Dodoma City ----

dom.cc = st_read("data/spatial/Dodoma_city.shp", quiet = TRUE) %>% 
  janitor::clean_names() %>% 
  st_transform(crs = 4326)


dom.cc = dom.cc %>% 
  mutate(area_Km2 = st_area(geometry) %>% as.numeric(), area_Km2 = area_Km2/1000000, pop_density = population/area_Km2 ) %>% 
  select(ward_name,population, pop_density, waste_gene, waste_collection = huduma_tak, tree_planting = std_trees) %>% 
  mutate(across(is.numeric, round, 0))


dom.cc.sf = dom.cc %>% 
  mutate(jiji = "dom") %>% 
  st_make_valid() %>% 
  group_by(jiji) %>% 
  summarise() %>% 
  st_as_sf() 



# lakes data----
lakes = read_csv("data/spatial/water_level_lakes.csv")

lakes.latest = lakes #%>%   filter(date >= "2000-01-01")

lake.name = lakes.latest %>% distinct(water_bodies) %>% pull()

# Loop throught the lakes and compute anomaly
lakes.anomaly = list()


## Rivers begin
rivers = read_csv("data/spatial/river_flow_master_long.csv")

rivers = rivers %>% 
  mutate(station = if_else(is.na(station), "Kibungo", station)) 

rivers.station = rivers %>% 
  distinct(station, river, lon, lat)

## convert daily discharge into months for each station
rivers.liters = rivers %>% 
  arrange(river, station, date) %>% 
  mutate(month = lubridate::month(date), year = lubridate::year(date), 
         # convert flow to 
         flow_liter_s = flow *1000)


## compute mean disharge value for each station
river.month = rivers.liters %>% 
  group_by(river,station, year, month) %>% 
  summarise(flow_liter_s = median(flow_liter_s, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(month = lubridate::make_date(year = year, month = month)) %>% 
  ungroup() %>% 
  select(date = month, river, station, flow_liter_s)

## pull out names of river station, used to gauge
stations = river.month %>% distinct(station) %>% pull()





## Rivers end




for (m in 1:length(lake.name)){
  
  aa = lakes.latest %>% 
    filter(water_bodies == lake.name[m])
  
  base.wl = aa %>% slice(1) %>% pull(wl)
  
  lakes.anomaly[[m]] = aa %>% 
    mutate(anomaly = wl-base.wl)
  
}

## bind anomaly of lakes
lakes.anomaly.tb = lakes.anomaly %>% 
  bind_rows() %>% 
  mutate(date = lubridate::as_date(date)) %>% 
  filter(anomaly < 5) 
# end lakes data


# begin wetland
wetland.change = st_read("data/spatial/wetland_change_02_14_selected.shp", quiet = TRUE) %>%
  mutate(year = as.factor(year))

wet.name = wetland.change %>% st_drop_geometry() %>% distinct(name) %>% pull()


wetland.ha = wetland.change %>% 
  group_by(name, year) %>% 
  summarise() %>% 
  st_as_sf()%>% 
  mutate(area = st_area(geometry) %>% as.numeric, area_ha = area*0.0001)

wetland.change.rate = read_csv("data/spatial/wetland_changed.csv")

# end weltand

## waste updated ----
waste = readxl::read_excel("data/spatial/solid waste management generation, collection and disposal.xlsx", sheet = "original") %>% 
  select(1:5) %>% 
  mutate(disposal = str_to_title(disposal))


lga.waste = districts %>% 
  left_join(waste, by = "lga") %>% 
  mutate(disposal = as.factor(disposal))%>%
  mutate(disposal = na_if(x = disposal, y = "No Data"))

sewarage =  read_csv("data/spatial/sewarage_coverage.csv")
sewarage.pop =  read_csv("data/spatial/population_coverage_sewage.csv")

## Climate ----

change = readxl::read_excel("data/spatial/Climate data - changes 2010-2040.xlsx") %>% 
  janitor::clean_names() %>% 
  rename(Temperature =2, Rain=3, Aridity = 4, Moisture = 5, Discharge = 6, Runoff = 7) %>% 
  pivot_longer(cols = Temperature:Runoff) %>% 
  rename(region = regions)

climate.change.var = change %>% arrange(name) %>% distinct(name) %>% pull()

change.regions = regions %>% 
  left_join(change) %>% 
  filter(!is.na(value))

## Bioidiversity -----
hotcoldspot.biodiversity = st_read("data/spatial/hot_cold_richness_occurance_species.shp", quiet = TRUE)
# gbif.tz = vroom::vroom("d:/semba/vpo/data/gbif/0224426-210914110416597/gbif_occurrence_reduced.csv")


## Mining hotspot and cold spot analysed data ----
## This file has been processed and analysed before imported in the app. TO update this file you must go back to the original code found in the root folder of this web app
hot.cold.mines = st_read("data/spatial/hot_cold_richness_mining_types.shp", quiet = FALSE) %>% 
  rename(commodity_type = cmmdty_, clusterSignify = clstrSg)
commodity = hot.cold.mines %>% st_drop_geometry() %>% distinct(commodity_type) %>% pull()


## livestock population change -----
livestock = readxl::read_excel("data/spatial//livestock_statistics.xlsx", sheet = 2)%>% 
  janitor::clean_names() %>% 
  mutate(region = str_to_title(region))

livestock.region = regions %>% 
  mutate(region = str_to_title(region)) %>% 
  left_join(livestock) %>% 
  filter(region %in% mikoa) %>% 
  mutate(across(is.numeric, round, 2), label = paste0(region, "\n", percentage_change, "%"))

# UI ---- 

ui = navbarPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "journal"), 
  tags$img(src = "coat.png", width = "30%", height = "30%"),
  dashboardHeader(title = "", disable = TRUE),
  useShinydashboard(),
  ## link CSS 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "nemp.css")
    ),
  windowTitle = "The Environmental Master Plan",
  # Home begin ----
  tabPanel(title = "NEMP",
           fluidRow(
             column(width = 1),
             column(width = 10,
                    tags$img(src = "Jipe.png", width = "100%", height = "100%")
             ),
             column(width = 1)
           ),
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$br(),
                    tags$h4("What is the Environmental Master Plan?"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 4,
                    tags$p("Common environmental challenges that threaten ecosystem services and human livelihoods include landscape degradation, forest clearing, water scarcity, pollution, climate change, and poor waste disposal. Several initiatives and interventions to address these challenges have been implemented in Tanzania for decades."),
                    tags$p("However, the scarcity of data that is heterogeneous across multiple fields and spatial scales has forced most interventions to take a generic approach to addressing these environmental challenges. The approach assumes that certain common environmental challenges affect a large geographic area. The problem with this approach is that it tends to overlook environmental challenges at the local scale by visualizing poor and scarce data."),
                    tags$br()
             ),
             # column(width = 2),
             column(width = 3,
                    tags$p("Recognizing this challenge, the Vice President's Office developed a National Environmental MasterPlan for strategic interventions in areas where environmental challenges exist or occur. The MasterPlan is supplemented by a dashboard that combines spatial and non-spatial data from environmental challenges to calculate their current state. The dashboard, a web-based decision support tool, is a critical component designed to aid in the monitoring of Tanzania's environmental challenges.")
                    ),
             column(width = 3,
                    tags$h6("Why Dashboard?"),
                    tags$p("If you have ever used dashboard before, welcome! The Environmental Master Plan for Strategic Interventions is rendered in a data driven dashboard that allows you to easily interact with and query environmental information of your interest. <br> This dashboard enables sharing information to anyone using web browser and adaptive that allows quick update of environmental information.")
                    )
           ),
           ),
  ## land degradation ----
  tabPanel(title = "Degradation",
           fluidRow(
             column(width = 1),
             column(width = 11,
                    tabsetPanel(
                      tags$br(),
                      tabPanel(title = "Current State",
                               fluidRow(
                                 column(width = 4, tags$p("Land degradation is the loss of a land's ability to provide services such as biodiversity, soil fertility, and over-soil fertility. Land degradation is increasing in Tanzania as the human population grows, necessitating more land for agriculture and infrastructure development. The recent analysis indicates that the degraded land in Tanzania has jumped from 50% in 2012 to an estimated 80% in 2018.")),
                                 column(width = 4, tags$p("The most degraded region in Tanzania is Tabora. Some other Tanzania regions on the list include, Dodoma, Singida, Shinyanga, Lindi, Pwani Simiyu, Manyara, Arusha, and Ruvuma. Kagera, Geita, Kigoma, Njombe, Kilimanjaro, and Ruvuma are among the least degraded regions in the country.")),
                                 column(width = 3,imageOutput("landDegradation", height = "100px")%>% shinycustomloader::withLoader(type = "html", loader = "loader1"))
                                 ),
                               # tags$br(),
                               tags$br(),
                                fluidRow(
                                 column(width = 3, 
                                        # tags$h3("Land Degradation Status"),
                                        helpText("About 80% of the land in Tanzania is degraded. But the level of degradation vary across space in the country.")
                                 ),
                                 column(width = 3,
                                        helpText("Simply choose the severity class in the box below to plot area of each region"),
                                        selectInput(inputId = "degradedSeverity", label = "", choices = c("Low","Moderate","High"), selected = "High")),
                                 column(width = 3,
                                        helpText("Choose the region to visualize spatial distribution of land degradation"),
                                        selectInput(inputId = "region_idDegr", label = "", choices = region.name, selected = "Tabora")),
                                 column(width = 2, 
                                        helpText("The area of land degradation in selected region for each category is plotted in the figure")
                                        )
                               ),
                               fluidRow(
                                 column(width = 3, tmapOutput(outputId = "landDegraded_tmap") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")), 
                                 column(width = 3,
                                        highchartOutput(outputId = "degradedBar") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                 column(width = 3, tmapOutput(outputId = "degrdedRegion") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                 column(width = 2, 
                                        # tags$p("The severity of land degradation varies from region to region"), 
                                        highchartOutput(outputId = "pieDegraded") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"))
                               )
                               ),
                      tabPanel(title = "Major causes",
                               fluidRow(
                                 #### livestock ----
                                 column(width = 4, 
                                        tags$h6("Livestock Change"),
                                        tags$p("The livestock population has dramatically increased from 2008 and 2020 and degraded the land cover, increase soil erodibility and render the land prone to other agents.")
                                        ),
                                 #### mining ----
                                 column(width = 4, 
                                        tags$h6("Mining Activities"),
                                        tags$p("Abandoned open pits, tailings and waste rocks left after mining of metallic, gemstones and building materials contributes to land degradation")
                                        ),
                                 #### agriculture expansion ----
                                 column(width = 3, 
                                        tags$h6("Agricultural Expansion"),
                                        tags$p("Between 2008 and 2020 a sharp increase in expansion of agricultural land was evidenced in the country.")
                                        )
                               ),
                               fluidRow(
                                 column(width = 2,
                                        sliderInput(inputId = "livestockId", label = "", min = -50, max =1000, value = c(-50,1000))),
                                 column(width = 2, helpText("Slide to specify the minimum and maximum percentage change of livestock")),
                                 column(width = 2,
                                        selectInput(inputId = "commodityId", label = "", choices = commodity, selected = commodity[1])),
                                 column(width = 2, helpText("Select a commodity from the box to visualize hotspots and coldspot")),
                                 column(width = 2,
                                        sliderInput(inputId = "pct_agri", label = "", min = 0, max = 1350, value = c(0, 1350))),
                                 column(width = 1, helpText("Speficy the percentage range"))
                               ),
                               fluidRow(
                                 column(width = 4, tmapOutput(outputId = "livestockMap") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                 column(width = 4, tmapOutput(outputId = "miningMap") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                 column(width = 3, highchartOutput(outputId = "agriExpansion") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                
                               )
                               ),
                      #### Impacts ----
                      tabPanel(title = "Related Impacts",
                               fluidRow(
                               column(width = 3, 
                                      tags$h6("Loss of agriculture and grazing land"),
                                      tags$p("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                               column(width = 4,
                                      tags$h6("Deterioration of water sources"),
                                      tags$p("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                               column(width = 4,
                                      tags$h6("Silting"),
                                      tags$p("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                               column(width = 1)
                               
                               ),
                               fluidRow(
                                 column(width = 3, 
                                        tags$h6("Loss of biodiversity"),
                                        tags$p("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                 column(width = 4,
                                        tags$h6("Internal migration"),
                                        tags$p("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                 column(width = 4,
                                        tags$h6("Air pollution from dusts"),
                                        tags$p("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")
                                        )
                                 )
                               ),
                      tabPanel(title = "Intervations",
                               fluidRow(
                                 column(width = 3,
                                 tags$h6("The Goal"),
                                 tags$p("Maintain, restore and enhance the land resource base and ecosystem services that flow from the land resources.")
                                        ),
                                 column(width = 7,
                                        tags$h6("Expected Results"),
                                        tags$p("Ensure that degraded landscapes are restored; River catchments, flood plains, agricultural and grazing lands are conserved; soil erosion is reduced; infiltration and ground-water recharge improved; run-off and siltation are reduced; and the downstream effects of drying of rivers and floods are minimized.")
                                        
                                        ),
                               ),
                               fluidRow(column(width = 10, tags$hr())),
                               fluidRow(
                                 column(width = 3, 
                                        tags$h6("Restoration of highly degraded "),
                                        tags$li("Develop a program to restore agro-ecosystem services in severely degraded watersheds of the Great Ruaha River, wami-Ruvu river sub-basins, Uluguru mountain, and the Eastern watershed of Lake Victoria"),
                                        tags$br(),
                                        tags$li("Implementing best agricultural practices and agroforestry in at least 70% of the degraded landscape by 2032."),
                                        tags$br(),
                                        tags$li("Develop programmes in land degradation hotspots (Dodoma, Singida, Tabora, Shinyanga, Pwani and Manyara) through vegetation regeneration, afforestation, and gully rehabilitation by 2032")
                                        ),
                                 
                                 column(width = 4,
                                        tags$h6("Strengthen sustainable mining activities"),
                                        tags$li("Develop programmes to ensure 25% of excavated pits are refiled and vegetated after mining is complete by 2032."),
                                        tags$br(),
                                        tags$li("Strengthen capacity of LGAs in potential gold rush areas being able to rehabilitate degraded areas by 2032"),
                                        tags$br(),
                                        tags$li("A rehabilitation program for mining-damaged landscapes in Geita, Chunya, Nzega, Mara, Singida, Shinyanga Katavi, and Mwanza will be developed and implemented by 2032."),
                                        tags$br(),
                                        tags$li("d.	Develop and implement plans for promotion of use of plantation trees as an alternative supporting poles in mining areas, for at least 50% of the 12,000 primary mining licenses by 2032")
                                        ),
                                 column(width = 4,
                                        tags$h6("Promote Altenative Economic Incentives "),
                                        tags$li("Conduct assessment to identify investment opportunities in sustainable land management by 2026"),
                                        tags$br(),
                                        tags$li("Business model for attracting investment in SLM activities is established and promoted in degraded landscape by 2025"),
                                        tags$br(),
                                        tags$li("Strengthen capacity for access to finance for land management by 2032"),
                                        tags$br(),
                                        tags$li("Develop and implement a sensitization and outreach program on environmental stewardship and landscape management by 2030"),
                                        tags$br(),
                                        tags$li("Promote community participation in environmental management through the existing the national campaign on environmental management by 2032")
                                        ),
                                 column(width = 1)
                                 
                               ),
                               fluidRow()
                               ),
                      tags$br(),
                      tags$br()
                      
                    )
             )
           )),
  ## Deforestation ----
  tabPanel(title = "Deforestation",
           fluidRow(
             column(width = 1),
             column(width = 11,
                    tabsetPanel(
                      tabPanel(title = "Current State",
                               fluidRow(
                                 column(width = 5),
                                 tags$h4("The State of Deforestation")
                               ),
                               # tags$br(),
                               fluidRow(
                                 # column(width = 1),
                                        column(width = 3,
                                               tags$p("Forests cover 48.1 million hectares (ha), or 55% of the total surface land area of Mainland Tanzania. Tabora top in the list of deforestation in Tanzania. Singida, Ruvuma, Songwe, Lindi, Kigoma, Katavi, Rukwa and Pwani  are in the list of regions in the country with largest area of deforestation.")
                                              ),
                                        column(width = 3, imageOutput(outputId = "deforesti1", height = "100px")%>% shinycustomloader::withLoader(type = "html", loader = "loader1"))
                               ),
                               tags$br(),
                               tags$br(),
                               fluidRow(
                                 # column(width = 1),
                                 column(width = 3, 
                                        helpText("Because of the large file size of forest and deforestation, the computation is hungry and take a bit of time. You can click on the button to plot"),
                                        tags$br()
                                        # actionBttn(inputId = "clickForest", label = "Click to Plot Forest", color = "primary", style = "bordered")
                                 ),
                                 column(width = 3,
                                        helpText("Select either forest or deforested type to plot the area in descending order"),
                                        selectInput(inputId = "typeForest", label = "", choices = c("Deforested", "Forest"), selected = "Forest")
                                        ),
                                 column(width = 3,
                                        helpText("Choose a region of interest to visualize the are of forested and deforested"), 
                                        selectInput(inputId = "region_idFo", label = "", choices = region.name, selected = "Tabora")),
                                 column(width = 2)
                               ), 
                               tags$br(),
                               fluidRow(
                                 # column(width = 1),
                                 column(width = 3, tmapOutput(outputId = "deforestTmap")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                 column(width = 3, 
                                        highchartOutput(outputId = "forestBar")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                 column(width = 3, 
                                        tmapOutput(outputId = "deforest_map")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")), 
                                 column(width = 2, 
                                        highchartOutput(outputId = "area_deforest")%>% shinycustomloader::withLoader(type = "html", loader = "loader1"))
                               ), 
                               tags$br(),
                               
                               
                               ),
                      ##Causes ----
                      
                      tabPanel(title = "Major causes",
                               fluidRow(
                                 column(width = 4,
                                        tags$h6("Clearing land for agricultural activities"),
                                        tags$p("The livestock population has dramatically increased contributing to reduced land cover, increase soil erodibility and making the land fragile and prone to other agents.")
                          
                                        ),
                                 column(width = 3,
                                        tags$h6("Wildfire and Bush fire"),
                                        tags$p("The motives for setting wildfires normally include opening land for farms, improving pasture quality for grazing, facilitating wildlife hunting, honey collection, and charcoal burning.")
                                        
                                        ),
                                 column(width = 4,
                                        tags$h6("Map of Tanzania Showing Bushfire distribution"),
                                        tags$p("")
                                        
                                        )
                               ),
                               # tags$br(),
                              fluidRow(
                                column(width = 4,
                                       tags$h6("Mining Activities"),
                                       tags$p("The population increase in these mining areas creates high demand for forest products to satisfy population within the area (Logs used in stabilization of mining pits, furniture, building materials, fuelwood etc.).")
                                       ),
                                column(width = 3,
                                       tags$h6("Tobacco Curing"),
                                       tags$p("The destruction and deforestation happened during farming preparation and time of curing of the tobacco leaves  where massive trees are cut for farm preparation and at the time of curing the tobacco leaves.")
                                       
                                ),
                                column(width = 4,
                                       tags$h6("Overdependency on Biomass Fuel"),
                                       tags$p("It is estimated that more than 90% of households in Tanzania depends on firewood and charcoal as source of energy for cooking. The country's reliance on the biomass energy indicates a potential over exploitation of natural forests and at the same time a potential extinction of tree species.")
                                       )
                                
                                
                              ) 
                               ),
                      ## Impacts  ----
                      tabPanel(title = "Related Impacts",
                               tags$br(),# is left empty as spacer,
                               fluidRow(
                                 column(width = 3, tags$p("The deforestation’s impacts include land degradation, reduced land productivity due to loss of soil fertility, habitat loss, loss of biodiversity both plants and animals. ")),
                                 column(width = 2, tags$p("Deforestation also increased human–wildlife conflicts due to proximity and overlap in the use of space between wildlife, Incidences of property damage by wildlife, livestock and humans. ")),
                                 column(width = 2, tags$p("Further, deforestation led to loss of tourism potentials due to destruction of principle resources including charismatic wildlife species and attractive site and ecosystem services")),
                                 column(width = 3, tags$p("Loss of livelihood options among the communities who rely on forests for food, medicine, fuelwood, building poles and furniture; inadequate or unreliable rainfall patterns; and increase of climate change impacts."))
                               ),
                               tags$br(),
                               fluidRow(), # row for photos related to impacts
                               tags$br()
                              
                           
                               ),
                      
                      ## Intervations  ----
                      tabPanel(title = "Intervations",
                               fluidRow(
                                 tags$br(),
                                 column(width = 3,
                                        tags$h6("Goal"),
                                        tags$p("Enhance conservation of forest ecosystems and promote sustainable utilization of forestry resources")),
                                 column(width = 4, 
                                        tags$h6("Expected Results"),
                                        tags$p("Deforested area reduced, promote use of alternative energy source, improve forest management, promote sustainable agricultural practices, raise awareness on sustainable use of forest resources.")),
                                 column(width = 4,
                                        tags$h6("Priority areas"),
                                        tags$p("Priority areas for strategic interventions in Western zone account for 35 percent of the total deforested area in the country and which covers  Tabora, Shinyanga, Kigoma and Katavi Regions."))
                               ),
                               # tags$br(),
                               fluidRow(
                                 column(width = 5, 
                                        tags$h4("Interventions and Targets")
                                        )
                               ),
                               # tags$br(),
                               fluidRow(
                                 column(width = 3, tags$h6("Restore deforested areas"), 
                                        tags$li("Develop and implement conservation programmes and projects in 4 highly deforested regions by 2032"), 
                                        tags$li("Programmes for tree planting strengthened and implemented in regions with high deforestation rate by 2032"), 
                                        tags$li("Develop and implement awareness creation and capacity building programmes to access carbon credit market by 2032")
                                        
                                        ),
                                 column(width = 3, 
                                        tags$h6("Build Institutional capacity"), 
                                        tags$li("Develop programme for strengthening of legislation that control and manage forest resources by 2032"), 
                                        tags$li("Programmes for forest fire management strengthened and implemented in forest areas by 2032"), 
                                        tags$li("Build capacity of MDAs and LGAs to monitor implementation of the tree planting programme of 1.5 million trees per district per year by 2032")
                                        
                                        ),
                                 column(width = 5, 
                                        tags$h6("Reduce overdependence of biomass energy"), 
                                        tags$li("Develop and implement programmes to provide incentives to reduce initial investment cost in accessing LPG by 2032"), 
                                        tags$li("Develop and implement awareness programme on targeted behavioral change towards use of charcoal and firewood by 2032"), 
                                        tags$li("Promote establishment of infrastructure to support supply and accessibility of Natural Gas for uses in Households and industries by 2032"), 
                                        tags$li("Promote use of energy efficient technologies (cooking stoves and charcoal kiln) to at least 50% of the households")
                                        )
                               ),
                               tags$br()
                               
                               )
                      
                    )
             )
           )),
  ## Water Sources ---- 
  navbarMenu(title = "Water",
             ## Lakes  ----
             tabPanel(title = "Lakes",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tags$br(),
                                 tabPanel(title = "Current State",
                                          fluidRow(
                                            column(width = 6,
                                                   tags$h5("Water Level Flunctutions in Lakes")
                                                   )),
                                          fluidRow(
                                            column(width = 3,
                                                   tags$p("The level of water in major lakes in Tanzania has experienced changes over the last decade. The Lake Rukwa’s water level today is 4.8 meters above the baseline level recorded in 2010. The rising water level in Lake Rukwa is the highest compared to rising water level in other lakes during the same period. Lake Tanganyika experiences water level that is 2.04 meter above that recorded in 2010 and the Lake Victoria water level has risen at relatively 1.4 meters to the baseline level of 2010. Lake Nyasa has seen no changes of water level of the same period.")
                                                   ),
                                            column(width = 3, highchartOutput(outputId = "lakesLevelBar") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
                                                   ),
                                            column(width = 2,
                                                   helpText("To have a glimpse of changing lake level you can can achieve that by specifying the date and then click the action button below to plot according to your specify period. You can also click on the lake legend to toggle on and off the lake of interest"),
                                                   tags$br(),
                                                   tags$br(),
                                                   actionBttn(inputId = "click", label = "Click to Plot", color = "success", style = "material-flat", icon = icon("sliders-h"), block = TRUE),
                                                   tags$br(),
                                                   sliderInput(inputId = "lakesDate", label = "", min = as.Date("2010-01-01"), max = as.Date("2022-02-01"), value = as.Date(c("2010-01-01", "2022-02-01")))
                                                   
                                                   ),
                                            column(width = 3, 
                                                   highchartOutput(outputId = "lakesTrends") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
                                                   )
                                            
                                            ),
                                          tags$br(),
                                          fluidRow( )
                                 ),
                                 tabPanel(title = "Major causes",
                                          fluidRow(
                                            #### Deforestation ----
                                            column(width = 3, 
                                                   tags$h6("Deforestation"),
                                                   tags$p("Deforestation, agricultural expansion, land degradation in the highlands and water catchment areas are the main cause of change in water level in lakes and water flow in rivers")),
                                            #### mining ----
                                            column(width = 2, 
                                                   tags$h6("Mining Activities"),
                                                   tags$p("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                                            #### weather events ----
                                            column(width = 3,
                                                   tags$h6("Weather Events"),
                                                   tags$p("Increase in rainfall in recent years is also a major  factor that contributes to the observed rising water levels in lakes in the country.  For instance, the country has received rainfall above historical mean since 2016, and the heavy rainfall of 2019 was the country’s wettest year on record ")
                                                   ),
                                            ### land degradation ----
                                            column(width = 3, 
                                                   tags$h6("Land degradation"),
                                                   tags$p("Deforestation decreases soil infiltration of water and increases soil erosion leading to higher level of soil erosion that is washed during rainy season and carried into lakes with high sediment load. ")
                                            )
                                          ),
                                          fluidRow()
                                 ),
                                 #### Impacts ----
                                 tabPanel(title = "Related Impacts",
                                          fluidRow(
                                            column(width = 4, 
                                                   tags$p("The water level in major lakes of Tanzania has risen above 2 meters since 2010 with significant impact on ecosystem, social and economic activities of people depending on these lakes for their livelihood. Some of the impacts include surging waters causing flooding, soil erosion, lake shore erosion, and increase siltation in the lakes. The impacts results into flooded lakeshores submerge homes, schools, and damaged business resulting in displacement of people living near the lakes to move to higher ground. In many locations along the Lake Victoria, Tanganyika, Nyasa and Rukwa lakeshore, flooding during rainy season is more frequent than it was in the past")),
                                            column(width = 3,
                                                   tags$p("The increases and decreases in river water levels have an impact on livelihood and socio-economic activities. Flooding can result from high river discharge, whereas low river discharge impact on ecosystem-dependent species and, in extreme cases, develop into a hydrological drought, affecting public water supply, hydroelectric power generation, and commercial river transport. For example, a Rufiji River Basin, which has six major dams")),
                                            column(width = 3,
                                                   tags$p("This has been attributed to unsustainable human activities in the basin and upstream areas, as well as low summer rainfall levels, which created water demand. Improper water allocation for upstream rice irrigation in the Usangu Plains, as well as an increase in the area and inefficiency of traditional smallholder irrigation, contribute to a decrease in water flows in the Little Ruaha and Mbarali Rivers, threatening hydropower generation downstream.")),
                                            column(width = 1)
                                            
                                          ),
                                          fluidRow()
                                          ),
                                 
                                 tabPanel(title = "Intervations",
                                          fluidRow(
                                            column(width = 9,
                                                   tags$h4("The Goal"),
                                                   tags$p("Water sources are conserved, protected and managed in sustainable manners")),
                                          ),
                                          fluidRow(
                                            column(width = 4, tags$h4("Priority Areas"))
                                          ),
                                          fluidRow(
                                            column(width = 4,
                                                   tags$p("To conserve water sources in Kagera, Mara, Ruaha Mkuu, Momba, Lufurio, Wami and Ruvuma rivers, as well as Basin in Lake Victoria, Lake Tanganyika, Lake Nyasa, Lake Rukwa. Mara Wetland, Malagarasi, Malagarasi - Muyowosi Wetland"))
                                            # column(width = 4, tmapOutput(outputId = "riversDecline"))
                                          ),
                                          fluidRow(
                                            column(width = 5, tags$h4("Interventions and targets"))
                                          ),
                                          fluidRow(
                                            column(width = 4, 
                                                   tags$h6("Enhance water catchments management"),
                                                   tags$li("Develop and implement plans for restoration and management of land cover through   tree planting and natural regeneration by 2032"),
                                                   tags$li("Develop and implement program to promote sustainable livestock keeping including destocking, rangeland management and access to water for livestock  by 2032"),
                                                   tags$li("Promote sustainable agriculture through the adoption of best agricultural practices, conservation agriculture and climate-smart agriculture by 2032; and Establish and enhance of Water Users Associations by 2032")
                                                   ),
                                            column(width = 3, 
                                                   tags$h6("Promote efficient water use"),
                                                   tags$li("Promote the use of appropriate infrastructures to support efficient water use for irrigation and other uses by 2032"),
                                                   tags$li("Develop and implement plans for awareness to water users on the conservation and efficient use of water resources by 2032"),
                                                   tags$li("Enhance enforcement through regular monitoring surveillance to control illegal abstraction of water by 2032")
                                            ),column(width = 3, 
                                                     tags$h6("Enhance water quality control"),
                                                     tags$li("Build capacity of institutions responsible for water management in adopting the best available technologies for monitoring water quality by 2032"),
                                                     tags$li("Enhance institutional coordination in the generation, availability and access to water quality data and information by 2032"),
                                                     tags$li("Enhance NEMC capacity to continue monitoring water quality for pollution control by 2032")
                                            )
                                            
                                          ),
                                          fluidRow()
                                          ),
                                 tags$br(),
                                 tags$br()
                                 
                               )
                        )
                      )),
             ## Rivers  ----
             tabPanel(title = "Rivers",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tags$br(),
                                 tabPanel(title = "Current State",
                                          fluidRow(
                                            column(width = 11),
                                            tags$h4("River Flow Rate"),
                                          ),
                                          fluidRow(
                                            column(width = 4,
                                                   tags$p("Across the country, river discharge in water basin showed either a decreasing or increasing trends for a period between 2010 and 2021l. Annual river discharge increased in Malagarasi, Ruhuhu, Songwe, Ruaha Mkuu,  Ruvu and Pangani but decreased in Kagera, Mara, Momba, Lufurio, Mbarali, Ruaha Mdogo,Wami, and Ruvuma Rivers"),
                                                   # tags$br(),
                                                   helpText("Select the station"), 
                                                   selectInput(inputId = "station_id", label = "", choices = stations, selected = stations[1]),
                                                   # tags$br(),
                                                   helpText("Specify the date"), 
                                                   sliderInput(inputId = "dateRiver_id", label = "", min = lubridate::dmy(010110), max = lubridate::dmy(311221), value = c(lubridate::dmy(010110), lubridate::dmy(311221)))),
                                            column(width = 4,
                                                   plotOutput(outputId = "discharge")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 2, textOutput(outputId = "trend_stats")),
                                            column(width = 1)  
                                          )
                                 ),
                                 tabPanel(title = "Major causes",
                                          fluidRow(
                                            #### Deforestation ----
                                            column(width = 3, 
                                                   tags$h6("Deforestation"),
                                                   tags$p("Deforestation, agricultural expansion, land degradation in the highlands and water catchment areas are the main cause of change in water level in lakes and water flow in rivers")),
                                            #### mining ----
                                            column(width = 2, 
                                                   tags$h6("Mining Activities"),
                                                   tags$p("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                                            #### weather events ----
                                            column(width = 3,
                                                   tags$h6("Weather Events"),
                                                   tags$p("Increase in rainfall in recent years is also a major  factor that contributes to the observed rising water levels in lakes in the country.  For instance, the country has received rainfall above historical mean since 2016, and the heavy rainfall of 2019 was the country’s wettest year on record ")
                                            ),
                                            ### land degradation ----
                                            column(width = 3, 
                                                   tags$h6("Land degradation"),
                                                   tags$p("Deforestation decreases soil infiltration of water and increases soil erosion leading to higher level of soil erosion that is washed during rainy season and carried into lakes with high sediment load. ")
                                            )
                                          ),
                                          fluidRow()
                                 ),
                                 #### Impacts ----
                                 tabPanel(title = "Related Impacts",
                                          fluidRow(
                                            column(width = 4, 
                                                   tags$p("The water level in major lakes of Tanzania has risen above 2 meters since 2010 with significant impact on ecosystem, social and economic activities of people depending on these lakes for their livelihood. Some of the impacts include surging waters causing flooding, soil erosion, lake shore erosion, and increase siltation in the lakes. The impacts results into flooded lakeshores submerge homes, schools, and damaged business resulting in displacement of people living near the lakes to move to higher ground. In many locations along the Lake Victoria, Tanganyika, Nyasa and Rukwa lakeshore, flooding during rainy season is more frequent than it was in the past")),
                                            column(width = 3,
                                                   tags$p("The increases and decreases in river water levels have an impact on livelihood and socio-economic activities. Flooding can result from high river discharge, whereas low river discharge impact on ecosystem-dependent species and, in extreme cases, develop into a hydrological drought, affecting public water supply, hydroelectric power generation, and commercial river transport. For example, a Rufiji River Basin, which has six major dams")),
                                            column(width = 3,
                                                   tags$p("This has been attributed to unsustainable human activities in the basin and upstream areas, as well as low summer rainfall levels, which created water demand. Improper water allocation for upstream rice irrigation in the Usangu Plains, as well as an increase in the area and inefficiency of traditional smallholder irrigation, contribute to a decrease in water flows in the Little Ruaha and Mbarali Rivers, threatening hydropower generation downstream.")),
                                            column(width = 1)
                                            
                                          ),
                                          fluidRow()
                                 ),
                                 
                                 tabPanel(title = "Intervations",
                                          fluidRow(
                                            column(width = 9,
                                                   tags$h4("The Goal"),
                                                   tags$p("Water sources are conserved, protected and managed in sustainable manners")),
                                          ),
                                          fluidRow(
                                            column(width = 4, tags$h4("Priority Areas"))
                                          ),
                                          fluidRow(
                                            column(width = 4,
                                                   tags$p("To conserve water sources in Kagera, Mara, Ruaha Mkuu, Momba, Lufurio, Wami and Ruvuma rivers, as well as Basin in Lake Victoria, Lake Tanganyika, Lake Nyasa, Lake Rukwa. Mara Wetland, Malagarasi, Malagarasi - Muyowosi Wetland"))
                                            # column(width = 4, tmapOutput(outputId = "riversDecline"))
                                          ),
                                          fluidRow(
                                            column(width = 5, tags$h4("Interventions and targets"))
                                          ),
                                          fluidRow(
                                            column(width = 4, 
                                                   tags$h6("Enhance water catchments management"),
                                                   tags$li("Develop and implement plans for restoration and management of land cover through   tree planting and natural regeneration by 2032"),
                                                   tags$li("Develop and implement program to promote sustainable livestock keeping including destocking, rangeland management and access to water for livestock  by 2032"),
                                                   tags$li("Promote sustainable agriculture through the adoption of best agricultural practices, conservation agriculture and climate-smart agriculture by 2032; and Establish and enhance of Water Users Associations by 2032")
                                            ),
                                            column(width = 3, 
                                                   tags$h6("Promote efficient water use"),
                                                   tags$li("Promote the use of appropriate infrastructures to support efficient water use for irrigation and other uses by 2032"),
                                                   tags$li("Develop and implement plans for awareness to water users on the conservation and efficient use of water resources by 2032"),
                                                   tags$li("Enhance enforcement through regular monitoring surveillance to control illegal abstraction of water by 2032")
                                            ),column(width = 3, 
                                                     tags$h6("Enhance water quality control"),
                                                     tags$li("Build capacity of institutions responsible for water management in adopting the best available technologies for monitoring water quality by 2032"),
                                                     tags$li("Enhance institutional coordination in the generation, availability and access to water quality data and information by 2032"),
                                                     tags$li("Enhance NEMC capacity to continue monitoring water quality for pollution control by 2032")
                                            )
                                            
                                          ),
                                          fluidRow()
                                 ),
                                 tags$br(),
                                 tags$br()
                                 
                               )
                        )
                      )),
              ## Coastal  ----
             tabPanel(title = "Coastal",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tags$br(),
                                 tabPanel(title = "Current State",
                                          fluidRow(
                                            column(width = 11),
                                            tags$br(),
                                            tags$br(),
                                            tags$h3("kichwa cha Coastal")
                                          ),
                                          fluidRow(
                                            # column(width = 1),
                                            column(width = 4,
                                                   helpText("Wetlands cover roughly 10% of Tanzania's total land area. Wetlands have been designated by the Ramsar Convention on Wetlands as Malagarasi Muyovosi, Kilombero Floodplain, Rufiji Mafia Kilwa (RUMAKI), and Lake Natron. These wetlands are on the verge of becoming extinct. While wetland in Usangu/Ihefu, Kilombero, and Maragarasi shrank by more than 50% between 2002 and 2014, wetland in Mara increased by 14 percent in the same period."),
                                                   tags$br(),
                                                   tags$br(),
                                                   selectInput(inputId = "CoastalId1", label = "Select wetland", choices = wet.name, selected = wet.name[2])),
                                            column(width = 3,
                                                   highchartOutput(outputId = "CoastalId2")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 4,
                                                   tmapOutput(outputId = "CoastalId3")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 1)
                                          )
                                 ),
                                 tabPanel(title = "Major causes",
                                          fluidRow(
                                            #### livestock ----
                                            column(width = 4, 
                                                   tags$h6("Livestock Population"),
                                                   helpText("The livestock population has dramatically increased contributing to reduced land cover, increase soil erodibility and making the land fragile and prone to other agents.")),
                                            #### mining ----
                                            column(width = 3, 
                                                   tags$h6("Mining Activities"),
                                                   helpText("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                                            #### agriculture expansion ----
                                            column(width = 4, 
                                                   tags$h6("Agricultural Expansion"),
                                                   helpText("Between 2008 and 2020 a sharp increase in expansion of agricultural land was evidenced in the country."),
                                                   sliderInput(inputId = "CoastalId4", label = "Percentage change", min = -100, max = 1350, value = c(-100, 1300))
                                            )
                                          ),
                                          fluidRow(
                                            column(width = 4),
                                            column(width = 3),
                                            column(width = 4, highchartOutput(outputId = "CoastalId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            
                                          )
                                 ),
                                 #### Impacts ----
                                 tabPanel(title = "Related Impacts",
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 
                                 tabPanel(title = "Coastal Intervations",
                                          fluidRow(
                                            column(width = 9,
                                                   tags$h6("The Goal"),
                                                   helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )
                                          ),
                                 tags$br(),
                                 tags$br()
                                 
                               )
                        )
                      ))
  ),
  ## Wetland ----
  tabPanel(title = "Wetland",
           fluidRow(
             column(width = 1),
             column(width = 11,
                    tabsetPanel(
                      tags$br(),
                      tabPanel(title = "Current State",
                               fluidRow(
                                 column(width = 11),
                                 tags$br(),
                                 tags$br(),
                                 tags$h4("Shrinking of Wetland")
                               ),
                               fluidRow(
                                 # column(width = 1),
                                 column(width = 4,
                                        tags$p("Wetlands cover roughly 10% of Tanzania's total land area. Wetlands have been designated by the Ramsar Convention on Wetlands as Malagarasi Muyovosi, Kilombero Floodplain, Rufiji Mafia Kilwa (RUMAKI), and Lake Natron. These wetlands are on the verge of becoming extinct. While wetland in Usangu/Ihefu, Kilombero, and Maragarasi shrank by more than 50% between 2002 and 2014, wetland in Mara increased by 14 percent in the same period."),
                                        tags$br(),
                                        helpText("Of the four wetland in the system, you can pick one to map and visualize its changes over time"),
                                        selectInput(inputId = "wetland_id", label = "", choices = wet.name, selected = wet.name[2])),
                                 column(width = 3,
                                        highchartOutput(outputId = "wetPct_change")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                 column(width = 4,
                                        tmapOutput(outputId = "wet_change")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                 column(width = 1)
                               )
                      ),
                      tabPanel(title = "Major causes",
                               fluidRow(
                                 #### Human activities ----
                                 column(width = 4, 
                                        tags$h6("Catchment degradation"),
                                        tags$p("Unsustainable activities in catchment areas upstream causes reduced water flow and sedimentation in the wetland. Catchment degradation removes vegetation cover and hence the vegetation function of retaining water and maintaining steady water flow from river tributaries is compromised. These also causes nutrient enrichment in water bodies and proliferation of water weeds and other invasive plants as evidenced in Lake Babati and Lake Jipe")),
                                 #### Encroachment ----
                                 column(width = 4, 
                                        tags$h6("Encroachment"),
                                        tags$p("Encroachment of wetlands for agricultural activities has been very common in most wetlands in the country. Agricultural expansion towards wetlands is linked to the establishment of vegetable crops, paddy production and other offseason cropping. Continuous degradation of terrestrial ecosystem has triggered movement of agricultural activities towards wetland. Agricultural expansion evidenced in Kilombero and Usangu plain wetlands for rice production ")),
                                 #### agriculture expansion ----
                                 column(width = 3, 
                                        tags$h6("Aquaculture"),
                                        tags$p("Aquaculture investment has increased in recent years, and fish farming is on the rise. Though this is a good source of income, there have been reports of wetlands being invaded and cleared for the construction of fishponds. Wetland clearance disrupts the ecosystem and eliminates the wetland's ecosystem function. This is evident in the wetlands of Lake Victoria in Misungwi Mwanza.")
                                 )
                               )
                               # fluidRow(
                               #   column(width = 4),
                               #   column(width = 3),
                               #   column(width = 4, highchartOutput(outputId = "outputWet") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                               #   
                               # )
                      ),
                      #### Impacts ----
                      tabPanel(title = "Related Impacts",
                               fluidRow(
                                 column(width = 4, 
                                        tags$h6("Loss of wildlife habitats"),
                                        tags$p("The drying of the Ruaha river has disrupted the wildlife habitat causing loss of some species. Example are freshwater oysters that seem to disappear from the Ruaha River, along with clawless otters that lived off them.")),
                                 column(width = 3,
                                        tags$h6("Loss of Biodiversity"),
                                        tags$p("The drying has threatened the White Crowned Plover (Vanellus albiceps) that build their nests on raised sand banks in the river protected from access by terrestrial predators. With the drop in flow have made the Crown Plover to become exposed to predators")),
                                 column(width = 3,
                                        tags$h6("Loss of ecosystem services"),
                                        tags$p("A decline in Tiger Fish (Hydrocynus sp) which require oxygenated water is also experienced. The drying of the river has also disrupted birds of prey and consequently put pressure on the African Fish Eagle (Haliaetus vocifer) resulting in an increasing scarcity of this magnificent bird in Ruaha river")),
                                 column(width = 1)
                                 
                               ),
                               fluidRow()
                               ),
                      
                      tabPanel(title = "Intervations",
                               fluidRow(
                                 column(width = 5,
                                        tags$h4("The Goal"),
                                        tags$p("Enhance the sustainable management and wise use of wetlands.")
                                        ),
                                 
                                 column(width = 5, 
                                        tags$h4("Priority Areas"),
                                        tags$p("Severely degraded wetlands in great Ruaha and Kilombero sub-basin, Mara River wetlands, Malagarasi and dryland ecosystems.")
                                 ),
                               ),
                               fluidRow(
                                 column(width = 5, tags$h4("Interventions and targets"))
                               ),
                               fluidRow(
                                 column(width = 4, 
                                        tags$h6("Promote restoration and conservation of wetlands"),
                                        tags$li("Promote sustainable management of livestock to communities surrounding wetlands by 2025"),
                                        tags$li("Develop and implement wetland management plan for all wetland outside protected areas under the jurisdiction of respective Local Government Authorities by 2032"),
                                        tags$li("Enhance management of at least 50% of the wetlands in protected areas by 2032"),
                                        tags$li("Promote agroforestry and soil conservation practices to address soil erosion and sedimentation by 2032"),
                                        tags$li("Develop land use plans to at least 50% of villages in communities surrounding wetlands by 2032")
                                 ),
                                 column(width = 3, 
                                        tags$h6("Strengthern Wetland Management"),
                                        tags$li("Assess the status of wetland resources for all wetlands by 2025"),
                                        tags$li("Develop programmes and project for enhance institutional capacity responsible for wetland management to undertake research on wise use of wetland resources by 2027"),
                                        tags$li("Enhance coordination among institutions responsible for wetlands management by 2032"),
                                        tags$li("Promote and enhance cooperation on management of transboundary wetlands by 2032"),
                                        tags$li("Demarcate and buffer at least 50% of unprotected wetlands by 2032")
                                 ),column(width = 3, 
                                          tags$h6("Enhance stewardship and build community consciousness"),
                                          tags$li("Establish community Wetlands Management Committees (CWMC) to enhance community participation in wetland conservation by 2025"),
                                          tags$li("Develop and implement programmes and projects for Sensitization and awareness raising on wetland ecosystem stewardship in all wetlands by 2032")
                                          
                                 )
                               ),
                               tags$br(),
                               fluidRow(
                                 column(width = 4, 
                                        tags$h6("Sustainable alternative livelihood Programme "),
                                        tags$li("Inventory on community livelihoods that depend on wetland resources(direct and indirect) by 2025"),
                                        tags$li("Promotion of alternative livelihood to communities surrounding wetland communities in all wetlands by 2025; and"),
                                        tags$li("Upscale the existing environmentally friendly practices to echo the wise use of wetland resources by 2032.")
                                 ),column(width = 3, 
                                          tags$h6("Enhance community capacity"),
                                          tags$li("Develop and implement a Programme for enhancing access to water for domestic and livestock to communities surrounding wetlands through construction of cattle trough to at least 50% of the wetlands outside the protected areas by 2032; and"),
                                          tags$li("Develop and implement Programme for building capacity for accessing finance for wetland management for at least 50% of the major wetlands by 2032.")
                                 ),column(width = 3, 
                                          tags$h6("Promote sustainable irrigation "),
                                          tags$li("Assess water uses and water losses in all irrigation schemes for proper water allocation by 2025"),
                                          tags$li("Improve irrigation efficiency and adopting best water conservation practices to reduce water stress by 2032"),
                                          tags$li("Promote and implement In situ rainwater harvesting technique by 2032.")
                                 )
                               ),
                               
                               ),
                      tags$br(),
                      tags$br()
                      
                    )
             )
           )),
  
## pollution ----
   navbarMenu(title = "Pollution",
              ## Air  ----
              tabPanel(title = "Air",
                       fluidRow(
                         column(width = 1),
                         column(width = 11,
                                tabsetPanel(
                                  tags$br(),
                                  tabPanel(title = "Current State",
                                           fluidRow(
                                             column(width = 11,
                                                    tags$h3("Air Quality")
                                                    ),
                                           ),
                                           fluidRow(
                                             column(width = 5,
                                                    tags$p("Air pollution is one of the global public health risks of concern to human health. Prolonged exposure to air pollutants that is above the prescribed limit may lead to the development of diseases associated with air pollution. The degrading air quality is attributable to different sources of emissions including open burning of forests generating PM, CO, smoke; vehicles and automobiles generating SO2, NOx, CO, VOCs, PM; emissions from power plants and industrial facilities including manufacturing factories, mines and oil refineries generating SO2, CO, NOx, VOCs and PM; improper waste management from municipal and agricultural waste dumping sites generating  Methane, dioxins and furans; fumigation generating chemical hazardous and particulates from un-tarmacked roads. "),
                                               ),
                                             column(width = 5,
                                                    tags$p("In Tanzania, the air pollutant of concern includes Particulate matter (PM2.5 and PM10) which affects more people than any other air pollutant. In most cases, PM2.5 have the ability to remain largely dominant in most areas that still lack tarmac roads including Kasulu, Kibondo, . Pollutants emission such as nitrogen oxides (NOx), Sulfur dioxide (SO2), carbon monoxide (CO) and others are largely present in cities, municipalities and other areas with a larger number of vehicles and fossil fuels from power plants")
                                                    )
                                             
                                             
                                           )
                                  ),
                                  tabPanel(title = "Major causes",
                                           fluidRow(
                                             column(width = 5, 
                                                    tags$h6("Industrial Air Pollutants "),
                                                    tags$p("Emissions from industries affect 66% of major cities including Dar es Salaam, Mwanza, Arusha and Mbeya. It extends further to 40% of municipalities including Iringa, Singida, Temeke, Morogoro, Musoma, Ilemela, Kigoma and Kigamboni. Pollutants are commonly classified into solid, liquid, or gaseous substances that are discharged into the air from a fixed or mobile source, then transmit through the air, undergoes chemical or physical transformation and eventually return to the ground.")),
                                            
                                             column(width = 5, 
                                                    tags$p("The common industries that generate air pollutants include coal and oil-fired thermal plants, cement production industries generating particulate matter, waste recycling and recovery industries generating emissions but also heavy metal air pollutants depending on the raw waste processed. The major challenge of industrial pollution is lack of the efficient air pollution control equipment in the industries and their locations in close proximity to urban populations."),
                                                    ),
                                           ),
                                           fluidRow(
                                             column(width = 4),
                                             column(width = 3),
                                             column(width = 4, highchartOutput(outputId = "AirId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                             
                                           )
                                  ),
                                  #### Impacts ----
                                  tabPanel(title = "Related Impacts",
                                           fluidRow(
                                             column(width = 5, 
                                                    tags$p("Exposure to air pollutants leads to human health that include breathing disorders, cardiovascular diseases, cancer, health and lung diseases. Furthermore, it leads to impairment of the neurological and immune system development. Air emissions and particulate matters have the potential to affect the ecological system that including acidification for areas where pollutants are sulphides in nature. This is more experienced in major cities that contain industries where sulphides are emitted or areas where the particulate matters are of a sulphide nature. "),
                                                    ),
                                             column(width = 5,
                                                    tags$p("Air pollution also leads to eutrophication of water and soil due to atmospheric deposition of the air pollutant. This can be seen in Lake Victoria, which is affected not only by effluent eutrophication but also by atmospheric depositions from industries within the lake catchment. Particulate matters in particular present throughout the country especially in the area where the soil is bare have the potential to reduce visibility and impair plant photosynthesis which in turn affects plants' growth. "),
                                                    ),
                                           ),
                                           ),
                                  
                                  tabPanel(title = "Air Intervations",
                                           fluidRow(
                                             column(width = 9,
                                                    tags$h6("The Goal"),
                                                    helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                                           ),
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of agriculture and grazing land"),
                                                    helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                             column(width = 3,
                                                    tags$h6("Deterioration of water sources"),
                                                    helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                             column(width = 3,
                                                    tags$h6("Silting"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           ),
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of biodiversity"),
                                                    helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                             column(width = 3,
                                                    tags$h6("Internal migration"),
                                                    helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                             column(width = 3,
                                                    tags$h6("Air pollution from dusts"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           )),
                                  tags$br(),
                                  tags$br()
                                  
                                )
                         )
                       )),
              ## Water  ----
              tabPanel(title = "Water",
                       fluidRow(
                         column(width = 1),
                         column(width = 11,
                                tabsetPanel(
                                  tags$br(),
                                  tabPanel(title = "Current State",
                                           fluidRow(
                                             column(width = 11,
                                                    tags$h3("Water Pollution")),
                                           ),
                                           fluidRow(
                                             column(width = 5,
                                                    tags$p("Wastewater pollution in Tanzania emanates from point and non-point sources. Municipal effluent, mining, and industrial discharges are the main sources of point source pollution. These activities are the major sources of heavy metals, low pH values, high sulphide content and turbidity, in both surface and groundwater bodies.  Countrywide, 80% of domestic water usage is turned into wastewater of which the majority is released into the environment without treatment. Both wastewaters generated from domestic sources and industrial activities result in the pollution of water bodies including rivers and lakes. "),
                                                   ),
                                             column(width = 5,
                                                    tags$p("The major pollutants of concern in surface water bodies are nutrients particular nitrates and phosphates from agricultural activities that are taking place throughout the country. The surface water monitoring in various waterbodies detected nitrate levels within acceptable limits. However, in some water sources, such as the Ngerengere River in Morogoro, the Great Ruaha River in Iringa, and the Mbezi River in Dar es Salaam, nitrate and phosphate levels have been shown to rise over time, resulting in eutrophication."),
                                                   ),
                                             column(width = 1,
                                                    #tmapOutput(outputId = "WaterId3")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")
                                                    ),
                                             
                                           )
                                  ),
                                  ###Major causes ----
                                  tabPanel(title = "Major causes",
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Domestic Sources"),
                                                    ),
                                             column(width = 3, 
                                                    tags$h6("Industrial Activities"),
                                                    ),
                                             column(width = 2,
                                                    tags$h6("Mining Activities")
                                             ),
                                             column(width = 3, 
                                                    tags$h6("Agricultural Activities"),
                                                    helpText("The major pollutants of concern in surface water bodies are nutrients particular nitrates and phosphates from agricultural activities that are taking place throughout the country."),
                                                   # sliderInput(inputId = "WaterId4", label = "Percentage change", min = -100, max = 1300, value = c(-100, 1350))
                                             )
                                           ),
                                           fluidRow(
                                             column(width = 4),
                                             column(width = 3),
                                             column(width = 4, highchartOutput(outputId = "WaterId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                             
                                           )
                                  ),
                                  #### Impacts ----
                                  tabPanel(title = "Related Impacts",
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of agriculture and grazing land"),
                                                    helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                             column(width = 3,
                                                    tags$h6("Deterioration of water sources"),
                                                    helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                             column(width = 3,
                                                    tags$h6("Silting"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           ),
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of biodiversity"),
                                                    helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                             column(width = 3,
                                                    tags$h6("Internal migration"),
                                                    helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                             column(width = 3,
                                                    tags$h6("Air pollution from dusts"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           )),
                                  
                                  tabPanel(title = "Water Intervations",
                                           fluidRow(
                                             column(width = 9,
                                                    tags$h6("The Goal"),
                                                    helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                                           ),
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of agriculture and grazing land"),
                                                    helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                             column(width = 3,
                                                    tags$h6("Deterioration of water sources"),
                                                    helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                             column(width = 3,
                                                    tags$h6("Silting"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           ),
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of biodiversity"),
                                                    helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                             column(width = 3,
                                                    tags$h6("Internal migration"),
                                                    helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                             column(width = 3,
                                                    tags$h6("Air pollution from dusts"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           )),
                                  tags$br(),
                                  tags$br()
                                  
                                )
                         )
                       )),
              ## Noise  ----
              tabPanel(title = "Noise",
                       fluidRow(
                         column(width = 1),
                         column(width = 11,
                                tabsetPanel(
                                  tags$br(),
                                  tabPanel(title = "Current State",
                                           fluidRow(
                                             column(width = 11),
                                             tags$br(),
                                             tags$br(),
                                             tags$h3("kichwa cha Noise")
                                           ),
                                           fluidRow(
                                             # column(width = 1),
                                             column(width = 4,
                                                    helpText("Wetlands cover roughly 10% of Tanzania's total land area. Wetlands have been designated by the Ramsar Convention on Wetlands as Malagarasi Muyovosi, Kilombero Floodplain, Rufiji Mafia Kilwa (RUMAKI), and Lake Natron. These wetlands are on the verge of becoming extinct. While wetland in Usangu/Ihefu, Kilombero, and Maragarasi shrank by more than 50% between 2002 and 2014, wetland in Mara increased by 14 percent in the same period."),
                                                    tags$br(),
                                                    tags$br(),
                                                    selectInput(inputId = "NoiseId1", label = "Select wetland", choices = wet.name, selected = wet.name[2])),
                                             column(width = 3,
                                                    highchartOutput(outputId = "NoiseId2")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                             column(width = 4,
                                                    tmapOutput(outputId = "NoiseId3")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                             column(width = 1)
                                           )
                                  ),
                                  tabPanel(title = "Major causes",
                                           fluidRow(
                                             #### livestock ----
                                             column(width = 4, 
                                                    tags$h6("Livestock Population"),
                                                    helpText("The livestock population has dramatically increased contributing to reduced land cover, increase soil erodibility and making the land fragile and prone to other agents.")),
                                             #### mining ----
                                             column(width = 3, 
                                                    tags$h6("Mining Activities"),
                                                    helpText("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                                             #### agriculture expansion ----
                                             column(width = 4, 
                                                    tags$h6("Agricultural Expansion"),
                                                    helpText("Between 2008 and 2020 a sharp increase in expansion of agricultural land was evidenced in the country."),
                                                    sliderInput(inputId = "NoiseId4", label = "Percentage change", min = -100, max = 1300, value = c(-100, 1350))
                                             )
                                           ),
                                           fluidRow(
                                             column(width = 4),
                                             column(width = 3),
                                             column(width = 4, highchartOutput(outputId = "NoiseId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                             
                                           )
                                  ),
                                  #### Impacts ----
                                  tabPanel(title = "Related Impacts",
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of agriculture and grazing land"),
                                                    helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                             column(width = 3,
                                                    tags$h6("Deterioration of water sources"),
                                                    helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                             column(width = 3,
                                                    tags$h6("Silting"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           ),
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of biodiversity"),
                                                    helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                             column(width = 3,
                                                    tags$h6("Internal migration"),
                                                    helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                             column(width = 3,
                                                    tags$h6("Air pollution from dusts"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           )),
                                  
                                  tabPanel(title = "Noise Intervations",
                                           fluidRow(
                                             column(width = 9,
                                                    tags$h6("The Goal"),
                                                    helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                                           ),
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of agriculture and grazing land"),
                                                    helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                             column(width = 3,
                                                    tags$h6("Deterioration of water sources"),
                                                    helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                             column(width = 3,
                                                    tags$h6("Silting"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           ),
                                           fluidRow(
                                             column(width = 3, 
                                                    tags$h6("Loss of biodiversity"),
                                                    helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                             column(width = 3,
                                                    tags$h6("Internal migration"),
                                                    helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                             column(width = 3,
                                                    tags$h6("Air pollution from dusts"),
                                                    helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                             column(width = 2)
                                             
                                           )),
                                  tags$br(),
                                  tags$br()
                                  
                                )
                         )
                       ))
             ),
  navbarMenu(title = "Waste",
             ## Solid  ----
             tabPanel(title = "Solid",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tags$br(),
                                 tabPanel(title = "Current State",
                                          fluidRow(
                                            column(width = 11),
                                            tags$h4("Solid Waste Management")
                                            ),
                                          fluidRow(
                                              column(width = 3,
                                                     tags$p("Solid Waste Management is among the most serious environmental and public health issues confronting most of the urban areas in the country. Rapid urbanization, economic growth and changes in lifestyles and consumption patterns have resulted in a remarkable increase of waste volume and diversity mostly in cities, municipalities and town councils. While generation has been on the rise, collection and disposal practices continue to be a problem")
                                                     ),
                                              column(width = 3,
                                                     tags$p("Despite generating nearly 7 million tons annually, it is only 32.72% of the generated waste is collected countrywide. Cities are leading with highest collection rate while district councils exhibiting less collection rate"),
                                                     tags$br(),
                                                     helpText("Slide the cursor to specify the range of percentage collection by LGA"),
                                                     sliderInput(inputId = "pct_wasteID", label = "", min = 0, max = 100, value = c(0,100), step = 10)
                                              ),
                                              column(width = 4, tmapOutput(outputId = "percent_collection") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                              column(width = 1)
                                            ),
                                            tags$br(),
                                            tags$br(),
                                            fluidRow(
                                              column(width = 6, 
                                                     tags$h4("Mode of Waste Disposal"))
                                            ),
                                            fluidRow(
                                              # column(width = 1),
                                              column(width = 3,
                                                     tags$p("There are four major waste disposal methods in the country. These landfills are classified as illegal, uncontrolled, controlled, or sanitary. Only sanitary landfills are environmentally friendly waste disposal, controlled and sound as a form of waste management, but they are classified alongside uncontrolled and illegal waste disposal, both of which pollute the environment. These three non-environmentally friendly modes account for more than 92 percent of all LGAs, implying that the country is failing to manage the waste generated daily.")),
                                              column(width = 4, tmapOutput(outputId = "disposal")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                              column(width = 3,
                                                     helpText("Hover on the donut plot to get more information on generation, collection and percentage collected"),
                                                     plotlyOutput(outputId = "waste_pie")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                              column(width = 1)
                                            ),
                                            tags$br(),
                                            fluidRow()
                                         
                                          
                                 ),
                                 tabPanel(title = "Major causes",
                                          fluidRow(
                                            column(width = 3,
                                                   tags$h6("Poor Waste Minimization Practises"),
                                                   tags$p("One of the most effective approach in waste minimization is the waste Reduce, Reuse, and Recycling (3Rs) Practices. However, nearly all LGAs in the country lack 3Rs initiatives which consequently increases the waste management cost by increasing the amount of waste to be transported to the disposal sites. It is only, Arusha and Mbeya cities, as well as Moshi, Kinondoni and Iringa Municipalities where there are few cases of waste Reduce, Reuse and Recycling initiatives. Arusha and Mbeya cities and Kinondoni and Moshi municipalities have established composting plants to reduce the amount of organic waste to be sent to disposal sites. ")
                                                   ),
                                            column(width = 3,
                                                   tags$h6("Poor Contracts"),
                                                   tags$p("There has been poor contracts to waste collection companies for solid waste in LGAs. This include; delays in the compensation of service providers, inadequate contract duration and sharing of waste collection charges. For example, in the compensation of service providers, there has been payment delays where majority of the LGAs take at least 30 days to pay a service provider as shown in Figure 11-7. The delays have been affecting the effectiveness and efficiency of the service providers and consequently, contributed to inadequate waste management services")
                                                   ),
                                            column(width = 3,
                                                   tags$h6("Financial Constraints"),
                                                   tags$p("Effective management of 1 tonne of solid waste in developing countries requires at least Tshs. 80,000. However, inadequate resources in LGAs has been affecting solid waste management financing. Consequently, effective collection of waste charges becomes the most viable waste management financing option. However,  there has been ineffective collection of waste charges. This is attributed to ineffective customer registrations which denies LGAs  reliable customer database; ineffective billing system which affects timely payment for service recipients and  inconvenient payment systems which affect customers’ willingness  to pay")
                                                   ),
                                            column(width = 2)
                                          ),
                                          fluidRow()
                                 ),
                                 #### Impacts ----
                                 tabPanel(title = "Related Impacts",
                                          fluidRow(
                                            column(width = 5,
                                                   tags$p("Improper waste disposal practice through controlled, uncontrolled and illegal dumping account for over 90% of disposal practice to all local Government authorities in the country.")
                                                   ),
                                            column(width = 5,
                                                   tags$p("However, such practices causes significant economic, social and environmental impacts including blockage of drainage systems and hence aggravates floods incidents and leakage of waste into the environment which resulted into land, air and water pollution with subsequent impact to human health.")
                                            ),
                                          ),
                                          fluidRow(
                                            column(width = 10, tags$hr()
                                                   )
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Emission of Poisonous Chemicals"), 
                                                   tags$p("Inadequate waste management lead to waste open burning practice which causes significant air pollution that impacts human health through emission of extremely poisonous “dioxins” and “furans”. The “furans” and “dioxins” are unintended Persistent Organic pollutants (uPOPs) with some peculiar characteristics such as ability to  remain in the environment for a long time, ability to travel long distances through air and water, bioaccumulation in the fatty tissue of animals and hence potential for causing cancer")
                                                   ),
                                            column(width = 3, 
                                                   tags$h6("Health Related Impacts"), 
                                                   tags$p("In urban authorities especially cities and municipalities, the indiscriminate dumping of waste creates risks of diseases. Research has shown a correlation between proximity to open dumpsites and incidence of disease, including increased risk of cholera, malaria, typhoid fever, dengue fever and Zika .Also, exposure to chemicals such “Furans” and dioxins from open waste burning practice can be associated with a wide range of adverse health effects including endocrine disruption, reproductive and immune dysfunction, neurobehavioral disorders, cancer, organ damage and death. Further, smoke from open waste burning can cause other adverse health effects such as respiratory irritation of the airways, coughing or difficulty breathing, decreased lung function and aggravated chronic bronchitis")
                                                   ),
                                            column(width = 3, 
                                                   tags$h6("Flooding Incidences"), 
                                                   tags$p("Indiscrimate disposal of generated waste cause waste to be transported by flood waters downstream, where it gets trapped in bridge and culvert openings. This waste accumulates together with sediments, leading to reduced flow capacity and flooding upstream of the roads and bridges and other manmade infrastructure barriers. Consequently, it causes severe floods and economic loss including destruction of infrastructure ")
                                                   ),
                                            column(width = 2, 
                                                   tags$h6("Degraded Beaches"), 
                                                   tags$p("Improper waste management has also been associated with littering at water bodies and thus, contributing to the death of aquatic organisms and deterioration of beach qualities ")
                                                   )
                                            
                                          ),
                                          fluidRow()
                                 ),
                                 
                                 tabPanel(title = "Intervations",
                                          
                                         
                                          ),
                                 tags$br(),
                                 tags$br()
                                 
                               )
                        )
                      )),
             ## Liquid  ----
             tabPanel(title = "Liquid",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tags$br(),
                                 tabPanel(title = "Current State",
                                       fluidRow(
                                         column(width = 5,
                                                tags$h4("Liquid Waste Management ")
                                                )
                                       ),
                                       fluidRow(
                                         column(width = 5, 
                                                tags$p("Liquid waste is the end water form material produced from household level or industrial level or health center and disposed of, sometimes are called unwanted water for use for the domestic purposes.")),
                                         column(width = 5, 
                                                tags$p("Depending on the source of generation the generated waste water can be categorized into sanitary sewage, industrial liquid waste, storm sewage and mixed sewage"))
                                       ),
                                       fluidRow(
                                         column(width = 3, 
                                                tags$h6("Sewage Management"), 
                                                tags$p("The amount of sewage collected and transported to treatment plants in the country is still very low. In average, less than 30 percent of the generated sewage were collected and transported by truck and sewer networks in the country. Table 11-2 shows estimates of the generated and collected sewage annually in some selected Local Government Authorities those who are not connected to sewer network (off-site) and those relying on on-site services")
                                                ),
                                         column(width = 3, 
                                                tags$h6("Industrial sewage management"), 
                                                tags$p("Most industries are located in urban areas and in large towns of Dar es Salaam, Arusha, Morogoro, Moshi and Mwanza were established without environmental consideration, and have been operating without liquid waste pre-treatment facilities. Although these industries have put in place liquid waste disposal facilities, but are inappropriate and ineffective. Some industries have been reported to pollute water bodies including rivers Msimbazi, Kizinga and Mzinga in Dar es Salaam; Themi in Arusha; Ngerengere in Morogoro; Karanga in Kilimanjaro; Mirongo and Lake Victoria in Mwanza.")
                                         ),
                                         column(width = 3, 
                                                tags$h6("Sewer network coverage"), 
                                                tags$p("According to the Waste Management Investment Guide (2020), only 10 out of more than 100 urban centres, in the country have sewerage systems which serve less than 20% of the total urban population. The access to the sewerage services by population residing in urban settlements continues to be low without significant improvement over the time. Only 7.5% of the population living in urban areas connected to the sewer network. More than 70 percent could not access the sewer networks in their respective urban centres ")
                                         ),
                                         column(width = 3, 
                                                tags$h6("Sanitary sewage management"), 
                                                tags$p("This is the liquid waste produced from homes that containing human wastes and wash water. This is mostly generated from homes, public buildings or commercial and industrial site. Sometimes is referred as domestic waste water meant waste from kitchen, barn, bathroom, laundry, and others that do not contain human sewage. Most households in the country lack sewage infrastructure and directly discharge liquid waste into the environment through drainage line, seeping pit, latrines, septic tanks, public toilets, open ground excreta defection and gray water from kitchens and bathrooms that connect to rivers and open spaces without any treatment.")
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 3, highchartOutput(outputId = "sewagebar")  %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                         column(width = 3),
                                         column(width = 3, highchartOutput(outputId = "sewagebarPop")  %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                         
                                       )
                                 ),
                                 tabPanel(title = "Major causes",
                                          fluidRow(
                                            #### livestock ----
                                            column(width = 4, 
                                                   tags$h6("Livestock Population"),
                                                   helpText("The livestock population has dramatically increased contributing to reduced land cover, increase soil erodibility and making the land fragile and prone to other agents.")),
                                            #### mining ----
                                            column(width = 3, 
                                                   tags$h6("Mining Activities"),
                                                   helpText("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                                            #### agriculture expansion ----
                                            column(width = 4, 
                                                   tags$h6("Agricultural Expansion"),
                                                   helpText("Between 2008 and 2020 a sharp increase in expansion of agricultural land was evidenced in the country."),
                                                   sliderInput(inputId = "LiquidId4", label = "Percentage change", min = -100, max = 1300, value = c(-100, 1350))
                                            )
                                          ),
                                          fluidRow(
                                            column(width = 4),
                                            column(width = 3),
                                            column(width = 4, highchartOutput(outputId = "LiquidId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            
                                          )
                                 ),
                                 #### Impacts ----
                                 tabPanel(title = "Related Impacts",
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 
                                 tabPanel(title = "Liquid Intervations",
                                          fluidRow(
                                            column(width = 9,
                                                   tags$h6("The Goal"),
                                                   helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 tags$br(),
                                 tags$br()
                                 
                               )
                        )
                      )),
             ## eWaste  ----
             tabPanel(title = "eWaste",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tags$br(),
                                 tabPanel(title = "Current State",
                                          fluidRow(
                                            column(width = 11),
                                            tags$br(),
                                            tags$br(),
                                            tags$h3("kichwa cha eWaste")
                                          ),
                                          fluidRow(
                                            # column(width = 1),
                                            column(width = 4,
                                                   helpText("Wetlands cover roughly 10% of Tanzania's total land area. Wetlands have been designated by the Ramsar Convention on Wetlands as Malagarasi Muyovosi, Kilombero Floodplain, Rufiji Mafia Kilwa (RUMAKI), and Lake Natron. These wetlands are on the verge of becoming extinct. While wetland in Usangu/Ihefu, Kilombero, and Maragarasi shrank by more than 50% between 2002 and 2014, wetland in Mara increased by 14 percent in the same period."),
                                                   tags$br(),
                                                   tags$br(),
                                                   selectInput(inputId = "eWasteId1", label = "Select wetland", choices = wet.name, selected = wet.name[2])),
                                            column(width = 3,
                                                   highchartOutput(outputId = "eWasteId2")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 4,
                                                   tmapOutput(outputId = "eWasteId3")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 1)
                                          )
                                 ),
                                 tabPanel(title = "Major causes",
                                          fluidRow(
                                            #### livestock ----
                                            column(width = 4, 
                                                   tags$h6("Livestock Population"),
                                                   helpText("The livestock population has dramatically increased contributing to reduced land cover, increase soil erodibility and making the land fragile and prone to other agents.")),
                                            #### mining ----
                                            column(width = 3, 
                                                   tags$h6("Mining Activities"),
                                                   helpText("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                                            #### agriculture expansion ----
                                            column(width = 4, 
                                                   tags$h6("Agricultural Expansion"),
                                                   helpText("Between 2008 and 2020 a sharp increase in expansion of agricultural land was evidenced in the country."),
                                                   sliderInput(inputId = "eWasteId4", label = "Percentage change", min = -100, max = 1300, value = c(-100, 1350))
                                            )
                                          ),
                                          fluidRow(
                                            column(width = 4),
                                            column(width = 3),
                                            column(width = 4, highchartOutput(outputId = "eWasteId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            
                                          )
                                 ),
                                 #### Impacts ----
                                 tabPanel(title = "Related Impacts",
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 
                                 tabPanel(title = "eWaste Intervations",
                                          fluidRow(
                                            column(width = 9,
                                                   tags$h6("The Goal"),
                                                   helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 tags$br(),
                                 tags$br()
                                 
                               )
                        )
                      ))
  ),
## Biodiversity ----
  navbarMenu(title = "Biodiversity",
             ## Terrestrial  ----
             tabPanel(title = "Terrestrial",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tags$br(),
                                 tabPanel(title = "Current State",
                                          # fluidRow(
                                          #   column(width = 11),
                                          #   # tags$br(),
                                          #   # tags$br(),
                                          #   tags$h4("Terrestrial Biodiversity")
                                          # ),
                                          fluidRow(
                                            column(width = 5,
                                                   tags$h4("Loss of Habitats and Biodiversity"))
                                          ),
                                          fluidRow(
                                           
                                            column(width = 3,
                                                   tags$p("Tanzania is one of the mega rich biodiversity hotspots in the world with 32,836 known important plant and mammal’s species recorded and confirmed according to Global Biodiversity Information Facility (GBIF).")
                                                   ),
                                            column(width = 4,
                                                   tags$p("Wildlife habitats and biodiversity in country have been threatened by various factors including, Conversion of natural lands to other land uses such as settlements, agriculture and grazing; habitat degradation due to wild-fires, unplanned land use and unmanaged natural resource extraction has resulted into serious habitat degradation and consequent loss of biodiversity in the country.")
                                            ),
                                            column(width = 4,
                                                   tags$p("Other serious threats to loss of wildlife habitats and biodiversity include charcoal burning, firewood collection, slash and burning for subsistence farming, mineral and aggregate mining, commercial logging, poaching, coral destruction, infrastructural development, proliferation of Invasive Species (IS), illegal global trade in plant and animal species, accidents in the roads crossing reserved areas and impacts of climate change. ")
                                            )
                                          ), fluidRow(
                                            column(width = 5,
                                                   tags$h5("Biodiversity in Tanzania"))
                                          ),
                                          fluidRow(
                                            column(width = 6, 
                                                   tags$p("Tanzania is home to over 32,826 species, according to the GBIF. Approximately 24,882 records are classified as not evaluated (NE), accounting for roughly 76% of the total. The remaining 5,847 evaluated species are classified as least concern (LC) (18 percent ).")),
                                            
                                            column(width = 5, 
                                                   tags$p("Other threatened groups include vulnerable (VU) 619 (2%) endangered (EN) 577 (2%) data deficiency (DD) 373 (1%) near threaten (NT) 288 (1%) and critically endangered (CR) 243 (1 percent)"))
                                             ),
                                          tags$br(),
                                          # Adding the following line to your ui.r file will expand Shiny support for Font Awesome 5.7.2 icons:
                                          tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                                          fluidRow(
                                            column(width = 2, infoBoxOutput(outputId = "birds", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 2, infoBoxOutput(outputId = "fish", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 2, infoBoxOutput(outputId = "rain", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 2, infoBoxOutput(outputId = "visibility", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 2, infoBoxOutput(outputId = "waves", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 1)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 2, infoBoxOutput(outputId = "current", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 2, infoBoxOutput(outputId = "oxygen", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 2, infoBoxOutput(outputId = "air", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 2, infoBoxOutput(outputId = "water", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            # column(width = 2, infoBoxOutput(outputId = "turbidity", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 2, infoBoxOutput(outputId = "soil", width = NULL)%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 1)
                                            
                                          ),
                                          tags$br(),
                                          fluidRow(
                                            # column(width = 1),
                                            column(width = 4,
                                                   tags$h5("Richness and Occurance"),
                                                   tags$p("Species richness is the number of different species represented in an ecological community, landscape or region. Species richness is simply a count of species, and it does not take into account the abundances of the species or their relative abundance distributions. Patterns in species occurrence may be caused by a variety of factors, which influence how a species is distributed across a region at any point in time."),
                                                   helpText("You can choose between richness and occurance of species"),
                                                   selectInput(inputId = "bioTerrestrial", label = "", choices = c("Richness", "Occurances"), selected = "Richness"),
                                                   selectInput(inputId = "hotCold", label = "", choices = c("Significant High", "Significant Low", "Insignificant"), selected = "Significant High")
                                                   ),
                                            column(width = 4,
                                                   tmapOutput(outputId = "speciesTerrestrialMap")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 3,
                                                   highchartOutput(outputId = "speciesBar")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                           
                                            column(width = 1)
                                          )
                                 ),
                                 tabPanel(title = "Major causes",
                                          fluidRow(
                                            #### livestock ----
                                            column(width = 2, 
                                                   tags$h6("Livestock Population"),
                                                   helpText("In many parts of the country, there is an increasing demand for grazing land and feeds for the growing number of livestock.")),
                                            #### mining ----
                                            column(width = 2, 
                                                   tags$h6("Mining Activities"),
                                                   helpText("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                                            #### agriculture expansion ----
                                            column(width = 2, 
                                                   tags$h6("Pollution"),
                                                   helpText("Most aquatic habitats suffer from excessive levels of nutrients mainly phosphates and nitrates that originate from domestic, industrial as well as agricultural activities. "),
                                                   ),
                                            column(width = 2,
                                                   tags$h6("Bush fire"),
                                                   helpText("Bush fire is a threat to forests and game areas in the country where most fires are caused by human activities particularly farm preparation.")),
                                            column(width = 2,
                                                   tags$h6("Agricultural Expansion"),
                                                   helpText("Opening new land for agriculture contributes significantly to habitat fragmentation and subsequent biodiversity loss in many areas of the country. ")),
                                            column(width = 2,
                                                   tags$h6("Poaching"),
                                                   helpText("The wildlife sector faces several challenges among them poaching is ranked the top. Poaching is pursued to cater for subsistence and commercial needs."))
                                          ),
                                          fluidRow(
                                            column(width = 2),
                                            column(width = 2),
                                            column(width = 2),
                                            column(width = 2),
                                            column(width = 2),
                                            column(width = 2, highchartOutput(outputId = "TerrestrialId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            
                                          )
                                 ),
                                 #### Impacts ----
                                 tabPanel(title = "Related Impacts",
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 
                                 tabPanel(title = "Terrestrial Intervations",
                                          fluidRow(
                                            column(width = 9,
                                                   tags$h6("The Goal"),
                                                   helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 tags$br(),
                                 tags$br()
                                 
                               )
                        )
                      )),
             ## Freshwater  ----
             tabPanel(title = "Freshwater",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tags$br(),
                                 tabPanel(title = "Current State",
                                          fluidRow(
                                            column(width = 11),
                                            tags$br(),
                                            tags$br(),
                                            tags$h3("kichwa cha Freshwater")
                                          ),
                                          fluidRow(
                                            # column(width = 1),
                                            column(width = 4,
                                                   helpText("Wetlands cover roughly 10% of Tanzania's total land area. Wetlands have been designated by the Ramsar Convention on Wetlands as Malagarasi Muyovosi, Kilombero Floodplain, Rufiji Mafia Kilwa (RUMAKI), and Lake Natron. These wetlands are on the verge of becoming extinct. While wetland in Usangu/Ihefu, Kilombero, and Maragarasi shrank by more than 50% between 2002 and 2014, wetland in Mara increased by 14 percent in the same period."),
                                                   tags$br(),
                                                   tags$br(),
                                                   selectInput(inputId = "FreshwaterId1", label = "Select wetland", choices = wet.name, selected = wet.name[2])),
                                            column(width = 3,
                                                   highchartOutput(outputId = "FreshwaterId2")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 4,
                                                   tmapOutput(outputId = "FreshwaterId3")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 1)
                                          )
                                 ),
                                 tabPanel(title = "Major causes",
                                          fluidRow(
                                            #### livestock ----
                                            column(width = 4, 
                                                   tags$h6("Livestock Population"),
                                                   helpText("The livestock population has dramatically increased contributing to reduced land cover, increase soil erodibility and making the land fragile and prone to other agents.")),
                                            #### mining ----
                                            column(width = 3, 
                                                   tags$h6("Mining Activities"),
                                                   helpText("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                                            #### agriculture expansion ----
                                            column(width = 4, 
                                                   tags$h6("Agricultural Expansion"),
                                                   helpText("Between 2008 and 2020 a sharp increase in expansion of agricultural land was evidenced in the country."),
                                                   sliderInput(inputId = "FreshwaterId4", label = "Percentage change", min = -100, max = 1300, value = c(-100, 1350))
                                            )
                                          ),
                                          fluidRow(
                                            column(width = 4),
                                            column(width = 3),
                                            column(width = 4, highchartOutput(outputId = "FreshwaterId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            
                                          )
                                 ),
                                 #### Impacts ----
                                 tabPanel(title = "Related Impacts",
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 
                                 tabPanel(title = "Freshwater Intervations",
                                          fluidRow(
                                            column(width = 9,
                                                   tags$h6("The Goal"),
                                                   helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 tags$br(),
                                 tags$br()
                                 
                               )
                        )
                      )),
             ## Marine  ----
             tabPanel(title = "Marine",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tags$br(),
                                 tabPanel(title = "Current State",
                                          fluidRow(
                                            column(width = 11),
                                            tags$br(),
                                            tags$br(),
                                            tags$h3("kichwa cha Marine")
                                          ),
                                          fluidRow(
                                            # column(width = 1),
                                            column(width = 4,
                                                   helpText("Wetlands cover roughly 10% of Tanzania's total land area. Wetlands have been designated by the Ramsar Convention on Wetlands as Malagarasi Muyovosi, Kilombero Floodplain, Rufiji Mafia Kilwa (RUMAKI), and Lake Natron. These wetlands are on the verge of becoming extinct. While wetland in Usangu/Ihefu, Kilombero, and Maragarasi shrank by more than 50% between 2002 and 2014, wetland in Mara increased by 14 percent in the same period."),
                                                   tags$br(),
                                                   tags$br(),
                                                   selectInput(inputId = "MarineId1", label = "Select wetland", choices = wet.name, selected = wet.name[2])),
                                            column(width = 3,
                                                   highchartOutput(outputId = "MarineId2")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 4,
                                                   tmapOutput(outputId = "MarineId3")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            column(width = 1)
                                          )
                                 ),
                                 tabPanel(title = "Major causes",
                                          fluidRow(
                                            #### livestock ----
                                            column(width = 4, 
                                                   tags$h6("Livestock Population"),
                                                   helpText("The livestock population has dramatically increased contributing to reduced land cover, increase soil erodibility and making the land fragile and prone to other agents.")),
                                            #### mining ----
                                            column(width = 3, 
                                                   tags$h6("Mining Activities"),
                                                   helpText("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                                            #### agriculture expansion ----
                                            column(width = 4, 
                                                   tags$h6("Agricultural Expansion"),
                                                   helpText("Between 2008 and 2020 a sharp increase in expansion of agricultural land was evidenced in the country."),
                                                   sliderInput(inputId = "MarineId4", label = "Percentage change", min = -100, max = 1300, value = c(-100, 1350))
                                            )
                                          ),
                                          fluidRow(
                                            column(width = 4),
                                            column(width = 3),
                                            column(width = 4, highchartOutput(outputId = "MarineId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                                            
                                          )
                                 ),
                                 #### Impacts ----
                                 tabPanel(title = "Related Impacts",
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 
                                 tabPanel(title = "Marine Intervations",
                                          fluidRow(
                                            column(width = 9,
                                                   tags$h6("The Goal"),
                                                   helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of agriculture and grazing land"),
                                                   helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                                            column(width = 3,
                                                   tags$h6("Deterioration of water sources"),
                                                   helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                                            column(width = 3,
                                                   tags$h6("Silting"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          ),
                                          fluidRow(
                                            column(width = 3, 
                                                   tags$h6("Loss of biodiversity"),
                                                   helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                                            column(width = 3,
                                                   tags$h6("Internal migration"),
                                                   helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                                            column(width = 3,
                                                   tags$h6("Air pollution from dusts"),
                                                   helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                                            column(width = 2)
                                            
                                          )),
                                 tags$br(),
                                 tags$br()
                                 
                               )
                        )
                      ))
  ),
## Climate Change ----
tabPanel(title = "Climate",
         fluidRow(
           column(width = 1),
           column(width = 11,
                  tabsetPanel(
                    tags$br(),
                    tabPanel(title = "Current State",
                             fluidRow( tags$h4("Climate is Real!")),
                             fluidRow(
                               column(width = 5,
                               # tags$br(),
                               tags$p("The scientific evidence for warming of the climate system is unequivocal, and the evidence for rapid climate change is compelling for both land and sea temperature rises.")                                      
                             ),
                             column(width = 5,
                                    tags$p(" Tanzania is already experiencing changes in climate, evidenced by increasing temperature trends characterized by incidences of the highest ever recorded temperature mostly observed over the last decade. ")),
                             ),
                             fluidRow(
                               # column(width = 1),
                               column(width = 3,
                                      tags$h5("Hot and Cold spots"),
                                      tags$p("Significant hotspots and coldspots by several climate change indices aggregated by Tanzania's regions and then mapped to show areas with clusters of either hot or cold of selected climate variable"),
                                      # tags$br(),
                                      helpText("Select the variable to assess the extent of climate change by regions in the country"),
                                      selectInput(inputId = "climateVariable", label = "", choices = climate.change.var, selected = climate.change.var[2])
                                      ),
                               column(width = 4,
                                      highchartOutput(outputId = "climateBar")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                               column(width = 4,
                                      tmapOutput(outputId = "climateMap")%>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                               column(width = 1)
                             )
                    ),
                    tabPanel(title = "Major causes",
                             fluidRow(
                               #### livestock ----
                               column(width = 4, 
                                      tags$h6("Livestock Population"),
                                      helpText("The livestock population has dramatically increased contributing to reduced land cover, increase soil erodibility and making the land fragile and prone to other agents.")),
                               #### mining ----
                               column(width = 3, 
                                      tags$h6("Mining Activities"),
                                      helpText("Unsustainable mining activities cause serious physical disturbance to the land, river banks and bed, hence accelerate soil erosion and siltation.")),
                               #### agriculture expansion ----
                               column(width = 4, 
                                      tags$h6("Agricultural Expansion"),
                                      helpText("Between 2008 and 2020 a sharp increase in expansion of agricultural land was evidenced in the country."),
                                      sliderInput(inputId = "ClimateId4", label = "Percentage change", min = -100, max = 1300, value = c(-100, 1350))
                               )
                             ),
                             fluidRow(
                               column(width = 4),
                               column(width = 3),
                               column(width = 4, highchartOutput(outputId = "ClimateId5") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")),
                               
                             )
                    ),
                    #### Impacts ----
                    tabPanel(title = "Related Impacts",
                             fluidRow(
                               column(width = 3, 
                                      tags$h6("Loss of agriculture and grazing land"),
                                      helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                               column(width = 3,
                                      tags$h6("Deterioration of water sources"),
                                      helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                               column(width = 3,
                                      tags$h6("Silting"),
                                      helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                               column(width = 2)
                               
                             ),
                             fluidRow(
                               column(width = 3, 
                                      tags$h6("Loss of biodiversity"),
                                      helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                               column(width = 3,
                                      tags$h6("Internal migration"),
                                      helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                               column(width = 3,
                                      tags$h6("Air pollution from dusts"),
                                      helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                               column(width = 2)
                               
                             )),
                    
                    tabPanel(title = "Climate Change Intervations",
                             fluidRow(
                               column(width = 9,
                                      tags$h6("The Goal"),
                                      helpText("Maintain, restore and enhance the land resource base and the ecosystem services that flow from the land resources. The aspirational goal is that, by the year 2030, degraded landscapes in all basins are revitalized to maintain their functionality in supporting biodiversity and ecosystem services. River catchments, flood plains, agricultural and grazing lands are conserved, soil erosion is reduced, infiltration and ground-water recharge improved, run-off and siltation are reduced, and the downstream effects of drying of rivers and floods are reduced.")),
                             ),
                             fluidRow(
                               column(width = 3, 
                                      tags$h6("Loss of agriculture and grazing land"),
                                      helpText("Land degradation causes decline in soil fertility and productivity, crop and pasture production hence diminishing of arable land. Degradation can further decrease arable land through physical soil removal and formation of gullies (Plate 2 3). The loss of land productivity leads to limited growth of pasture and hence reduce grazing land. This is evidenced in most dryland areas of Arusha, Manyara, Simiyu, Dodoma, Shinyanga and Singida.")),
                               column(width = 3,
                                      tags$h6("Deterioration of water sources"),
                                      helpText("Land degradation is associated to catchment degradation which leads to soil erosion and sedimentation in river beds that cause degradation of water quality. Degradation of catchments reduce the catchment capacity to retain rainwater and maintain gradual water flow downstream. This reduces perennial flow of water and making some rivers to become seasonal and increasing flood incidences")),
                               column(width = 3,
                                      tags$h6("Silting"),
                                      helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                               column(width = 2)
                               
                             ),
                             fluidRow(
                               column(width = 3, 
                                      tags$h6("Loss of biodiversity"),
                                      helpText("Land degradation leads to removal of vegetation cover which consists of number of species. The vegetation is also a habitat to number of faunas which when the vegetation is removed, their habitat is lost and hence their survival is threatened.")),
                               column(width = 3,
                                      tags$h6("Internal migration"),
                                      helpText("Loss of agriculture and grazing land due to land degradation has caused the land to become hostile to the surrounding community and hence some of them have decided to move to other parts of the country. In Shinyanga, Mwanza and Simiyu for example, grazing area during the drying period is very scarce, pastoralists have to make use of crop residue in the harvested fields of maize, cotton and rice.")),
                               column(width = 3,
                                      tags$h6("Air pollution from dusts"),
                                      helpText("The impact of wide spread erosion in the catchment is manifested by deposition of sediments in rivers, wetlands lakes, and dams. Consequently, leading to increased flooding and diverted water during high peak rainfall. The most affected water bodies are Lake Rukwa, Lake Manyara, Lake Jipe, Lake Babati and Nyumba ya Mungu Dam with an estimated sediment deposit 13 t/ha/yr")),
                               column(width = 2)
                               
                             )),
                    tags$br(),
                    tags$br()
                    
                  )
           )
         )),
  navbarMenu(title = "Dodoma",
             tabPanel(title = "Land degradation",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                          tabsetPanel(
                            tabPanel(title = "Current State"),
                            tabPanel(title = "Major causes"),
                            tabPanel(title = "Related Impacts"),
                            tabPanel(title = "Intervations")
                        
                            )
                          )
                        )
                      ),
             tabPanel(title = "Deforestatation",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tabPanel(title = "Current State"),
                                 tabPanel(title = "Major causes"),
                                 tabPanel(title = "Related Impacts"),
                                 tabPanel(title = "Intervations")
                                 
                               )
                        )
                      )),
             tabPanel(title = "Waste Management",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tabPanel(title = "Current State"),
                                 tabPanel(title = "Major causes"),
                                 tabPanel(title = "Related Impacts"),
                                 tabPanel(title = "Intervations")
                                 
                               )
                        )
                      )),
             tabPanel(title = "Pollution",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tabPanel(title = "Current State"),
                                 tabPanel(title = "Major causes"),
                                 tabPanel(title = "Related Impacts"),
                                 tabPanel(title = "Intervations")
                                 
                               )
                        )
                      )),
             tabPanel(title = "Deterioration water sources",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tabPanel(title = "Current State"),
                                 tabPanel(title = "Major causes"),
                                 tabPanel(title = "Related Impacts"),
                                 tabPanel(title = "Intervations")
                                 
                               )
                        )
                      )),
             tabPanel(title = "Biodiversity",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tabPanel(title = "Current State"),
                                 tabPanel(title = "Major causes"),
                                 tabPanel(title = "Related Impacts"),
                                 tabPanel(title = "Intervations")
                                 
                               )
                        )
                      )),
             tabPanel(title = "Invasive Species",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tabPanel(title = "Current State"),
                                 tabPanel(title = "Major causes"),
                                 tabPanel(title = "Related Impacts"),
                                 tabPanel(title = "Intervations")
                                 
                               )
                        )
                      )),
             tabPanel(title = "Climate change impacts",
                      fluidRow(
                        column(width = 1),
                        column(width = 11,
                               tabsetPanel(
                                 tabPanel(title = "Current State"),
                                 tabPanel(title = "Major causes"),
                                 tabPanel(title = "Related Impacts"),
                                 tabPanel(title = "Intervations")
                                 
                               )
                        )
                      ))
  ),
## Task force ----- 
tabPanel(title = "Task Force",
         fluidRow(),
         fluidRow(
           column(width = 1),
           column(width = 6, 
                  tags$h4("The National Task Force"),
                  tags$p("The development of this First National Environmental Master Plan for Strategic Interventions and Projects is a result of commitment and collective efforts of key stakeholders who deserve a vote of appreciation. Special recognition is extended to the Task Force composed of experts from various institutes with wide range of skill who were selected to develop the MasterPlan"))
         ),
         tags$br(),
         fluidRow(
           column(width = 1),
           column(width = 2,imageOutput(outputId = "member1", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member2", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member3", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member4", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member5", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br())
         ),
         tags$br(),
         fluidRow(
           column(width = 1),
           column(width = 2, tags$h6("Dr.Hussein Omar"), tags$p(" is a Principal Environmental Officer at the VPO's. He wrote chapter that focus on waste management and pollution. You can Contact him through", HTML(paste0(tags$a(href = "mailto:hussein.omar@vpo.go.tz", "Hussein"), ".")))),
           column(width = 2, tags$h6("Dr.Thomas Bwana"), tags$p(" is a Principal Environmental Officer at the VPO's. He led the team and wrote land degradation and wetland chapters. You can Contact him through", HTML(paste0(tags$a(href = "mailto:thomas.bwana@vpo.go.tz", "Mmasa"), ".")))),
           column(width = 2, tags$h6("Dr.Deogratias Paul"), tags$p(" is a Principal Environmental Officer at the VPO's. Paul wrote chapters of deforestation and loss of habitats and biodiversity. Paul is contacted", HTML(paste0(tags$a(href = "mailto:dnyangu@gmail.com", "here"), ".")))),
           column(width = 2, tags$h6("Eng.Dr.Befrina Igulu"), tags$p(" is an Environmental Officer at the NEMC. She wrote environmental pollution chapter. You can contact her through", HTML(paste0(tags$a(href = "mailto:befrina.igulu@nemc.or.tz", "Befrina"), ".")))),
           column(width = 2, tags$h6("Dr.Kanizio Manyika"), tags$p(" is a Principal Environmental Officer at the VPO's. Dr Manyika led the team on issues related to climate change impacts. You can Contact him through", HTML(paste0(tags$a(href = "mailto:Freddy_Manyika@hotmail.com", "Manyika"), "."))))
         ),
         tags$br(),
         fluidRow(
           column(width = 1),
           column(width = 2,imageOutput(outputId = "member6", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member7", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member8", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member9", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member10", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br())
         ),
         tags$br(),
         fluidRow(
           column(width = 1),
           column(width = 2, tags$h6("Mr.Enock Sanga"), tags$p(" is an Environmental Urban Planner from the VPO's Office. His led writing of Dodoma City chapter of the MasterPlan.You can Contact her through", HTML(paste0(tags$a(href = "mailto:enocksanga52@gmail.com", "Sanga"), ".")))),
           column(width = 2, tags$h6("Mr.Kalokola Rwabizi"), tags$p("Is a senior economist at VPO. He led environmental governance and institutional setup and monitoring and evaluations chapters. You can Contact him through", HTML(paste0(tags$a(href = "mailto:jselestini@gmail.com", "Kalokola"), ".")))),
           column(width = 2, tags$h6("Ms.Kemilembe Mutasa"), tags$p(" is a Principal Environmental Officer at the VPO's. She led the task force and reviewed the MasterPlan.You can Contact her through", HTML(paste0(tags$a(href = "mailto:kemilembe.mutasa@vpo.go.tz", "Kemilembe"), ".")))),
           column(width = 2, tags$h6("Mr.Julius Enock"), tags$p(" is an Engineer and Principal Environmental Officer at the VPO's. Enock reviewed and typeset the MasterPlan. He also translated it into Swahili language.You can Contact him through", HTML(paste0(tags$a(href = "mailto:julius.moshi@vpo.go.tz", "Julius"), ".")))),
           column(width = 2, tags$h6("Ms.Emelda T.Adam"), tags$p(" is a Principal Environmental Officer at the VPO's. Emelda led coastal and marine chapter and assisted with several other chapters. She can be contacted her through", HTML(paste0(tags$a(href = "mailto:emeldateikwa@hotmail.com", "Emelda"), ".")))),
         ),
         tags$br(),
         fluidRow(
           column(width = 1),
           column(width = 2,imageOutput(outputId = "member11", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member12", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member13", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member14", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br()),
           column(width = 2,imageOutput(outputId = "member15", height = "100px") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"), tags$br())
         ),
         tags$br(),
         fluidRow(
           column(width = 1),
           column(width = 2, tags$h6("Mr.George J. Miringay"), tags$p(" is an Environmental Urban Planner from the TAMISEMI. He wrote Urban and Environment Chapter for the MasterPlanin. You can Contact him through", HTML(paste0(tags$a(href = "mailto:miringay.joseph@tamisemi.go.tz ", "Miringay"), ".")))),
           column(width = 2, tags$h6("Ms.Sophia Kivina"), tags$p(" is an ICT Officer at the VPO. Her roles in the task force dealt with design and develop this interactive Dashboard.You can Contact her through", HTML(paste0(tags$a(href = "mailto:skivina7@gmail.com", "Sophia"), ".")))),
           column(width = 2, tags$h6("Mr.Msifuni Mmasa"), tags$p(" is an accounting officer at VPO. His roles in the task force dealt with financial management and contributed to write several chapters. You can Contact her through", HTML(paste0(tags$a(href = "mailto:msifuni.mmasa@vpo.go.tz ", "Mmasa"), ".")))),
           column(width = 2, tags$h6("Mr.Masumbuko Semba"), tags$p(" is a data scientist from NM-AIST and engineered the analyticall framework of the dashboard. He can be contacted  through", HTML(paste0(tags$a(href = "mailto:lugosemba@gmail.com", "Semba"), ".")))),
           column(width = 2, tags$h6("Mr.Rawson Yonazi "), tags$p(" is a Principal Environmental Officer at the VPO's. His roles in the task force dealt with urban and cities enviroment with focus with Dodoma city."))
         ),
         # tags$br(),
         tags$br(), 
         fluidRow(
           column(width = 1),
           column(width = 6,tags$h4("About the Developer"))
           ),
         fluidRow(
           column(width = 1),
           column(width = 4,
                  tags$p(
                    "Semba is a data scientist from Nelson Mandela African Institution of Science and Techlogy, where I focus on",
                    "data analysis, statistical and machine learning and present the findings in visual form like plots, interactive reporting,",
                    ". I enjoy building data tools that are capable of solving problem facing the society,",
                    "like this dashboard. You can find more of the things I like to build on my webpage",
                    HTML(paste0(tags$a(href = "https://semba-blog.netlify.app/", "semba blog", target = "_blank"), "."))
                  )),
           column(width = 4,
                  tags$p(
                    "I also use data science to support research with",
                    tags$a(href = "https://hinger.netlify.app/", "hinger", target = "_blank"),
                    "I'm lucky to get to use R and RStudio tools on a daily basis to help others",
                    "learn from clients, I have succumb myself into a myriad of data that evolved my skills to handle both spatial and non-spatial data",
                    " and report them through dashboards and interactive reports like this one."
                  ),),
           column(width = 2,
                  tags$p(
                    "Get in touch with me on Github at",
                    HTML(paste0("(", tags$a(href = "https://github.com/lugoga", "@semba", target = "_blank"), "),")),
                    "online at",
                    HTML(paste0(tags$a(href = "https://hinger.netlify.app/", "", target = "_blank"), ",")),
                    "or by email at",
                    HTML(paste0(tags$a(href = "mailto:lugosemba@gmail.com", "Semba"), "."))
                  )),
           column(width = 1),
         ),
         tags$br(),
         tags$br(),
)
  # navbarMenu(title = "Urban & Cities",
  #            navbarMenu(title = "Dar es Salaam"
  #                       ,
  #                       navbarMenu(title = "Dodoma City",
  #                                  tabPanel(title = "Land degradation",
  #                                           fluidRow(
  #                                             column(width = 1),
  #                                             column(width = 11,
  #                                                    tabsetPanel(
  #                                                      tabPanel(title = "Current State"),
  #                                                      tabPanel(title = "Major causes"),
  #                                                      tabPanel(title = "Related Impacts"),
  #                                                      tabPanel(title = "Intervations")
  #                                                      
  #                                                    )
  #                                             )
  #                                           )
  #                                  ),
  #                                  tabPanel(title = "Deforestatation"),
  #                                  tabPanel(title = "Waste Management"),
  #                                  tabPanel(title = "Pollution"),
  #                                  tabPanel(title = "Deterioration water sources"),
  #                                  tabPanel(title = "Biodiversity"),
  #                                  tabPanel(title = "Invasive Species"),
  #                                  tabPanel(title = "Climate change impacts")
  #                       )
  #                       ),
  #            navbarMenu(title = "Dodoma"),
  #            navbarMenu(title = "Mwanza"),
  #            navbarMenu(title = "Arusha"),
  #            navbarMenu(title = "Tanga"),
  #            navbarMenu(title = "Mbeya") )
  
             
  )

# server ----
server = function(input, output, session){
  
# Land Degradation ----
  output$landDegraded_tmap = renderTmap({
    
    tm_shape(shp = land.degradation.three, name = "Degradation") +
      tm_fill(col = "severity", id = "region", palette = c("#f73a08", "#0d8632", "#fdfd66"), title = "Degradation")+
      tm_shape(shp = regions, name = "Regions")+
      tm_fill(id = "region", alpha = 0)+
      tm_borders(col = "black")
    
  })
  
  output$degradedBar = renderHighchart({
    
    degraded.region.area %>% 
      filter(severity == input$degradedSeverity) %>%
      hchart(type = "bar", hcaes(x = region, y = total)) %>% 
      hc_yAxis(title = list(text = "Area (accre)")) %>% 
      hc_xAxis(title = FALSE)
    
  })
  
  
  mkoa.degradation = reactive(
    degraded.region.sf %>% filter(region == input$region_idDegr)
  )
  
  
  output$degrdedRegion = renderTmap({
    
    tm_shape(shp = mkoa.degradation() %>% filter(severity == "High"), name = "High Degraded") +
      tm_fill(col = "#f73a08") +
      tm_shape(shp = mkoa.degradation() %>% filter(severity == "Moderate"), name = "Moderate Degraded") +
      tm_fill(col = "#fdfd66")+
      tm_shape(shp = mkoa.degradation() %>% filter(severity == "Low"), name = "Low Degraded") +
      tm_fill(col = "#0d8632")
    
    # degraded.region.sf %>% 
    #   filter(region == input$region_idDegr) %>% 
    #   tm_shape() +
    #   tm_fill(col = "severity", palette = c("#f73a08", "#0d8632", "#fdfd66"), title = "Degradation")
    
  })
  
  
  output$pieDegraded = renderHighchart({
    
    degraded.region.area  %>% 
      filter(region == input$region_idDegr) %>% 
      hchart(type = "column", 
             hcaes(x = severity, y = total, 
                   color = c(Low = "#0d8632", Moderate = "#fdfd66", High = "#f73a08")[severity])) %>% 
      # hc_colors(colors = c("#f73a08", "#0d8632", "#fdfd66")) %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 0,sort = TRUE,table = TRUE)%>% 
      hc_yAxis(title = list(text = "Area (acre)"))  %>% 
      hc_xAxis(title = FALSE)  
    
  })
  
  ## mining hot and cold clusters ----
  miningPallette = c("#ffffbf", "#FF0000" ,"#0000FF", "#464646", "#999999")
  
  hot.cold.mining.sf = reactive({
    hot.cold.mines %>% filter(commodity_type == input$commodityId)
  })
  
  output$miningMap = renderTmap({
    
    # tm_basemap(server = c("Open Street" = "OpenStreetMap",
    #                       "Topo Map" = "OpenTopoMap")) +
      tm_shape(shp = hot.cold.mining.sf())+
      tm_fill(col = "clusterSignify", palette = miningPallette, id = "lga", 
              popup.vars = c("Mines" = "n", "Cluster" = "clusterSignify"), 
              title = paste(input$commodityId,"Cluster"))+
      tm_borders(alpha = .4, lwd = .2)
    
  })
  
  ## livestock change ----
  
  livestock.change = reactive({
    livestock.region %>% 
      filter(percentage_change >= input$livestockId[1] & percentage_change <= input$livestockId[2])
  })
  
  pal = wesanderson::wes_palette("Zissou1", 4, type = "continuous")
  
  output$livestockMap = renderTmap({
    
    # tm_basemap(server = c("Open Street" = "OpenStreetMap",
    #                       "Topo Map" = "OpenTopoMap")) +
    tm_shape(shp = livestock.change())+
      tm_fill(col = "percentage_change", n = 4, palette = pal, 
              alpha = .6, style = "quantile", id = "region", 
              popup.vars = c("2008" ="y2008", "2020"="y2020", "%Change" = "percentage_change"), 
              # labels = c("Below 0", "0-90", "90-130", "Above 130"),
              title = "Percentage <br> Change (%)")+
      tm_borders(col = "ivory", lwd = .5)
    
  })
  
  ## expansion agriculture
  
  output$agriExpansion = renderHighchart({
    change.class %>% 
      filter( change >= 0 & pct_change >= input$pct_agri[1] & pct_change <= input$pct_agri[2]) %>%    
      arrange(desc(pct_change)) %>% 
      hchart(type = "bar", hcaes(x = region, y = pct_change)) %>% 
      hc_xAxis(title = FALSE) %>% 
      hc_yAxis(title = list(text = "Percentage Change (%)"))
  })
  
  # WATER ----
  ## Lakes level ----
  output$lakesLevelBar = renderHighchart({
    tibble(lakes = c("Victoria", "Tanganyika", "Rukwa", "Nyasa"), 
           level_m = c(1.46,2.04,4.8,0)) %>% 
      mutate(level_cm = level_m*100) %>% 
      arrange(desc(level_cm))%>% 
      hchart(type = "column", hcaes(x = lakes, y = level_m)) %>% 
      hc_xAxis(title = FALSE) %>% 
      hc_yAxis(title = list(text = "Rising Lake Level (meter)"))
    
  })
  
  # waterLevel = eventReactive(input$click,{
  #   lakes.anomaly.tb %>% 
  #     mutate(across(is.numeric, round, 2)) %>% 
  #     filter(date >= input$lakesDate[1] & date <= input$lakesDate[2])
  # })

  waterLevel = reactive({
    lakes.anomaly.tb %>% 
      mutate(across(is.numeric, round, 2)) %>% 
      filter(date >= input$lakesDate[1] & date <= input$lakesDate[2])
  })
  
  output$lakesTrends = renderHighchart({
    
    waterLevel() %>% 
      hchart(type = "spline",hcaes(x = date, y = anomaly, group = water_bodies)) %>% 
      hc_yAxis(title = list(text = "Changes in Lake Level (m)")) %>% 
      hc_xAxis(title = FALSE)%>%
      hc_legend(
        align = "top",
        verticalAlign = "top",
        layout = "horizontal"
        )
    
  })
  
  ## River flow ----
  
  river.my =  reactive({
    river.month %>% 
      filter(station == input$station_id & date >= input$dateRiver_id[1] & date <= input$dateRiver_id[2])
    
  })
  
  output$discharge = renderPlot({
    river.my() %>% 
      ggplot(aes(x = date, y = flow_liter_s))+
      geom_line()+
      geom_smooth(method = "lm", color = "red", fill = "red", alpha = .1)+
      scale_y_continuous(name = "Flow (L/s)", breaks = scales::pretty_breaks(n = 6), 
                         labels = scales::label_number(big.mark = ","))+
      labs(subtitle = paste(river.my()$river[1] ))+
      theme_bw(base_size = 14)+
      theme(axis.title.x = element_blank())
    
  })
  
  ## Coastal ----
  
  # DEFORESTATION ----
  
  myforest = eventReactive(input$clickForest,{
    forest.deforested.valid %>% filter(type == "Forest")
  })
  
  
  output$deforestTmap = renderTmap({
    
      
    tm_shape(shp = myforest(), name = "Forest")+
      tm_fill(col = "green")+
      tm_shape(shp = forest.deforested.valid %>% filter(type == "Deforested"), name = "Deforested")+
      tm_fill("red")+
      tm_legend(legend.position = c("left", "bottom"))
      
    
    
  })
  
  output$forestBar = renderHighchart({
    
    regions.deforested.foresta %>%
      filter(!type == input$typeForest) %>%
      mutate(area = st_area(geometry) %>% as.numeric(), area_ha = area / 10000) %>%
      st_drop_geometry()  %>%
      group_by(region) %>%
      summarise(area_ha = sum(area_ha)) %>%
      ungroup() %>%
      mutate(across(is.numeric, round, 0)) %>%
      arrange(desc(area_ha)) %>%
      hchart(type ="bar", hcaes(x = region, y = area_ha)) %>%
      hc_yAxis(title = list(text = "Area (ha)")) %>%
      hc_xAxis(title = FALSE)
    
    # if(input$typeForest == "Forest"){
    #   regions.deforested.foresta %>% 
    #     filter(!type == input$typeForest) %>%
    #     mutate(area = st_area(geometry) %>% as.numeric(), area_ha = area / 10000) %>% 
    #     st_drop_geometry()  %>% 
    #     group_by(region, type) %>% 
    #     summarise(area_ha = sum(area_ha)) %>% 
    #     ungroup() %>% 
    #     mutate(across(is.numeric, round, 0)) %>% 
    #     arrange(desc(area_ha)) %>% 
    #     hchart(type ="bar", hcaes(x = region, y = area_ha, color = c(Forest = "#00ff00")[type])) %>% 
    #     hc_yAxis(title = list(text = "Area (ha)")) %>% 
    #     hc_xAxis(title = FALSE)
    #   
    # }else{
    #   regions.deforested.foresta %>% 
    #     filter(!type == input$typeForest) %>%
    #     mutate(area = st_area(geometry) %>% as.numeric(), area_ha = area / 10000) %>% 
    #     st_drop_geometry()  %>% 
    #     group_by(region, type) %>% 
    #     summarise(area_ha = sum(area_ha)) %>% 
    #     ungroup() %>% 
    #     mutate(across(is.numeric, round, 0)) %>% 
    #     arrange(desc(area_ha)) %>% 
    #     hchart(type ="bar", hcaes(x = region, y = area_ha, color = c(Deforested = "#f73a08")[type])) %>% 
    #     hc_yAxis(title = list(text = "Area (ha)")) %>% 
    #     hc_xAxis(title = FALSE)
    # }
    
    
  })
  
  
  mkoa.forest.deforest = reactive({
    regions.deforested.foresta %>% 
      filter(region == input$region_idFo)
  })
  
  output$deforest_map = renderTmap({
    
    tm_shape(shp = mkoa.forest.deforest() %>% filter(type == "Forest"), name = "Forest")+
      tm_fill(col = "green")+
      tm_shape(shp = mkoa.forest.deforest() %>% filter(type == "Deforested"), name = "Deforested")+
      tm_fill("red")+
      tm_legend(legend.position = c("left", "bottom"))
    
  })
  
  output$area_deforest = renderHighchart({
    mkoa.forest.deforest() %>% 
      mutate(area = st_area(geometry) %>% as.numeric(), area_ha = area / 10000) %>% 
      st_drop_geometry()  %>% 
      group_by(type) %>% 
      summarise(total = sum(area_ha)) %>% 
      mutate(pct = total/sum(total) * 100) %>% 
      ungroup() %>% 
      # filter(severity == "High") %>% 
      arrange(desc(pct)) %>% 
      hchart(type = "column", hcaes(x = type, y = total, color = c(Forest = "#00ff00", Deforested = "#f73a08")[type])) %>% 
      hc_yAxis(title = list(text = "Area (ha)")) %>% 
      hc_xAxis(title = FALSE) %>% 
      # hc_colors(colors = c("red", "green"))%>% 
      hc_legend(enabled = FALSE)
    
  })
  
  # Wetland ----
  
  wet.selected = reactive({
    wetland.ha %>% 
      filter(name == input$wetland_id) 
  })
  
  
  output$wet_change = renderTmap({
    
    tm_shape(shp = wet.selected() %>% filter(year == 2002), name = "2002") +
      tm_fill(col = "blue", alpha = .6, id = "year", popup.vars = c("Wetland" = "name","Area ha" = "area_ha"))+
      tm_shape(shp = wet.selected() %>% filter(year == 2014), name = "2014")+
      tm_fill(col = "red", alpha = .6, id = "year", popup.vars = c("Wetland" = "name","Area ha" = "area_ha"))+
      tm_borders(col = "ivory", lwd = .3)
  })
  
  
  output$wetPct_change = renderHighchart({
    
    # wetland.change.rate %>% 
    #   hchart(hcaes(x = name, y = pct), type = "bar") %>% 
    #   hc_xAxis(title = FALSE) %>% 
    #   hc_yAxis(title = list(text = "wetland Change (%) "))
    
    wetland.change.rate %>% 
      filter(name == input$wetland_id) %>% 
      pivot_longer(cols = 2:3, values_to = "area_ha", names_to = "year") %>% 
      separate(col = year, into = c("aa", "year"), sep = 1) %>% 
      select(name, year, area_ha) %>% 
      mutate(across(is.numeric, round, 0)) %>% 
      hchart(hcaes(x = year, y = area_ha, color = c("2002" = "#6060f9", "2014" = "#bf2664")[year]), type = "column") %>% 
      hc_yAxis(title = list(text = "Area (Ha)")) %>% 
      hc_xAxis(title = FALSE)
  })
  
  # CLIMATE ----
  
          climate.sf = reactive({
            change.regions %>% 
            filter(name == input$climateVariable)
          })
  

  output$climateMap = renderTmap({
    
    climate.getis = climate.sf() %>% 
      select(value) %>% 
      mutate(value = na_if(x = value, y = 0)) %>% 
      rgeoda::queen_weights() %>% 
      local_gstar(df = climate.sf() %>% select(value))
    
    cluster = climate.getis %>% lisa_clusters()
    labels = climate.getis %>% lisa_labels()
    colors = climate.getis %>% lisa_colors()
    colors[1] = "#ffffbf"
    
    
    hot.cold.clusters = climate.sf() %>% 
      mutate(clusters = climate.getis %>% lisa_clusters() %>% as_factor()) 
    
    hot.cold.clusters %>%
      tm_shape() +
      tm_fill(col = "clusters", palette = colors, alpha = .4, labels = labels, 
              id = "region", title = paste("Change in ", input$climateVariable))+
      tm_borders(lwd = .2)
    
  })
  
  
  output$climateBar = renderHighchart({
    
    climate.sf() %>% 
      st_drop_geometry() %>% 
      mutate(state = if_else(value < 0, "negative", "positive")) %>% 
      arrange(desc(value)) %>% 
      hchart(type = "bar", hcaes(x = region, y = value, color = c(positive = "#6060f9", negative = "#bf2664")[state])) %>% 
      hc_yAxis(title = list(text = paste("Change in ", input$climateVariable))) %>% 
      hc_xAxis(title = FALSE)
    
  })
  
  
  # Biodiversity----
  verify_fa = FALSE
  
  speciesTerrial = reactive({
    hotcoldspot.biodiversity %>% 
      # filter(scenari == "Richness")# %>%
    filter(scenari == input$bioTerrestrial)
  })
  
  

  output$speciesTerrestrialMap = renderTmap({
    
    speciesTerrial() %>% 
      tm_shape()+
      tm_fill(col = "clstrSg", alpha = .4, id = "lga", palette = c("#ffffbf", "#FF0000", "#0000FF", "#464646", "#999999"), 
              popup.vars = c("Species" = "value", "Cluster" = "clstrSg"), title = "Hot & Cold Spots")+
      tm_borders(col = "grey60", lwd = .2)
    
  })
  
  
 
  
  output$speciesBar = renderHighchart({
    
    speciesTerrial() %>% 
      st_drop_geometry() %>% 
      # filter(clstrSg == "Significant High") %>% 
      filter(clstrSg == input$hotCold) %>% 
      arrange(desc(value)) %>% 
      hchart(type = "bar", hcaes(x = lga, y = value)) %>% 
      hc_xAxis(title = FALSE) %>% 
      hc_yAxis(title = list(text = input$bioTerrestrial))
  })
  
  
  spp.tz.total = reactive({
    gbif.tz %>% 
      distinct(species, .keep_all = TRUE) %>% 
      filter(iucnRedListCategory %in% c("CR", "DD", "EN", "EW", "EX", "LC", "NE", "NT", "VU")) %>% 
      group_by(iucnRedListCategory) %>% 
      count() %>% 
      ungroup() 
  })
  
  output$iucnCategory = renderPlotly({
    

    spp.tz.total() %>% 
      filter(!n>20000) %>% 
      # add_row(iucnRedListCategory = "Others", n = 9865) %>% 
      mutate(pct = n/sum(n)*100, across(is.numeric,round, 2)) %>% 
      plot_ly(type = "pie", labels = ~iucnRedListCategory, values = ~n, 
              hole = 0.6,
              textposition = 'inside', 
              # direction='clockwise', 
              # sort=False,
              # showlegend=False,
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste('Species', n,
                            # ' <br> Generated =', generation, "tons",
                            # " <br> collected =",collection, "tons" ,
                            ' <br> Percentage =', pct, "%"),
              marker = list(line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE) 
  })
  
## Indicator Biodiversity ----
  
  output$chl <- renderInfoBox({
    infoBox(title = HTML("Mammals<br>"),
            value = HTML("<p style='font-size:20px'>",
                         81043,"</p>"),
            color = "green",
            icon = icon("area-chart"),
            fill = TRUE
    )
  })
  
  output$birds = renderInfoBox({
    infoBox(title = HTML("Birds<br>"),
            value = HTML("<p style='font-size:20px'>",
                         908344,"</p>"),
            color = "maroon", 
            icon = icon("vote-yea"),
            fill = TRUE)
  })
  
  output$fish = renderInfoBox({
    infoBox(title = HTML("Bony fish<br>"),
            value = HTML("<p style='font-size:20px'>",
                         52333,"</p>"),
            color = "blue",
            icon = icon("envira"),
            fill = TRUE)
  })
  
  output$rain = renderInfoBox({
    infoBox(title = HTML("Amphibia<br>"),
            value = HTML("<p style='font-size:20px'>",
                         20987,"</p>"),
            color = "yellow",
            icon = icon("anchor"),
            fill = TRUE)
  })
  
  output$visibility = renderInfoBox({
    infoBox(title = HTML("Insects<br>"),
            value = HTML("<p style='font-size:20px'>",
                         131091,"</p>"),
            color = "aqua",
            icon = icon("bars"),
            fill = TRUE)
  })
  
  
  output$waves = renderInfoBox({
    infoBox(title = HTML("Reptile<br>"),
            value = HTML("<p style='font-size:30px'>",
                         22475,"</p>"),
            color = "navy",
            icon = icon("bell"),
            fill = TRUE)
  })
  
  output$current = renderInfoBox({
    infoBox(title = HTML("Mollusc<br>"),
            value = HTML("<p style='font-size:20px'>",
                         23805,"</p>"),
            color = "orange",
            icon = icon("comments-o"),
            fill = TRUE)
  })
  
  output$oxygen = renderInfoBox({
    infoBox(title = HTML("Arachnids<br>"),
            value = HTML("<p style='font-size:20px'>",
                         4490,"</p>"),
            color = "teal",
            icon = icon("fa-tachometer"),
            fill = TRUE)
  })
  
  output$soil = renderInfoBox({
    infoBox(title = HTML("Plants<br>"),
            value = HTML("<p style='font-size:20px'>",
                         341242,"</p>"),
            color = "olive",
            icon = icon("tree"),
            fill = TRUE)
  })
  
  
  output$air = renderInfoBox({
    infoBox(title = HTML("Ferns<br>"),
            value = HTML("<p style='font-size:20px'>",
                         10270,"</p>"),
            color = "lime",
            icon = icon("fa-cog"),
            fill = TRUE)
  })
  
  output$water = renderInfoBox({
    infoBox(title = HTML("Mosses<br>"),
            value = HTML("<p style='font-size:20px'>",
                         8165,"</p>"),
            color = "purple",
            icon = icon("globe"),
            fill = TRUE)
  })
  
  output$turbidity = renderInfoBox({
    infoBox(title = HTML("Basidiomycota<br>"),
            value = HTML("<p style='font-size:20px'>",
                         2011,"</p>"),
            color = "fuchsia",
            icon = icon("envira"),
            fill = TRUE)
  })
  
  
  ## Waste management ----
  
  
  output$percent_collection = renderTmap({
    
    
    lga.waste %>% 
      janitor::clean_names() %>% 
      mutate(percent_collection = percent_collection*100) %>% 
      filter(percent_collection >= input$pct_wasteID[1] & percent_collection <= input$pct_wasteID[2]) %>% 
      tm_shape() +
      tm_fill(id = "lga", col = "percent_collection", style = "quantile", palette = mycolor4, title = "Collection (%)", drop.levels = TRUE, colorNA = "black") +
      tm_borders(col = "beige", lwd = .2)
    
  })
  
  
  output$disposal = renderTmap({
    
    lga.waste %>% 
      janitor::clean_names() %>% 
      tm_shape() +
      tm_fill(id = "lga", col = "disposal",palette = waste.colors, title = "Mode of Disposal", drop.levels = TRUE, colorNA = "black") +
      tm_borders(col = "beige", lwd = .2)
    
  })
  
  output$waste_pie = renderPlotly({
    lga.waste %>% 
      st_drop_geometry() %>% 
      filter(!is.na(disposal)) %>%
      group_by(disposal) %>% 
      summarise(count = n(), generation = sum(generation, na.rm = TRUE), 
                collection = sum(collection, na.rm = TRUE)) %>% 
      mutate(pct = collection/generation*100) %>% 
      ungroup() %>% 
      mutate(across(is.numeric, round, 0)) %>% 
      plot_ly(type = "pie", labels = ~disposal, values = ~count, 
              hole = 0.6,
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste('Mean Waste at', disposal,
                            ' <br> Generated =', generation, "tons",
                            " <br> collected =",collection, "tons" ,
                            ' <br> Percentage =', pct, "%"),
              marker = list(colors = waste.colors,
                            line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE)
    
  })
  
  
  output$sewagebar = renderHighchart({
    sewarage %>% 
      arrange(desc(percentage)) %>% 
      mutate(state = if_else(percentage < 20, "Poor", "Moderate")) %>% 
      hchart(type = "bar", hcaes(x = town, y = percentage, 
                                 color = c(Poor = "#ff0000", Moderate = "#b719d8")[state])) %>% 
      hc_yAxis(title = list(text = "Percentage")) %>% 
      hc_xAxis(title = FALSE)
    
  })
  
  
  output$sewagebarPop = renderHighchart({
    
    sewarage.pop %>% 
      arrange(desc(percentage)) %>% 
      hchart(type = "bar", hcaes(x = town, y = percentage, group = year))%>% 
      hc_yAxis(title = list(text = "Percentage")) %>% 
      hc_xAxis(title = FALSE) %>% 
      hc_colors(colors = c("#bf2663", "#6060f9"))
    
  })
  
  
  # PHOTOS ----
  
  ### land degradation -----
  output$landDegradation = renderImage({
    list(src = "images/landDegradation/landDegradation.png",
         contentType = 'image/png', height = 180,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  
  ### biodiversity -----
  output$goal15 = renderImage({
    list(src = "images/bio/goal15.png",
         contentType = 'image/png', height = 180,
         alt = "SDG goal 15")
  }, deleteFile = FALSE)
  
  
  ### biodiversity -----
  output$deforesti1 = renderImage({
    list(src = "images/deforestation/deforesti1.png",
         contentType = 'image/png', height = 200,
         alt = "Deforestation")
  }, deleteFile = FALSE)
  
  ### task force -------
  output$member1 = renderImage({
    list(src = "images/taskforce/hussein.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member2 = renderImage({
    list(src = "images/taskforce/bwana.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member3 = renderImage({
    list(src = "images/taskforce/paul.png",
         contentType = 'image/png', height = 140,
         alt = "Paul Deogratias")
  }, deleteFile = FALSE)
  
  output$member4 = renderImage({
    list(src = "images/taskforce/befrina.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member5 = renderImage({
    list(src = "images/taskforce/manyika.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member6 = renderImage({
    list(src = "images/taskforce/sanga.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member7 = renderImage({
    list(src = "images/taskforce/kalokola.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  
  output$member8 = renderImage({
    list(src = "images/taskforce/kemi.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  
  output$member9 = renderImage({
    list(src = "images/taskforce/juliusEnock.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member10 = renderImage({
    list(src = "images/taskforce/imelda.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member11 = renderImage({
    list(src = "images/taskforce/george.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member12 = renderImage({
    list(src = "images/taskforce/sofia.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member13 = renderImage({
    list(src = "images/taskforce/mmasa.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  output$member14 = renderImage({
    list(src = "images/taskforce/semba.png",
         contentType = 'image/png', height = 140,
         alt = "Degraded Land in Simiyu Region")
  }, deleteFile = FALSE)
  
  
  
  
  ## end of server session ----
  
  
  
}


shinyApp(ui = ui, server = server)