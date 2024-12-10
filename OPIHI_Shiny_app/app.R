## This is a Shiny dashboard to visualize data collected by the OPIHI Program (https://coe.hawaii.edu/opihi/)
## Created following this tutorial: https://rstudio.github.io/shinydashboard/get_started.html

## Load Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(ggmap) # to make map of sites
library(ggspatial) # to add scale bars and compass arrows
library(ggrepel) # adding site labels to map


## Define UI for application/dashboard
ui <- fluidPage(

     dashboardPage( # entire dashboard page
        skin = "black", # skin/color of the entire dashboard
      
        dashboardHeader(  # dashboard header title
          title = "OPIHI Data Visualization", # title
          titleWidth = 250), # width of the header title section
      
        dashboardSidebar( # side bar menu
          width = 250, # width to match title
          sidebarMenu( # menu
            menuItem("Home", tabName = "home", icon = icon("house")), # Home tab
            menuItem("Sites", tabName = "sites", icon = icon("map")), # Sites tab (with map)
            menuItem("Data Visualization", icon = icon("chart-simple"), startExpanded = TRUE, # Data viz, branches out
                      menuSubItem("Percent Cover by Species", tabName = "spp_pcover"), # tab for percent cover by species plot
                      menuSubItem("Percent Cover by Category", tabName = "cat_pcover"), # tab for percent cover by category plot
                      menuSubItem("Relative Abundance", tabName = "rel_ab") # tab for relative abundance plot
            )
          )
        ),
      
        dashboardBody( 
          tabItems( # content of each tab
          
          # Home tab content
              # Description of OPIHI Program + Photo
            tabItem(tabName = "home", # tab name, matches name given above in sidebar menu
                    h2("Our Project In Hawaiʻi's Intertidal"), # title
                    img(src="OPIHI_image.jpg", height="100%", width="100%", align = "center"), # image loaded in from my Shiny App folder
                    h6("Image Source: OPIHI Website"), # smaller text - image source
                    br(), # line break
                    tags$div( # paragraphs with information about OPIHI and this app
                      tags$p("Our Project in Hawaʻi's Intertidal (OPIHI) is a participatory science program where teachers and students survey the rocky intertidal communities around Hawai‘i."),
                      tags$p("Here, we will explore data collected between 2004 and 2020 in Oʻahu, Hawaiʻi.")
                      ),
                    tags$a(href="https://coe.hawaii.edu/opihi/", "Click here for more information about OPIHI.") # adding in link to the OPIHI website
            ),
          
          # Sites tab content
              # Site map
            tabItem(tabName = "sites", # tab name, matches name in sidebar menu
                    h2("Oʻahu Sites"), # title
                    h4("Sites around Oʻahu that were surveyed between 2004 and 2020."), # subtitle
                    plotOutput("site_map", height = 600, width = 800) # map plot
            ),
          
          
          # Percent cover by Species content
              # Pick a species and plot for all sites over time
            tabItem(tabName = "spp_pcover", # tab name
                    box( # each box of content on the page
                        width = 4, # specify width of box (total page width = 12)
                        selectInput("select_spp1", # input ID
                                label = h3("Select a species"), # title of the dropdown box
                                # All species options, matches dataset
                                choices = list("Acanthophora spicifera", "Actinopyga varians", "Ahnfeltiopsis concinna", "Ahnfeltiopsis flabelliformis", "Aiptasia pulchella", "Akalaphycus setchelliae", "Amansia glomerata", "Amphipod spp", "Anthopleura nigrescens" , "Articulated corallines", "Asparagopsis taxiformis" ,"Asteronema breviarticulatum",  "Avrainvillea amadelpha",  "Barnacles spp" , "Bleached coral"  , "Boodlea composita" , "Bornetella sphaerica", "Botryocladia skottsbergii" , "Brachidontes crebristriatus", "Brittle star" , "Brown crust", "Bryopsis spp" , "Calcinus elegans" , "Calcinus laevimanus" ,"Calcinus seurati", "Caulerpa racemosa" , "Caulerpa sertularioides" , "Caulerpa spp" , "Cellana exarata" , "Cellana sandwicensis" , "Centroceras clavulatum", "Chaetomorpha antennina" , "Champia parvula","Chnoospora minima" , "Chondria spp" , "Cladophora or cladophoropsis spp" , "Codium arabicum"  , "Codium edule"  ,  "Codium spp"  , "Coelothrix irregularis" , "Colobocentrotus atratus" , "Colpomenia sinuosa" , "Conus ebraeus" , "Conus spp"  , "Coral spp"  ,  "Crab spp" , "Crustose coralline algae"  , "Cyanobacteria spp"  , "Dasya spp"  , "Dendropoma gregaria"  , "Dichotomaria marginata" , "Dictyopteris spp" , "Dictyosphaeria cavernosa" , "Dictyosphaeria spp" ,"Dictyosphaeria versluysii"  , "Dictyota acutiloba" , "Dictyota sandvicensis"  ,  "Dictyota spp" , "Drupa morum"  ,  "Echinolittorina hawaiiensis"  ,  "Echinometra mathaei"  ,  "Echinometra oblonga" , "Echinometra sp"  ,   "Echinothrix spp"  , "Galaxaura spp" ,  "Galaxaura or liagora sp" , "Gelid spp"  , "Gelidiella acerosa" , "Gelidiopsis intricata" ,  "Gelidiopsis spp" ,  "Gelidium pusillum" , "Gracilaria salicornia"  , "Grateloupia spp" , "Griffithsia spp" , "Halimeda discoidea" ,  "Haminoea cymbalum"   , "Hermit crab"  ,  "Holothuria atra"  ,  "Holothuria cinerascens"  ,  "Holothuria difficilis" ,  "Holothuria hilla"  ,   "Holothuria spp" , "Hydroclathrus clathratus"  , "Hypnea cervicornis"  ,  "Hypnea chordacea" ,  "Hypnea musciformis" ,  "Hypnea spp" ,  "Isognomon californicum"   ,  "Isognomon perna"  ,  "Laurencia dendroidea" ,  "Laurencia dotyi" ,   "Laurencia mcdermidiae"  ,  "Laurencia spp"   ,  "Leptolyngbya crosbyana"  ,  "Liagora spp"  ,  "Littoraria pintado"  ,  "Lobophora variegata"  ,  "Lyngbya majuscula" , "Martensia spp"  ,   "Mauritia mauritiana"  , "Microdictyon setchellianum" ,  "Monetaria caputserpentis"  ,  "Morula granulata" ,  "Morula sp"  , "Morula uva"  ,  "Neomeris spp"  , "Nerita picea"  , "Nesochthamalus intertextus" ,  "Ophiocoma brevipes"  , "Ophiocoma erinaceus"  , "Padina spp" ,  "Palisada parvipapillata"  ,  "Palythoa caesia" , "Parvocaulis parvulus"  , "Peasiella tantilla" , "Pennaria disticha"  , "Portieria hornemannii" ,  "Protopalythoa spp"  , "Pterocladiella caerulescens" ,  "Pterocladiella capillacea" , "Pterocladiella spp" ,  "Rhodymenia spp"  ,  "Sargassum aquifolium" , "Sargassum polyphyllum"  ,  "Sargassum spp" ,  "Siphonaria normalis"  ,  "Snail spp"  , "Sphacelaria spp"  , "Sponge spp" , "Spyridia filamentosa" , "Stomatopod spp" , "Stylocheilus striatus" , "Symploca hydnoides"  , "Thylacodes variabilis" ,  "Trichogloea spp" ,  "Tripneustes gratilla"  ,  "Tube snail"  ,  "Turbinaria ornata"  , "Turf spp" ,   "Ulva fasciata" ,  "Ulva flexuosa" , "Ulva spp"  ,   "Wrangelia elegantissima"  , "Zoanthid spp"  ,  "Unknown brown" ,  "Unknown cyanobacteria"  ,   "Unknown fuzzy"  ,  "Unknown green" ,  "Unknown polychaete" ,  "Unknown red"  , "Unknown species" , "Substrate" ) # things to choose from (matches data column names)
                                   )
                        ),
                    box( # new box
                      width = 8,
                      plotOutput("spp_pcover_plot", height = 600) # species percent cover plot
                      )
                    ), 
          
          
          # Percent cover by Category content
              # Pick a category and plot for all sites over time
            tabItem(tabName = "cat_pcover", # tab name
                    box( 
                      width = 4, # box width
                      selectInput("select_cat1", # input for picking a category (matching data)
                                  label = h3("Select a category"),
                                  choices = list("Red Algae", "Green Algae", "Brown Algae", "Cyanobacteria", "Coral","Crustose Coralline Algae", "Turf Algae", "Mobile Invertebrates","Sessile Invertebrates", "Unknown", "Substrate")
                                 )            
                      ),
                     box( 
                       width = 8, # box width
                       plotOutput("cat_pcover_all_plot", height = 600) # category percent cover plot
                     )
            ),
                    
          
          # Relative abundance content
              # Pick a site and plot relative abundances over time
            tabItem(tabName = "rel_ab", # tab name
                    box(
                      width = 4, 
                      selectInput("select_site", # input for picking a site (matching data)
                                  label = h3("Select a site"),
                                  choices = list("Kahana Bay","Diamond Head","Sand Island","Barbers Point","Turtle Bay","Ewa Beach","Sandy Beach","Maili Beach","Baby Makapuu")
                      )            
                    ),
                    box(
                      width = 8,
                      plotOutput("rel_ab_plot", height = 600) # relative abundance of each category plot
                    )
            )
          
          )        
        )
      )
        
     
)


## Define server 
server <- function(input, output) {
  

# -------------------------------------------
  ## Site Map
  output$site_map <- renderPlot({ # making map plot
    
    # read in data here, I don't need it to be reactive
    # keeping one row for each distinct location, and keeping other columns because we need to keep lat and long
    opihi_coords <- read_csv(here("Data", "opihi_data_clean.csv")) %>% distinct(location, .keep_all = TRUE)
    
    # list of all site names as they are originally written
    old_names <- c("Kahana Bay", "Diamond Head", "Sand Island", "Barbers Point", "Turtle Bay", "Ewa Beach", "Sandy Beach", "Maili Beach", "Baby Makapuu") 
    
    # list of site names as I want them written
    new_names <- c("Kahana Bay", "Diamond Head", "Sand Island", "Barber's Point", "Turtle Bay", "ʻEwa Beach", "Sandy Beach", "Māʻili Beach", "Baby Makapuʻu") 
    
    # replace names from the old_names vector with names from the new_names vector
    opihi_coords$location <- replace(opihi_coords$location, opihi_coords$location %in% old_names, new_names)
    
    # get a satellite map centered at the specified coordinates (roughly center of Oahu)
    opihi_map <- get_map(c(lon = -157.976, lat = 21.467), zoom = 10, maptype = "satellite") 
    
    
    # make map plot
    ggmap(opihi_map) +
      geom_point(data = opihi_coords, # using the data opihi_coords
                 aes(x = longitude, # plot each site using their lat and long
                     y = latitude),
                 color = "gold",
                 size = 4) + 
      
      # site names labels
      geom_label_repel(data = opihi_coords, # using repel so they don't overlap
                       aes(x = longitude,
                           y = latitude,
                           label = location),
                       size = 6) +
      
      theme_minimal() + # theme
      theme(axis.title = element_text(size = 16), # set axis title and text size
            axis.text = element_text(size = 14)) +
      
      labs(x = "Longitude", # set labels
           y = "Latitude") +
    
      
      # add scale bar
      annotation_scale(bar_cols = c("black", "white"), # add scale bar, make colors black and white
                       location = "tr",  # put it in the top right
                       text_col = "white") + # make text color white so it can be seen
      
     # add North arrow
     annotation_north_arrow(location = "tl",# add a north arrow in top left
                             height = unit(2, "cm"), # set size
                             width = unit(2, "cm"),
                             style = north_arrow_fancy_orienteering(text_col = 'white', # set type of arrow and colors for all its parts
                                                                    line_col = 'white',
                                                                    fill = 'white')) +
     coord_sf(crs = 4326) # necessary crs for scale bar to work
    
  })
  
  
# -------------------------------------------
  ## Species Percent Cover Plot
  species_data <- reactive({ # make reactive df for species data
    
    # read in data
    species_data <- read_csv(here("Data", "opihi_data_clean.csv")) %>%
      
    # create new column with percent cover of each species (points counted for each spp / total points counted that day * 100)
    mutate(spp_percent_cover = ((num_points/total_number_of_tallies)*100))
  
    # fix species names (replace _ with space)
    species_data$id <- species_data$id %>% str_replace_all(c("_" = " ")) %>% 
      str_to_sentence() # make the first letter uppercase (most are Genus species, some aren't but oh well)
    
    species_data <- species_data %>%
      filter(id %in% input$select_spp1) %>% # keep only the species selected by user input
      group_by(date, location) %>% # group by date and location (sometimes multiple rows for each combination)
      summarise(mean_pcover = mean(spp_percent_cover, na.rm = TRUE)) # take a mean percent cover value for each day at each location
    
    
  })
  
  output$spp_pcover_plot <- renderPlot({ # make plot
    
    ggplot(species_data(), # use reactive df
           aes(x = date,
               y = mean_pcover)) +
      facet_wrap(~location) + # facet by location
      geom_point() + # points
      geom_line() + # lines
      theme_minimal() + # theme
      labs(x = "Date", # axis titles
           y = "Percent Cover",
           title = paste(input$select_spp1, "Percent Cover over Time in Oʻahu")) + # plot title using the species chosen
      theme(title = element_text(size = 16, face = "bold"), # make title bigger and bold
            axis.title = element_text(size = 14), # make axis titles and text bigger
            axis.text = element_text(size = 12),
            strip.text.x = element_text(size = 14, face = "bold")) # make facet labels bigger and bold
  })
  
# -------------------------------------------  
  ## Categories Percent Cover Plot

  cat_pcover_data <- reactive({
    # read in data
    cat_data <- read_csv(here("Data", "opihi_data_clean.csv")) %>%
      # now group to have one row per category
      group_by(date, location, category, total_number_of_tallies) %>% 
      # sum all the points recorded for each category
      summarise(cat_sum = sum(num_points, na.rm = TRUE)) %>%
      # convert to percent cover - num points for each category/total num points each day * 100
      mutate(cat_percent_cover = ((cat_sum/total_number_of_tallies)*100)) %>%
      
      # filter by category chosen by user input
      filter(category %in% input$select_cat1)
    
  })
  
  output$cat_pcover_all_plot <- renderPlot({ # make plot

      ggplot(data = cat_pcover_data(), # use reactive df
             aes(x = date,
                 y = cat_percent_cover)) +
      facet_wrap(~location) + # facet by location
      geom_point() + 
      geom_line() +
      theme_minimal() +
      labs(x = "Date",
           y = "Percent Cover",
           title = paste(input$select_cat1, "Percent Cover over Time in Oʻahu")) + # plot title using the category chosen
      theme(title = element_text(size = 16, face = "bold"), # make title bigger and bold
            axis.title = element_text(size = 14), # make axis titles and text bigger
            axis.text = element_text(size = 12),
            strip.text.x = element_text(size = 14, face = "bold")) # make facet labels bigger and bold
  })
  
# -------------------------------------------    
    ## Relative Abundance Plot
  rel_ab_data <- reactive({
    # read in data
    rel_ab_data <- read_csv(here("Data", "opihi_data_clean.csv")) %>%
      
      filter(num_points > 0) %>%  # only keep cases when points are positive (there's a few cases were the number of points recorded exceeds the number of points there should be based on the number of quadrats, so when calculate substrate points we get a negative value)
      
      filter(location %in% input$select_site)  # pick which location to plot
      
  })
  
  output$rel_ab_plot <- renderPlot({ # make plot
    
    ggplot(data = rel_ab_data(), # use reactive df
           aes(x= as.factor(date),
               y= num_points, 
               # manually set the order of the categories
               fill= factor(category,levels=c("Red Algae", "Crustose Coralline Algae", "Brown Algae", "Green Algae", "Turf Algae", "Cyanobacteria",  "Coral", "Sessile Invertebrates", "Mobile Invertebrates", "Substrate", "Unknown" )), 
               color= category)) + # set lines surrounding each color to match the fill colors
      geom_bar(stat="identity", position="fill") + # stacked bars
      
      theme_minimal() +  # theme
      theme(title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 16), # make all text bigger
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            axis.text.x = element_text(angle= 45, vjust = 1.1, hjust = 1)) + # angle + placement of the date text
      
      labs(x = "Date", # labels
           y="Relative Abundance",
           fill = "Category",
           title = paste("Relative Abundance of Each Category in", input$select_site, "over Time")) +
      
      guides(color = "none") + # keep only legend for fill since fill and color are the same
      
      scale_fill_manual(values = c("Red Algae" = "indianred2", # specify fill colors for each category
                                   "Green Algae" = "olivedrab",
                                   "Brown Algae" = "sienna",
                                   "Cyanobacteria" = "cadetblue4",
                                   "Crustose Coralline Algae" = "lightpink2", 
                                   "Turf Algae" = "darkseagreen3",
                                   "Coral" = "slateblue",
                                   "Mobile Invertebrates" = "burlywood",
                                   "Sessile Invertebrates" = "mediumpurple2",
                                   "Substrate" = "mistyrose4",
                                   "Unknown" = "grey20")) +
      
      scale_color_manual(values = c("Red Algae" = "indianred2", # specify line colors for each category (same as fill)
                                    "Green Algae" = "olivedrab",
                                    "Brown Algae" = "sienna",
                                    "Cyanobacteria" = "cadetblue4",
                                    "Crustose Coralline Algae" = "lightpink2", 
                                    "Turf Algae" = "darkseagreen3",
                                    "Coral" = "slateblue",
                                    "Mobile Invertebrates" = "burlywood",
                                    "Sessile Invertebrates" = "mediumpurple2",
                                    "Substrate" = "mistyrose4",
                                    "Unknown" = "grey20"))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
