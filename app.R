# Shiny Code changes

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(reshape2)
library(bbplot)
library(png)
library(DT)
library(dplyr)
library(bbplot)
library(shinyLP)
library(shinyBS)
library(RColorBrewer)
library(bbplot)
library(shinyBS)
library(shinyWidgets)


# load in data
full_data <- read.csv("full_sample_metadata_4_11_19_ER.csv", header=TRUE)

# gather the data
full_data_gathered <- full_data %>% gather(Env_variable, Value, Mean_Annual_Temperature_Celsius:dd_30)
full_data_gathered$Ecotype <- trimws(full_data_gathered$Ecotype , which="right")

#sort by longitude
# Add in new nomenclature for the populations
full_data_gathered_ordered <- full_data_gathered[order(full_data_gathered$Lat),]
rownames(full_data_gathered_ordered) <- 1:nrow(full_data_gathered_ordered)
full_data_gathered_ordered$Plot_name <- factor(full_data_gathered_ordered$Plot_name, levels = unique(full_data_gathered_ordered$Plot_name))

regions<- c("All", levels(full_data_gathered_ordered$Region))
ecotype<- c("All",unique(full_data_gathered_ordered$Ecotype))
wild_sel <- c("All",levels(full_data_gathered_ordered$Wild.Sel))

# Define UI

ui <-
  fluidPage(theme=shinytheme("flatly"),
            titlePanel(windowTitle = "",
                       title =
                         div(
                           img(
                             src = "EOGC_waves.png",style = "margin:0px 0px"
                           ), ""
                         )
           ),
navbarPage(theme=shinytheme("flatly"),
                       title = "",
tabPanel("About", icon=icon("home"),
         jumbotron("EOGC","Read the Panels Below to learn about our history, goals, and usage of this app",
                   buttonLabel="Go to the Genome"),
                            fluidRow(
                                  #panel_div
                                  column(6, panel_div(class_type="primary",panel_title= "Original Genome Sequence",
                                                      content="The genome of the easter oyster (GenBank assembly ACC: GCA_002022765.4) was sequenced in September 2017 from a single gynogenetic oyster, RU13XGHG1-28 generated from a highly inbred line
                                    of easter oysters from the Haskin Shellfish Laboratory, Rutgers University. Sequences were generated on the Pacific Biosciences RSII instrument (P6/C4 chemistry) to approx.
                                                      74x genome coverage based on a genome size estimate of 675 Mb. All reads were assembled with the FALCON algorithm then error corrected using the Quiver and Pilon algorithms.
                                                      Scaffolds were generated with the SSPACE-LongRead software")),
                                  column(6, panel_div("success",panel_title="Genome Resequencing Project",
                                         content="In order to better understand the evolution of eastern oysters along the east coast, and to supplement data from the original sequencing project,
                                    DNA from 91 oysters from 16 populations along the east coast was collected from researchers in the Eastern Oyster Genome Consortium and extracted by
                                         Dina Proestou (USDA ARS Shellfish Genetics Laboratory) and Erin Roberts (PhD Candidate URI, Marta Gomez-Chiarri Lab) and was resequenced by Genome Quebec at 20X coverage in 2018. This app allows for
                                         the exploration of environmental data and selection variables that have potentially shaped the evolution of the eastern oyster.")),
                                  column(6, panel_div("info",panel_title="Usage of This App",
                                                      content="This app includes several tools and drop down boxes that allow users to interact with data in real time. The app was created by Erin Roberts for the purpose of exploratory analysis and will be disseminated between members of Oyster Genome Consortium
                                    prior to publication of any papers. Raw data is not included in the app, rather only data that Erin was provided from members of the consortium that has been previously processed using R scripts that she made.
                                    It is Erin's goal to develop this tool as a companion to papers published by the consortium so that it could be used as a visual aid to readers")),
                                  column(6, panel_div("primary", panel_title= "Funding and Contact",
HTML("Funding for the Marta Gomez-Chiarri Lab, URI, is generously provided by USDA NIFA, NSF, the URI Coastal Institute, the Shellfish Restoration Foundation, and Matunuck Oyster Bar. Application Maintainer: <a href='mailto:erin_roberts@uri.edu?Subject=EOGC App' target='_top'>Erin Roberts</a>"))),
                                  column(12, panel_div("danger",panel_title="Members of the Eastern Oyster Genome Consortium", content=DT::dataTableOutput('EOGC_members_table')))
                                                       )),
                                  
                                  #### FAVICON TAGS SECTION ####
                                  tags$head(tags$link(rel="shortcut icon", href="./www/global_stats.png")),

                                bsModal("Eastern Oyster Genome", "Website", "tabBut", size = "large" ,
                                p("Here are the global statistics for the C. virginica genome. Please go to the NCBI assembly link for detailed information and downloading"),
                                tags$img(src="global_stats.png"),
                                uiOutput("tab")),

tabPanel("Project Populations and Environmental Data", icon=icon("cog"),
                                         sidebarLayout(
                                           sidebarPanel(
                                             h3("Data Exploration"),
                                             p("Select which Environmental Variables you wish to plot using the check-boxes below"),
                                             selectInput("Region","Select Region", choices=regions, selected=regions[1]),
                                          
                                             selectInput("Ecotype","Select Ecotype", choices = ecotype, selected=ecotype[1]),
                                            
                                             selectInput("Wild.Sel","Select Wild or Selected", choices=wild_sel,selected=wild_sel[1]),
                     
                                             checkboxGroupInput("temp_choose", "Select Temperature Data",
                                                                choices=c("Mean_Annual_Temperature_Celsius","Max_temperature_Celsius",
                                                                          "Min_temperature_Celsius")),
                                             bsTooltip("temp_choose", "Select temperature data type to view below", placement = "bottom", trigger = "hover", options = NULL),
                                             checkboxGroupInput("sal_choose", "Select Salinity Data",
                                                                choices="Mean_Annual_Salinity_ppt"),
                                             bsTooltip("sal_choose", "Select salinity data to view in plot", placement = "bottom", trigger = "hover", options = NULL),
                                             checkboxGroupInput("dd_choose","Select Annual Degree Days Below Temp.",
                                                                choices=c("dd_0","dd_15","dd_30")),
                                             bsTooltip("dd_choose", "Select the number of days below 0, 15, or 30 degrees", placement = "bottom", trigger = "hover", options = NULL)
                                             ),
                                           mainPanel(
                                             h2("Project Populations and Environmental Data"),
                                             p("Zoom in on the interactive map to see the populations sampled for resequencing. Click on the oyster icon to view information about the sample."),

                                             column(12, panel_div("primary", panel_title="Map", leafletOutput("Oyster_req_popmap"))),
                                             column(12, panel_div("primary", panel_title= "Temperature", plotOutput("paramchkbxgrp"))),
                                             column(12, panel_div("primary", panel_title="Salinity", plotOutput("sal_checkbox"))),
                                             column(12, panel_div("primary", panel_title="Degree Days", plotOutput("dd_checkbox")))
                                           )
                                         )
                                )
))
            
            # Define server logic required to draw map
server <- function(input, output) {
              
              #load in the data
              lat_long <- read.csv("population_coordinates.csv",header=TRUE)
              oyster_icon <- makeIcon(
                iconUrl = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRwuNbqZFRV9IwLtPC3SxSWURKUBofLmT3T9LoWavHDelsQ2DiWMg",
                iconWidth = 12, iconHeight = 20)
              temp <- read.csv("combined_spreadsheet_subset_maxtemp_sorted_long_temp.csv", header=TRUE)
              
              #Make table with different EOGC members
              dat <- data.frame(
                Member = c('Marta Gomez-Chiarri, URI FAVS Dept. Chair',
                           'Erin Roberts, URI Gomez-Chiarri PhD Candidate',
                           'Rebecca Stevick, URI Gomez-Chiarri PhD Candidate',
                           'Kevin Johnson, LSU Kelly Lab PostDoc',
                           'Rachel Schwartz, URI Bio Dept. Faculty',
                           'Dina Proestou, USDA ARS Shellfish Genetics Laboratory',
                           'Bassem Allam, Stony Brook University Faculty',
                           'Tejashree Modak, URI Schwartz Lab PostDoc',
                           'Jose Antonio Robledo, Bigelow Marine Laboratory',
                           'Katie Lotterhos, Northeastern University Faculty',
                           'Jon Puritz, URI Bio Dept. Faculty',
                           'Ximing Guo, Rutgers University Faculty',
                           'Matthew Hare, Cornell University Faculty',
                           'Jose Eiren-Lopez, FIU',
                           'Wes Warren McDonell Genome Institute',
                           'Hollie Putnam, URI Bio Dept. Faculty',
                           'Steven Roberts, UW Faculty'),
                Picture = c('<img src="MartaGomezChiarri_small1.jpg" height="80"></img>',
                            '<img src="Erin_Roberts.jpg" height="80"></img>',
                            '<img src="stevick.jpg" height="80"></img>',
                            '<img src="Kevin.jpg" height="80"></img>',
                            '<img src="schwartz.jpg" height="80"></img>',
                            '<img src="proestou.jpg" height="80"></img>',
                            '<img src="bassem.jpg" height="80"></img>',
                            '<img src="tejashree.jpg" height="80"></img>',
                            '<img src="robledo.png" height="80"></img>',
                            '<img src="lotterhos285.jpg" height="80"></img>',
                            '<img src="Puritz.jpg" height="80"></img>',
                            '<img src="guo.jpg" height="80"></img>',
                            '<img src="hare.jpg" height="80"></img>',
                            '<img src="lopez.jpg" height="80"></img>',
                            '<img src="wes warren.jpg" height="80"></img>',
                            '<img src="Hollie_Putnam.jpg" height="80"></img>',
                            '<img src="sroberts.jpg" height="80"></img>'
                )
              )
              
output$EOGC_members_table <- DT::renderDataTable({
                
DT::datatable(dat, escape = FALSE)
              })

url <- a("C. virginica Genome", href="  https://www.ncbi.nlm.nih.gov/assembly/GCF_002022765.2/")
output$tab <- renderUI({
  tagList("URL link:", url)
})
          
output$Oyster_req_popmap <- renderLeaflet({
                leaflet() %>%
                  addTiles() %>%
                  addMarkers(data=lat_long, ~Long, ~Lat, icon=oyster_icon,
                             label=~as.character(Pop.ID.),
                             popup=~as.character(Label),
                             labelOptions = labelOptions(noHide=F, direction='auto'),
                             options = markerOptions(riseOnHover = TRUE),
                             clusterOptions = markerClusterOptions())
                
              })
              
              #output$full_data_gathered <- renderPlot({
              #  ggplot(data=full_data_gathered, aes(x=Pop.ID., y=input$Value, fill=input$variable)) + geom_col(position="dodge") +
              #    xlab("Population ID") + ylab("Temperature (C)") + ggtitle("Maximum, Mininum, Annual Temperature Across Populations") + bbc_style() +
              #    coord_flip() + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Temperature", labels=c("Mean Annual Temperature","Maximum Temperature","Minimum Temperature"))})



# color pallette for the different populations:
#RULES:
#REGION=5 COLORS (10 colors when you include light and dark for each)
#RED COLOR FOR TX GOING TO BLUE FOR MAINE
#DARK COLOR = HIGH SALINITY, LIGHT COLOR= LOW SALINITY WITHIN POP
#Closed triangle = WILD, CLOSED circle= SELECTED
#WITHIN SELECTED, DIFFERENT PATTERNS BETWEEN INBRED AND PARENT (HG, NG POP VS NEH)

#TX-HSLM = black, closed triangle 17 , black
#LA-LSSL = Light orange, closed triangle 17, #fdae6b
#LA-HSCL = Dark orange, closed triangle 17, #d95f0e
#LA-OBOY = orange yellow, closed circle 16 #ffeda0
#CB-DEBY = Dark Green, closed circle 16, #31a354
#CB-HSHC = Medium green, closed triangle 17, #99d8c9
#CB-LOLA = Light green, closed circle 16, #a8ddb5
#CB-LSCP = Lightest green, closed triangle 17, #e0f3db
#DB-HSCS = Dark blue, closed triangle 17, #08519c
#DB-NEHG = selected, closed circle 16, light blue #6baed6
#DB-NEHD = selected, closed circle 16, lighter blue #bdd7e7
#DB-NEHS =selected, closed circle  16, light purple, #cbc9e2
#DB-LSHC = wild, closed triangle 17, ##b3cde3
#ME-HSHI = ,closed triangle 17, dark #54278f 
#ME-UMFS = Dark brown, closed circle 16, #756bb1
#ME-LSSM = Light purple, closed triangle 17, #cbc9e2


data_1 = reactive({
  a<-full_data_gathered_ordered[full_data_gathered_ordered$Env_variable%in%input$temp_choose,]
  if (input$Region != "All") {
    ab <- a %>% filter(Region == input$Region)
  } else {
    ab <- a
    return(ab)
  }
  
  if (input$Ecotype != "All") {
    abc <- ab %>% filter(Ecotype == input$Ecotype)
  } else {
    abc <- ab
    return(abc)
  }
  
  if (input$Wild.Sel != "All") {
    abcd <- abc %>% filter(Wild.Sel == input$Wild.Sel)
  } else {
    abcd <- abc
    return(abcd)
  }
 
  
  
  }
)

    output$paramchkbxgrp  <- renderPlot({
                ggplot(data=data_1(), aes(x=Plot_name, y=Value, fill=Env_variable)) +
                  geom_col(position="dodge") +
                  xlab("Population ID") +
                  ylab("Temperature (C)") +
                  coord_flip() + theme_bw() + bbc_style()+
                  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ 
                  scale_fill_manual(name="Temperature Data", values=c("#a6cee3","#1f78b4",
                    "#b2df8a"),labels=c("Mean_Annual_Temperature_Celsius","Max_temperature_Celsius",
                                                      "Min_temperature_Celsius"))
              }
              )
              
    data_2 = reactive({
      d<-full_data_gathered_ordered[full_data_gathered_ordered$Env_variable%in%input$sal_choose,]
      if (input$Region != "All") {
        db <- d %>% filter(Region == input$Region)
      } else {
        db <- d
        return(db)
      }
      
      if (input$Ecotype != "All") {
        dbc <- db %>% filter(Ecotype == input$Ecotype)
      } else {
        dbc <- db
        return(dbc)
      }
      
      if (input$Wild.Sel != "All") {
        dbcd <- dbc %>% filter(Wild.Sel == input$Wild.Sel)
      } else {
        dbcd <- dbc
        return(dbcd)
      }
      
      
    })
        
              output$sal_checkbox  <- renderPlot({
                ggplot(data=data_2(), aes(x=Plot_name, y=as.numeric(Value))) +
                  geom_point(aes(shape=Wild.Sel, color=Plot_name, size=Wild.Sel)) +
                  xlab("Population ID") +
                  ylab("Mean Annual Salinity (ppt)") +
                  coord_flip() +
                  scale_shape_manual(values=c(13,16,17))+
                  scale_size_manual(values=c(5,5,5)) +
                  theme_bw() + bbc_style()+
                  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
                  scale_color_manual(name="Populations", values=c("black", "#fdae6b", "#a63603", "#fd8d3c", 
                                                                  "#006d2c", "#31a354", "#bae4b3","#a6d854","#08519c",
                                                                  "#6a51a3","#9e9ac8", "#dadaeb","skyblue","#f768a1","#fbb4b9", "#7a0177"))+
                  guides(color=guide_legend(override.aes = list(size = 6)),shape = guide_legend(override.aes = list(size = 6)))
              
           
              })
              
              
data_3 = reactive({

  c<-full_data_gathered_ordered[full_data_gathered_ordered$Env_variable%in%input$dd_choose,]
  if (input$Region != "All") {
    cb <- c %>% filter(Region == input$Region)
  } else {
    cb <- c
    return(cb)
  }
  
  if (input$Ecotype != "All") {
    cbc <- cb %>% filter(Ecotype == input$Ecotype)
  } else {
    cbc <- cb
    return(cbc)
  }
  
  if (input$Wild.Sel != "All") {
    cbcd <- cbc %>% filter(Wild.Sel == input$Wild.Sel)
  } else {
    cbcd <- cbc
    return(cbcd)
  }
  
  
  })                
              output$dd_checkbox  <- renderPlot({
                ggplot(data=data_3(), aes(x=Plot_name, y=as.numeric(Value), fill=Env_variable)) +
                  geom_col(position="dodge") + bbc_style()+ 
                  xlab("Population ID") +
                  ylab("Mean Degree Days per Year") +
                  coord_flip()
              }
              )
              
                        
}
            
# Run the application
shinyApp(ui = ui, server = server)
            
            # shinyBS, bsTooltip
            