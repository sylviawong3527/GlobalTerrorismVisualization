
library(shiny)
library(plotly)
library(plyr)
library(dplyr)

GT <- read.csv("GT.csv")
GT1 <- select(GT,eventid,iyear,imonth,iday,country,country_txt,region,region_txt,provstate,city,latitude,longitude,specificity,vicinity,summary,alternative,alternative_txt,multiple,success,suicide,attacktype1,attacktype1_txt,targtype1,targtype1_txt,targsubtype1,targsubtype1_txt,corp1,target1,natlty1,natlty1_txt,gname,motive,weaptype1,weaptype1_txt,weapsubtype1,weapsubtype1_txt,weapdetail,nkill,nkillus,nkillter,nwound,nwoundus,nwoundte,property,propextent,propextent_txt,propvalue,propcomment,addnotes)
gt1 <- tbl_df(GT1)
sub1 <- select(gt1,iyear,imonth,iday,attacktype1_txt) #time
sub2 <- select(gt1,country,country_txt,region,region_txt,provstate,city,latitude,longitude) #location
sub3 <- filter(sub2, country == 217) #USA
sub4 <- mutate(sub2, isUSA = (country == 217))


subTime <-  select(gt1,iyear,imonth,iday,country,attacktype1_txt) # time
usaTime <- filter(subTime,country == 217)
chinaTime <- filter(subTime,country == 44)
iraqTime <- filter(subTime,country == 95)

colnames(usaTime)[5]<-c("attackType")
colnames(chinaTime)[5]<-c("attackType")
colnames(iraqTime)[5]<-c("attackType")

# Define UI for application that draws a histogram
ui <- navbarPage("Global Terrorism",
                 navbarMenu("World Map",
                            tabPanel("Total Attacks from 1970 to 2017",
                                     mainPanel(
                                       plotlyOutput("mapPlot1")
                                     )),
                            tabPanel("Attacks of each year",
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("integer",
                                                     "Year :",
                                                     min = 1970, 
                                                     max = 2017, 
                                                     value = 38,animate=TRUE) 
                                       )
                                      ,
                                       mainPanel(
                                         plotlyOutput("mapPlot2")
                                       )))),
                 navbarMenu("Countries",
                            tabPanel("Attacks in USA",
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("integer",
                                                     "Year :",
                                                     min = 1970, 
                                                     max = 2017, 
                                                     value = 38,animate=TRUE) 
                                       )
                                     ,
                                     mainPanel(
                                       plotOutput("Plot1")
                                     ))),
                            tabPanel("Attacks in China",
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("integer",
                                                     "Year :",
                                                     min = 1970, 
                                                     max = 2017, 
                                                     value = 38,animate=TRUE) 
                                       )
                                     ,
                                     mainPanel(
                                       plotOutput("Plot2")
                                     ))),
                            tabPanel("Attacks in Iraq",sidebarLayout(
                              sidebarPanel(
                                sliderInput("integer",
                                            "Year :",
                                            min = 1970, 
                                            max = 2017, 
                                            value = 38,animate=TRUE) 
                              )
                            ,
                            mainPanel(
                              plotOutput("Plot3")
                            )))))


fig_h = 5 #height of each plot
fig_w = 8 #width of each plot

m = list(b = 100, #typical margin for each plot
         t = 100,
         pad = 10)

codes <- c('AFG','ALB','DZA','AND','AGO','ATG','ARG','ARM','AUS','AUT','AZE','BHS','BHR','BGD','BRB',
           'BLR','BEL','BLZ','BEN','BTN','BOL','BIH','BWA','BRA','BRN','BGR','BFA','BDI','KHM',
           'CMR','CAN','RCA','TCD','CHL','CHN','COL','COM','CRI','HRV','CUB','CYP','CZE',NA,'COD',
           'DNK','DJI','DMA','DOM','DEU','TMP','ECU','EGY','SLV','GNQ','ERI','EST','ETH','FLK',
           'FJI','FIN','FRA','GUF','PYF','GAB','GMB','GEO','DEU','GHA','GRC','GRD','GLP',
           'GTM','GIN','GNB','GUY','HTI','HND','HKG','HUN','ISL','IND','IDN',NA,'IRN','IRQ','IRL',
           'ISR','ITA',NA,'JAM','JPN','JOR','KAZ','KEN',NA,'KWT','KGZ','LAO','LVA','LBN','LSO',
           'LBR','LBY','LTU','LUX','MAC','MKD','MDG','MWI','MYS','MDV','MLI','MLT','MTQ','MRT',
           'MUS','MEX','MDA','MNE','MAR','MOZ','MMR','NAM','NPL','NLD','NCL','NHB','NZL','NIC',
           'NER','NGA','PRK','YEM','NOR','PAK','PAN','PNG','PRY','RCB','PER','PHL','POL','PRT',
           'QAT','COG','ZWE','ROU','RUS','RWA','SAU','SEN','SRB','SRB','SYC','SLE','SGP','SVK',
           'SVN','SLB','SOM','ZAF','KOR','SSD','VNM','YEM','RUS','ESP','LKA','KNA','LCA','SDN',
           'SUR','SWZ','SWE','CHE','SYR','TWN','TJK','TZA','THA','TGO','TTO','TUN','TUR','TKM',
           'UGA','UKR','ARE','GBR','USA','URY','UZB','VUT',NA,'VEN','VNM','WLF',NA,'DEU','ESH',
           'YEM','YUG','ZRE','ZMB','ZWE')
country_totals <- data.frame(table(GT$country_txt))
country_totals$codes <- codes


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mapPlot1 <- renderPlotly({
     plot_geo(country_totals) %>%
       add_trace(
         type = 'choropleth',
         z = country_totals$Freq,
         locations = country_totals$codes,
         color = country_totals$Freq,
         colors = 'Greys',
         text = ~paste("Country: ",country_totals$Var1, 
                       "Attacks: ",country_totals$Freq, 
                       sep = "<br />"),
         hoverinfo = "text",
         marker = list(line = list(color = toRGB("grey"), width = 0.5))
       ) %>% 
       layout(geo = list(showland = TRUE,
                         showcoastlines = FALSE,
                         showcountries = TRUE,
                         countrywidth = 0.2),
              title = "Total Attacks in each Country from 1970 to 2017",
              margin = list(t = 100))
     
   })
   
   output$mapPlot2 <- renderPlotly({ 
     Y <- filter(GT,iyear==input$integer)
     countrys <- data.frame(table(Y$country_txt))
     countrys$codes <- codes
     
     plot_geo(countrys) %>%
       add_trace(
         type = 'choropleth',
         z = countrys$Freq,
         locations = countrys$codes,
         color = countrys$Freq,
         colors = 'Blues',
         text = ~paste("Country: ",countrys$Var1, 
                       "Attacks: ",countrys$Freq, 
                       sep = "<br />"),
         hoverinfo = "text",
         marker = list(line = list(color = toRGB("grey"), width = 0.5))
       ) %>%
       layout(geo = list(showland = TRUE,
                         showcoastlines = FALSE,
                         showcountries = TRUE,
                         countrywidth = 0.2),
              title = "Attacks in each Country",
              margin = list(t = 100))
     
     })
   output$Plot1 <- renderPlot({
     # generate bins based on input$bins from ui.R
     #  sub1 <- select(gt1,iyear,imonth,iday)
     usa <- filter(usaTime,iyear == input$integer)
     
     ggplot(usa,aes(x=factor(imonth)))+
       geom_bar(aes(fill=attackType))+
       geom_hline(yintercept = nrow(usa)/12,linetype = "dashed")+
       labs(title="Different Types of Attacks in USA",x="Month",y="Number")+
       theme(plot.title = element_text(hjust = 0.5))+
       geom_text(x=11, y=nrow(usa)/12, label="Average Attacks",vjust = -1)
     
   })
   output$Plot2 <- renderPlot({
     # generate bins based on input$bins from ui.R
     #  sub1 <- select(gt1,iyear,imonth,iday)
     china <- filter(chinaTime,iyear == input$integer)
     
     ggplot(china,aes(x=factor(imonth)))+
       geom_bar(aes(fill=attackType))+
       geom_hline(yintercept = nrow(china)/12,linetype = "dashed")+
       labs(title="Different Types of Attacks in China",x="Month",y="Number")+
       theme(plot.title = element_text(hjust = 0.5))+
       geom_text(x=11, y=nrow(china)/12, label="Average Attacks",vjust = -1)
     
   })
   output$Plot3 <- renderPlot({
     # generate bins based on input$bins from ui.R
     #  sub1 <- select(gt1,iyear,imonth,iday)
     iraq <- filter(iraqTime,iyear == input$integer)
     
     ggplot(iraq,aes(x=factor(imonth)))+
       geom_bar(aes(fill=attackType))+
       geom_hline(yintercept = nrow(iraq)/12,linetype = "dashed")+
       labs(title="Different Types of Attacks in Iraq",x="Month",y="Number")+
       theme(plot.title = element_text(hjust = 0.5))+
       geom_text(x=11, y=nrow(iraq)/12, label="Average Attacks",vjust = -1)
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

