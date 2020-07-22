library(tidyverse)
library(tidycensus)
library(shiny)
library(ggplot2)
library(leaflet)
library(stringr)
library(sf)
library(reshape2)
library(plotly)

# If you don't have a Census API key, go to : https://api.census.gov/data/key_signup.html
census_api_key("e442a94c752f909f2c3de0089081e96a4da59eb8")

fa <- read_csv('food_access.csv')

state_names <- fa %>%
  distinct(State)

state_names <- as.list(as.vector(state_names$State))

ui <- fluidPage(
  titlePanel("Food Access Housing Tool"),
  "This project uses data from the USDA's Food Access Research Atlas, which was last updated in 2015. The aim of the study is
  to effectively communicate the degree of food access, which has historically had disproportionately greater impact on 
  low-income and minority communities, and to help people consider food availability in their housing decisions.",
  br(), br(),
  
  splitLayout(
    p(strong("GeoID: "), " a unique identifier for each census tract"),
    p(strong("Census Tract: "), "a small subdivision of a county, typically populations of 2,500 to 8,000 people")),
  
  "Census tracts have unique numbers within their county, but they are not unique at the state level. Fortunately, as GeoIDs are
  unique, we can use them to search for census tracts we are interested in learning more about. While we have used GeoIDs to structure
  the plots within this application, we have chosen to display census tracts within our visuals, as GeoIDs can be challenging to read.",
  br(), br(),
  sidebarLayout(sidebarPanel(selectInput(inputId = "state",
                                         label="Please select a state of your choice",
                                         choices = state_names),
                             selectInput(inputId = "hl",
                                         label = "Please select a sensitivity level \n (distance to nearest food store)",
                                         choices = c("High (1/2 mile for urban areas ; 10 miles for rural areas)",
                                                     "Low (1 mile for urban areas ;\n 20 miles for rural areas)"),
                                         selected = "High (1/2 mile for urban areas ; 10 miles for rural areas)"),
                             sliderInput(inputId="inc",
                                         label="Please select your income",
                                         value=50000,min=0,max=250000,
                                         step=1000),
                             em("This will provide the table below with suggestions that fit your budget."), br(), br(),
                             textInput(inputId = "tract",
                                       label="Please enter a GeoID of your choice"),
                             tableOutput(outputId="table"), width=4),
                mainPanel(leafletOutput(outputId = "cpleth"),
                          plotOutput(outputId = "demo"))),
  h2("Census Tract Demographics"),
  
  fluidRow(column(width = 4, plotOutput(outputId = "race")),
           column(width = 3, plotlyOutput(outputId = "pie"),),
           column(width = 5, plotOutput(outputId = "box"))),
  h2("Sensitivity Plots"),
  p("The Food Access Atlas measures food access by the distance between participants and their nearest food store. It defines
  food stores as grocery stores, supermarkets and supercenters, as these stores offer access to affordable and healthful food. \n 
    They provide two measures for food access:"),
  br(),
  strong("High : "), "1/2 mile for urban areas ; 10 miles for rural areas",
  br(),
  strong("Low : "), "1 mile for urban areas ; 20 miles for rural areas",
  br(), br(),
  "This means that the 'High' sensitivity measure will regard a participant living in a city as low acesss if they are living
  0.75 miles from their nearest food store, whereas the 'Low' measure will not. ",
  br(), br(),
  "We consider sensitivity in order to get a better understanding of whether or not our metrics do a good job of consistently
  representing food access, and to contextualize our graphs. This gives us information on whether our results depend greatly 
  on which measure of food access we use.",
  br(), br(),
  splitLayout(plotOutput(outputId = "dumbell"),
              plotlyOutput(outputId = "sbar")),
  br(), br()
)

server<-function(input, output){
  state_geom <- reactive({
    
    # fetching tract-level geometries from Census API - requires strong internet connectivity *
    # If internet is not sufficiently strong, this will throw an "Opening layer failed" error.
    
    state_geom <- get_acs(geography = "tract",
                          variables = "B01003_001",
                          state = input$state, 
                          geometry = TRUE)
    
  })
  fa_comb <- reactive ({
    
    state_geom <- state_geom()
    
    state_geom %>%
      separate(NAME, c("TractNumber","County", "State"), sep=',', remove = F) -> state_check
    
    state_check$State <- str_trim(state_check$State, side = "left")
    state_in <- state_check$State[1]
    
    fa_state <- fa %>%
      filter(State == state_in) %>%
      mutate(rate_low=(LAPOP1_20/POP2010)*100) %>%
      mutate(rate_high=(LAPOP05_10/POP2010)*100)
    
    fa_state$rate_low[is.nan(fa_state$rate_low)] <- 0
    fa_state$rate_high[is.nan(fa_state$rate_high)] <- 0
    
    colnames(state_geom)[colnames(state_geom)=="GEOID"] <- "CensusTract"
    fa_comb <- merge(fa_state, state_geom, all.y=T, by="CensusTract")
    
    fa_comb$tract <- str_extract(state_geom$NAME, "^([^,]*)")
    
    remove(state_geom, state_check, fa_state)
    fa_comb
    
  })
  bar <- reactive({
    
    fa_comb <- fa_comb()
    
    bar <- fa_comb %>%
      select(CensusTract, tract, State, County, Urban, PovertyRate, MedianFamilyIncome, POP2010,
             lawhitehalfshare, lablackhalfshare, laasianhalfshare, laomultirhalfshare, lanhopihalfshare, laaianhalfshare, lahisphalfshare, 
             lawhite1share, lablack1share, laasian1share, laomultir1share, lanhopi1share, laaian1share, lahisp1share,
             lawhite10share, lablack10share, laasian10share, laomultir10share, lanhopi10share, laaian10share, lahisp10share,
             lawhite20share, lablack20share, laasian20share, laomultir20share, lanhopi20share, laaian20share, lahisp20share)
    
    colnames(bar) <- tolower(colnames(bar))
    
    d_groups <- c("white", "black", "asian", "omultir", "nhopi", "aian", "hisp")
    vars <- c("half", "1", "10", "20")
    
    oop <- crossing(d_groups, vars)
    
    vars1 <- paste0("la", oop$d_groups, oop$vars,"share")
    vars2 <- paste0(oop$d_groups, oop$vars, "share")
    tracts <- paste0("tract", oop$d_groups)
    
    add_vars <- dim(bar)[2]
    for (i in 1:length(tracts)) {
      bar[, add_vars + i] <- 1-bar[,vars1[i]]
      
      names(bar)[add_vars + i] <- vars2[i]
    }
    
    q <- bar %>%
      melt(id = c("censustract", "tract", "state", "county", "urban", "povertyrate", "medianfamilyincome", "pop2010")) 
    
    
    r <- q %>%
      filter(censustract == input$tract) %>%
      mutate(location = ifelse(grepl("half", variable) | grepl("1share", variable), 1, 0)) %>%
      mutate(sens = ifelse(grepl("half", variable) | grepl("10", variable), "Low", "High")) %>%
      filter(urban == location) %>%
      mutate(la = ifelse(grepl("^la", variable), 1 ,0)) %>%
      mutate(race = str_remove_all(str_remove_all(str_remove_all(str_remove_all(as.character(variable), "half"), "[012]"), "^la"), "share")) %>%
      mutate(race = factor(race, 
                           levels = c("white", "black", "asian", "omultir", "nhopi", "aian", "hisp"),
                           labels = c("white"="White", "black"="Black","asian" ="Asian", "omultir"="Other & Multiracial",
                                      "hopi"="Pacific Islander","aian"="Native American", "hisp"="Hispanic & Latino")))
    
    remove(fa_comb, bar, oop, q)
    r
  })
  bar2 <- reactive ({ 
    
    fa_comb <- fa_comb()
    
    bar <- fa_comb %>%
      select(CensusTract, tract, State, County, Urban, PovertyRate, MedianFamilyIncome, POP2010,
             lawhitehalf, lablackhalf, laasianhalf, laomultirhalf, lanhopihalf, laaianhalf, lahisphalf, 
             lawhite1, lablack1, laasian1, laomultir1, lanhopi1, laaian1, lahisp1,
             lawhite10, lablack10, laasian10, laomultir10, lanhopi10, laaian10, lahisp10,
             lawhite20, lablack20, laasian20, laomultir20, lanhopi20, laaian20, lahisp20,
             TractWhite,TractBlack, TractAsian, TractNHOPI, TractAIAN, TractOMultir, TractHispanic)
    
    remove(fa_comb)
    bar
  })
  output$cpleth <- renderLeaflet ({
    
    fa_comb <- fa_comb()
    state_geom <- state_geom()
    
    # state_geom$povrate <- ifelse(is.na(fa_comb$PovertyRate)==T, NA, round(fa_comb$PovertyRate, 2))
    # state_geom$mhhi <- ifelse(is.na(fa_comb$MedianFamilyIncome)==T, NA, paste0("$", fa_comb$MedianFamilyIncome))
    # state_geom$lowrate <- ifelse(is.na(fa_comb$rate_low)==T, NA, fa_comb$rate_low)
    # state_geom$highrate <- ifelse(is.na(fa_comb$rate_high)==T, NA, fa_comb$rate_high)
    # state_geom$tract <- str_extract(state_geom$NAME, "^([^,]*)")
    
    fa_comb <- fa_comb %>%
      mutate(tract = str_extract(NAME, "^([^,]*)"),
             rate_high = ifelse(rate_high > 100, 100, rate_high)) # assigning tracts w/ 100% low access to 100 (debugging)
    
    sf<- st_as_sf(fa_comb)
    
    pal <- colorBin(palette = "viridis", domain = 0:100, bins = 5)
    
    if(input$hl=="High (1/2 mile for urban areas ; 10 miles for rural areas)"){
      sf %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(popup = paste(sf$tract, paste0("(", sf$CensusTract,")"), "<br>",
                                  "Low Access: ", paste0(round(sf$rate_high, 1), "%"), "<br>",
                                  "Poverty Rate: ", paste0(round(sf$PovertyRate,2), "%"), "<br>",
                                  "Median Household Income:", paste0("$", sf$MedianFamilyIncome)),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = .7,
                    color = ~ pal(rate_high)) %>%
        addLegend("bottomright", pal = pal, values = ~rate_high, 
                  title = "Limited<br>Food Access", 
                  labFormat = labelFormat(suffix = " %"),
                  opacity = .7)
    } 
    
    else{
      sf %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(popup = ~ paste(sf$tract, paste0("(", sf$CensusTract,")"), "<br>",
                                    "Low Access: ", paste0(round(sf$rate_low, 1), "%"), "<br>",
                                    "Poverty Rate: ", paste0(sf$PovertyRate, "%"), "<br>",
                                    "Median Household Income:", paste0("$", sf$MedianFamilyIncome)),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.7,
                    color = ~ pal(low_rate)) %>%
        addLegend("bottomright", pal = pal, values = ~rate_low, 
                  title = "Limited<br>Food Access", 
                  labFormat = labelFormat(suffix = " %"),
                  opacity = .7) 
    }
  })
  output$dumbell <-renderPlot({
    
    fa_comb <- fa_comb()
    
    fa_comb$lowrate <- ifelse(is.na(fa_comb$rate_low)==T, NA, fa_comb$rate_low)
    fa_comb$highrate <- ifelse(is.na(fa_comb$rate_high)==T, NA, fa_comb$rate_high)
    fa_comb$tract <- str_remove_all(str_extract(fa_comb$NAME, "^([^,]*)"), "Census ")
    
    dumbell <- fa_comb %>%
      select(lowrate, highrate, tract, CensusTract) %>%
      arrange(desc(highrate-lowrate)) %>%
      head(30)
    
    dumbell <- dumbell %>%
      arrange(desc(highrate-lowrate)) %>%
      mutate(tract2 = factor(CensusTract, levels=dumbell$CensusTract, labels = paste0(dumbell$tract, " (", dumbell$CensusTract, ")"), ordered = T))
    
    ggplot(data=dumbell)+
      geom_segment(aes(x=lowrate, xend=highrate, y=tract2, yend=tract2))+
      geom_point(aes(x=lowrate, y=tract2, color="Low"), size=2.5)+
      geom_point(aes(x=highrate, y=tract2, color="High"), size=2.5)+ 
      scale_color_manual("Sensitivity", values=c("Low"="#21908CFF", "High"="#F0DF07")) + 
      ggtitle(paste0("30 Most Sensitive Census Tracts in ", input$state))+
      xlab("Low Access Rate")+
      ylab("Census Tract")+
      labs(subtitle = "Sensitivity Differences for Food Access Measurement",
           caption = "The tracts within this chart are highly succeptible to changes in their access rate based off \n of which sensitivity measure is used. Take the access rates within these tracts with a grain of salt.")
    
  })
  output$demo <- renderPlot({
    
    r <- bar()
    
    ggplot(r, aes(fill=factor(la), y=value, x=race)) + 
      geom_bar(position="fill", stat="identity")+ 
      ggtitle(paste0("Share of Food Store Access by Race: ", r$tract))+
      ylab("Proportion of the Population with Low Food Store Access")+
      xlab("Race")+
      scale_fill_manual(name = "Access", labels = c("Access", "Low Access"), values = alpha(c("#B0DCDD", "#2A788E"), 0.9))
    
  })
  output$race <- renderPlot ({
    
    bar <- bar2()
    
    test <- bar %>% 
      select(c(CensusTract, tract, State, County, Urban, PovertyRate, MedianFamilyIncome, POP2010,
               TractWhite,TractBlack, TractAsian, TractNHOPI, TractAIAN, TractOMultir, TractHispanic)) %>%
      melt(id = c("CensusTract", "tract", "State", "County", "Urban", "PovertyRate", "MedianFamilyIncome", "POP2010"))
    
    test <- test %>%
      filter(CensusTract == input$tract) %>%
      mutate(var = factor(variable, levels = c("TractWhite","TractBlack", "TractAsian","TractNHOPI", "TractAIAN", "TractOMultir", "TractHispanic"), 
                          labels = c("TractWhite"="White","TractBlack"="Black", "TractAsian"="Asian","TractNHOPI"="Pacific Islander",
                                     "TractAIAN"="Native American","TractOMultir"="Other & Multiracial", "TractHispanic"="Hispanic & Latino")))
    
    ggplot(test, aes(x=var, y=value)) + 
      geom_bar(stat = "identity", fill="#60296c", alpha = 0.8)+
      ggtitle(paste0("Race and Ethnicity: ", test$tract[1]))+
      xlab("")+
      ylab("Count")+
      theme(axis.text.x = element_text(angle = 60, hjust=1))
    
  })
  output$box <- renderPlot({
    
    fa_comb <- fa_comb()
    
    box <- fa_comb %>%
      select(CensusTract, tract, State, County, Urban, PovertyRate, MedianFamilyIncome, POP2010,
             TractSNAP, TractLOWI, TractHUNV) %>%
      melt(id = c("CensusTract", "tract", "State", "County", "Urban", "PovertyRate", "MedianFamilyIncome", "POP2010")) %>%
      mutate(prop = (value / POP2010) * 100) %>%
      mutate(val = ifelse(CensusTract == input$tract, prop, NA)) %>%
      mutate(prop = ifelse(prop > 100, NA, prop)) %>%
      mutate(variable = factor(variable, levels = c("TractSNAP", "TractLOWI", "TractHUNV"), 
                               labels = c("Receiving Food Stamps", "Low Income", "Without a Vehicle"))) %>%
      mutate(tract = ifelse(CensusTract != input$tract, NA, tract)) %>%
      arrange(tract)
    
    # this bugs only with the 'Run App' button (works fine if you select code and run)
    ggplot(box)+
      geom_boxplot(aes(x=variable, y=prop, fill=variable), alpha=0.7)+ 
      geom_point(aes(x=variable, y=as.numeric(val), color = "Tract"))+
      scale_fill_manual("", values = c("Receiving Food Stamps"="#440154FF", "Low Income"="#21908CFF", "Without a Vehicle"="#FDE725FF"))+
      scale_color_manual("", values = c("Tract" ="red"), labels=c(paste0(str_remove_all(box$tract, "Census "), "")))+
      ggtitle(paste0("Comparing ", str_remove_all(box$tract, "Census "),  " \nto Alabama's State Distributions"))+
      xlab("")+
      ylab("Proportion of Affected Population")     
  })
  
  output$table<-renderTable({
    
    fa_comb <- fa_comb()
    
    fa_comb$tract <- str_remove_all(fa_comb$tract, "Census Tract ")
    
    table <- fa_comb %>%
      mutate(diff = abs(input$inc - MedianFamilyIncome)) %>%
      filter(diff <= 0.33*input$inc) %>%
      arrange(diff) %>%
      select(CensusTract, tract, MedianFamilyIncome) %>%
      mutate(MedianFamilyIncome = paste0("$", MedianFamilyIncome)) %>%
      head(10)
    
    names(table) <- c('GeoID', 'Census Tract', 'Median Family Income')
    table
    
  })
  output$sbar <- renderPlotly({
    
    bar <- bar2()
    
    stacked <- bar %>% 
      select(-c(TractWhite,TractBlack, TractAsian, TractNHOPI, TractAIAN, TractOMultir, TractHispanic)) %>%
      melt(id = c("CensusTract", "tract", "State", "County", "Urban", "PovertyRate", "MedianFamilyIncome", "POP2010"))  
    
    stacked <- stacked %>%
      mutate(location = ifelse(grepl("half", variable) | grepl("1$", variable), 1 , 0)) %>%
      filter(CensusTract == input$tract & Urban == location) %>%
      mutate(sens = ifelse(grepl("half", variable) | grepl("10", variable), "High", "Low")) %>%
      group_by(sens) %>%
      mutate(total = sum(value)) %>%
      mutate(perc = round((value/total)*100, 1)) %>%
      arrange(sens) %>%
      mutate(race = str_remove_all(str_remove_all(str_remove_all(as.character(variable), "half"), "[012]"), "^la")) %>%
      mutate(race = factor(race, 
                           levels = c("white", "black", "asian", "omultir", "nhopi", "aian", "hisp"),
                           labels = c("white"="White", "black"="Black","asian" ="Asian", "omultir"="Other & Multiracial",
                                      "hopi"="Pacific Islander","aian"="Native American", "hisp"="Hispanic & Latino"))) %>%
      mutate(racelab = ifelse(location == 1, ifelse(sens=="High", "(1/2 mile)", "(1 mile)"), ifelse(sens=="High", "(10 miles)", "(20 miles)"))) %>%
      mutate(race2 = paste(race, racelab, sep = " ")) 
    
    
    plot_ly(data = stacked, x = ~sens, y = ~perc, type = 'bar',
            name = ~factor(race2), color= ~factor(race2), colors = c("#440154", "#481D6F", "#453581", "#3D4D8A", "#34618D", "#2B748E", "#2B748E",
                                                                     "#1F998A", "#25AC82", "#40BC72", "#67CC5C", "#97D83F", "#CBE11E", "#FDE725"), opacity = 0.8) %>%
      layout(title = paste0("Racial Breakdown of Low Access Population in ", stacked$tract[1]), titlefont = list(size=16),
             xaxis = list(title = "Sensitivity \n \n Bars will not display if all participants in the tract have food access in that category",
                          titlefont = list(size=10)), 
             yaxis = list(title = 'Percentage of Low-Acess Population', ticksuffix = "%"), barmode = 'stack') 
    
  })
  output$pie <- renderPlotly({
    
    r <- bar()
    
    a <- r %>%
      mutate(la = factor(la, levels = c(0, 1), labels = c("0"="Access", "1"="Low Access"))) %>%
      group_by(la) %>%
      summarize(access = mean(value)) 
    
    plot_ly(a, labels = ~la, values = ~access, marker = list(colors = c('#B0DCDD', '#2A788E')), type = 'pie', opacity = 0.9) %>%
      layout(title=paste0("Low Access Population: \n", r$tract[1]), titlefont=list(size=12))
    
  })
}
shinyApp(ui=ui, server=server)