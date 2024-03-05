library(shiny)
library(tidyverse)
library(sf)
library(ggtext)
library(scales)
library(cowplot)

yr <- 2010
# ---
# BASE PLOTS

erap <- read_csv("data/data_catalog_index_nonspatial.csv") %>% 
  mutate(census_tract = as.numeric(geoid))

evictions <- read_csv("data/eviction_data_tract.csv")

back_rent <- evictions %>% 
  pivot_longer(col = c("case_type_single_action", "case_type_joint_action"),
               names_to = "back_rent",
               values_to = "comp_cases") %>% 
  select(filing_year, tract, back_rent, comp_cases) %>% 
  group_by(filing_year, back_rent) %>% 
  summarize(comp_cases = sum(comp_cases)) %>% 
  mutate(back_rent = case_when(
    back_rent == "case_type_single_action" ~ as.factor("No"),
    back_rent == "case_type_joint_action" ~ as.factor("Yes")
  ))

# EVICTION
evict_base <- ggplot(back_rent, aes(x = filing_year, y = comp_cases)) + 
  geom_area(aes(fill = back_rent)) + 
  scale_x_continuous(limits = c(2010, 2020.75),
                     breaks = seq(from = 2010, to = 2019, by = 1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 0, to = 25000, by = 7500),
                     labels = label_comma()) +
  scale_fill_manual(values = c("No" = "#fce39e", "Yes" = "#fccb41")) +
  annotate("text", x = 2012, y = 27000, 
           label = 'atop("24,762 evictions", bold("80.97% back rent cases"))', 
           family = "lato", parse = TRUE) +
  annotate("text", x = 2019, y = 17822, 
           label = 'atop("15,584 evictions", bold("78.81% back rent cases"))',
           family = "lato", parse = TRUE) +
  labs(y = "Completed eviction filings", fill = "Back rent case") +
  theme(
    text = element_text(family = "lato"),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# ---
# APP

ui <- fluidPage(
  includeCSS("www/shiny.css"), # From urbntemplates
  tags$style(type = "text/css", ".irs-min, .irs-max, .irs-single {font-family: 'lato'}"),
  title = "Housing Instability and Eviction in Chicago",
  fluidRow(
    column(2), # Margin
    column(8, # Main
      h1("Housing Instability and Eviction in Chicago"),
      h3("The total number of evictions in Chicago fell after peaking in 2012,
         but the vast majority continued to involved unpaid back rent, indicating an
         ongoing need for rental assistance:"),
      # Static plot
      # img(src="evict.png", height="113.25%", width="113.25%"), 
      plotOutput("evict_plot", hover = "hover")
      ),
    column(2) # Margin
  ),
)

server <- function(input, output){
  # EVICTION
  output$evict_plot <- renderPlot({
    yr <- ifelse(is.null(input$hover), 0, round(input$hover$x))
    evict_base + 
      {if(yr == 2010)annotate("text", x = 2010.57, y = 25296, 
                                    label = 'atop("23,058 evictions", bold("81.1% back rent cases"))',
                                    family = "lato", parse = TRUE)} +
      {if(yr == 2011)annotate("text", x = 2010.75, y = 26441, 
                                    label = 'atop("24,203 evictions", bold("80.7% back rent cases"))',
                                    family = "lato", parse = TRUE)} +
      {if(yr == 2013)annotate("text", x = 2013, y = 25533, 
                                    label = 'atop("23,295 evictions", bold("81.1% back rent cases"))',
                                    family = "lato", parse = TRUE)} +
      {if(yr == 2014)annotate("text", x = 2014, y = 23904, 
                                    label = 'atop("21,666 evictions", bold("82.3% back rent cases"))',
                                    family = "lato", parse = TRUE)} +
      {if(yr == 2015)annotate("text", x = 2015, y = 21802, 
                                    label = 'atop("19,564 evictions", bold("84.0% back rent cases"))',
                                    family = "lato", parse = TRUE)} +
      {if(yr == 2016)annotate("text", x = 2016, y = 19270, 
                                    label = 'atop("17,032 evictions", bold("84.6% back rent cases"))',
                                    family = "lato", parse = TRUE)} +
      {if(yr == 2017)annotate("text", x = 2017, y = 19215, 
                                    label = 'atop("16,977 evictions", bold("84.4% back rent cases"))',
                                    family = "lato", parse = TRUE)} +
      {if(yr == 2018)annotate("text", x = 2017.75, y = 18229, 
                                    label = 'atop("15,991 evictions", bold("78.8% back rent cases"))',
                                    family = "lato", parse = TRUE)}
  },
    width = 1110,
    height = 647.5
  )
}

shinyApp(ui = ui, server = server)
