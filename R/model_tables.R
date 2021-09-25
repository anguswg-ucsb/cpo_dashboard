

library(htmltools)
library(reactable)
library(shiny)
app = shinyApp(
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30),
          bsTooltip("bins", "The wait times will be broken into this many equally spaced bins",
                    "right", options = list(container = "body"))
        ),
        mainPanel(
          popify(plotOutput("distPlot"), title = "tipify"),
          uiOutput("uiExample")
        )
      )
    ),
  server =
    function(input, output, session) {
      output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
          hist(x, breaks = bins, col = 'darkgray', border = 'white')

      })
      output$uiExample <- renderUI({
        tags$span(
          popify(bsButton("pointlessButton", "Button", style = "primary", size = "large"),
                 "A Pointless Button",
                 "This button is <b>pointless</b>. It does not do <em>anything</em>!"),
          tipify(bsButton("pB2", "Button", style = "inverse", size = "extra-small"),
                 "This button is pointless too!")
        )
      })
      # addPopover(session, "distPlot", "Data", content = paste0("<p>Waiting time between ",
      #                                                          "eruptions and the duration of the eruption for the Old Faithful geyser ",
      #                                                          "in Yellowstone National Park, Wyoming, USA.</p><p>Azzalini, A. and ",
      #                                                          "Bowman, A. W. (1990). A look at some data on the Old Faithful geyser. ",
      #                                                          "Applied Statistics 39, 357-365.</p>"), trigger = 'click')
    }
)
bsExample("Modals")
## Not run:
library(devtools)
install_github("jasdumas/shinyLP")
library(shinyLP)
shinyLP::runExample()
install.packages("htmltools")
runApp(app, display.mode= "showcase")

  library(htmltools)
shinyLP::runExample()
bsExample("Tooltips_and_Popovers")
devtools::install_github("ijlyttle/bsplus")
player_stats <- read.csv("C:/Users/angus/OneDrive/Desktop/reactable/player_stats.csv", stringsAsFactors = FALSE)
team_stats <- read.csv("C:/Users/angus/OneDrive/Desktop/reactable/team_stats.csv", stringsAsFactors = FALSE)
line_score <- read.csv("C:/Users/angus/OneDrive/Desktop/reactable/line_score.csv", stringsAsFactors = FALSE)

line_score <- line_score[, c("TEAM_ID", "TEAM_CITY_NAME", "TEAM_NICKNAME", "TEAM_WINS_LOSSES",
                             "PTS_QTR1", "PTS_QTR2", "PTS_QTR3", "PTS_QTR4", "PTS")]

box_score_tbl <- function(player_stats, team_stats, team) {
  # Convert M:SS strings to datetimes for proper sorting
  player_stats$MIN_STR <- player_stats$MIN
  player_stats$MIN <- strptime(player_stats$MIN, format = "%M:%S")

  cols <- c("PLAYER_ID", "PLAYER_NAME", "START_POSITION", "MIN", "MIN_STR",
            "FGM", "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA",
            "FT_PCT", "OREB", "DREB", "REB", "AST", "STL", "BLK", "TO", "PF",
            "PTS", "PLUS_MINUS")
  stats <- player_stats[player_stats$TEAM_ABBREVIATION == "TOR", cols]
  team_stats <- team_stats[team_stats$TEAM_ABBREVIATION == "TOR", ]

  reactable(
    stats,
    pagination = FALSE,
    defaultSortOrder = "desc",
    defaultSorted = "PTS",
    defaultColDef = colDef(
      sortNALast = TRUE,
      minWidth = 45,
      class = JS("function(rowInfo, colInfo, state) {
        // Highlight sorted columns
        for (var i = 0; i < state.sorted.length; i++) {
          if (state.sorted[i].id === colInfo.id) {
            return 'sorted'
          }
        }
      }"),
      headerClass = "box-score-header",
      footer = function(values, name) {
        value <- team_stats[[name]]
        # Format shots made-attempted
        if (name %in% c("FGM", "FG3M", "FTM")) {
          attempted_name <- c(FGM = "FGA", FG3M = "FG3A", FTM = "FTA")[name]
          value <- sprintf("%s-%s", value, team_stats[[attempted_name]])
        }
        # Format percentages
        if (name %in% c("FG_PCT", "FG3_PCT", "FT_PCT")) {
          value <- paste0(value * 100, "%")
        }
        # Format +/-
        if (name == "PLUS_MINUS") {
          value <- sprintf("%+d", value)
        }
        value
      }
    ),
    columns = list(
      PLAYER_ID = colDef(show = FALSE),
      PLAYER_NAME = colDef(
        name = "Player",
        defaultSortOrder = "asc",
        width = 130,
        cell = function(value, index) {
          player_id <- stats[index, "PLAYER_ID"]
          player_url <- sprintf("https://stats.nba.com/player/%s", player_id)
          start_position <- stats[index, "START_POSITION"]
          if (start_position != "") {
            value <- tagList(value, " ", tags$sup(start_position))
          }
          tags$a(href = player_url, target = "_blank", value)
        },
        footer = span(class = "box-score-total", "Totals")
      ),
      START_POSITION = colDef(show = FALSE),
      MIN = colDef(name = "Min", minWidth = 60, align = "right", cell = function(value, index) {
        if (!is.na(value)) stats[index, "MIN_STR"] else "DNP"
      }),
      MIN_STR = colDef(show = FALSE),
      FGM = colDef(name = "FG", minWidth = 55, cell = function(value, index) {
        if (!is.na(value)) sprintf("%s-%s", value, stats[index, "FGA"])
      }),
      FGA = colDef(show = FALSE),
      FG_PCT = colDef(name = "FG%", minWidth = 55, format = colFormat(percent = TRUE)),
      FG3M = colDef(name = "3P", minWidth = 55, cell = function(value, index) {
        if (!is.na(value)) sprintf("%s-%s", value, stats[index, "FG3A"])
      }),
      FG3A = colDef(name = "3PA", show = FALSE),
      FG3_PCT = colDef(name = "3P%", minWidth = 55, format = colFormat(percent = TRUE)),
      FTM = colDef(name = "FT", minWidth = 55, cell = function(value, index) {
        if (!is.na(value)) sprintf("%s-%s", value, stats[index, "FTA"])
      }),
      FTA = colDef(show = FALSE),
      FT_PCT = colDef(name = "FT%", minWidth = 55, format = colFormat(percent = TRUE)),
      OREB = colDef(name = "ORB"),
      DREB = colDef(name = "DRB"),
      PLUS_MINUS = colDef(name = "+/-", cell = function(value) {
        if (is.na(value)) "" else sprintf("%+d", value)
      })
    ),
    showSortIcon = FALSE,
    highlight = TRUE,
    striped = TRUE,
    class = "box-score-tbl",
    theme = reactableTheme(cellPadding = "8px")
  )
}
box_score_tbl
# base URL to CDSS website
url <- "https://dwr.state.co.us/Tools/Structures/"

# filter to specific WDID
ditch_data <- ditch_data %>% left_join(dplyr::select(ditch_names, admin, name))
saveRDS(ditch_data, "ditch_data3.rds")

# all admins from node
admins <- ditch_data %>%
  filter(node_id == node) %>%
  # filter(node_id == 3600729) %>%
  group_by(admin) %>%
  summarize(
    node_id     = node_id,
    name        = name,
    supply      = mean(supply_all),
    supply_dir  = mean(supply_dir),
    demand      = mean(demand_dir),
    short       = mean(short_all),
    short_dir   = mean(short_dir)
    ) %>%
  group_by(admin) %>%
  slice(n = 1) %>%
  # filter(WDID == 700614) %>%
  mutate(WDID = as.numeric(node_id)) %>%
  # dplyr::select(node_id, admin, name, WDID)
  mutate(
    admin_number  = round(as.numeric(admin), 0 ),
    admin         = as.numeric(admin)
  ) %>%
  # mutate(admin_number = round(as.numeric(admin), 0 )) %>%
  dplyr::select(name, node_id, admin, admin_number, supply:short_dir) %>%
  # group_by(admin) %>%
  # slice(n = 1) %>%
  ungroup() %>%
  left_join(admin_dates, by = "admin_number") %>%
  dplyr::select(name, node_id, admin, date, supply:short_dir)
  # mutate(
  #   cdss_url = paste0(url, node_id)
  # )

reactable::reactable(
  admins,
  columns = list(
        name     = colDef(name = "Structure Name",
                      align = "center"
                      ),
        node_id  = colDef(name = "WDID",
                          align = "center",
                          cell = function(value, index) {
                            node_url <- sprintf("https://dwr.state.co.us/Tools/Structures/%s", admins[index, "node_id"])
                            node_id <- paste(admins[index, "node_id"])
                            # team_record <- line_score[index, "TEAM_WINS_LOSSES"]
                            tagList(
                              tags$a(class = "node_id", href = node_url, target = "_blank", node_id)
                              # span(class = "team-record", team_record)
                              )
                            }),
        admin      = colDef(name = "Priority Admin No.", align = "center"),
        date       = colDef(name = "Appropriation Date", align = "center"),
        supply     = colDef(name = "Average annual supply", align = "center"),
        supply_dir = colDef(name = "Average annual direct supply", align = "center"),
        demand     = colDef(name = "Average annual demand", align = "center"),
        short      = colDef(name = "Average annual total shortage", align = "center"),
        short_dir  = colDef(name = "Average annual direct shortage", align = "center")
                     ),
                     highlight = TRUE,
                     outlined = TRUE
)
tbl
div(class = "node-table",
    h2(class = "header", "Node Table",
       tags$a(class = "game-date", href="https://stats.nba.com/game/0041800403", target = "_blank", "Jun 5, 2019")),

    div(class = "line-score", line_score_tbl),

    div(class = "box-score-title", "Toronto Raptors"),
    box_score_tbl(player_stats, team_stats, "TOR"),

    div(class = "box-score-title", "Golden State Warriors"),
    box_score_tbl(player_stats, team_stats, "GSW")
)


lm_model  <-  lm_run$model
results   <-  augment(lm_model)
tidied    <-  tidy(lm_model)
metrics   <-  glance(lm_model)

rsq_pval    <-  metrics %>%
  dplyr::select(r_squared = r.squared, p_value = p.value) %>%
  mutate(across(where(is.numeric), round, 3))
  # pivot_longer(cols = c(1:2)) %>%
  # setNames(c("term", "estimate"))

coeff  <-  tidied %>%
  dplyr::select(term, estimate) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  pivot_wider(names_from = "term", values_from = "estimate")


tbl_all <- bind_cols(coeff, rsq_pval)

# recode names
clean_names <- names(rename_all(tbl_all, recode,
                               "(Intercept)"     = "Intercept",
                               r_squared         = "R squared",
                               p_value           = "p-value",
                               swe_max           = "SWE maximum (in)",
                               prcp              = "Precipitation (mm)",
                               pdsi              = "PDSI",
                               pdsi_gridmet      = "PDSI (gridMET)",
                               eddi1             = "EDDI 1 month",
                               eddi3             = "EDDI 3 month",
                               eddi6             = "EDDI 6 month",
                               eddi12            = "EDDI 12 month",
                               spi1              = "SPI 1 month",
                               spi3              = "SPI 3 month",
                               spi6              = "SPI 6 month",
                               spi9              = "SPI 9 month",
                               spi12             = "SPI 12 month",
                               tavg              = "Average temperature (C)",
                               aet               = "Actual evapotranspiration (mm)",
                               pet               = "Potential Evapotranspiration (mm)",
                               soilm             = "Soil moisture (mm)")


)

tbl_clean <-  tbl_all %>% setNames(clean_names)
tbl_long <- tbl_clean %>%
  pivot_longer(cols = c(1:ncol(tbl_clean))) %>%
  setNames(c("Metric", "Value")) %>%
  mutate(
    Type = case_when(
      Metric %in% c("R squared", "p-value") ~ "Metrics",
      !Metric %in% c("R squared", "p-value") ~ "Coefficients"
    )
  )



# Clean reactable table
reactable(
  tbl_clean,
  defaultColDef = colDef(style = pos_neg_colors("red", "green", bold = TRUE)),
  columns = list(
    Intercept = colDef(
      style = pos_neg_colors("red", "green", bold = TRUE))
    )
  )

rsq_colors <- data.frame("R squared" = c(0, 1))

# Clean reactable table
tbl_react <- reactable(
  tbl_clean,
  # groupBy = "Type",
  style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px", fontWeight = 600),
  defaultColDef = colDef(
    align = "center",
    # style = function(value) {
    #   color <- if (value > 0) {
    #     "#008000"
    #   } else if (value < 0) {
    #     "#e00000"
    #   }
    # },
    style = pos_neg_colors("red", "green", bold = TRUE)),
#       list(
#         # background = color,
#       #   fontWeight = 600,
#       #   color = color
#       # ),
# ),
  columns = list(
    "R squared" = colDef(
      # style = color_scales(tbl_clean, span = T, colors = c("#FFFF99", "#FF8000", "#990000"), opacity = 0.5)
      style = color_scales(rsq_colors, span = TRUE, colors = c("#FFFFcc", "#4c9900"), opacity = .7, bold_text = T,
                           text_color = "black",
                           brighten_text_color = "black"),
    ),
    # "SPI 1 month" = colDef(
    #   # style = color_scales(tbl_clean, span = T, colors = c("#FFFF99", "#FF8000", "#990000"), opacity = 0.5)
    #   style = color_scales(tbl_clean, span = TRUE, colors = c("#FFFFcc", "#4c9900"), opacity = 1),
    # ),
      "Intercept" = colDef(
        style = pos_neg_colors("black", "black", bold = TRUE)
    )),



      # style = pos_neg_colors("red", "green", bold = TRUE))
    # ),
  highlight = TRUE,
  outlined = T,
  bordered = T,
  compact = T,
  theme = reactableTheme(
    borderColor = "black",
    cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
    headerStyle = list(
      backgroundColor = "hsl(207, 16%, 80%)"),
    # groupHeaderStyle = list(
    #   backgroundColor = "hsl(208, 8%, 6%)"),
    # borderColor = "#555"
    ),

  # defaultExpanded = TRUE

  ) %>%
  add_subtitle("Model Results",
               align = "center",  font_size = 26, margin = 4)

tbl_react

# names(tbl_all) <-
tbl_data <- bind_rows(coeff, rsq_pval)

reactable::reactable(
  tbl_clean)
  columns = list(
    name     = colDef(name = "Structure Name",
                      align = "center"
    ),
    node_id  = colDef(name = "WDID",
                      align = "center",
                      cell = function(value, index) {
                        node_url <- sprintf("https://dwr.state.co.us/Tools/Structures/%s", admins[index, "node_id"])
                        node_id <- paste(admins[index, "node_id"])
                        # team_record <- line_score[index, "TEAM_WINS_LOSSES"]
                        tagList(
                          tags$a(class = "node_id", href = node_url, target = "_blank", node_id)
                          # span(class = "team-record", team_record)
                        )
                      }),
    admin      = colDef(name = "Priority Admin No.", align = "center"),
    date       = colDef(name = "Appropriation Date", align = "center"),
    supply     = colDef(name = "Average annual supply", align = "center"),
    supply_dir = colDef(name = "Average annual direct supply", align = "center"),
    demand     = colDef(name = "Average annual demand", align = "center"),
    short      = colDef(name = "Average annual total shortage", align = "center"),
    short_dir  = colDef(name = "Average annual direct shortage", align = "center")
  ),
  highlight = TRUE,
  outlined = TRUE
)
metrics %>%
mutate(
  coefficient   = round(coefficient, 3),
  r_squared     = round(r_squared,3),
  p_value       = round(p_value, 5)
) %>%
  # mutate(across(where(is.numeric), round, 3)) %>%
  # mutate(across(where(is.numeric), as.character)) %>%
  rename(
    # District      = district,
    Term          = term,
    Coefficient   = coefficient,
    "R Squared"   = r_squared,
    "p-value"     = p_value
  )
tbl_coeff <- tbl_data %>%
  filter(Term != "(Intercept)") %>%
  mutate(
    Term = indicator_label
  )











