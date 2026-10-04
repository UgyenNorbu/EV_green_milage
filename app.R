library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(scales)
library(shiny)
library(tidyr)

# ASSUMPTIONS -------------------------------------------------------------
# Assumed average monthly km travelled per vehicle
KM_CARS <- 1200 # km/month per car
KM_BUSES <- 3300 # km/month per bus
KM_TWOWHEELERS <- 800 # km/month per two-wheeler

# Petrol price (Nu. per liter)
PETROL_PRICE <- 90

# Fuel consumption of equivalent ICE vehicle (liters per km)
FUEL_CONS_CARS <- 0.066 # ~12.5 km/litre
FUEL_CONS_BUSES <- 0.30 # ~3.3 km/litre
FUEL_CONS_TWO <- 0.035 # ~28.5 km/litre

# CO2 emission factors — petrol ICE equivalent (kg CO2 per km)
CO2_PER_LITRE <- 2.31 # IPCC/IEA standard petrol emission factor

# Gauge max scale (set to a round number above expected cumulative km)
GAUGE_MAX_KM <- 100000000

# DATA INPUT --------------------------------------------------------------
# Import EV data
EV_DATA <- read_csv("./EV_data_input_Greenmileage_app - data.csv")

# DATA PROCESSING ---------------------------------------------------------
# Compute metrics

EV_DATA <- EV_DATA %>%
  mutate(
    RegDate = my(paste(Month, Year)),
    MonthsSinceReg = interval(RegDate, floor_date(today(), "month")) %/%
      months(1) + 1 # interval() returns a lubridate object; %/% months(1) extracts whole months
  ) %>%
  arrange(RegDate)

LAST_UPDATED <- floor_date(today(), "month") %>%
  format("%B %Y")

EV_DATA <- EV_DATA %>%
  mutate(
    km_Cars = Cars * KM_CARS * MonthsSinceReg,
    km_Buses = Buses * KM_BUSES * MonthsSinceReg,
    km_Two = TwoWheelers * KM_TWOWHEELERS * MonthsSinceReg,
    Total_km = km_Cars + km_Buses + km_Two,
    # litres of fossil fuel displaced by each vehicle category
    lit_Cars = km_Cars * FUEL_CONS_CARS,
    lit_Buses = km_Buses * FUEL_CONS_BUSES,
    lit_Two = km_Two * FUEL_CONS_TWO,
    Total_lit = lit_Cars + lit_Buses + lit_Two,
    # CO2 derived from fuel displaced
    CO2_kg = Total_lit * CO2_PER_LITRE,
    CO2_t = CO2_kg / 1000,
    # monetary value of fuel displaced
    Fuel_Nu = Total_lit * PETROL_PRICE
  )

EV_DATA <- EV_DATA %>%
  mutate(
    Period        = paste(Month, Year),
    # cumulative fleet size (total vehicles on road, not new registrations)
    Fleet_Cars    = cumsum(Cars),
    Fleet_Buses   = cumsum(Buses),
    Fleet_Two     = cumsum(TwoWheelers)
  )

TOTAL_KM <- sum(EV_DATA$Total_km, na.rm = TRUE)
TOTAL_CO2_T <- sum(EV_DATA$CO2_t, na.rm = TRUE)
TOTAL_FUEL_NU <- sum(EV_DATA$Fuel_Nu, na.rm = TRUE)

# GAUGE SVG BUILDER -------------------------------------------------------
# Draws a semicircular speedometer needle gauge
make_gauge_svg <- function(value, max_val) {
  pct <- min(value / max_val, 1)
  angle <- -180 + pct * 180 # -180 (left) to 0 (right) degrees

  # needle tip coordinates (semicircle radius = 130, centre = 160,155)
  cx <- 160
  cy <- 155
  r <- 130
  rad <- angle * pi / 180
  nx <- cx + r * cos(rad)
  ny <- cy + r * sin(rad)

  # arc colour zones — dark to bright green
  zone_cols <- c("#d4867a", "#e8b48a", "#e0e89a", "#b8d9a0", "#6aad72")
  zone_pct <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)

  arc_paths <- ""
  for (i in seq_along(zone_cols)) {
    a1 <- (-180 + zone_pct[i] * 180) * pi / 180
    a2 <- (-180 + zone_pct[i + 1] * 180) * pi / 180
    x1 <- cx + r * cos(a1)
    y1 <- cy + r * sin(a1)
    x2 <- cx + r * cos(a2)
    y2 <- cy + r * sin(a2)
    arc_paths <- paste0(
      arc_paths,
      sprintf(
        '<path d="M %0.1f %0.1f A %d %d 0 0 1 %0.1f %0.1f"
               fill="none" stroke="%s" stroke-width="22" stroke-linecap="butt"/>',
        x1, y1, r, r, x2, y2, zone_cols[i]
      )
    )
  }

  # tick marks
  ticks <- ""
  for (t in seq(0, 1, by = 0.1)) {
    ta <- (-180 + t * 180) * pi / 180
    r1 <- 107
    r2 <- 118
    tx1 <- cx + r1 * cos(ta)
    ty1 <- cy + r1 * sin(ta)
    tx2 <- cx + r2 * cos(ta)
    ty2 <- cy + r2 * sin(ta)
    ticks <- paste0(
      ticks,
      sprintf(
        '<line x1="%0.1f" y1="%0.1f" x2="%0.1f" y2="%0.1f"
               stroke="#6a8f68" stroke-width="2"/>',
        tx1, ty1, tx2, ty2
      )
    )
  }

  val_fmt <- if (value >= 1e6) {
    sprintf("%.2f million km", value / 1e6)
  } else {
    paste0(format(round(value), big.mark = ","), " km")
  }

  max_fmt <- if (max_val >= 1e6) {
    sprintf("%.0f M km", max_val / 1e6)
  } else {
    paste0(format(round(max_val), big.mark = ","), " km")
  }

  sprintf(
    '
<svg viewBox="0 0 320 220" xmlns="http://www.w3.org/2000/svg"
     style="width:100%%;max-width:420px;display:block;margin:0 auto;">
  <!-- Background arc track -->
  <path d="M 30 155 A 130 130 0 0 1 290 155"
        fill="none" stroke="#1e3320" stroke-width="24" stroke-linecap="round"/>
  <!-- Colour zones -->
  %s
  <!-- Tick marks -->
  %s
  <!-- Scale labels -->
  <text x="24"  y="175" fill="#a5c8a0" font-size="9" text-anchor="middle">0</text>
  <text x="305" y="175" fill="#a5c8a0" font-size="9" text-anchor="end">%s</text>
  <!-- Needle shadow -->
  <line x1="%0.1f" y1="%0.1f" x2="%0.1f" y2="%0.1f"
        stroke="rgba(0,0,0,0.3)" stroke-width="5" stroke-linecap="round"/>
  <!-- Needle -->
  <line x1="%0.1f" y1="%0.1f" x2="%0.1f" y2="%0.1f"
        stroke="#f5f5f5" stroke-width="3" stroke-linecap="round"/>
  <!-- Hub -->
  <circle cx="%d" cy="%d" r="10" fill="#f5f5f5"/>
  <circle cx="%d" cy="%d" r="5"  fill="#4caf50"/>
  <!-- Value text -->
  <text x="%d" y="199" fill="#f5f5f5" font-size="27" font-weight="bold"
        text-anchor="middle" font-family="DM Mono,monospace">%s</text>
</svg>',
    arc_paths, ticks, max_fmt,
    cx + 2, cy + 2, nx + 2, ny + 2, # shadow
    cx, cy, nx, ny, # needle
    cx, cy, cx, cy, # hub
    cx, val_fmt, cx
  )
}


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=DM+Mono:wght@400;500&family=DM+Sans:wght@300;400;500;600&display=swap"
    ),
    tags$style(HTML("
      * { box-sizing: border-box; }
      body {
        background: #0d1f0f;
        font-family: 'DM Sans', Arial, sans-serif;
        color: #f5f5f5;
        margin: 0;
      }
      .header {
        background: #1a3d1f;
        padding: 18px 30px 14px;
        border-bottom: 2px solid #7ab648;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .header-title {
        font-size: 22px;
        font-weight: bold;
        color: #f5f5f5;
        letter-spacing: 0.5px;
      }
      .header-sub {
        font-size: 12px;
        color: #a5c8a0;
        margin-top: 3px;
      }
      .last-updated {
        font-size: 11px;
        color: #6a8f68;
        text-align: right;
      }
      .last-updated strong {
        color: #7ab648;
        font-family: 'DM Mono', monospace;
      }
      .main-wrap {
        padding: 28px 24px 40px;
        max-width: 1100px;
        margin: 0 auto;
      }
      .gauge-section {
        background: #132716;
        border-radius: 16px;
        padding: 30px 20px 20px;
        text-align: center;
        border: 1px solid rgba(122,182,72,0.12);
        margin-bottom: 24px;
      }
      .gauge-title {
        font-size: 14px;
        color: #a5d6a7;
        text-transform: uppercase;
        letter-spacing: 1px;
        margin-bottom: 10px;
        font-family: 'DM Mono', monospace;
      }
      .kpi-row {
        display: flex;
        gap: 18px;
        margin-bottom: 24px;
      }
      .kpi-card {
        flex: 1;
        background: #132716;
        border-radius: 14px;
        padding: 22px 16px 18px;
        text-align: center;
        border: 1px solid rgba(122,182,72,0.12);
        border-top: 2px solid;
      }
      .kpi-card.green  { border-top-color: #4caf50; }
      .kpi-card.yellow { border-top-color: #a5d6a7; }
      .kpi-icon  { font-size: 28px; margin-bottom: 8px; }
      .kpi-value {
        font-size: 30px;
        font-weight: 800;
        line-height: 1.1;
        margin-bottom: 6px;
        font-family: 'DM Mono', monospace;
      }
      .kpi-card.green  .kpi-value { color: #4caf50; }
      .kpi-card.yellow .kpi-value { color: #a5d6a7; }
      .kpi-label { font-size: 13px; color: #a5c8a0; }
      .kpi-sub   { font-size: 11px; color: #6a8f68; margin-top: 4px; }
      .chart-section {
        background: #132716;
        border-radius: 14px;
        padding: 24px 20px 16px;
        border: 1px solid rgba(122,182,72,0.12);
        margin-bottom: 24px;
      }
      .chart-title {
        font-size: 13px;
        color: #6a8f68;
        text-transform: uppercase;
        letter-spacing: 1px;
        margin-bottom: 16px;
        font-family: 'DM Mono', monospace;
      }
      .footnote {
        font-size: 11px;
        color: #6a8f68;
        text-align: center;
        margin-top: 30px;
        line-height: 1.7;
        font-family: 'DM Mono', monospace;
        border-top: 1px solid rgba(122,182,72,0.08);
        padding-top: 20px;
      }
    "))
  ),

  # Header
  div(
    class = "header",
    div(
      div(class = "header-title", "🌿 Bhutan EV Green Distance Tracker"),
      div(class = "header-sub", "Department of Surface Transport")
    ),
    div(
      class = "last-updated",
      "Data as of:", br(), strong(LAST_UPDATED)
    )
  ),
  div(
    class = "main-wrap",

    # Gauge
    div(
      class = "gauge-section",
      div(class = "gauge-title", "Cumulative Green Distance"),
      uiOutput("gauge_svg")
    ),

    # KPI cards
    div(
      class = "kpi-row",
      div(
        class = "kpi-card green",
        div(class = "kpi-icon", "🌱"),
        div(class = "kpi-value", textOutput("kpi_co2")),
        div(class = "kpi-label", "GHG Emissions Avoided"),
        div(class = "kpi-sub", "tonnes CO₂ vs. petrol ICE equivalent")
      ),
      div(
        class = "kpi-card yellow",
        div(class = "kpi-icon", "⛽"),
        div(class = "kpi-value", textOutput("kpi_fuel")),
        div(class = "kpi-label", "Fuel Import Avoided"),
        div(class = "kpi-sub", "Fuel import expenditure avoided")
      )
    ),

    # Trend chart
    div(
      class = "chart-section",
      div(class = "chart-title", "EV Population Trend"),
      plotOutput("plot_trend", height = "280px")
    ),

    # Footnote / assumptions
    div(
      class = "footnote",
      "Assumptions: Cars ", KM_CARS, " km/month · Buses ", KM_BUSES,
      " km/month · Two-wheelers ", KM_TWOWHEELERS, " km/month",
      br(),
      "Petrol price Nu. ", PETROL_PRICE, "/litre · CO₂ factor: ",
      CO2_PER_LITRE, " kg CO₂/litre (IPCC/IEA petrol)",
      br(),
      "Source: BCTA vehicle statistics · Methodology: petrol ICE displacement baseline"
    )
  )
)

# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  output$gauge_svg <- renderUI({
    HTML(make_gauge_svg(TOTAL_KM, GAUGE_MAX_KM))
  })

  output$kpi_co2 <- renderText({
    paste(format(round(TOTAL_CO2_T, 1), big.mark = ","), "t CO₂")
  })

  output$kpi_fuel <- renderText({
    if (TOTAL_FUEL_NU >= 1e6) {
      paste0("Nu. ", format(round(TOTAL_FUEL_NU / 1e6, 2), big.mark = ","), " M")
    } else {
      paste0("Nu. ", format(round(TOTAL_FUEL_NU), big.mark = ","))
    }
  })

  output$plot_trend <- renderPlot(
    {
      df <- EV_DATA %>%
        mutate(Period = factor(Period, levels = unique(EV_DATA$Period))) %>%
        select(Period, Fleet_Cars, Fleet_Buses, Fleet_Two) %>%
        pivot_longer(-Period, names_to = "Type", values_to = "Count") %>%
        mutate(Type = recode(Type,
          Fleet_Cars  = "Cars",
          Fleet_Buses = "Buses",
          Fleet_Two   = "Two-Wheelers"
        ))

      ggplot(df, aes(x = Period, y = Count, color = Type, fill = Type, group = Type)) +
        geom_area(alpha = 0.3, position = "identity", show.legend = FALSE) +
        geom_line(linewidth = 0.8, position = "identity") +
        scale_color_manual(values = c(
          "Cars"         = "#4caf50",
          "Buses"        = "#e8f5a3",
          "Two-Wheelers" = "#a5d6a7"
        )) +
        scale_fill_manual(values = c(
          "Cars"         = "#4caf50",
          "Buses"        = "#e8f5a3",
          "Two-Wheelers" = "#a5d6a7"
        )) +
        scale_y_continuous(labels = comma, name = "") +
        scale_x_discrete(
          breaks = levels(df$Period)[grepl("^Jan ", levels(df$Period))],
          labels = function(x) sub("Jan ", "", x)
        ) +
        coord_cartesian(clip = "off") +
        labs(x = NULL, color = NULL) +
        theme_minimal(base_size = 12) +
        theme(
          plot.background   = element_rect(fill = "#132716", color = NA),
          panel.background  = element_rect(fill = "#132716", color = NA),
          panel.grid.major  = element_line(color = "#1e3320"),
          panel.grid.minor  = element_blank(),
          axis.text         = element_text(color = "#a5c8a0"),
          axis.title        = element_text(color = "#6a8f68", size = 10),
          axis.text.x       = element_text(color = "#a5c8a0"),
          legend.background = element_rect(fill = "#132716", color = NA),
          legend.text       = element_text(color = "#a5c8a0"),
          plot.margin       = margin(5, 80, 5, 5)
        )
    },
    bg = "#132716"
  )
}


# APP ---------------------------------------------------------------------
shinyApp(ui, server)
