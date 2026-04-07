library(shiny)
library(bslib)
library(DT)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
femtech <- read.csv("data/femtech_africa.csv", stringsAsFactors = FALSE)

# Country coordinates for the map
country_coords <- data.frame(
  Country_Clean = c(
    "Nigeria", "Kenya", "South Africa", "Ghana", "Rwanda",
    "Cameroon", "Ethiopia", "Egypt", "Tunisia", "Zimbabwe",
    "Uganda", "Tanzania", "Democratic Republic of Congo"
  ),
  lat = c(9.08, -1.29, -30.56, 7.95, -1.94, 5.95, 9.15, 26.82, 33.89, -19.02, 1.37, -6.37, -4.04),
  lng = c(7.49, 36.82, 22.94, -1.02, 29.87, 10.15, 40.49, 30.80, 9.54, 29.15, 32.29, 34.89, 21.76)
)

# Parse countries (take first country for multi-country entries)
femtech$Country_Clean <- sapply(femtech$Country, function(x) {
  trimws(strsplit(x, "/|,")[[1]])[1]
})

# Simplify categories into broader groups for better charts
femtech$Category_Broad <- sapply(femtech$Category, function(x) {
  x_lower <- tolower(x)
  if (grepl("maternal|pregnancy|pregnan|motherhood|fertility|antenatal|obstetric", x_lower)) return("Maternal & Reproductive Health")
  if (grepl("menstrual|period|menopause|pelvic", x_lower)) return("Menstrual & Cycle Health")
  if (grepl("sexual|reproductive", x_lower)) return("Sexual & Reproductive Health")
  if (grepl("telehealth|telemedicine|virtual|digital health|healthcare", x_lower)) return("Telehealth & Digital Health")
  if (grepl("child|baby|parenting", x_lower)) return("Child & Parenting Health")
  if (grepl("cancer|screening|diagnostic", x_lower)) return("Screening & Diagnostics")
  if (grepl("e-commerce|pharmacy|supply|product", x_lower)) return("Health Products & E-commerce")
  if (grepl("wellness", x_lower)) return("Women's Wellness")
  if (grepl("ecosystem|community|education", x_lower)) return("Education & Community")
  return("Other Women's Health")
})

# Pastel theme colours
pastel_pink <- "#F8BBD0"
pastel_purple <- "#CE93D8"
pastel_blue <- "#90CAF9"
dark_text <- "#4A1942"
card_bg <- "#FFF0F5"

# Custom theme
femtech_theme <- bs_theme(
  version = 5,
  bg = "#FAFAFF",
  fg = dark_text,
  primary = "#B39DDB",
  secondary = pastel_pink,
  success = pastel_blue,
  info = pastel_purple,
  base_font = font_google("Nunito"),
  heading_font = font_google("Quicksand"),
  font_scale = 1.0
)

ui <- page_navbar(
  theme = femtech_theme,
  title = tags$span(
    style = "font-weight: 700; font-size: 1.3rem;",
    "FemTech Africa"
  ),
  bg = "#E1BEE7",
  id = "main_nav",
  header = tags$head(tags$style(HTML(sprintf("
    body { background: linear-gradient(135deg, %s 0%%, %s 50%%, %s 100%%); min-height: 100vh; }
    .navbar { background: linear-gradient(90deg, %s, %s, %s) !important; border-bottom: 3px solid %s; }
    .navbar .nav-link { color: %s !important; font-weight: 600; }
    .navbar .nav-link:hover, .navbar .nav-link.active { color: white !important; background-color: rgba(255,255,255,0.2) !important; border-radius: 8px; }
    .navbar-brand { color: %s !important; }
    .card { border: none; border-radius: 16px; box-shadow: 0 4px 15px rgba(179, 157, 219, 0.2); background: %s; }
    .card-header { background: linear-gradient(90deg, %s, %s) !important; color: white; border-radius: 16px 16px 0 0 !important; font-weight: 600; }
    .value-box { border-radius: 16px !important; }
    .dataTables_wrapper { font-size: 0.9rem; }
    table.dataTable thead th { background: linear-gradient(90deg, %s, %s) !important; color: white !important; }
    .btn-primary { background: linear-gradient(90deg, %s, %s) !important; border: none; border-radius: 20px; }
    .btn-primary:hover { background: linear-gradient(90deg, %s, %s) !important; }
    .selectize-input { border-radius: 12px !important; border: 2px solid %s !important; }
    .hero-section { background: linear-gradient(135deg, %s, %s, %s); border-radius: 20px; padding: 2rem; margin-bottom: 1.5rem; color: white; text-align: center; }
    .hero-section h2 { font-weight: 700; margin-bottom: 0.5rem; }
    .hero-section p { font-size: 1.1rem; opacity: 0.9; }
    .stat-card { background: white; border-radius: 16px; padding: 1.5rem; text-align: center; box-shadow: 0 4px 15px rgba(179, 157, 219, 0.15); transition: transform 0.2s; }
    .stat-card:hover { transform: translateY(-5px); }
    .stat-number { font-size: 2.5rem; font-weight: 700; background: linear-gradient(90deg, %s, %s); -webkit-background-clip: text; -webkit-text-fill-color: transparent; }
    .stat-label { color: %s; font-weight: 600; font-size: 0.95rem; margin-top: 0.3rem; }
    .leaflet-container { border-radius: 16px; }
    .company-card { background: white; border-radius: 12px; padding: 1rem; margin-bottom: 0.8rem; border-left: 4px solid %s; box-shadow: 0 2px 8px rgba(0,0,0,0.06); }
    .company-card h5 { color: %s; margin-bottom: 0.3rem; }
    .company-card .badge { border-radius: 20px; font-weight: 500; }
    .badge-category { background-color: %s; color: white; }
    .badge-country { background-color: %s; color: white; }
    .badge-funded { background-color: %s; color: white; }
    footer { text-align: center; padding: 1rem; color: %s; font-size: 0.85rem; }
  ",
    pastel_pink, pastel_purple, pastel_blue,
    "#CE93D8", "#B39DDB", "#90CAF9",
    pastel_pink,
    dark_text,
    dark_text,
    card_bg,
    "#B39DDB", "#90CAF9",
    "#CE93D8", "#B39DDB",
    "#B39DDB", "#CE93D8",
    "#CE93D8", "#B39DDB",
    pastel_purple,
    "#CE93D8", "#B39DDB", "#90CAF9",
    "#B39DDB", "#CE93D8",
    dark_text,
    pastel_purple,
    dark_text,
    "#B39DDB", "#90CAF9", "#81C784",
    dark_text
  )))),

  # --- Overview Tab ---
  nav_panel(
    title = "Overview",
    icon = icon("home"),
    div(
      class = "hero-section",
      h2("Mapping FemTech Innovation Across Africa"),
      p("Exploring the startups and solutions transforming women's health on the continent")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      div(class = "stat-card",
        div(class = "stat-number", textOutput("total_startups", inline = TRUE)),
        div(class = "stat-label", "Total Startups")
      ),
      div(class = "stat-card",
        div(class = "stat-number", textOutput("total_countries", inline = TRUE)),
        div(class = "stat-label", "Countries")
      ),
      div(class = "stat-card",
        div(class = "stat-number", textOutput("total_funded", inline = TRUE)),
        div(class = "stat-label", "Funded")
      ),
      div(class = "stat-card",
        div(class = "stat-number", textOutput("total_categories", inline = TRUE)),
        div(class = "stat-label", "Categories")
      )
    ),
    br(),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Startups by Region"),
        card_body(plotOutput("region_chart", height = "350px"))
      ),
      card(
        card_header("Startups by Category"),
        card_body(plotOutput("category_chart", height = "350px"))
      )
    ),
    br(),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Funding Status"),
        card_body(plotOutput("funding_chart", height = "300px"))
      ),
      card(
        card_header("Platform Types"),
        card_body(plotOutput("platform_chart", height = "300px"))
      )
    )
  ),

  # --- Map Tab ---
  nav_panel(
    title = "Map",
    icon = icon("map"),
    card(
      card_header("African FemTech Startups Map"),
      card_body(
        leafletOutput("map", height = "600px")
      )
    )
  ),

  # --- Database Tab ---
  nav_panel(
    title = "Database",
    icon = icon("database"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 280,
        bg = card_bg,
        selectInput("filter_region", "Region", choices = c("All", sort(unique(femtech$Region))), selected = "All"),
        selectInput("filter_country", "Country", choices = c("All", sort(unique(femtech$Country_Clean))), selected = "All"),
        selectInput("filter_category", "Category", choices = c("All", sort(unique(femtech$Category))), selected = "All"),
        selectInput("filter_funding", "Funding Status", choices = c("All", sort(unique(femtech$Funding_Status))), selected = "All"),
        hr(),
        downloadButton("download_data", "Download CSV", class = "btn-primary w-100")
      ),
      card(
        card_header("FemTech Startups Database"),
        card_body(
          DTOutput("data_table")
        )
      )
    )
  ),

  # --- Directory Tab ---
  nav_panel(
    title = "Directory",
    icon = icon("building"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Search",
        width = 280,
        bg = card_bg,
        textInput("search_name", "Search by Name", placeholder = "e.g. Babymigo"),
        selectInput("dir_region", "Filter by Region", choices = c("All", sort(unique(femtech$Region))), selected = "All"),
        selectInput("dir_category", "Filter by Category", choices = c("All", sort(unique(femtech$Category))), selected = "All")
      ),
      uiOutput("directory_cards")
    )
  ),

  # --- About Tab ---
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("About This Project"),
        card_body(
          h4("FemTech Africa Database", style = sprintf("color: %s;", dark_text)),
          p("This interactive dashboard maps the growing ecosystem of female-focused health technology (FemTech) startups across Africa."),
          p("FemTech encompasses technology solutions tailored to women's health needs including maternal care, reproductive health, menstrual health, fertility, breast cancer screening, and general women's wellness."),
          h5("Key Findings", style = sprintf("color: %s;", dark_text)),
          tags$ul(
            tags$li("Africa has a rapidly growing femtech ecosystem with startups across all regions"),
            tags$li("West Africa (particularly Nigeria) and East Africa (particularly Kenya) lead in femtech innovation"),
            tags$li("~81% of funded African femtech startups focus on fertility and maternal health"),
            tags$li("Significant gaps remain in menopause, pelvic health, and endometriosis solutions"),
            tags$li("Mobile and SMS-based platforms dominate, reflecting Africa's mobile-first digital landscape")
          ),
          h5("Data Sources", style = sprintf("color: %s;", dark_text)),
          p("Data compiled from Tracxn, TechCabal, Jaza Rift, Africa Tech Festival, HealthTech Hub Africa, Built in Africa, and various startup databases and news sources."),
          p(tags$em("Last updated: April 2026"))
        )
      ),
      card(
        card_header("Get Involved"),
        card_body(
          h5("Know a startup we missed?"),
          p("The African femtech ecosystem is growing rapidly. If you know of a startup that should be included in this database, please reach out."),
          br(),
          h5("Resources"),
          tags$ul(
            tags$li(tags$a(href = "https://www.femhealthafrica.com/", "FemHealth Africa", target = "_blank")),
            tags$li(tags$a(href = "https://jazarift.com/femtech-2-mapping-out-femtech-solutions-in-africa/", "Jaza Rift - Mapping FemTech in Africa", target = "_blank")),
            tags$li(tags$a(href = "https://thehealthtech.org/", "HealthTech Hub Africa", target = "_blank"))
          )
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$span(style = "color: white; font-size: 0.85rem; opacity: 0.8;", "Built with R Shiny")
  )
)

server <- function(input, output, session) {

  # --- Overview Stats ---
  output$total_startups <- renderText(nrow(femtech))
  output$total_countries <- renderText(length(unique(femtech$Country_Clean)))
  output$total_funded <- renderText(sum(grepl("Funded|Grant|Series", femtech$Funding_Status, ignore.case = TRUE)))
  output$total_categories <- renderText(length(unique(femtech$Category)))

  # --- Charts ---
  pastel_palette <- c("#F8BBD0", "#CE93D8", "#B39DDB", "#90CAF9", "#80DEEA", "#A5D6A7", "#FFCC80", "#EF9A9A")

  base_theme <- theme_minimal(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      text = element_text(colour = dark_text),
      axis.text = element_text(colour = dark_text, size = 10),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "#E0E0E0")
    )

  output$region_chart <- renderPlot({
    region_data <- femtech %>%
      count(Region) %>%
      arrange(desc(n))

    ggplot(region_data, aes(x = reorder(Region, n), y = n, fill = Region)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = n), hjust = -0.3, fontface = "bold", colour = dark_text, size = 4.5) +
      coord_flip() +
      scale_fill_manual(values = rep(pastel_palette, length.out = nrow(region_data))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      base_theme
  }, bg = "transparent")

  output$category_chart <- renderPlot({
    cat_data <- femtech %>%
      count(Category_Broad) %>%
      arrange(desc(n)) %>%
      rename(Category = Category_Broad) %>%
      head(10)

    ggplot(cat_data, aes(x = reorder(Category, n), y = n, fill = Category)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = n), hjust = -0.3, fontface = "bold", colour = dark_text, size = 4.5) +
      coord_flip() +
      scale_fill_manual(values = rep(pastel_palette, length.out = nrow(cat_data))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      base_theme +
      theme(axis.text.y = element_text(size = 9))
  }, bg = "transparent")

  output$funding_chart <- renderPlot({
    fund_data <- femtech %>%
      mutate(Funding_Simple = case_when(
        grepl("Funded|Series", Funding_Status) ~ "Funded",
        grepl("Grant|NGO|Government", Funding_Status) ~ "Grant/Gov",
        grepl("Acquired", Funding_Status) ~ "Acquired",
        TRUE ~ "Not Disclosed"
      )) %>%
      count(Funding_Simple) %>%
      arrange(desc(n))

    ggplot(fund_data, aes(x = "", y = n, fill = Funding_Simple)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(Funding_Simple, "\n(", n, ")")),
                position = position_stack(vjust = 0.5), colour = "white", fontface = "bold", size = 3.5) +
      scale_fill_manual(values = c("#B39DDB", "#90CAF9", "#F8BBD0", "#CE93D8")) +
      theme_void() +
      theme(legend.position = "none")
  }, bg = "transparent")

  output$platform_chart <- renderPlot({
    plat_labels <- c("Mobile App", "SMS", "Web", "Telehealth", "Medical Device", "E-commerce", "Community", "Clinics")
    plat_counts <- sapply(plat_labels, function(p) sum(grepl(p, femtech$Platform, ignore.case = TRUE)))
    plat_data <- data.frame(Platform = plat_labels, Count = plat_counts) %>%
      filter(Count > 0) %>%
      arrange(desc(Count))

    ggplot(plat_data, aes(x = reorder(Platform, Count), y = Count, fill = Platform)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = Count), hjust = -0.3, fontface = "bold", colour = dark_text, size = 4.5) +
      coord_flip() +
      scale_fill_manual(values = rep(pastel_palette, length.out = nrow(plat_data))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      base_theme
  }, bg = "transparent")

  # --- Map ---
  output$map <- renderLeaflet({
    map_data <- femtech %>%
      group_by(Country_Clean) %>%
      summarise(
        count = n(),
        companies = paste(Name, collapse = ", "),
        .groups = "drop"
      ) %>%
      left_join(country_coords, by = "Country_Clean") %>%
      filter(!is.na(lat))

    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 20, lat = 5, zoom = 3) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~sqrt(count) * 8,
        color = "#B39DDB",
        fillColor = "#CE93D8",
        fillOpacity = 0.7,
        weight = 2,
        popup = ~paste0(
          "<div style='font-family: Nunito, sans-serif; min-width: 200px;'>",
          "<h4 style='color: #4A1942; margin-bottom: 5px;'>", Country_Clean, "</h4>",
          "<p style='color: #B39DDB; font-weight: 700; font-size: 1.2rem; margin: 0;'>", count, " startup(s)</p>",
          "<hr style='border-color: #F8BBD0;'>",
          "<p style='font-size: 0.85rem; color: #555;'>", companies, "</p>",
          "</div>"
        )
      )
  })

  # --- Database Table ---
  filtered_data <- reactive({
    df <- femtech
    if (input$filter_region != "All") df <- df %>% filter(Region == input$filter_region)
    if (input$filter_country != "All") df <- df %>% filter(Country_Clean == input$filter_country)
    if (input$filter_category != "All") df <- df %>% filter(Category == input$filter_category)
    if (input$filter_funding != "All") df <- df %>% filter(Funding_Status == input$filter_funding)
    df
  })

  output$data_table <- renderDT({
    df <- filtered_data() %>%
      mutate(
        Website = ifelse(
          !is.na(Website) & Website != "",
          paste0('<a href="', Website, '" target="_blank" style="color: #B39DDB;">Visit</a>'),
          ""
        )
      ) %>%
      select(Name, Country, Region, Category, Description, Website, Year_Founded, Funding_Status, Platform)

    datatable(
      df,
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "frtip",
        language = list(search = "Search:")
      ),
      rownames = FALSE,
      colnames = c("Name", "Country", "Region", "Category", "Description", "Website", "Founded", "Funding", "Platform")
    )
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("femtech_africa_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )

  # --- Directory Cards ---
  dir_filtered <- reactive({
    df <- femtech
    if (input$search_name != "") {
      df <- df %>% filter(grepl(input$search_name, Name, ignore.case = TRUE))
    }
    if (input$dir_region != "All") df <- df %>% filter(Region == input$dir_region)
    if (input$dir_category != "All") df <- df %>% filter(Category == input$dir_category)
    df
  })

  output$directory_cards <- renderUI({
    df <- dir_filtered()
    if (nrow(df) == 0) {
      return(div(
        style = "text-align: center; padding: 3rem; color: #999;",
        icon("search", style = "font-size: 2rem;"),
        h4("No startups found matching your filters")
      ))
    }

    card_list <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      website_link <- if (!is.na(row$Website) && row$Website != "") {
        tags$a(href = row$Website, target = "_blank",
               style = "color: #B39DDB; font-weight: 600; text-decoration: none;",
               icon("external-link-alt"), " Website")
      } else {
        NULL
      }

      div(
        class = "company-card",
        div(
          style = "display: flex; justify-content: space-between; align-items: flex-start;",
          div(
            h5(row$Name),
            span(class = "badge badge-country", style = "background-color: #90CAF9; margin-right: 4px;", row$Country_Clean),
            span(class = "badge badge-category", style = "background-color: #B39DDB; margin-right: 4px;", row$Category),
            if (grepl("Funded|Series|Grant", row$Funding_Status)) {
              span(class = "badge badge-funded", style = "background-color: #81C784;", row$Funding_Status)
            }
          ),
          website_link
        ),
        p(style = "margin-top: 0.5rem; font-size: 0.9rem; color: #666;", row$Description),
        if (!is.na(row$Year_Founded) && row$Year_Founded != "") {
          tags$small(style = "color: #999;", paste("Founded:", row$Year_Founded))
        }
      )
    })

    tagList(
      p(style = "color: #999; font-size: 0.9rem; margin-bottom: 1rem;",
        paste("Showing", nrow(df), "startup(s)")),
      card_list
    )
  })
}

shinyApp(ui = ui, server = server)
