library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(htmltools)

# ─── Data ────────────────────────────────────────────────────────────────────

study_locations <- data.frame(
  country   = c("Brazil", "Colombia", "Mexico", "Chile", "Singapore"),
  n_studies = c(28, 11, 12, 1, 8),
  lat       = c(-14.2, 4.7, 23.6, -35.7, 1.35),
  lng       = c(-51.9, -74.1, -102.5, -71.6, 103.82),
  region    = c("Latin America", "Latin America", "Latin America",
                "Latin America", "SIDS"),
  color     = c("#2dd4bf", "#2dd4bf", "#2dd4bf", "#2dd4bf", "#f59e0b"),
  stringsAsFactors = FALSE
)

be_domains <- data.frame(
  domain       = c("Land Use", "Traffic Safety", "Connectivity",
                   "Density", "Greenspace", "Community",
                   "Experience", "Surveillance", "Parking"),
  relationships = c(82, 56, 49, 40, 28, 19, 15, 7, 0),
  stringsAsFactors = FALSE
) |> arrange(desc(relationships))

be_pa_matrix <- data.frame(
  domain      = rep(c("Surveillance","Experience","Traffic Safety","Community",
                      "Greenspace","Density","Connectivity","Land Use"), 3),
  pa_type     = rep(c("Active Transport","Leisure-time PA","MVPA"), each = 8),
  n_relations = c(
    2, 4, 19, 5, 4, 13, 17, 27,
    2, 7, 10, 8, 16, 8, 14, 27,
    0, 2,  6, 2,  6, 3,  5, 15
  ),
  stringsAsFactors = FALSE
)

direction_data <- data.frame(
  domain    = rep(c("Surveillance","Experience","Traffic Safety","Community",
                    "Greenspace","Density","Connectivity","Land Use"), 3),
  pa_type   = rep(c("Active Transport","Leisure-time PA","MVPA"), each = 8),
  positive  = c(1,3,8,2,3,11,12,20,
                1,3,4,3,11,2,5,10,
                0,0,6,2,6,1,3,8),
  negative  = c(1,1,14,3,1,2,8,7,
                1,4,6,1,3,2,6,4,
                0,2,0,0,1,2,1,2),
  stringsAsFactors = FALSE
)

study_chars <- data.frame(
  attribute = c("Cross-Sectional","Longitudinal","Brazil","Colombia",
                "Mexico","Chile","Singapore","Adult","Children/Adolescents",
                "Mixed Age"),
  category  = c("Design","Design","Country","Country","Country","Country",
                "Country","Age Group","Age Group","Age Group"),
  value     = c(50, 1, 28, 11, 12, 1, 8, 33, 9, 7),
  stringsAsFactors = FALSE
)

pa_types <- data.frame(
  type  = c("Active Commuting","Moderate-Vigorous PA","Recreational Walking",
            "Leisure-time PA","General Walking","General Cycling"),
  count = c(29, 15, 12, 11, 8, 5),
  stringsAsFactors = FALSE
) |> arrange(desc(count))

be_features <- data.frame(
  feature = c("Residential/Population Density","Street Connectivity",
              "Mixed Land Use","Public Spaces/Parks","Recreation Proximity",
              "Transit Stops","Trails/Sidewalks","Non-recreational Land Use",
              "Street Lighting/Shade","Walkability Index"),
  count   = c(32, 32, 32, 31, 12, 19, 15, 8, 6, 4),
  stringsAsFactors = FALSE
) |> arrange(desc(count))

included_studies <- data.frame(
  Author_Year       = c(
    "Andrade, 2019","Bautista-Hernandez, 2021","Bojorquez, 2018","Borjorquez, 2021",
    "Borchardt, 2019","Cerin, 2017","Cerin, 2018","Chen, 2020","Christiansen, 2016",
    "Custódio, 2021","Da Silva, 2017","Dias, 2019","Dias, 2020","Dias, 2021",
    "Faerstein, 2018","Florindo, 2019","Giehl, 2016","Gomes, 2016","Gomes, 2021",
    "Gomez, 2010","Gomez, 2010a","Gonzalez, 2020","Guerra, 2018","Guerra, 2021",
    "Guzman, 2020","Higuera-Mendieta, 2021","Hino, 2011","Hino, 2012","Hino, 2013",
    "Hino, 2019","Hou, 2019","Hou, 2020","Larranaga, 2016","Lee, 2016","Lim, 2017",
    "Lopes, 2018","Mello, 2020","Mo, 2018","Nakamura, 2016","Neves, 2021","Nyunt, 2015",
    "Oliva, 2018","Parra, 2010","Pentrunoff, 2021","Rosas-Satizabal, 2020","Sallis, 2016",
    "Salvo, 2014","Schipperijn, 2017","Silva, 2020","Siqueirra, 2013","Song, 2020"
  ),
  Country           = c(
    "Brazil","Mexico","Mexico","Mexico","Brazil","Mexico/Brazil/Colombia","Mexico/Brazil/Colombia",
    "Singapore","Mexico/Brazil/Colombia","Brazil","Brazil","Brazil","Brazil","Brazil","Brazil",
    "Brazil","Brazil","Brazil","Brazil","Colombia","Colombia","Colombia","Mexico","Mexico",
    "Colombia","Colombia","Brazil","Brazil","Brazil","Brazil","Singapore","Singapore","Brazil",
    "Mexico","Singapore","Brazil","Brazil","Singapore","Brazil","Brazil","Singapore","Chile",
    "Colombia","Singapore","Colombia","Mexico/Brazil/Colombia","Mexico","Mexico/Brazil","Brazil",
    "Brazil","Singapore"
  ),
  Population        = c(
    "Adult","Adult/Children/Adolescents","Adult","Adult","Adult","Adult","Adult",
    "Children/Adolescents","Adult","Adult","Children/Adolescents","Children/Adolescents",
    "Children/Adolescents","Children/Adolescents","Adult","Adult","Adult","Adult","Adult",
    "Adult","Adult","Children/Adolescents","Adult/City","Adult","Adult/Children/Adolescents",
    "Adult/Adolescents","Adult/Children/Adolescents","Adult","Adult","Adult","Adult","Adult",
    "Adult/Children/Adolescents","Children/Adolescents","Adult","Adult","Children/Adolescents",
    "Adult","Adult","Adult","Adult","Adult","Adult","Adult","Adult/Children/Adolescents",
    "Adult","Adult","Adult","Adult/Children/Adolescents","Adult","Adult"
  ),
  Design            = c(
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional",
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional",
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Longitudinal",
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional",
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional",
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional",
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional",
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional",
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional",
    "Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional","Cross-sectional",
    "Cross-sectional"
  ),
  Sample_Size       = c(
    3815,64355,2345,2928,1300,6712,6712,73,11674,2591,5249,1130,1113,1123,1731,3145,
    12911,5779,5779,1966,1315,2845,2460000,2435133,21378,16495,1206,699,2097,699,25922,
    900,442,1321,1972,1419,236,10000,1588,15775,402,1487,1966,3435,968,14222,662,6181,
    493,697,810
  ),
  BE_Features       = c(
    "Mixed Land-Use; Public Space/Parks; Trails/sidewalks; Pedestrian amenities",
    "Residential Density; Street Connectivity; Mixed Land-Use; Transit proximity",
    "Public Space/Parks; Recreational land use proximity",
    "Mixed Land-Use; Public Space/Parks; Slope; Pedestrian facilities",
    "Residential Density; Public Space/Parks; Recreation proximity; Transit proximity; Pedestrian amenities",
    "Residential Density; Street Connectivity; Mixed Land-Use; Retail Floor; Public Space/Parks; Recreation proximity; Non-recreational land use; Transit proximity",
    "Residential Density; Street Connectivity; Mixed Land-Use; Retail Floor; Public Space/Parks; Recreation proximity; Non-recreational land use; Transit proximity",
    "Pedestrian amenities (street lighting/shade/furniture)",
    "Residential Density; Street Connectivity; Mixed Land-Use; Public Space/Parks",
    "Mixed Land-Use",
    "Residential Density; Street Connectivity; Public Space/Parks; Recreation proximity; Trails/sidewalks; Pedestrian amenities",
    "Residential Density; Street Connectivity; Public Space/Parks; Recreation proximity; Trails/sidewalks",
    "Residential Density; Street Connectivity; Public Space/Parks; Walkability index",
    "Residential Density; Street Connectivity",
    "Street Connectivity; Public Space/Parks; Recreation proximity; Non-recreational land use; Trails/sidewalks",
    "Mixed Land Use; Public Space/Parks; Non-recreational land use; Transit proximity; Trails/sidewalks",
    "Residential Density; Street Connectivity; Mixed Land-Use; Public Space/Parks; Trails/sidewalks; Pedestrian amenities",
    "Residential Density; Public Space/Parks; Recreation proximity",
    "Residential Density; Mixed Land-Use; Public Space/Parks",
    "Street Connectivity; Mixed Land Use; Public Space/Parks; Recreation proximity; Transit proximity",
    "Residential Density; Mixed Land-Use; Public Space/Parks; Transit proximity; Trails/sidewalks",
    "Non-recreational land use",
    "Residential Density; Street Connectivity; Urban sprawl/form",
    "Residential Density; Street Connectivity",
    "Residential Density; Street Connectivity; Mixed Land-use; Public Space/Parks",
    "Street Connectivity; Mixed Land-use; Transit proximity; Trails/sidewalks",
    "Residential Density; Public Space/Parks; Recreation proximity; Recreational facility density",
    "Residential Density; Street Connectivity; Mixed Land-Use",
    "Residential Density; Street Connectivity; Mixed Land-Use; Recreation proximity; Non-recreational land use",
    "Public Space/Parks",
    "Residential Density; Street Connectivity; Mixed Land-Use; Public Space/Parks; Transit proximity; Trails/sidewalks",
    "Residential Density; Street Connectivity; Public Space/Parks; Transit proximity; Pedestrian amenities",
    "Residential Density; Street Connectivity; Mixed Land-Use; Transit proximity",
    "Residential Density; Street Connectivity; Mixed Land-Use; Trails/sidewalks",
    "Mixed Land Use; Public Space/Parks; Transit proximity",
    "Street Connectivity; Mixed Land Use; Transit proximity",
    "Residential Density; Street Connectivity; Mixed Land-Use; Non-recreational land use; Walkability index",
    "Residential Density; Mixed Land-Use; Transit proximity",
    "Residential Density; Mixed Land-Use",
    "Residential Density; Street Connectivity; Mixed Land-Use; Transit proximity",
    "Residential Density; Street Connectivity; Mixed Land-Use; Public Space/Parks",
    "Residential Density; Street Connectivity; Mixed Land Use; Retail Floor; Public Space/Parks; Transit proximity; Trails/sidewalks",
    "Street Connectivity; Mixed Land Use; Public Space/Parks",
    "Public Space/Parks",
    "Non-recreational land use",
    "Residential Density; Street Connectivity; Mixed Land-Use; Public Space/Parks; Transit proximity; Trails/sidewalks",
    "Residential Density; Street Connectivity; Mixed Land-Use; Public Space/Parks; Transit proximity; Walkability index",
    "Public Space/Parks",
    "Trails/sidewalks; Pedestrian amenities",
    "Residential Density; Mixed Land-Use",
    "Public Space/Parks; Transit proximity; Trails/sidewalks"
  ),
  PA_Type           = c(
    "Recreational walking","Transportation walking; General cycling","Recreational walking; MVPA",
    "Transportation walking; Leisure-time PA; Total PA","Recreational walking; Transportation walking; MVPA",
    "MVPA","MVPA","MVPA","Transportation walking; General cycling",
    "MVPA","Recreational & General walking; MVPA; Active Transport","Transportation walking/cycling to school",
    "Leisure-time PA","Transportation walking","Any physical activity","Transportation walking",
    "Recreational walking; Transportation walking; General walking",
    "Recreational walking; General walking","Leisure-time PA",
    "Recreational walking; General walking","Recreational walking/Leisure-time PA",
    "Transportation walking","Transportation walking; Commuting biking","Transportation walking",
    "Transportation walking","Bicycle commuting","Recreational walking",
    "MVPA","Transportation walking","MVPA",
    "Transportation walking","Transportation walking","Transportation & General walking",
    "Frequency of outdoor play","General walking & Sports participation",
    "Transportation walking; Cycling","Transportation walking; General walking",
    "Transportation walking","Recreational walking; MVPA","Transportation walking",
    "Transportation walking","Cycling for transport","Frequency of active park use",
    "Leisure-time PA","General cycling/biking","MVPA",
    "MVPA","Recreational walking; MVPA","Transportation walking","Recreational walking; Transportation walking; MVPA",
    "Recreational walking; Transportation walking"
  ),
  Main_Finding      = c(
    "Individuals in census tracts with higher walking environment indicators were more likely to be active during leisure time.",
    "Factors associated with bike use were distance to centre, density of mass-transit stations, street intersection density, and flat surface.",
    "No interaction between access to public spaces and quality in their effect on PA. Presence of public spaces in 400m buffer associated with lower PA.",
    "No quantitative association between access to public spaces and PA was found.",
    "Only proximity to the seafront, private gyms/sports clubs and higher average income were associated with PA.",
    "Time of day, day of week, gender and employment status were significant moderators of environment-MVPA associations.",
    "Objective net residential density, public transport density, and number of parks were consistently associated with MVPA.",
    "MVPA was similar throughout the week. More children engaged in MVPA outdoors than indoors.",
    "Positive associations of walking and cycling for transport with all environmental features.",
    "Women and those who use public open spaces during the week or in the morning are more likely to practice MVPA.",
    "Street lighting positively associated with MVPA; paved streets and family income associated with lower MVPA; proximity to beach increased leisure-time MVPA.",
    "Neighbourhood recreation facilities, bicycle paths and residential density associated with active commuting to school.",
    "Land use mix, neighbourhood recreation facilities and places for walking positively associated with leisure walking.",
    "Residential density mediates the association between active commuting and perceived environmental factors.",
    "Participants living closest to waterfronts had 2.6x higher odds of reporting non-work PA across all study waves.",
    "Presence of public transportation stations and destinations mix significantly associated with walking for transport >=150 min/week.",
    "Higher population density, street connectivity, sidewalk proportion and paved streets associated with more transportation walking.",
    "Increased density of private PA places and lower homicide rates increased leisure-time physical activity.",
    "Higher density of PA places, population density, and family income clustered together.",
    "Residents in areas with middle park area more likely to walk >=60 min/week. Feeling safe from traffic also associated with walking.",
    "Residents in neighbourhoods with highest park area more likely to be regularly active.",
    "Greater distance to school and vehicle ownership associated with lower likelihood of active school transport.",
    "Commuters less likely to drive in areas with better public transit. Urban form as strongly related to car commuting as household income.",
    "Urban residents in denser, more centrally located housing less likely to commute by private vehicle.",
    "Areas with low land-use mix and job/population ratio have longer walking distances; policies needed to encourage higher land-use mix.",
    "Bike paths at trip destination positively associated with cycling for women; living in lowest SES areas also positively associated.",
    "Leisure-time walking associated with area income and having >=2 gyms nearby. MVPA associated with neighbourhood income and gyms.",
    "16 high-walkability sectors included in study; walkability defined by density and income strata.",
    "Higher BRT station density and proportion of residential/commercial areas associated with walking. Higher bike path access inversely associated.",
    "Number of public leisure spaces within 1,000m positively associated with MVPA; those within 500m negatively associated with walking.",
    "Better access to planned urban centres associated with more walking trips for some older age groups; density and connectivity increase walking for all adults.",
    "Perceived access to recreational facilities exerts independent positive effects on daily walking and transit trips in older adults.",
    "Only population density had a marginally higher effect on walking frequency than other environmental variables.",
    "Fewer path obstructions and more pedestrian amenities associated with outdoor play. Walkability negatively associated with all physical activities.",
    "Greater distance to train station associated with lower odds of sports participation.",
    "Medium streetscape score inversely associated with walking >=150 min/week and bicycling.",
    "Active commuting to school associated with residence distance to school and walkability; association only in girls.",
    "Built environment - especially distance to MRT station, land-use mix and socioeconomic activities - significantly influences first/last-mile travel.",
    "Walking during leisure-time negatively associated with very high population density. Different associations found for various PA intensities.",
    "Built environment variables more relevant at origin; diversity most related to walking choice in Sao Paulo.",
    "Objective GIS Accessibility Index positively and independently associated with transportation physical activity.",
    "Residential density and bike lanes length have a positive effect on cycling commuting at the origin.",
    "Residents from areas with higher park density and high land use mix more likely to report active park use.",
    "Better perceived (not objective) park access significantly associated with greater park use.",
    "Marked differences in potential accessibility to work/study opportunities between clusters.",
    "Four of six environmental features significantly and positively related to physical activity.",
    "Walkability inversely related to total weekly minutes of MVPA.",
    "More parks within 1 km associated with greater leisure-time PA and accelerometer-measured MVPA.",
    "Presence of safety signs inversely associated with active commuting. Traffic safety and distance to school associated with active commuting.",
    "Walkability showed an independent association with walking for transport and leisure-time MVPA.",
    "Accessibility to destinations and walkability features promote transportation or recreational PA. Built environment features related differently to transportation vs recreational PA."
  ),
  stringsAsFactors = FALSE
)

# Add Year and Unit_of_Analysis columns
included_studies$Year <- as.integer(
  sub(".*,\\s*(\\d{4}).*", "\\1", included_studies$Author_Year)
)
included_studies$Unit_of_Analysis <- c(
  "Individual","Individual","Individual","Individual","Individual",
  "Individual","Individual","Individual","Individual","Individual",
  "Individual","Individual","Individual","Individual","Individual",
  "Individual","Individual","Individual","Individual","Individual",
  "Individual","Individual","City/Area","City/Area","City/Area",
  "Individual","Individual","Individual","Individual","Individual",
  "Individual","Individual","Individual","Individual","Individual",
  "Individual","Individual","Individual","Individual","Individual",
  "Individual","Individual","Individual","Individual","Individual",
  "Individual","Individual","Individual","Individual","Individual",
  "Individual"
)
included_studies <- included_studies[, c(
  "Author_Year","Year","Country","Population","Design",
  "Unit_of_Analysis","Sample_Size","BE_Features","PA_Type","Main_Finding"
)]

key_findings <- list(
  list(icon="🏙️", title="Land Use Dominates",
       text="Land use mix had the most BE-PA relationships (82), with mixed commercial, residential and educational zones linked to increased PA across all domains."),
  list(icon="🌿", title="Green Spaces for Leisure",
       text="Presence and proximity to outdoor greenspaces showed the strongest positive associations (11 positive vs 3 negative) with leisure-time PA."),
  list(icon="🚶", title="Density Drives Commuting",
       text="Higher residential density had the clearest positive association with active commuting (11 positive vs 2 negative relationships)."),
  list(icon="🚦", title="Traffic Safety Mixed",
       text="Traffic safety features had the highest ratio of negative to positive relationships for active transport (14 negative vs 8 positive), highlighting road safety concerns."),
  list(icon="📍", title="Evidence Gap",
       text="Zero published studies from Caribbean SIDS met inclusion criteria. Singapore was the only SIDS represented, with important economic and structural differences from the Caribbean."),
  list(icon="🔬", title="Study Design Bias",
       text="50 of 51 included studies were cross-sectional, limiting causal inference. Only 2 studies adjusted for neighbourhood self-selection bias.")
)

# ─── Palettes & helpers ──────────────────────────────────────────────────────

TEAL   <- "#2dd4bf"
AMBER  <- "#f59e0b"
CORAL  <- "#fb7185"
SLATE  <- "#334155"
DARK   <- "#0f172a"
MID    <- "#1e293b"
LIGHT  <- "#f8fafc"
MUTED  <- "#94a3b8"

# ─── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700;900&family=IBM+Plex+Sans:wght@300;400;500;600&family=IBM+Plex+Mono&display=swap",
              rel = "stylesheet"),
    tags$style(HTML(paste0("
      *, *::before, *::after { box-sizing: border-box; margin:0; padding:0; }

      :root {
        --teal:  ", TEAL,  ";
        --amber: ", AMBER, ";
        --coral: ", CORAL, ";
        --slate: ", SLATE, ";
        --dark:  ", DARK,  ";
        --mid:   ", MID,   ";
        --light: ", LIGHT, ";
        --muted: ", MUTED, ";
      }

      html, body { background: var(--dark); color: var(--light);
                   font-family: 'IBM Plex Sans', sans-serif;
                   font-weight: 300; line-height: 1.7; }

      /* ── Scrollbar ── */
      ::-webkit-scrollbar { width: 6px; }
      ::-webkit-scrollbar-track { background: var(--dark); }
      ::-webkit-scrollbar-thumb { background: var(--slate); border-radius: 3px; }

      /* ── Hero ── */
      .hero {
        position: relative; overflow: hidden;
        background: linear-gradient(135deg, #0f172a 0%, #1e3a5f 50%, #0f2027 100%);
        padding: 80px 5% 60px; text-align: center;
        border-bottom: 1px solid rgba(45,212,191,.2);
      }
      .hero::before {
        content:''; position:absolute; inset:0;
        background: radial-gradient(ellipse 80% 60% at 50% 0%,
                    rgba(45,212,191,.12) 0%, transparent 70%);
        pointer-events:none;
      }
      .hero-label {
        display: inline-block; font-family: 'IBM Plex Mono', monospace;
        font-size: 2.0rem; letter-spacing: .25em; text-transform: uppercase;
        color: var(--teal); border: 1px solid rgba(45,212,191,.4);
        padding: 4px 14px; border-radius: 20px; margin-bottom: 24px;
      }
      .hero h1 {
        font-family: 'Playfair Display', serif; font-weight: 900;
        font-size: clamp(2.8rem, 5vw, 3.6rem); line-height: 1.15;
        color: #f8fafc; max-width: 820px; margin: 0 auto 20px;
      }
      .hero h1 span { color: var(--light); }
      .hero-sub {
        font-size: 1.5rem; color: var(--light); max-width: 640px;
        margin: 0 auto 36px; font-weight: 300;
      }
      .hero-stats {
        display: flex; justify-content: center; gap: 40px;
        flex-wrap: wrap; margin-top: 10px;
      }
      .hero-stat { text-align: center; }
      .hero-stat .num {
        font-family: 'Playfair Display', serif; font-size: 2.6rem;
        font-weight: 700; color: var(--teal); line-height: 1;
      }
      .hero-stat .lbl { font-size: 2.0rem; color: var(--light);
                        letter-spacing: .01em; text-transform: uppercase; }

      /* ── Nav ── */
      .story-nav {
        position: sticky; top: 0; z-index: 100;
        background: rgba(15,23,42,.9); backdrop-filter: blur(12px);
        border-bottom: 1px solid rgba(255,255,255,.07);
        display: flex; gap: 4px; padding: 0 5%; overflow-x: auto;
      }
      .story-nav button {
        background: none; border: none; cursor: pointer;
        color: var(--light); font-family: 'IBM Plex Sans', sans-serif;
        font-size: 1.5rem; font-weight: 500; letter-spacing: .05em;
        text-transform: uppercase; padding: 14px 18px;
        border-bottom: 2px solid transparent; white-space: nowrap;
        transition: color .2s, border-color .2s;
      }
      .story-nav button:hover { color: var(--light); }
      .story-nav button.active { color: var(--teal); border-bottom-color: var(--teal); }

      /* ── Sections ── */
      .section { display: none; padding: 60px 5% 40px; max-width: 1200px;
                 margin: 0 auto; animation: fadeUp .5s ease; }
      .section.visible { display: block; }
      @keyframes fadeUp {
        from { opacity:0; transform:translateY(20px); }
        to   { opacity:1; transform:translateY(0); }
      }

      .section-label {
        font-family: 'IBM Plex Mono', monospace; font-size: 2rem;
        letter-spacing: .2em; text-transform: uppercase; color: var(--teal);
        margin-bottom: 10px;
      }
      .section-title {
        font-family: 'Playfair Display', serif; font-weight: 700;
        font-size: clamp(3.6rem, 3vw, 2.4rem); line-height: 1.2;
        margin-bottom: 16px;
      }
      .section-body {
        color: var(--light); font-size: 2rem; max-width: 720px;
        margin-bottom: 40px; line-height: 1.8;
      }

      /* ── Cards ── */
      .card-grid { display: grid; gap: 20px;
                   grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); }
      .card {
        background: var(--mid); border: 1px solid rgba(255,255,255,.07);
        border-radius: 12px; padding: 24px; transition: border-color .2s, transform .2s;
      }
      .card:hover { border-color: rgba(45,212,191,.35); transform: translateY(-3px); }
      .card-icon { font-size: 3.8rem; margin-bottom: 12px; }
      .card-title { font-weight: 600; font-size: 3rem; margin-bottom: 8px; }
      .card-text { font-size: 2rem; color: var(--light); line-height: 1.7; }

      /* ── Chart wrappers ── */
      .chart-wrap {
        background: var(--mid); border: 1px solid rgba(255,255,255,.07);
        border-radius: 12px; padding: 24px; margin-bottom: 28px;
      }
      .chart-title {
        font-size: 2rem; font-weight: 600; letter-spacing: .06em;
        text-transform: uppercase; color: var(--teal); margin-bottom: 16px;
      }

      /* ── Static image display ── */
      .img-wrap {
        background: var(--mid); border: 1px solid rgba(255,255,255,.07);
        border-radius: 12px; padding: 24px; margin-bottom: 28px;
      }
      .img-wrap .chart-title {
        font-size: 2rem; font-weight: 600; letter-spacing: .06em;
        text-transform: uppercase; color: var(--teal); margin-bottom: 16px;
      }
      .img-wrap img {
        width: 100%; height: auto; display: block;
        border-radius: 8px;
        border: 1px solid rgba(255,255,255,.06);
      }

      /* ── Progress bars ── */
      .prog-item { margin-bottom: 14px; }
      .prog-label { display: flex; justify-content: space-between;
                    font-size: 2rem; margin-bottom: 5px; }
      .prog-label span:last-child { color: var(--teal); font-family: 'IBM Plex Mono'; }
      .prog-bar { height: 6px; background: rgba(255,255,255,.08); border-radius: 3px; }
      .prog-fill { height: 100%; border-radius: 3px;
                   background: linear-gradient(90deg, var(--teal), #06b6d4);
                   transition: width 1s ease; }

      /* ── Two col layout ── */
      .two-col { display: grid; gap: 28px;
                 grid-template-columns: repeat(auto-fit, minmax(340px, 1fr)); }

      /* ── Pill tags ── */
      .tag {
        display: inline-block; font-size: 2rem; font-weight: 500;
        letter-spacing: .2em; text-transform: uppercase;
        padding: 3px 10px; border-radius: 20px; margin: 3px;
      }
      .tag-teal  { background: rgba(45,212,191,.15); color: var(--teal); }
      .tag-amber { background: rgba(245,158,11,.15);  color: var(--amber); }
      .tag-coral { background: rgba(251,113,133,.15); color: var(--coral); }

      /* ── Methods timeline ── */
      .timeline { position: relative; padding-left: 28px; }
      .timeline::before { content:''; position:absolute; left:8px; top:0; bottom:0;
                          width:2px; background: rgba(45,212,191,.2); }
      .tl-item { position:relative; margin-bottom:28px; }
      .tl-dot { position:absolute; left:-24px; top:4px; width:12px; height:12px;
                border-radius:50%; background:var(--teal); box-shadow:0 0 8px var(--teal); }
      .tl-title { font-weight:600; font-size:2rem; margin-bottom:5px; }
      .tl-body  { font-size:1.5rem; color:var(--light); line-height:1.7; }

      /* ── Conclusion callout ── */
      .callout {
        border-left: 3px solid var(--teal); background: rgba(45,212,191,.06);
        border-radius: 0 10px 10px 0; padding: 20px 24px; margin-bottom: 20px;
      }
      .callout p { font-size:2rem; color:var(--light); line-height:1.8; }
      .callout strong { color: var(--light); }

      /* ── Citation ── */
      .citation-box {
        background: rgba(255,255,255,.03); border: 1px solid rgba(255,255,255,.08);
        border-radius: 10px; padding: 20px 24px; font-size: 2rem;
        color: var(--muted); font-family: 'IBM Plex Mono', monospace; line-height: 1.7;
      }

      /* ── Footer ── */
      .footer {
        text-align: center; padding: 40px 5%; color: var(--muted);
        font-size: 2rem; border-top: 1px solid rgba(255,255,255,.07);
        margin-top: 60px;
      }

      /* ── Plotly overrides ── */
      .js-plotly-plot .plotly { background: transparent !important; }

      /* ── Filter dropdowns dark theme ── */
      .selectize-input {
        background: rgba(255,255,255,.05) !important;
        border: 1px solid rgba(255,255,255,.12) !important;
        border-radius: 6px !important; color: #e2e8f0 !important;
        font-size: 0.8rem !important; font-family: 'IBM Plex Sans', sans-serif !important;
        box-shadow: none !important;
      }
      .selectize-input.focus { border-color: rgba(45,212,191,.5) !important; }
      .selectize-dropdown {
        background: #1e293b !important; border: 1px solid rgba(255,255,255,.12) !important;
        color: #e2e8f0 !important; font-size: 0.8rem !important;
        font-family: 'IBM Plex Sans', sans-serif !important;
      }
      .selectize-dropdown .option:hover,
      .selectize-dropdown .option.active { background: rgba(45,212,191,.15) !important; color: #2dd4bf !important; }
      .selectize-dropdown .option.selected { background: rgba(45,212,191,.1) !important; }

      /* ── DataTables dark theme ── */
      table.dataTable { background: transparent !important; border-collapse: collapse; width: 100% !important; }
      table.dataTable thead th { border-bottom: 1px solid rgba(255,255,255,.1) !important; }
      table.dataTable tbody tr { background: transparent !important; }
      table.dataTable tbody tr:hover { background: rgba(45,212,191,.06) !important; }
      table.dataTable tbody tr td { border-top: 1px solid rgba(255,255,255,.05) !important; vertical-align: top; padding: 8px 10px !important; }
      .dataTables_wrapper .dataTables_filter input {
        background: rgba(255,255,255,.05); border: 1px solid rgba(255,255,255,.12);
        border-radius: 6px; color: #e2e8f0; padding: 4px 10px; margin-left: 8px;
      }
      .dataTables_wrapper .dataTables_filter label,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate { color: #94a3b8 !important; font-size: 0.8rem; }
      .dataTables_wrapper .dataTables_paginate .paginate_button { color: #94a3b8 !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: rgba(45,212,191,.15) !important; border: 1px solid rgba(45,212,191,.3) !important;
        color: #2dd4bf !important; border-radius: 4px;
      }
      table.stripe tbody tr.odd { background: rgba(255,255,255,.02) !important; }

      /* ── Responsive tweaks ── */
      @media (max-width: 600px) {
        .hero-stats { gap: 20px; }
        .hero-stat .num { font-size: 3rem; }
      }
    ")))
  ),
  
  # ── Hero ──────────────────────────────────────────────────────────────────
  div(class = "hero",
      div(class = "hero-label", "A Data Story based on the publication by · Rocke, Howitt & Hambleton, 2023"),
      tags$h1("Built Environment & ", tags$span("Physical Activity"), " in Latin America and SIDS"),
      p(class = "hero-sub",
        "A scoping review of 51 studies from Latin America and Small Island Developing States,
       examining how neighbourhood design shapes people's movement."),
      div(class = "hero-stats",
          div(class = "hero-stat", div(class = "num", "51"), div(class = "lbl", "Studies")),
          div(class = "hero-stat", div(class = "num", "306"), div(class = "lbl", "BE-PA Relationships")),
          div(class = "hero-stat", div(class = "num", "5"), div(class = "lbl", "Countries")),
          div(class = "hero-stat", div(class = "num", "0"), div(class = "lbl", "Caribbean Studies"))
      )
  ),
  
  # ── Nav ───────────────────────────────────────────────────────────────────
  div(class = "story-nav",
      tags$button(class = "active", onclick = "showSection('overview')",      "Overview"),
      tags$button(onclick = "showSection('methods')",      "Methods"),
      tags$button(onclick = "showSection('geography')",    "Geography"),
      tags$button(onclick = "showSection('evidence')",     "Evidence"),
      tags$button(onclick = "showSection('relationships')", "Relationships"),
      tags$button(onclick = "showSection('findings')",     "Key Findings"),
      tags$button(onclick = "showSection('conclusion')",   "Conclusion")
  ),
  
  # ── Section 1: Overview ───────────────────────────────────────────────────
  div(id = "sec-overview", class = "section visible",
      div(class = "section-label", "01 · Context"),
      h2(class = "section-title", "Why does the built environment matter?"),
      p(class = "section-body",
        "Physical inactivity is a growing public health crisis, particularly in the Caribbean,
       where it contributes to high rates of non-communicable diseases. The built environment
       (BE) - the human-made surroundings where people live, work, and play - is increasingly
       recognised as a powerful determinant of physical activity (PA). This review synthesises
       evidence from analogous settings to understand what features of the BE might help or
       hinder PA in Caribbean communities."),
      
      div(class = "two-col",
          div(class = "chart-wrap",
              div(class = "chart-title", "Physical Activity Outcomes Measured"),
              plotlyOutput("pa_types_plot", height = "280px")
          ),
          div(class = "chart-wrap",
              div(class = "chart-title", "Built Environment Features Studied"),
              plotlyOutput("be_features_plot", height = "280px")
          )
      ),
      
      div(class = "card-grid",
          div(class = "card",
              div(class = "card-icon", "🏝️"),
              div(class = "card-title", "Caribbean Context"),
              div(class = "card-text",
                  "Caribbean SIDS have a unique urban structure - often a single urban centre
           with connections to outlying communities - shaped by rapid, largely unplanned
           urbanisation. Seven of fifteen Caribbean SIDS have land areas smaller than 800 km².")
          ),
          div(class = "card",
              div(class = "card-icon", "📊"),
              div(class = "card-title", "Scope of Review"),
              div(class = "card-text",
                  "19,178 titles screened; 51 studies from Brazil, Colombia, Mexico, Chile and
           Singapore ultimately included, describing 306 unique built environment-physical
           activity relationships.")
          ),
          div(class = "card",
              div(class = "card-icon", "🔍"),
              div(class = "card-title", "Framework Used"),
              div(class = "card-text",
                  "Built environment features were classified into 9 neighbourhood design domains
           using the Walkability for Health framework: Surveillance, Experience, Parking,
           Traffic Safety, Community, Greenspace, Density, Connectivity, and Land Use.")
          )
      )
  ),
  
  # ── Section 2: Geography ──────────────────────────────────────────────────
  div(id = "sec-geography", class = "section",
      div(class = "section-label", "02 · Study Locations"),
      h2(class = "section-title", "Where is the evidence coming from?"),
      p(class = "section-body",
        "All 51 included studies came from urban settings in Latin America (43 studies)
       and Singapore (8 studies). Critically, no studies from Caribbean SIDS met
       the inclusion criteria - exposing a major evidence gap for regional policy."),
      
      div(style = "padding: 10px 0;",
          div(class = "section-label", "Region Split"),
          tags$br(),
          lapply(1:nrow(study_locations), function(i) {
            r <- study_locations[i,]
            pct <- round(r$n_studies / 51 * 100)
            div(class = "prog-item",
                div(class = "prog-label",
                    tags$span(r$country),
                    tags$span(paste0(r$n_studies, " studies (", pct, "%)"))
                ),
                div(class = "prog-bar",
                    div(class = "prog-fill", style = paste0("width:", pct, "%"))
                )
            )
          })
      )
  ),
  
  # ── Section 3: Methods ────────────────────────────────────────────────────
  div(id = "sec-methods", class = "section",
      div(class = "section-label", "03 · Methods"),
      h2(class = "section-title", "How was the evidence gathered?"),
      p(class = "section-body",
        "This scoping review followed the Arksey & O'Malley framework and PRISMA-ScR
       reporting guidelines. Only studies with objective measures of the built environment
       were included to limit subjectivity bias."),
      
      div(class = "timeline",
          div(class = "tl-item",
              div(class = "tl-dot"),
              div(class = "tl-title", "Database Search"),
              div(class = "tl-body",
                  "Searched MEDLINE, SciELO, CINAHL, LILACS, OARE, TRID, TRIS, IBECS and
               Web of Science for studies published 2010-2021. ",
                  tags$strong("19,178 titles identified."))
          ),
          div(class = "tl-item",
              div(class = "tl-dot"),
              div(class = "tl-title", "Title & Abstract Screening"),
              div(class = "tl-body",
                  "Two independent reviewers (KR & CH) screened all records using Rayyan.
               Discrepancies resolved by a third reviewer. ",
                  tags$strong("17,791 records excluded."))
          ),
          div(class = "tl-item",
              div(class = "tl-dot"),
              div(class = "tl-title", "Full-Text Review"),
              div(class = "tl-body",
                  "109 full-text articles assessed. 58 excluded for no objective BE measure,
               no PA outcome, or ineligible location. ",
                  tags$strong("51 studies included."))
          ),
          div(class = "tl-item",
              div(class = "tl-dot"),
              div(class = "tl-title", "Data Extraction"),
              div(class = "tl-body",
                  "Structured extraction in REDCap. BE features mapped to Walkability for
               Health framework. Effect sizes converted to ORs for consistency.")
          ),
          div(class = "tl-item",
              div(class = "tl-dot"),
              div(class = "tl-title", "Publication Bias"),
              div(class = "tl-body",
                  "Contour-enhanced funnel plot and Egger test conducted. ",
                  tags$strong("No significant publication bias detected"),
                  " (Egger p = 0.41).")
          )
      ),
      
      tags$br(),
      div(class = "img-wrap",
          div(class = "chart-title", "PRISMA-ScR Flow Diagram"),
          tags$img(
            src = "prisma_scr.jpg",
            alt = "PRISMA-ScR Flow Diagram showing study screening and inclusion steps"
          )
      ),
      tags$br(),
      div(style = "font-size:1.5rem; color:var(--light); line-height:1.8;",
          tags$strong(style="color:#e2e8f0;", "Built Environment Measurement Methods:"), tags$br(),
          "Desktop Mapping (GIS) - 80%", tags$br(),
          "Field Auditing - 13%", tags$br(),
          "GPS Tracking - 5%", tags$br(),
          "Remote Sensing - 2%"
      )
  ),
  
  # ── Section 4: Evidence Gap ───────────────────────────────────────────────
  div(id = "sec-evidence", class = "section",
      div(class = "section-label", "04 · Evidence Landscape"),
      h2(class = "section-title", "Mapping what we know - and what we don't"),
      p(class = "section-body",
        "The evidence gap matrix below visualises the frequency of reported BE-PA
       relationships across the 9 Walkability for Health domains and 3 physical
       activity outcomes. Darker cells indicate more relationships studied.
       Empty cells represent gaps in knowledge."),
      
      # ── Studies table FIRST ──────────────────────────────────────────────────
      div(class = "chart-wrap",
          div(class = "chart-title", "Summary of Included Studies"),
          p(style = "color:var(--light); font-size:1.5rem; margin-bottom:16px;",
            "All 51 studies included in the scoping review. Use the dropdowns and search box to filter."),
          
          # Filter row
          div(style = "display:flex; flex-wrap:wrap; gap:12px; margin-bottom:16px;",
              div(style = "flex:1; min-width:120px;",
                  tags$label("Year", style="font-size:1.5rem; color:var(--light); text-transform:uppercase; letter-spacing:.08em; display:block; margin-bottom:4px;"),
                  selectInput("filter_year", label=NULL,
                              choices = c("All", sort(unique(as.integer(sub(".*,\\s*(\\d{4}).*","\\1", included_studies$Author_Year))))),
                              selected = "All", width = "100%")
              ),
              div(style = "flex:1; min-width:120px;",
                  tags$label("Country", style="font-size:.1.5rem; color:var(--light); text-transform:uppercase; letter-spacing:.08em; display:block; margin-bottom:4px;"),
                  selectInput("filter_country", label=NULL,
                              choices = c("All", sort(unique(included_studies$Country))),
                              selected = "All", width = "100%")
              ),
              div(style = "flex:1; min-width:140px;",
                  tags$label("Population Type", style="font-size:1.5rem; color:var(--light); text-transform:uppercase; letter-spacing:.08em; display:block; margin-bottom:4px;"),
                  selectInput("filter_pop", label=NULL,
                              choices = c("All", sort(unique(included_studies$Population))),
                              selected = "All", width = "100%")
              ),
              div(style = "flex:1; min-width:140px;",
                  tags$label("Study Type", style="font-size:1.5rem; color:var(--light); text-transform:uppercase; letter-spacing:.08em; display:block; margin-bottom:4px;"),
                  selectInput("filter_design", label=NULL,
                              choices = c("All", sort(unique(included_studies$Design))),
                              selected = "All", width = "100%")
              ),
              div(style = "flex:1; min-width:140px;",
                  tags$label("Unit of Analysis", style="font-size:1.5rem; color:var(--light); text-transform:uppercase; letter-spacing:.08em; display:block; margin-bottom:4px;"),
                  selectInput("filter_unit", label=NULL,
                              choices = c("All","Individual","City/Area"),
                              selected = "All", width = "100%")
              ),
              div(style = "flex:2; min-width:180px;",
                  tags$label("BE Features", style="font-size:1.5rem; color:var(--light); text-transform:uppercase; letter-spacing:.08em; display:block; margin-bottom:4px;"),
                  selectInput("filter_be", label=NULL,
                              choices = c("All",
                                          "Residential Density","Street Connectivity","Mixed Land Use",
                                          "Public Space/Parks","Recreation proximity","Transit proximity",
                                          "Trails/sidewalks","Non-recreational land use",
                                          "Pedestrian amenities","Walkability index"),
                              selected = "All", width = "100%")
              ),
              div(style = "flex:2; min-width:180px;",
                  tags$label("PA Type", style="font-size:1.5rem; color:var(--light); text-transform:uppercase; letter-spacing:.08em; display:block; margin-bottom:4px;"),
                  selectInput("filter_pa", label=NULL,
                              choices = c("All",
                                          "Transportation walking","Recreational walking","MVPA",
                                          "Leisure-time PA","General walking","Cycling",
                                          "Active Transport","Active Commuting"),
                              selected = "All", width = "100%")
              )
          ),
          
          DTOutput("studies_table")
      ),
      
      # ── Evidence gap matrix & charts below ──────────────────────────────────
      div(class = "img-wrap",
          div(class = "chart-title", "Evidence Gap Matrix: BE Domains x PA Outcomes"),
          tags$img(
            src = "evidence_matrix.jpg",
            alt = "Evidence gap matrix showing number of BE-PA relationships by domain and PA outcome"
          )
      ),
      
      div(class = "chart-wrap",
          div(class = "chart-title", "Relationships by BE Domain (All PA Types)"),
          plotlyOutput("domain_bar_plot", height = "280px")
      ),
      
      div(class = "section-label", style = "margin-top: 8px;", "Notable Gaps"),
      tags$br(),
      div(class = "two-col",
          div(class = "card",
              div(class = "card-icon", "🅿️"),
              div(class = "card-title", "Parking - Zero Relationships"),
              div(class = "card-text",
                  "No study examined parking infrastructure in relation to physical activity,
             despite its potential deterrent effect on active transport.")
          ),
          div(class = "card",
              div(class = "card-icon", "👁️"),
              div(class = "card-title", "Surveillance - Only 7 Relationships"),
              div(class = "card-text",
                  "Street lighting and police surveillance were rarely studied despite their
             known importance for perceived safety, especially for women and older adults.")
          )
      )
  ),
  
  # ── Section 5: Relationships ──────────────────────────────────────────────
  div(id = "sec-relationships", class = "section",
      div(class = "section-label", "05 · BE-PA Associations"),
      h2(class = "section-title", "Direction of effects: what helps, what hinders?"),
      p(class = "section-body",
        "Of 220 relationships with usable effect size data (OR range: 0.12-8.17),
       61% were positive (OR > 1, more BE feature = more PA) and 39% were negative.
       The chart below shows the balance of positive vs. negative relationships for
       each BE domain, stratified by physical activity type."),
      
      div(class = "chart-wrap",
          div(class = "chart-title", "Positive vs. Negative Relationships by Domain & PA Type"),
          selectInput("pa_filter", label = NULL,
                      choices = c("Active Transport","Leisure-time PA","MVPA"),
                      selected = "Active Transport",
                      width = "240px"
          ),
          plotlyOutput("direction_plot", height = "340px")
      ),
      
      div(class = "card-grid",
          div(class = "card",
              div(class = "card-icon", "✅"),
              div(class = "card-title", "Strongest Positive - Active Transport"),
              div(class = "card-text",
                  tags$strong("Residential density"), ": 11 positive vs 2 negative.", tags$br(),
                  tags$strong("Street connectivity"), ": 12 positive vs 8 negative.")
          ),
          div(class = "card",
              div(class = "card-icon", "✅"),
              div(class = "card-title", "Strongest Positive - Leisure PA"),
              div(class = "card-text",
                  tags$strong("Greenspace"), ": 11 positive vs 3 negative.", tags$br(),
                  tags$strong("Land use mix"), ": 10 positive vs 4 negative.")
          ),
          div(class = "card",
              div(class = "card-icon", "✅"),
              div(class = "card-title", "Strongest Positive - MVPA"),
              div(class = "card-text",
                  tags$strong("Traffic calming/safety"), ": 6 positive vs 0 negative.", tags$br(),
                  tags$strong("Land use"), ": 8 positive vs 2 negative.")
          ),
          div(class = "card",
              div(class = "card-icon", "⚠️"),
              div(class = "card-title", "Notable Negatives"),
              div(class = "card-text",
                  tags$strong("Traffic safety - active transport"), ": 14 negative vs 8 positive.", tags$br(),
                  "Suggests road danger may deter walking/cycling despite infrastructure presence.")
          )
      )
  ),
  
  # ── Section 6: Key Findings ───────────────────────────────────────────────
  div(id = "sec-findings", class = "section",
      div(class = "section-label", "06 · Key Findings"),
      h2(class = "section-title", "What this review tells us"),
      p(class = "section-body",
        "Six critical insights emerge from synthesising 306 BE-PA relationships
       across 5 countries spanning Latin America and Singapore."),
      
      div(class = "card-grid",
          lapply(key_findings, function(f) {
            div(class = "card",
                div(class = "card-icon", f$icon),
                div(class = "card-title", f$title),
                div(class = "card-text", f$text)
            )
          })
      ),
      
      tags$br(),
      div(class = "chart-wrap",
          div(class = "chart-title", "Can Caribbean Regions Use This Evidence? - Applicability Assessment"),
          plotlyOutput("applicability_plot", height = "260px")
      )
  ),
  
  # ── Section 7: Conclusion ─────────────────────────────────────────────────
  div(id = "sec-conclusion", class = "section",
      div(class = "section-label", "07 · Conclusion & Implications"),
      h2(class = "section-title", "Moving towards evidence-based design in the Caribbean"),
      p(class = "section-body",
        "This is the first review to synthesise BE-PA evidence for application to the
       Caribbean region. While no Caribbean-specific studies were found, the available
       evidence from comparable settings offers actionable insights for urban planners,
       public health professionals, and policymakers."),
      
      div(class = "callout",
          p(tags$strong("Land use mix, greenspace access, residential density and street connectivity"),
            " are the BE features most consistently associated with increased physical activity.
         Combining BE changes with health promotion interventions offers the greatest potential
         for reducing physical inactivity in the Caribbean.")
      ),
      div(class = "callout",
          p(tags$strong("Translational research is urgently needed."),
            " Caribbean SIDS have a unique single-centre urban structure unlike the large multi-city
         landscapes of Brazil or Mexico. Whether walkability evidence translates requires
         dedicated local research.")
      ),
      div(class = "callout",
          p(tags$strong("A critical evidence gap exists."),
            " Zero peer-reviewed studies from the Caribbean met inclusion criteria.
         This calls for investment in regional research infrastructure and multidisciplinary
         collaboration to generate locally relevant evidence.")
      ),
      
      tags$br(),
      div(class = "section-label", "Cite This Work"),
      div(class = "citation-box",
          "Rocke K, Howitt C, Hambleton I.", tags$br(),
          tags$em("Understanding the relationship between built environment features and physical activity in the Caribbean: A scoping review."), tags$br(),
          "Dialogues in Health. 2023;2:100088.", tags$br(),
          "DOI: 10.1016/j.dialog.2022.100088"
      ),
      
      tags$br(),
      tags$br(),
      div(style = "display:flex; gap:10px; flex-wrap:wrap;",
          div(class="tag tag-teal", "Scoping Review"),
          div(class="tag tag-teal", "Built Environment"),
          div(class="tag tag-teal", "Physical Activity"),
          div(class="tag tag-amber", "Caribbean"),
          div(class="tag tag-amber", "SIDS"),
          div(class="tag tag-amber", "Latin America"),
          div(class="tag tag-coral", "Urban Health"),
          div(class="tag tag-coral", "NCD Prevention")
      )
  ),
  
  # ── Footer ────────────────────────────────────────────────────────────────
  div(class = "footer",
      "Data Story based on Rocke, Howitt & Hambleton (2023) · Dialogues in Health · ",
      tags$a(href="https://doi.org/10.1016/j.dialog.2022.100088",
             target="_blank", style="color:var(--teal);",
             "doi:10.1016/j.dialog.2022.100088")
  ),
  
  # ── JS nav ────────────────────────────────────────────────────────────────
  tags$script(HTML("
    function showSection(id) {
      document.querySelectorAll('.section').forEach(function(s){ s.classList.remove('visible'); });
      document.getElementById('sec-' + id).classList.add('visible');
      document.querySelectorAll('.story-nav button').forEach(function(b){ b.classList.remove('active'); });
      event.target.classList.add('active');
      window.scrollTo({top:0, behavior:'smooth'});
    }
  "))
)

# ─── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # Shared plotly layout helper
  base_layout <- function(p, xlab = "", ylab = "") {
    p |> layout(
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      font          = list(family = "IBM Plex Sans, sans-serif", color = "#e2e8f0"),
      xaxis         = list(title = list(text = xlab, font = list(size = 11)),
                           gridcolor = "#1e293b", zerolinecolor = "#334155",
                           tickfont = list(color = "#94a3b8", size = 10)),
      yaxis         = list(title = list(text = ylab, font = list(size = 11)),
                           gridcolor = "#1e293b", zerolinecolor = "#334155",
                           tickfont = list(color = "#94a3b8", size = 10)),
      margin        = list(l = 10, r = 10, t = 10, b = 10),
      showlegend    = FALSE
    ) |> config(displayModeBar = FALSE)
  }
  
  # PA types bar
  output$pa_types_plot <- renderPlotly({
    plot_ly(pa_types,
            y = ~reorder(type, count), x = ~count,
            type = "bar", orientation = "h",
            marker = list(color = TEAL, opacity = .85),
            hovertemplate = "%{y}: %{x} studies<extra></extra>"
    ) |> base_layout(xlab = "Number of Studies")
  })
  
  # BE features bar
  output$be_features_plot <- renderPlotly({
    plot_ly(be_features,
            y = ~reorder(feature, count), x = ~count,
            type = "bar", orientation = "h",
            marker = list(color = AMBER, opacity = .85),
            hovertemplate = "%{y}: %{x} studies<extra></extra>"
    ) |> base_layout(xlab = "Number of Studies")
  })
  
  # Country bar
  output$country_plot <- renderPlotly({
    plot_ly(study_locations,
            x = ~country, y = ~n_studies, type = "bar",
            marker = list(color = ~color, opacity = .9),
            hovertemplate = "%{x}: %{y} studies<extra></extra>"
    ) |> base_layout(ylab = "Studies") |>
      layout(showlegend = FALSE)
  })
  
  # Domain bar
  output$domain_bar_plot <- renderPlotly({
    plot_ly(be_domains,
            y = ~reorder(domain, relationships), x = ~relationships,
            type = "bar", orientation = "h",
            marker = list(
              color = ~relationships,
              colorscale = list(
                c(0, "rgba(45,212,191,.3)"),
                c(1, "rgba(6,182,212,1)")
              ),
              showscale = FALSE
            ),
            hovertemplate = "%{y}: %{x} relationships<extra></extra>"
    ) |> base_layout(xlab = "Number of Relationships")
  })
  
  # Direction plot (reactive to PA type selector)
  output$direction_plot <- renderPlotly({
    req(input$pa_filter)
    df <- direction_data |> filter(pa_type == input$pa_filter)
    df$domain <- factor(df$domain,
                        levels = df |> arrange(positive - negative) |> pull(domain))
    
    plot_ly() |>
      add_bars(data = df,
               y = ~domain, x = ~positive,
               name = "Positive (more PA)",
               orientation = "h",
               marker = list(color = TEAL, opacity = .85),
               hovertemplate = "%{y}<br>Positive: %{x}<extra></extra>"
      ) |>
      add_bars(data = df,
               y = ~domain, x = ~negative,
               name = "Negative (less PA)",
               orientation = "h",
               marker = list(color = CORAL, opacity = .75),
               hovertemplate = "%{y}<br>Negative: %{x}<extra></extra>"
      ) |>
      layout(
        barmode       = "stack",
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font   = list(family = "IBM Plex Sans", color = "#e2e8f0"),
        xaxis  = list(title = "Number of Relationships",
                      gridcolor = "#1e293b", zerolinecolor = "#334155",
                      tickfont = list(color = "#94a3b8")),
        yaxis  = list(title = "", tickfont = list(color = "#94a3b8")),
        legend = list(orientation = "h", y = -0.15,
                      font = list(color = "#94a3b8", size = 11)),
        margin = list(l=10, r=10, t=10, b=30)
      ) |> config(displayModeBar = FALSE)
  })
  
  # Applicability bar
  output$applicability_plot <- renderPlotly({
    df <- data.frame(
      dimension = c("Geographic Proximity","Urbanisation Level",
                    "Economic Profile","Urban Structure","NCD Burden",
                    "Cultural Similarity"),
      la_score  = c(8, 7, 5, 4, 8, 6),
      sg_score  = c(3, 9, 3, 5, 6, 2)
    )
    plot_ly() |>
      add_bars(data = df,
               x = ~dimension, y = ~la_score,
               name = "Latin America",
               marker = list(color = TEAL, opacity = .8),
               hovertemplate = "%{x}<br>Applicability: %{y}/10<extra></extra>"
      ) |>
      add_bars(data = df,
               x = ~dimension, y = ~sg_score,
               name = "Singapore",
               marker = list(color = AMBER, opacity = .8),
               hovertemplate = "%{x}<br>Applicability: %{y}/10<extra></extra>"
      ) |>
      layout(
        barmode       = "group",
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font   = list(family = "IBM Plex Sans", color = "#e2e8f0"),
        xaxis  = list(title = "", tickangle = -20,
                      gridcolor = "#1e293b",
                      tickfont = list(color = "#94a3b8", size = 10)),
        yaxis  = list(title = "Applicability Score (0-10)", range = c(0,10),
                      gridcolor = "#1e293b",
                      tickfont = list(color = "#94a3b8")),
        legend = list(font = list(color = "#94a3b8")),
        margin = list(l=10, r=10, t=10, b=60),
        annotations = list(list(
          text = "Estimated applicability to Caribbean SIDS context (author's interpretation)",
          x = .5, xref = "paper", y = -.28, yref = "paper",
          showarrow = FALSE,
          font = list(size = 9, color = LIGHT)
        ))
      ) |> config(displayModeBar = FALSE)
  })
  
  # ── FIX 1: Studies table ─────────────────────────────────────────────────
  # server = FALSE ensures the full table is sent to the browser at once,
  # which prevents blank rendering when the section is first revealed.
  # ── Studies table (reactive to filters) ─────────────────────────────────
  filtered_studies <- reactive({
    df <- included_studies
    if (!is.null(input$filter_year)   && input$filter_year   != "All")
      df <- df[df$Year == as.integer(input$filter_year), ]
    if (!is.null(input$filter_country) && input$filter_country != "All")
      df <- df[grepl(input$filter_country, df$Country, fixed = TRUE), ]
    if (!is.null(input$filter_pop)    && input$filter_pop    != "All")
      df <- df[grepl(input$filter_pop, df$Population, fixed = TRUE), ]
    if (!is.null(input$filter_design) && input$filter_design != "All")
      df <- df[grepl(input$filter_design, df$Design, fixed = TRUE), ]
    if (!is.null(input$filter_unit)   && input$filter_unit   != "All")
      df <- df[df$Unit_of_Analysis == input$filter_unit, ]
    if (!is.null(input$filter_be)     && input$filter_be     != "All")
      df <- df[grepl(input$filter_be, df$BE_Features, ignore.case = TRUE), ]
    if (!is.null(input$filter_pa)     && input$filter_pa     != "All")
      df <- df[grepl(input$filter_pa, df$PA_Type, ignore.case = TRUE), ]
    df
  })
  
  output$studies_table <- renderDT({
    datatable(
      filtered_studies(),
      colnames  = c("Author, Year","Year","Country","Population","Design",
                    "Unit of Analysis","Sample Size","BE Features","PA Type","Main Finding"),
      rownames  = FALSE,
      selection = "none",
      options   = list(
        pageLength  = 10,
        scrollX     = TRUE,
        dom         = "frtip",
        columnDefs  = list(
          list(width = "110px", targets = 0),
          list(width = "60px",  targets = 1),
          list(width = "130px", targets = 2),
          list(width = "140px", targets = 3),
          list(width = "100px", targets = 4),
          list(width = "100px", targets = 5),
          list(width = "90px",  targets = 6),
          list(width = "220px", targets = 7),
          list(width = "160px", targets = 8),
          list(width = "300px", targets = 9)
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({",
          "    'background-color':'#1e293b',",
          "    'color':'#94a3b8',",
          "    'font-size':'0.75rem',",
          "    'text-transform':'uppercase',",
          "    'letter-spacing':'0.08em'",
          "  });",
          "}"
        )
      ),
      class = "compact stripe"
    ) |>
      formatStyle(
        columns    = 1:10,
        color      = "#e2e8f0",
        fontSize   = "0.82rem",
        lineHeight = "1.5"
      ) |>
      formatStyle(
        columns    = "Author_Year",
        fontWeight = "600",
        color      = "#2dd4bf"
      ) |>
      formatStyle(
        columns   = "Sample_Size",
        textAlign = "right"
      )
  }, server = FALSE)
  
  # ── FIX 3: suspendWhenHidden = FALSE for all outputs ─────────────────────
  # Ensures plots and the table render even before the user navigates to
  # their respective sections (they live inside display:none divs on load).
  outputOptions(output, "pa_types_plot",      suspendWhenHidden = FALSE)
  outputOptions(output, "be_features_plot",   suspendWhenHidden = FALSE)
  outputOptions(output, "country_plot",       suspendWhenHidden = FALSE)
  outputOptions(output, "domain_bar_plot",    suspendWhenHidden = FALSE)
  outputOptions(output, "direction_plot",     suspendWhenHidden = FALSE)
  outputOptions(output, "applicability_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "studies_table",      suspendWhenHidden = FALSE)
}

shinyApp(ui, server)