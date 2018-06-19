#------------------------------------------------------------------------------*
# Standard output functions
#------------------------------------------------------------------------------*
# Functions to help standardize output across reports from each site type and
# syndrome
#------------------------------------------------------------------------------*




#------------------------------------------------------------------------------*
# Summary table for etiologies by site ----
#------------------------------------------------------------------------------*
# Function to calculate summary (positives, tested, percent) by etiology for
# any given site
#------------------------------------------------------------------------------*
site_table_named <- function(
  .data, site = "all", etiology_names, format = NULL, condition = TRUE, ...
){
  # Use spanish
  if(Sys.info()[["sysname"]] == "Windows") Sys.setlocale(locale = "Spanish_Guatemala.1252")
  if(Sys.info()[["sysname"]] == "Linux") Sys.setlocale(locale = "es_GT.UTF8")
  
  # rlang evaluation
  etiology_names <- rlang::splice(etiology_names)
  
  # Check site
  if(!is.character(site)) stop("The site name must be a character element.")
  
  # Filter sites
  if(!all(site == "all")){
    if(!all(site %in% .data$site_name)){
      not_found <- site[!site %in% .data$site_name]
      warning(
        "The site name \"",
        paste(not_found, collapse = "\", \""),
        "\" was not found in the dataset. ",
        "Try one of: ", paste(unique(.data$site_name), collapse = ", "),
        "\nReturning 0 rows tibble."
      )
    }
    
    .data <- .data %>%
      filter(site_name %in% site)
  }
  
  # Calculate summary
  .data %>%
    # Filter data by condition
    filter(!!!enquo(condition)) %>%
    # Assign time groups
    mutate(
      this_month = between(
        service_date,
        this_month_start,
        ceiling_date(this_month_start, unit = "month", change_on_boundary = TRUE)
      ),
      last_month = between(
        service_date,
        last_month_start,
        ceiling_date(last_month_start, unit = "month", change_on_boundary = TRUE)
      ),
      all_time = TRUE
    ) %>%
    # Drop unused variables
    select(
      -c(subject_department:age_dates)
    ) %>%
    gather(
      key = group, value = value, this_month:all_time
    ) %>%
    filter(value) %>% select(-value) %>%
    gather(
      key = etiology, value = result, na.rm = TRUE,
      -c(unique_id:site_department, group)
    ) %>%
    group_by(
      group, etiology
    ) %>%
    summarize(
      n_tested = n(),
      n_positive = sum(result),
      percent_positive = round(mean(result) * 100)
    ) %>%
    ungroup %>%
    complete(
      group = c("this_month", "last_month", "all_time"),
      etiology = names(etiology_names),
      fill = list(n_tested = 0, n_positive = 0, percent_positive = 0)
    ) %>%
    mutate(
      value = paste0(
        n_positive, " / ", n_tested,
        " <font color='#C5C5C5'>(", percent_positive, ")</font>"
      )
    ) %>%
    select(-c(n_tested:percent_positive)) %>%
    mutate(
      group = recode_factor(
        group,
        this_month = paste0(
          "Mes más reciente </br>(", strftime(this_month_start, format = "%B"),
          ") </br> (n, %)"
        ),
        last_month = paste0(
          "Mes anterior </br>(", strftime(last_month_start, format = "%B"),
          ") </br> (n, %)"
        ),
        all_time = "Histórico </br> (n, %)",
        .ordered = TRUE
      ),
      "Etiología" = factor(
        recode(etiology, etiology_names),
        levels = unlist(etiology_names),
        ordered = TRUE
      )
    ) %>%
    spread(
      key = group, value = value,
      fill = "<font color='lightgray'>NA</font>"
    ) %>%
    arrange_(.dots = "Etiología") %>% 
    select(., -etiology) %>%
    knitr::kable(format = format) %>%
    return
}




#------------------------------------------------------------------------------*
# Endemic corridor by site ----
#------------------------------------------------------------------------------*
# Function to calculate summary (positives, tested, percent) by etiology for
# any given site
#------------------------------------------------------------------------------*

endemic_corridor <- function(
  .data, site = "all", etiologies = "all", previous_years = 7,
  null = FALSE, ylim = 100, interactive = FALSE
){
  
  # Check site
  if(!is.character(site)) stop("The site name must be a character element.")
  
  # Filter sites
  if(!all(site == "all")){
    if(!all(site %in% .data$site_name)){
      not_found <- site[!site %in% .data$site_name]
      warning(
        "The site name \"",
        paste(not_found, collapse = "\", \""),
        "\" was not found in the dataset. ",
        "Try one of: ", paste(unique(.data$site_name), collapse = ", "),
        "Returning 0 rows tibble."
      )
    }
    
    .data <- .data %>%
      filter(site_name %in% site)
  }
  
  
  # Check etiologies
  if(!is.character(etiologies)) stop("The etiologies list must be a character vector.")
  
  # Filter data based on etiologies
  if(!all(etiologies == "all")){
    # Names exist in data
    if(!all(etiologies %in% names(.data))){
      missing <- etiologies[!etiologies %in% names(.data)]
      stop(
        "The following etiologies are not found in the dataset: ",
        paste(missing, collapse = ", ")
      )
    }
    
    # Names point to objects of type "logical"
    logical <- names(.data)[sapply(.data, class) == "logical"]
    if(!all(etiologies %in% logical)){
      not_logical <- etiologies[!etiologies %in% logical]
      stop(
        "The following variables are not logical: ",
        paste(not_logical, collapse = ", ")
      )
    }
    
    # Filter
    .data <- .data %>%
      filter_(paste(etiologies, collapse = " & "))
  }
  
  
  
  # Current year data
  inscritos_respi_actual <- .data %>%
    filter(epi_year == year(last_friday))
  
  
  # Casos acumulados
  inscritos_actuales <- inscritos_respi_actual %>%
    group_by(epi_week) %>%
    summarise(casos = n()) %>% 
    complete(epi_week = seq(1, max(epi_week)), fill = list(casos = NA_integer_))
  
  # Texto tipos de linea
  casos_actual <- paste("Casos", year(last_friday))
  
  # Zonas del corredor
  zonas <- c("Brote", "Alerta", "Seguridad", "Éxito")
  
  # Weekly count for previous years
  semanal_previos <- .data %>%                         
    filter(
      between(
        epi_year,
        year(last_friday) - previous_years,
        year(last_friday) - 1
      )
    )%>%
    group_by(epi_week, epi_year) %>%
    summarise(casos = n()) %>%
    summarise(
      min = min(casos),
      "Éxito" = quantile(casos, probs = 0.25),    # 0 a Q1
      "Seguridad" = quantile(casos, probs = 0.5),  # Q1 a Q2
      "Alerta" = quantile(casos, probs = 0.75),   # Q2 a Q3
      "Brote" = ylim,
      max = max(casos)
    ) %>%
    gather(key = zona, value = quantile, -epi_week) %>%
    ungroup
  
  semanal_previos_range <- semanal_previos %>%
    filter(zona %in% c("min", "max"))
  
  semanal_previos_poly <- semanal_previos %>%
    filter(!zona %in% c("min", "max")) %>%
    complete(epi_week = 1:53, zona, fill = list(quantile = 0)) %>%
    mutate(
      # Max for "Brote"
      quantile = ifelse(zona == "Brote", ylim, quantile)
    ) %>%
    arrange(zona, epi_week) %>%
    group_by(zona) %>%
    do({
      bind_rows(
        mutate(filter(., epi_week == min(epi_week)), quantile = 0),
        .,
        mutate(filter(., epi_week == max(epi_week)), quantile = 0)
      )
    }) %>%
    ungroup %>%
    mutate(zona = factor(zona, levels = zonas, ordered = TRUE))
  
  
  
  plot <- inscritos_actuales %>% 
    ggplot(aes(x = epi_week)) +
    geom_polygon(
      data = semanal_previos_poly,
      aes(y = quantile, fill = zona)
    ) +
    geom_line(
      data = semanal_previos_range, linetype = "dashed", size = 0.8,
      aes(y = quantile, group = zona)
    ) +
    geom_line(aes(y = casos), size=1.5) +
    labs(
      x = bquote(bold("Semana epidemiológica")),
      y = bquote(bold("Número de casos")),
      fill = "Áreas"
    ) +
    scale_x_continuous(
      expand = c(0, 0), breaks = c(1, seq(5, 53, by = 5)), limits = c(1, 53)
    ) +
    scale_y_continuous(
      expand = c(0, 0), limits = c(0, ylim),
      oob = function (x, range = c(0, 1), only.finite = TRUE){
        force(range)
        finite <- if (only.finite) 
          is.finite(x)
        else TRUE
        x[finite & x < range[1]] <- 0
        x[finite & x > range[2]] <- ylim
        x
      }
    ) +
    scale_fill_manual(
      values = c("grey80", "tomato", "khaki1", "chartreuse4"),
      guide = guide_legend(reverse=TRUE)
    ) +
    scale_size_manual(values = c(1, 0.7)) +
    coord_fixed(ratio = 0.6) +
    theme_bw(base_size = 16) +
    theme(
      panel.spacing.y = grid::unit(1, "line"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color ="black")
    )
  
  if(null){
    plot <- plot + theme(panel.ontop = TRUE)
    interactive <- FALSE
  }
  
  if(interactive){
    plotly::ggplotly(
      plot + theme(legend.justification=c(0,0), legend.position=c(0,0))
    )
  } else {
    plot
  }
}



#------------------------------------------------------------------------------*
# Prepare minimal datasets ----
#------------------------------------------------------------------------------*
# Minimal dataset containing case data and etiologies based on provided
# definitions
#------------------------------------------------------------------------------*


minimal_dataset_etiologies <- function(.data, condition, ...){
  # rlang evaluation
  dots <- quos(..., .named = TRUE)
  condition <- enquo(condition)
  
  .data %>%
    tbl_df() %>%
    mutate(
      service_date = as.Date(fechaHoraAdmision)
    ) %>%
    filter(
      !!condition,
      SiteName %in% places,
      service_date < this_month_start + months(1),
      !is.na(SASubjectID),
      # Drop duplicates
      !duplicated(SubjectID)
    ) %>%
    # Organize site and patient data
    mutate(
      site_type = ifelse(
        test = SiteType == "H",
        yes = "hospital",
        no = "centro de salud"
      ),
      site_name = SiteName,
      site_department = SiteDepartamento,
      epi_year = epiYearAdmision,
      epi_week = epiWeekAdmision,
      subject_department = departamento,
      subject_municipality = municipio,
      subject_community = censo_comunidad,
      age_years = edadAnios,
      age_months = edadMeses,
      age_days = edadDias,
      age_aprox = age_years * 365 +
        pmax(0, age_months * 30, na.rm = TRUE) +
        pmax(0, age_days, na.rm = TRUE),
      age_dates = as.Date(fechaHoraAdmision) - ymd(fechaDeNacimiento)
    ) %>%
    # Assign etiologies. NA does not apply, TRUE is positive, FALSE is negative
    mutate(!!!dots) %>%
    select(
      unique_id = SubjectID, subject_id = SASubjectID,
      service_date, epi_year, epi_week,
      site_type, site_name, site_department,
      subject_department, subject_municipality, subject_community,
      age_years, age_months, age_days, age_aprox, age_dates,
      one_of(names(dots))
    ) %>%
    arrange(
      site_type, site_department, service_date
    )
}





#------------------------------------------------------------------------------*
# Plot epidemic curves ----
#------------------------------------------------------------------------------*
# Epidemic curves showing detection of selected pathogens over the years
#------------------------------------------------------------------------------*

plot_epidemic_curve <- function(
  .data, show_site_type = "none", etiology_names, syndrome = NULL,
  interactive = TRUE, tooltip = "none", ...
){
  # rlang evaluation
  etiology_names <- rlang::splice(etiology_names)
  
  plot <- .data %>%
    mutate(
      site_department = recode_factor(
        site_department,
        SR = "Santa Rosa",
        QU = "Quetzaltenango"
      )
    ) %>%
    filter(
      site_type == show_site_type,
      !is.na(epi_year)
    ) %>%
    select(
      site_department, epi_year, epi_week, site_type,
      include = names(etiology_names)
    ) %>%
    gather(key = etiology, value = value, -c(site_department:site_type)) %>%
    mutate(
      etiology = factor(
        recode(etiology, etiology_names),
        levels = unlist(etiology_names),
        ordered = TRUE
      )
    ) %>%
    filter(value) %>%
    count(site_department, etiology, epi_year, epi_week) %>%
    complete(
      site_department, etiology, epi_year, epi_week,
      fill = list(n = 0)
    ) %>% 
    ggplot(aes(x = epi_week)) +
    geom_bar(
      aes(fill = site_department, y = n),
      stat = "identity", position = "stack"
    ) +
    labs(
      x = "Semana epidemiológica",
      y = "Número de casos",
      fill = "Sitio"
    ) +
    facet_grid(epi_year ~ etiology, as.table = FALSE, drop = FALSE) +
    coord_equal()

  if(interactive){
    plotly::ggplotly(plot, tooltip = tooltip)
  } else {
    plot
  }
}




#------------------------------------------------------------------------------*
# Plot etiologies map ----
#------------------------------------------------------------------------------*
# Interactive map showing detected pathogens over the few recent months
#------------------------------------------------------------------------------*

# Used package
library(package = leaflet)

interactive_case_map <- function(
  .data, department, etiology_names = NULL, n_months = 12,
  base_tiles = "carto-light"
){
  # rlang evaluation
  etiology_names <- rlang::splice(etiology_names)
  
  # Set tile options
  tiles <- frame_data(
    ~tile,         ~url,
    "wikimedia",   "https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}.png",
    "transport",   "http://{s}.tile.opencyclemap.org/transport/{z}/{x}/{y}.png",
    "carto-toner", "http://a.tile.stamen.com/toner/{z}/{x}/{y}.png",
    "carto-light", "http://a.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png"
  )
  
  if(!base_tiles %in% tiles$tile){
    tiles <- tiles %>%
      bind_rows(
        data_frame(
          tile = base_tiles,
          url = paste0("http://{s}.api.cartocdn.com/", base_tiles, "/{z}/{x}/{y}.png")
        )
      )
  }
  
  
  cases_locations <- .data %>%
    mutate(
      department_name = recode(
        subject_department,
        `1` = "Guatemala",
        `6` = "Santa Rosa",
        `9` = "Quetzaltenango",
        .default = NA_character_
      )
    ) %>%
    filter(
      department_name == department,
      between(
        as.Date(service_date),
        as.Date(last_friday - months(n_months)),
        as.Date(last_friday)
      )
    ) %>%
    left_join(locations) %>%
    select(
      department_name, subject_department, subject_community, community_code,
      service_date,
      # Etiologies
      one_of(names(etiology_names))
    ) %>%
    gather(key = etiology, value = result, one_of(names(etiology_names))) %>%
    filter(result) %>%
    mutate(
      etiology = factor(
        recode(etiology, etiology_names),
        levels = unlist(etiology_names),
        ordered = TRUE
      ),
      label = paste(
        etiology,
        service_date,
        sep = "</br>"
      )
    ) %>% 
    # count(
    #   department_name, subject_department, subject_community, community_code,
    #   etiology
    # ) %>%
    left_join(select(communities, community_code = group, long, lat)) %>%
    filter(!is.na(community_code), !is.na(long), !is.na(lat))
  
  
  # Get present etiologies
  etiologies <- unlist(etiology_names)
  
  # Set color palette
  pal <- colorFactor(
    palette = RColorBrewer::brewer.pal(length(etiologies), "Set1"),
    domain = unlist(etiologies)
  )
  
  # Set map options
  map_options <- list(
    minZoom = 10,
    maxZoom = 15
  )

  # Set boundary coordinates
  boundary <- departments %>%
    filter(region == department) %>%
    summarize(
      lat1 = max(lat) + 0.005,
      lng1 = min(long),
      lat2 = min(lat) - 0.005,
      lng2 = max(long)
    )
  
  # Offset
  offset <- 5e-4
  
  # Map
  if(nrow(cases_locations) > 0){
    cases_locations %>%
      mutate_at(
        vars(long_offset = long, lat_offset = lat),
        funs(. + rnorm(n(), mean = offset, sd = offset))
      ) %>%
      leaflet() %>%
      setMaxBounds(
        lat1 = boundary$lat1,
        lng1 = boundary$lng1,
        lat2 = boundary$lat2,
        lng2 = boundary$lng2
      ) %>%
      fitBounds(
        lat1 = boundary$lat1 - 0.06,
        lng1 = boundary$lng1 + 0.01,
        lat2 = boundary$lat2 + 0.06,
        lng2 = boundary$lng2 - 0.01
      ) %>%
      addTiles(
        urlTemplate = tiles[tiles$tile==base_tiles,]$url,
        options = map_options
      ) %>%
      # Draw places
      addLabelOnlyMarkers(
        data = count(getMapData(.), subject_community, long, lat),
        label = ~subject_community,
        labelOptions = labelOptions(
          textOnly = TRUE, opacity = 0.3, offset = c(0, 0),
          permanent = TRUE, noHide = TRUE,  direction = "center"
        ),
        lng = ~long, lat = ~lat, group = "Comunidades"
      ) %>%
      # Draw cases
      addCircleMarkers(
        lng = ~long_offset, lat = ~lat_offset, color = ~pal(etiology),
        # clusterOptions = markerClusterOptions(),
        popup = ~label,
        radius = 6
      ) %>%
      addLayersControl(
        overlayGroups = "Comunidades", position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(group = "Comunidades") %>%
      addLegend(
        position = "bottomleft", pal = pal, values = ~etiology,
        title = "Etiología",
        opacity = 1
      )
  } else {
    cat(
      "**No se detectó ninguna de las siguientes etiologías:**\n\n",
      paste0("- ", etiology_names, "\n"),
      sep = ""
    )
  }
}



#------------------------------------------------------------------------------*
# Plot etiologies map ----
#------------------------------------------------------------------------------*
# Interactive map showing detected pathogens over the few recent months
#------------------------------------------------------------------------------*

# Load used package
library(package = dygraphs)

# Function to plot dygraphs
case_timeseries <- function(
  .data, sites, etiology_name = NULL, group, ylim,
  scale = "month", count_all = FALSE
){
  
  # Configure time scale
  scale_unit <- case_when(
    scale == "week" ~ "Semana",
    scale == "month" ~ "Mes",
    scale == "quarter" ~ "Trimestre",
    scale == "year" ~ "Año"
  )

  # Tags
  .data <- .data %>%
    mutate(
      service_date = floor_date(service_date, unit = scale),
      site_department = recode_factor(
        site_department,
        SR = "Santa Rosa",
        QU = "Quetzaltenango"
      ),
      site_name = recode_factor(
        site_name,
        "H-Cuilapa" = "Hospital de Cuilapa",
        "H-Xela" = "Hospital de Quetzaltenango",
        "CS-NSR" = "Centro de Salud NSR"
      )
    )

  # Manage site selection
  if(!count_all){
    cases <- .data %>%
      filter(site_type == sites) %>%
      select(site_name, service_date, one_of(etiology_name)) %>%
      group_by(site_name, service_date) %>%
      summarize_all(
        funs(sum(., na.rm = TRUE))
      ) %>%
      ungroup %>%
      gather(key = etiology, value = cases, -c(site_name, service_date)) %>%
      spread(key = site_name, value = cases) %>%
      arrange(service_date) %>%
      select(service_date, everything(), -etiology) %>%
      as.data.frame()
  } else {
    max_cases <- .data %>%
      select(site_type, site_name, service_date, one_of(names(etiologies))) %>%
      group_by(site_type, site_name, service_date) %>%
      summarize_all(
        funs(sum(., na.rm = TRUE))
      ) %>%
      ungroup %>%
      gather(
        key = etiology, value = cases,
        -c(site_type, site_name, service_date)
      ) %>%
      magrittr::use_series(cases) %>%
      max()
    
    return(max_cases)
  }
  
  # Convert to eXtended Time Series object
  cases <- xts::xts(
    x = subset(cases, select = -1),
    order.by = cases[, 1]
  )
  
  # Prepare dygraph
  cases %>%
    dygraph(
      # main = etiologies[[etiology_name]],
      xlab = scale_unit, ylab = "Número de casos",
      group = group,
      height = 200
    ) %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
    dyLegend(show = "onmouseover") %>%
    # dyHighlight(
    #   hideOnMouseOut = FALSE, 
    #   highlightSeriesBackgroundAlpha = 0.5,
    #   highlightSeriesOpts = list(strokeWidth = 3)
    # ) %>%
    dyAxis(name = "y", valueRange = c(0, ylim)) %>%
    dyRangeSelector(height = 15)
}


# End of script
