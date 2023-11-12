# Librería de funciones
#
# Definición de funciones usadas para el procesado del dataset del INE sobre
# variaciones residenciales.
#
# Proyecto AED 2023
#
# Authors: Hinarejos, J.; Ortega, S.

if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(kableExtra, readxl, stringr, tidyr, dplyr, ggmap, leaflet, leaflet.extras2, sf, colorspace)

split_raw_data <- function(filtered_raw_data, vars_table) {
  
  splitted_raw_data <- list()
  for (var_idx in 1:nrow(vars_table)) {
    
    # Dividimos cada entrada según las posiciones y longitudes del excel
    aux <- substr(
      filtered_raw_data,
      vars_table$Posición[var_idx],
      vars_table$Posición[var_idx] + vars_table$Longitud[var_idx] - 1)
    
    splitted_raw_data[[vars_table$Variable[var_idx]]] <- aux
  }
  
  raw_data_df <- data.frame(splitted_raw_data)
  
  # Añadimos a los códigos de municipios los códigos de provincia para que sean únicos
  raw_data_df <- make_unique_codes(raw_data_df)
  
  return(raw_data_df)
}

make_unique_codes <- function(raw_data_df) {
  
  muni_vars <- grep("^MUNI", names(raw_data_df), value = TRUE)
  if (length(muni_vars) > 1) {
    
    for (muni_var in muni_vars) {
      
      prov_var <- grep(
        paste0("PROV", str_extract(muni_var, "(ALTA|NAC|BAJA)")),
        names(raw_data_df),
        value = TRUE)
      
      not_na_or_blank <- complete.cases(raw_data_df[c(muni_var, prov_var)]) &
        ! ( grepl("^[ \t]*$", raw_data_df[[muni_var]]) |
              grepl("^[ \t]*$", raw_data_df[[prov_var]]) )
      
      raw_data_df[not_na_or_blank, muni_var] <- paste0(
        raw_data_df[not_na_or_blank, prov_var],
        raw_data_df[not_na_or_blank, muni_var])
    }
  }
  return(raw_data_df)
}

load_excel_dicts <- function(excel_dict_file, main_sheet = "Diseño") {
  
  sheet_names <- excel_sheets(excel_dict_file)
  sheet_list <- list()
  
  for (sheet_name in sheet_names[sheet_names != main_sheet]) {
    
    sheet_data <- read_excel(excel_dict_file, sheet = sheet_name,
                             col_names = FALSE)
    
    # Añadimos el nombre del diccionario del anexo
    if (sheet_name == "Anexo - Lista de países") {
      sheet_data[4, 1] <- "T_MUNI"
    }
    sheet_list[[sheet_name]] <- sheet_data
  }
  return(sheet_list)
}

create_dict_list <- function(vars_table) {
  
  # Cogemos la información sobre los diccionarios
  dict_df <- vars_table[c("Diccionario de la variable",
                          "Diccionario ubicado en la hoja…")]
  colnames(dict_df) <- c("name", "sheet")
  dict_df <- unique( dict_df[complete.cases(dict_df), ] )
  
  # Creamos la lista con la información de cada diccionario
  dict_list <- list()
  for (idx in 1 : nrow(dict_df)) {
    
    related_vars <- vars_table$Variable[
      (vars_table$`Diccionario de la variable` ==
         dict_df$name[idx]) &
        (!is.na(vars_table$`Diccionario de la variable`))]
    
    dict_list[[ dict_df$name[idx] ]] <- list(sheet = dict_df$sheet[idx],
                                             vars = related_vars,
                                             is_obs = FALSE)
  }
  
  # Añadimos la información del diccionario indicado en observaciones
  related_vars <- vars_table$Variable[!is.na(vars_table$Observaciones)]
  dict_list[["T_MUNI"]] <- list(sheet = "Anexo - Lista de países",
                                vars = related_vars,
                                is_obs = TRUE)
  return(dict_list)
}

get_dict_from_sheets <- function(dict_list, sheet_list, aditional_data) {
  
  # Inicializamos la tabla-diccionario
  col_names <- c("name", "code", "value")
  dict_info <- data.frame(matrix(nrow = 0, ncol = length(col_names)))
  colnames(dict_info) <- col_names
  
  # Buscamos cada diccionario
  for (sheet_name in names(sheet_list)) {
    
    sheet_table <- sheet_list[[sheet_name]]
    
    for(dict_name in names(dict_list)) {
      
      if (dict_list[[dict_name]]$sheet == sheet_name) {
        
        dict_start <- which(sheet_table == dict_name, arr.ind = TRUE)
        dict_row_end <- dict_start[1]
        while ( (dict_row_end + 1) <= nrow(sheet_table) ) {
          
          if ( is.na(sheet_table[[(dict_row_end + 1), dict_start[2]]]) ) {
            break
          }
          dict_row_end <- dict_row_end + 1
        }
        # Creamos una tabla con el diccionario
        dict_df <- sheet_table[
          (dict_start[1] + 2) : dict_row_end,
          dict_start[2] : (dict_start[2] + 1)]
        
        if (dict_list[[dict_name]]$is_obs) {
          
          # Añadimos el código de provincia de extranjero
          dict_df[, 1] <- paste0("66", dict_df[[1]])
          aditional_data <- prepare_aditional_data(aditional_data, names(dict_df))
          dict_df <- rbind(dict_df, aditional_data)
        }
        dict_df <- cbind(dict_name, dict_df)
        
        # Añadimos cada diccionario a la tabla
        colnames(dict_df) <- colnames(dict_info)
        dict_info <- rbind(dict_info, dict_df)
      }
    }
  }
  return(dict_info)
}

prepare_aditional_data <- function(aditional_data, col_names) {
  
  if (any(colnames(aditional_data) != col_names)) {
    
    aditional_data <- aditional_data[c("CPRO", "CMUN", "NOMBRE")]
    aditional_data <- aditional_data %>%
      unite(CMUN_complete, CPRO, CMUN, sep = "")
    colnames(aditional_data) <- col_names
  }
  return(aditional_data)
}

apply_dict_to_data <- function(data_df, dict_info, dict_list) {
  
  for (dict_name in names(dict_list)) {
    
    this_dict <- dict_info[dict_info$name == dict_name, ]
    
    for (var_name in dict_list[[dict_name]]$vars) {
      
      data_df[var_name] <- apply_dict_to_variable(data_df[[var_name]], this_dict)
    }
  }
  return(data_df)
}

apply_dict_to_variable <- function(codes, dict) {
  
  values <- factor(
    x = codes,
    levels = dict$code,
    labels = dict$value)
  return(values)
}

check_na_procedence <- function(raw_data_df, data_df) {
  
  if ( any(colnames(raw_data_df) != colnames(data_df)) ) {
    stop("check_na_procedence: Las variables de los objetos data.frame deben coincidir.")
  }
  
  na_count <- colSums(is.na(data_df))
  vars_with_na <- names(which(na_count > 0))
  
  if (length(vars_with_na) < 1) {
    
    print("check_na_procedence: No se ha introducido ningún NA.")
    return(NULL)
    
  } else if (length(vars_with_na) < length(colnames(data_df))) {
    
    print("check_na_procedence: Omitidas variables sin NAs en la tabla resumen.")
  }
  
  na_df <- data.frame(
    variable = vars_with_na,
    introduced_na = na_count[vars_with_na],
    row.names = NULL)
  
  messages <- c()
  for (var_name in na_df$variable) {
    
    lost_values <- unique(raw_data_df[is.na(data_df[[var_name]]), var_name])
    
    all_spaces_or_tabs <- all(grepl("^[ \t]*$", lost_values))
    
    if ( all_spaces_or_tabs ) {
      
      message <- "Todos los NAs corresponden a entradas en blanco."
      
    } else {
      
      message <- paste("Valores perdidos:",
                       paste(lost_values, collapse = ", "))
    }
    messages <- c(messages, message)
  }
  na_df["message"] <- messages
  return(na_df)
}

convert_numeric_vars <- function(data_df, vars_table) {
  
  is_numeric <- vars_table$Tipo == "N"
  
  for (var_name in vars_table$Variable[is_numeric]) {
    
    data_df[var_name] <- as.numeric(data_df[[var_name]])
  }
  return(data_df)
}

add_comu_variables <- function(data_df, municipios_data, raw_data_df) {
  
  # Diccionario para obtener COMU a partir de PROV
  prov_to_comu <- municipios_data[c("CPRO", "CODAUTO")]
  prov_to_comu <- prov_to_comu %>%
    distinct(CODAUTO, CPRO)
  prov_to_comu <- rbind(
    prov_to_comu,
    list("CPRO" = "66", "CODAUTO" = "66"))
  
  # Diccionario para interpretar COMU
  comu_dict <- get_comu_dict()
  
  var_sufixes <- c("BAJA", "ALTA", "NAC")
  
  for (sufix in var_sufixes) {
    
    data_df[paste0("COMU", sufix)] <- factor(
      x = raw_data_df[[paste0("PROV", sufix)]],
      levels = prov_to_comu$CPRO,
      labels = prov_to_comu$CODAUTO)
    
    data_df[paste0("COMU", sufix)] <- factor(
      x = data_df[[paste0("COMU", sufix)]],
      levels = comu_dict$code,
      labels = comu_dict$value)
  }
  return(data_df)
}

get_comu_dict <- function(comu_dict_dir = "./data/comu_dict.RData") {
  
  if (file.exists(comu_dict_dir)) {
    
    load(comu_dict_dir)
    
  } else {
    
    comu_dict <- data.frame(
      code = c(paste0("0", 1:9), as.character(10:19), "66"),
      value = c("Andalucía", "Aragón", "Asturias", "Islas Baleares",
                "Canarias", "Cantabria", "Castilla y León",
                "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana",
                "Extremadura", "Galicia", "Madrid", "Murcia", "Navarra",
                "País Vasco", "La Rioja", "Ceuta", "Melilla", "Extranjero"))
    
    save(
      comu_dict,
      file = comu_dict_dir)
  }
  return(comu_dict)
}

get_prov_location <- function(fileDir, dict_prov = NULL, googleKey = NULL, useKey = FALSE) {
  
  generate_prov_locations <- function(dict_prov, fileDir, googleKey, useKey) {
    
    if (useKey) {
      dict_prov <- dict_info[dict_info$name == "T_PROV", ]
      if ("name" %in% colnames(dict_prov)) {
        dict_prov <- dict_prov %>%
          dplyr::select(-name) %>%
          mutate(long = NA, lat = NA)
      }
      if (!ggmap::has_google_key()) {
        ggmap::register_google(key = googleKey)
      }
      for (prov_name in dict_prov$value) {
        if (prov_name == "Extranjero") {
          search_name <- "France"
        } else {
          search_name <- paste0(prov_name, ", Spain")
        }
        prov_location <-geocode(search_name)

        dict_prov$long[dict_prov$value == prov_name] <- prov_location[["lon"]]
        dict_prov$lat[dict_prov$value == prov_name]  <- prov_location[["lat"]]
      }
      save(
        dict_prov, 
        file =  fileDir)
    } else {
      stop("Para generar la tabla de localizaciones de provincias, añada el argumento googleKey y useKey = TRUE.")
    }
    return(dict_prov)
  }
  
  if (file.exists(fileDir)) {
    
    load(fileDir) # -> dict_prov
    
  } else {
    
    dict_prov <- generate_prov_locations(dict_info, fileDir, googleKey, useKey)
  }
  return(dict_prov)
}

get_net_prov_movements <- function(data_df, selected_prov) {
  
  prov_data <- data_df %>%
    dplyr::select(PROVBAJA, PROVALTA) %>%
    filter(PROVBAJA != "Extranjero" & PROVALTA != "Extranjero") %>%
    filter(xor(PROVBAJA == selected_prov, PROVALTA == selected_prov)) %>%
    group_by(value = factor(
      x = ifelse(PROVBAJA == selected_prov, PROVALTA, PROVBAJA),
      levels = 1:length(levels(PROVALTA)),
      labels = levels(PROVALTA))) %>%
    summarise(net_count = sum(ifelse(PROVBAJA == selected_prov, -1, 1)))
  
  return(prov_data)
}

plot_residence_variation_map <- function(prov_data, dict_prov, selected_prov) {
  
  selected_prov_coords <- dict_prov %>%
    filter(value == selected_prov)
  
  target_provs <- dict_prov %>%
    filter(value != selected_prov) %>%
    filter(value %in% prov_data$value)
  
  col_names <- c("value", "geom", "description")
  line_data <- data.frame(matrix(nrow = 0, ncol = length(col_names)))
  colnames(line_data) <- col_names
  
  for (i in 1:nrow(prov_data)) {
    
    this_prov <- list()
    
    target_prov <- prov_data$value[i]
    target_coords <- target_provs %>%
      filter(value == target_prov) %>%
      dplyr::select(long, lat)
    
    this_prov$value <- as.character(prov_data$net_count[i])
    
    if (prov_data$net_count[i] > 0) {
      
      this_prov$geom <- paste0(
        "LINESTRING(", selected_prov_coords$long, " ",
        selected_prov_coords$lat, ",",
        target_coords$long, " ",
        target_coords$lat, ")")
      
      this_prov$description <- paste0(
        selected_prov, "-", target_prov)
    
    } else {
      
      this_prov$geom <- paste0(
        "LINESTRING(", target_coords$long, " ",
        target_coords$lat, ",",
        selected_prov_coords$long, " ",
        selected_prov_coords$lat, ")")
      
      this_prov$description <- paste0(
        target_prov, "-", selected_prov)
    }
    line_data <- rbind(line_data, this_prov)
  }
  
  line_data <- st_as_sf(line_data, wkt = "geom")
  
  color_domain <- abs(prov_data$net_count)
  color_palette <- rainbow_hcl(n = length(color_domain), c = 100)
  color_scale <- colorNumeric(
    palette = color_palette,
    domain = color_domain)
  
  residence_variations_map <- leaflet(
    data = target_provs) %>%
    addTiles(urlTemplate = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
    addCircleMarkers(
      radius = 3,
      color = "blue",
      label = ~value,
      group = "Provincias") %>%
    addCircleMarkers(
      data = selected_prov_coords,
      radius = 3,
      color = "red",
      popup = ~value,
      group = "Provincia seleccionada")
  
  for (i in 1 : nrow(line_data)) {
    
    residence_variations_map <- residence_variations_map %>%
      addArrowhead(
        data = line_data[i, ],
        weight = 5,
        opacity = 0.6,
        color = color_scale(color_domain[i]),
        label = ~description,
        popup = ~value,
        options = arrowheadOptions(
          yawn = 45,
          size = "10000m"
        ),
        group = "Variaciones residenciales")
  }
  
  residence_variations_map <- residence_variations_map %>%
    addLegend(
      values = color_domain,
      pal = color_scale,
      title = "Número de desplazados",
      position = "bottomright",
      group = "Variaciones residenciales") %>%
    addLayersControl(
      overlayGroups = c("Provincias", "Provincia seleccionada", "Variaciones residenciales")) %>%
    addScaleBar(
      position = "bottomleft")
  
  return(residence_variations_map)
}

get_country_location <- function(fileDir, country_data = NULL, googleKey = NULL, useKey = FALSE) {
  
  generate_country_location <- function(country_data, fileDir, googleKey, useKey) {
    
    if (useKey) {
      countries <- unique(country_data$value)
      
      if (!"Spain" %in% countries) {
        countries <- c(countries, "Spain")
      }
      dict_countries <- data.frame(
        value = countries,
        long = NA,
        lat = NA)
      
      if (!ggmap::has_google_key()) {
        ggmap::register_google(key = googleKey)
      }
      for (country in countries) {
        location <-geocode(country)
        if(country %in% c("Mauricio", "Santa Lucía")) {
          country <- paste(country, "Island")
        }
        dict_countries$long[dict_countries$value == country] <- location[["lon"]]
        dict_countries$lat[dict_countries$value == country]  <- location[["lat"]]
      }
      save(
        dict_countries,
        file = fileDir)
    } else {
      stop("Para generar la tabla de localizaciones de provincias, añada el argumento googleKey y useKey = TRUE.")
    }
    return(dict_countries)
  }
  
  if (file.exists(fileDir)) {
    
    load(fileDir) # -> dict_countries
    
  } else {
    
    dict_countries <- generate_country_location(country_data, fileDir, googleKey, useKey)
  }
  return(dict_countries)
}

get_net_country_movements <- function(data_df) {
  
  country_data <- data_df %>%
    dplyr::select(MUNIBAJA, PROVBAJA, MUNIALTA, PROVALTA) %>%
    # Limipieza de entradas no útiles
    filter(
      !MUNIBAJA %in% c("No Consta", "Baja por Caducidad") &
      !MUNIALTA %in% c("No Consta", "Baja por Caducidad")) %>%
    # Conversión de factor a character por simplicidad
    mutate(
      MUNIBAJA = as.character(MUNIBAJA),
      MUNIALTA = as.character(MUNIALTA)) %>%
    # Extracción de país
    mutate(
      PAISBAJA = ifelse(
        PROVBAJA == "Extranjero",
        MUNIBAJA,
        "Spain"),
      PAISALTA = ifelse(
        PROVALTA == "Extranjero",
        MUNIALTA,
        "Spain")) %>%
    filter(xor(PAISBAJA == "Spain", PAISALTA == "Spain")) %>%
    group_by(value = ifelse(
      PAISBAJA == "Spain",
      PAISALTA,
      PAISBAJA)) %>%
    summarise(net_count = sum(ifelse(PAISBAJA == "Spain", -1, 1)))
  
  return(country_data)
}

plot_countries_map <- function(country_data, dict_countries) {
  
  spain_coords <- dict_countries %>%
    filter(value == "Spain")
  
  target_countries <- dict_countries %>%
    filter(value != "Spain") %>%
    filter(value %in% country_data$value)
  
  col_names <- c("value", "geom", "description")
  line_data <- data.frame(matrix(nrow = 0, ncol = length(col_names)))
  colnames(line_data) <- col_names
  
  for (i in 1:nrow(country_data)) {
    
    this_country <- list()
    
    target_country <- country_data$value[i]
    target_coords <- target_countries %>%
      filter(value == target_country) %>%
      dplyr::select(long, lat)
    
    this_country$value <- as.character(country_data$net_count[i])
    
    if (country_data$net_count[i] < 0) {
      
      this_country$geom <- paste0(
        "LINESTRING(", spain_coords$long, " ",
        spain_coords$lat, ",",
        target_coords$long, " ",
        target_coords$lat, ")")
      
      this_country$description <- paste0(
        "Spain-", target_country)
      
    } else {
      
      this_country$geom <- paste0(
        "LINESTRING(", target_coords$long, " ",
        target_coords$lat, ",",
        spain_coords$long, " ",
        spain_coords$lat, ")")
      
      this_country$description <- paste0(
        target_country, "-Spain")
    }
    line_data <- rbind(line_data, this_country)
  }
  
  line_data <- st_as_sf(line_data, wkt = "geom")
  
  color_domain <- abs(country_data$net_count)
  color_palette <- rainbow_hcl(n = length(color_domain), c = 100)
  color_scale <- colorNumeric(
    palette = color_palette,
    domain = color_domain)
  
  residence_variations_map <- leaflet(
    data = target_countries) %>%
    addTiles(urlTemplate = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
    addCircleMarkers(
      radius = 3,
      color = "blue",
      label = ~value,
      group = "Países") %>%
    addCircleMarkers(
      data = spain_coords,
      radius = 3,
      color = "red",
      popup = ~value,
      group = "España")
  
  for (i in 1 : nrow(line_data)) {
    
    residence_variations_map <- residence_variations_map %>%
      addArrowhead(
        data = line_data[i, ],
        weight = 5,
        opacity = 0.6,
        color = color_scale(color_domain[i]),
        label = ~description,
        popup = ~value,
        options = arrowheadOptions(
          yawn = 45,
          size = "10000m"
        ),
        group = "Variaciones residenciales")
  }
  
  residence_variations_map <- residence_variations_map %>%
    addLegend(
      values = color_domain,
      pal = color_scale,
      title = "Número de desplazados",
      position = "bottomright",
      group = "Variaciones residenciales") %>%
    addLayersControl(
      overlayGroups = c("Países", "España", "Variaciones residenciales")) %>%
    addScaleBar(
      position = "bottomleft")
  
  return(residence_variations_map)
}