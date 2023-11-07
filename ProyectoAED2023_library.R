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
pacman::p_load(kableExtra, readxl, stringr, tidyr, dplyr)

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
          dict_df[, 1] <- paste0("66", dict_df[[1]]) ########################
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
      unite(CMUN_complete, CPRO, CMUN, sep = "") ###########
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
