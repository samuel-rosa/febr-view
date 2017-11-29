# Calcular contribuições e gerar metadados para website #######################################################
createSiteMetadata <-
  function (n, dataset, observation, layer, metadata, sharing) {
    
    # Preparar URL
    docs_sheet <- "https://docs.google.com/spreadsheets/d/"
    drive_folder <- "https://drive.google.com/open?id="
    metadata <- paste(docs_sheet, metadata, sep = "")
    sharing <- paste(drive_folder, sharing, sep = "")
    
    # Definir opções de local
    locale <- readr::locale(decimal_mark = ",")
    
    # Descarregar dados
    dataset <- suppressMessages(
      googlesheets::gs_read_csv(
        googlesheets::gs_key(dataset), verbose = FALSE, na = c("NA", "-", ""), locale = locale))
    obs_cols <- c("observacao_id", "estado_id", "coord_sistema", "coord_x", "coord_y")
    observation <- suppressMessages(
      googlesheets::gs_read_csv(
        googlesheets::gs_key(observation), verbose = FALSE, na = c("NA", "-", ""), 
        locale = locale)[, obs_cols])
    layer <- suppressMessages(
      googlesheets::gs_read_csv(
        googlesheets::gs_key(layer), comment = "unidade", verbose = FALSE, na = c("NA", "-", ""), 
        locale = locale))
    
    # Processar dados das coordenadas espaciais -- independente de possuir ou não dados de ferro -- com 
    # transformação do sistema de referência de coordenadas para EPSG:4326 (se necessário)
    obs_coords <- na.exclude(observation)
    if (nrow(obs_coords) >= 1) {
      if (nlevels(as.factor(obs_coords$coord_sistema)) > 1) {
        obs_coords <- split(obs_coords, as.factor(obs_coords$coord_sistema))
        if ("EPSG:4326" %in% names(obs_coords)) {
          j <- which(!names(obs_coords) %in% "EPSG:4326")
        } else {
          j <- 1:length(obs_coords)
        }
        obs_coords[j] <- lapply(obs_coords[j], function (x) {
          sp::coordinates(x) <- c("coord_x", "coord_y")
          sp::proj4string(x) <- sp::CRS(paste("+init=", tolower(x$coord_sistema[1]), sep = ""))
          x <- sp::spTransform(x, sp::CRS("+init=epsg:4326"))
          as.data.frame(x)
        })
        obs_coords <- do.call(rbind, obs_coords)
        
      } else if (unique(obs_coords$coord_sistema) != "EPSG:4326") {
        sp::coordinates(obs_coords) <- c("coord_x", "coord_y")
        sp::proj4string(obs_coords) <- 
          sp::CRS(paste("+init=", tolower(obs_coords$coord_sistema[1]), sep = ""))
        obs_coords <- sp::spTransform(obs_coords, sp::CRS("+init=epsg:4326"))
        obs_coords <- as.data.frame(obs_coords)
      }
      obs_coords$observacao_id <- paste(obs_coords$observacao_id, "@", n, sep = "")
      obs_coords$sharing <- paste('<a href="', sharing, '">Acessar dados</a>', sep = "")
      obs_coords$mailto <- paste(
        '<a href="mailto:febr-forum@googlegroups.com?subject=', obs_coords$observacao_id, 
        '">Relatar problema</a>', sep = "")
      rownames(obs_coords) <- NULL
      
      # Salvar arquivo com as coordenadas das observações para publicação no website
      # file <- paste("./febr-website/data/", n, "-coords.csv", sep = "")
      file <- glue('../data/{n}-coords.csv')
      write.csv(
        obs_coords[, c("observacao_id", "coord_x", "coord_y", "sharing", "mailto")], 
        file = file, fileEncoding = "UTF-8") 
      
    }
    
    # Agregar dados das observações e camadas
    # Usar apenas as colunas necessárias: assume-se que o número máximo de colunas necessárias da tabela
    # 'layer' seja 15.
    db <- merge(
      observation[, c("observacao_id", "estado_id")],
      layer[, 1:ifelse(ncol(layer) > 15, 15, ncol(layer))],
      by = "observacao_id")
    
    # Identificar linhas e colunas contendo dados de ferro
    # Gerar metadados apenas se realmente houver dados de ferro
    # id_col <- colnames(db)[grep("^fe_", colnames(db))]
    # if (length(id_col) > 0) {
      # idx <- which(!is.na(db[, id_col]), arr.ind = TRUE)
      # if (is.null(dim(idx))) {
        # id_row <- idx
      # } else if (prod(dim(idx)) == 0) {
        # cat("Não há dados de ferro")
        # return (NULL)
      # } else {
        # id_col <- id_col[unique(idx[, 2])]
        # id_row <- unique(idx[, 1])
      # }
      
      # Preparar nome dos autores. Para conjuntos de dados com múltiplos autores, apenas os dois primeiros
      # são apresentados.
      Nome <- stringr::str_split_fixed(dataset[dataset$item == "autor_nome", 2], ";", n = Inf)
      if (length(Nome) > 2) {
        Nome <- paste(paste(Nome[1:2], collapse = "; "), "et alli")
      } #else {
      # Nome <- paste(Nome, collapse = "; ")
      # }
      
      # Preparar descrição da contribuição 
      ctb <- data.frame(
        Nome = paste(Nome, collapse = "; "),
        Instituição =
          stringr::str_split_fixed(dataset[dataset$item == "organizacao_nome", 2], ";", n = Inf)[1],
        Título = dataset[[2]][dataset$item == "dataset_titulo"],
        # UF = levels(as.factor(db[id_row, "estado_id"])),
        UF = levels(as.factor(db[, "estado_id"])),
        # Contribuição = summary(as.factor(na.omit(db[id_row, "estado_id"]))),
        Contribuição = summary(as.factor(na.omit(db[, "estado_id"]))),
        # Por padrão, se mais de uma área do conhecimento é especificada, então assume-se que o trabalho é
        # do tipo EDAFOLÓGICO.
        Tipo = ifelse(
          dataset[[2]][dataset$item == "area_conhecimento"] == "Gênese, Morfologia e Classificação dos Solos",
          "PEDOLÓGICO", "EDAFOLÓGICO"),
        url = sharing)
      rownames(ctb) <- NULL
      print(ctb)
      
      # Salvar arquivo com descrição da contribuição para publicação no website
      # file <- paste("./febr-website/data/", n, ".csv", sep = "")
      file <- glue('../data/{n}.csv')
      write.csv(ctb, file = file, fileEncoding = "UTF-8")
      
    # } else {
      # cat("Não há dados de ferro")
    # }
  }
