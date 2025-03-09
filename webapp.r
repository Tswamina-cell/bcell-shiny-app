library(Seurat)
library(ShinyCell)
library(RColorBrewer)
library(data.table)
library(reticulate)
library(Matrix)

# Step 1: Create a custom ShinyCell configuration function
createConfig <- function(data_obj, meta.to.include = NA, legendCols = 4, maxLevels = 50) {
  # Extract metadata
  objMeta <- data_obj@meta.data
  
  # Check metadata to include
  if(is.na(meta.to.include[1])) {
    meta.to.include <- colnames(objMeta)
  }
  if(length(meta.to.include) < 2) {
    stop("At least 2 metadata columns are required!")
  }
  
  # Create config data.table
  scConf <- data.table()
  
  for(iMeta in meta.to.include) {
    # Convert characters to factors
    if(is.character(objMeta[[iMeta]])) {
      objMeta[[iMeta]] <- factor(objMeta[[iMeta]])
    }
    
    tmpConf <- data.table(
      ID = iMeta,
      UI = iMeta,
      fID = NA,
      fUI = NA,
      fCL = NA,
      fRow = NA,
      default = 0,
      grp = FALSE
    )
    
    # Process categorical metadata
    if(is.factor(objMeta[[iMeta]])) {
      nLevels <- nlevels(objMeta[[iMeta]])
      
      if(nLevels <= maxLevels) {
        if(nLevels >= 2) {
          tmpConf$fID <- paste0(levels(objMeta[[iMeta]]), collapse = "|")
          tmpConf$fUI <- tmpConf$fID
          tmpConf$fCL <- paste0(colorRampPalette(brewer.pal(12, "Paired"))(nLevels),
                                collapse = "|")
          tmpConf$fRow <- ceiling(nLevels / legendCols)
          tmpConf$grp <- TRUE
        } else if(nLevels == 1) {
          tmpConf$fID <- levels(objMeta[[iMeta]])
          tmpConf$fUI <- tmpConf$fID
          tmpConf$fCL <- "black"
          tmpConf$fRow <- 1
        }
      }
    }
    scConf <- rbindlist(list(scConf, tmpConf))
  }
  
  # Set defaults (prioritize predicted_labels and leiden if they exist)
  def1 <- which(scConf$ID == "predicted_labels")[1]
  def2 <- which(scConf$ID == "leiden")[1]
  
  if(is.na(def1)) {
    def1 <- grep("ident|library", scConf$ID, ignore.case = TRUE)[1]
  }
  if(is.na(def2)) {
    def2 <- grep("clust", scConf$ID, ignore.case = TRUE)[1]
  }
  
  if(is.na(def1)) {def1 <- setdiff(c(1,2), def2)[1]}
  if(is.na(def2)) {def2 <- setdiff(c(1,2), def1)[1]}
  
  if(!is.na(def1)) scConf[def1]$default <- 1
  if(!is.na(def2)) scConf[def2]$default <- 2
  
  return(scConf)
}




# Step 2: Extract motif data from H5AD file
message("Reading motif data from H5AD file...")
anndata <- reticulate::import("anndata")
np <- reticulate::import("numpy")
scipy_sparse <- reticulate::import("scipy.sparse")

adata <- anndata$read_h5ad("/Users/tanushswaminathan/Downloads/donor1_multiome_atac_motif_scores.h5ad")

# Get motif and cell information
motif_names <- adata$var_names$to_list()
cell_names <- adata$obs_names$to_list()

# Extract matrix values
message("Extracting motif scores...")
x_matrix <- adata$X
if ("toarray" %in% names(x_matrix)) {
  # Sparse matrix
  x_array <- x_matrix$toarray()
  motif_matrix <- np$array(x_array)
} else {
  # Already dense
  motif_matrix <- np$array(x_matrix)
}

# Convert to R matrix
motif_data <- t(as.matrix(reticulate::py_to_r(motif_matrix)))

# Set dimension names
rownames(motif_data) <- motif_names
colnames(motif_data) <- cell_names

# Step 2: Find common cells with the Seurat object
common_cells <- intersect(colnames(motif_data), colnames(seurat_obj))
message("Found ", length(common_cells), " common cells between RNA and motif data")

# Step 3: Add motif data to the existing ShinyCell app
shiny_dir <- "~/b_cell_shiny"
motif_dir <- file.path(shiny_dir, "motif_data")
if (!dir.exists(motif_dir)) {
  dir.create(motif_dir, recursive = TRUE)
}

# Save motif data as CSV files
message("Creating CSV files for motif data...")
for (i in 1:nrow(motif_data)) {
  motif_name <- rownames(motif_data)[i]
  motif_scores <- motif_data[i, ]
  
  # Create data frame with scores
  df <- data.frame(
    CellID = names(motif_scores),
    Score = as.numeric(motif_scores)
  )
  
  # Add metadata from Seurat object if available
  common_with_seurat <- intersect(df$CellID, colnames(seurat_obj))
  if (length(common_with_seurat) > 0) {
    meta_subset <- seurat_obj@meta.data[common_with_seurat, , drop = FALSE]
    
    # Match cell IDs
    cell_idx <- match(df$CellID, rownames(meta_subset))
    valid_idx <- !is.na(cell_idx)
    
    # Add each metadata column 
    for (col in colnames(meta_subset)) {
      df[[col]] <- NA
      df[[col]][valid_idx] <- meta_subset[[col]][cell_idx[valid_idx]]
    }
  }
  
  # Add UMAP coordinates if available
  if ("umap" %in% names(seurat_obj@reductions)) {
    umap_coords <- as.data.frame(seurat_obj@reductions$umap@cell.embeddings)
    common_with_umap <- intersect(df$CellID, rownames(umap_coords))
    
    if (length(common_with_umap) > 0) {
      df$UMAP1 <- NA
      df$UMAP2 <- NA
      
      cell_idx <- match(df$CellID, rownames(umap_coords))
      valid_idx <- !is.na(cell_idx)
      
      df$UMAP1[valid_idx] <- umap_coords[cell_idx[valid_idx], 1]
      df$UMAP2[valid_idx] <- umap_coords[cell_idx[valid_idx], 2]
    }
  }
  
  # Save to CSV
  write.csv(df, file = file.path(motif_dir, paste0(motif_name, ".csv")), row.names = FALSE)
}

# Step 4: Modify the ui.R file to add a motif tab
ui_file <- file.path(shiny_dir, "ui.R")

# Read the existing ui.R file
ui_content <- readLines(ui_file)

# Find where to add the motif tab
navbarPage_line <- grep("navbarPage", ui_content)[1]
if (!is.na(navbarPage_line)) {
  # Find the closing parenthesis for navbarPage
  parens_count <- 0
  closing_line <- navbarPage_line
  
  for (i in navbarPage_line:length(ui_content)) {
    # Count opening parentheses
    open_parens <- gregexpr("\\(", ui_content[i])[[1]]
    if (open_parens[1] > 0) {
      parens_count <- parens_count + length(open_parens)
    }
    
    # Count closing parentheses
    close_parens <- gregexpr("\\)", ui_content[i])[[1]]
    if (close_parens[1] > 0) {
      parens_count <- parens_count - length(close_parens)
    }
    
    # If we've found the matching closing parenthesis
    if (parens_count == 0) {
      closing_line <- i
      break
    }
  }
  
  # Create motif tab content
  motif_tab <- c(
    "  ,",
    "  tabPanel('Motif Scores',",
    "    fluidRow(",
    "      column(width = 3,",
    "        wellPanel(",
    "          selectInput('motif', 'Select Motif:', ",
    "                     choices = list.files('motif_data', pattern = '*.csv') %>% ",
    "                               sub('.csv$', '', .),",
    "                     selected = list.files('motif_data', pattern = '*.csv')[1] %>% ",
    "                               sub('.csv$', '', .)),",
    "          hr(),",
    "          checkboxInput('colorByClusters', 'Color by metadata', FALSE),",
    "          conditionalPanel(",
    "            condition = 'input.colorByClusters == true',",
    "            selectInput('clusterCol', 'Metadata Column:', choices = c())",
    "          ),",
    "          hr(),",
    "          sliderInput('pointSize', 'Point Size:', ",
    "                     min = 0.5, max = 5, value = 1.5, step = 0.5),",
    "          sliderInput('opacity', 'Point Opacity:', ",
    "                     min = 0.1, max = 1, value = 0.7, step = 0.1)",
    "        )",
    "      ),",
    "      column(width = 9,",
    "        tabsetPanel(",
    "          tabPanel('Histogram', plotlyOutput('motifHist', height = '500px')),",
    "          tabPanel('UMAP', plotlyOutput('motifUMAP', height = '500px')),",
    "          tabPanel('Data Table', DTOutput('motifTable'))",
    "        )",
    "      )",
    "    )",
    "  )"
  )
  
  # Insert motif tab before the closing parenthesis
  ui_content <- c(
    ui_content[1:closing_line-1],
    motif_tab,
    ui_content[closing_line:length(ui_content)]
  )
  
  # Add required libraries at the top
  library_section <- grep("library\\(", ui_content)
  if (length(library_section) > 0) {
    last_library <- max(library_section)
    
    # Add additional libraries
    additional_libraries <- c(
      "library(plotly)",
      "library(DT)",
      "library(dplyr)"
    )
    
    ui_content <- c(
      ui_content[1:last_library],
      additional_libraries,
      ui_content[(last_library+1):length(ui_content)]
    )
  }
  
  # Write the modified ui.R file
  writeLines(ui_content, ui_file)
  message("Modified ui.R file to add Motif Scores tab")
}

# Step 5: Modify the server.R file to add motif data visualization
server_file <- file.path(shiny_dir, "server.R")

# Read the existing server.R file
server_content <- readLines(server_file)

# Find the end of the server function
end_line <- grep("\\}", server_content)
if (length(end_line) > 0) {
  last_bracket <- max(end_line)
  
  # Create motif server code
  motif_server_code <- c(
    "  # Motif data visualization",
    "  motifData <- reactive({",
    "    req(input$motif)",
    "    df <- read.csv(file.path('motif_data', paste0(input$motif, '.csv')))",
    "    return(df)",
    "  })",
    "  ",
    "  # Create a histogram of motif scores",
    "  output$motifHist <- renderPlotly({",
    "    df <- motifData()",
    "    ",
    "    # Create histogram",
    "    p <- plot_ly(df, x = ~Score, type = 'histogram', ",
    "                nbinsx = 50, marker = list(color = 'steelblue')) %>%",
    "         layout(title = paste('Distribution of', input$motif, 'Scores'),",
    "                xaxis = list(title = 'Score'),",
    "                yaxis = list(title = 'Count'))",
    "    ",
    "    return(p)",
    "  })",
    "  ",
    "  # Create a UMAP plot colored by motif scores",
    "  output$motifUMAP <- renderPlotly({",
    "    df <- motifData()",
    "    ",
    "    # Check if UMAP coordinates are available",
    "    if (!('UMAP1' %in% colnames(df) && 'UMAP2' %in% colnames(df))) {",
    "      return(plotly_empty(type = 'scatter', mode = 'markers') %>%",
    "               layout(title = 'UMAP coordinates not available'))",
    "    }",
    "    ",
    "    # Remove NA values",
    "    df <- df[!is.na(df$UMAP1) & !is.na(df$UMAP2), ]",
    "    ",
    "    # Prepare plot data",
    "    marker_props <- list(",
    "      size = input$pointSize,",
    "      opacity = input$opacity",
    "    )",
    "    ",
    "    if (input$colorByClusters && input$clusterCol %in% colnames(df)) {",
    "      # Color by clusters",
    "      p <- plot_ly(df, x = ~UMAP1, y = ~UMAP2, color = as.formula(paste0('~', input$clusterCol)),",
    "                  type = 'scatter', mode = 'markers', marker = c(marker_props, list())) %>%",
    "           layout(title = paste('UMAP Colored by', input$clusterCol),",
    "                  xaxis = list(title = 'UMAP1'),",
    "                  yaxis = list(title = 'UMAP2'))",
    "    } else {",
    "      # Color by motif score",
    "      p <- plot_ly(df, x = ~UMAP1, y = ~UMAP2, color = ~Score,",
    "                  type = 'scatter', mode = 'markers', ",
    "                  marker = c(marker_props, list(colorscale = 'Viridis', showscale = TRUE))) %>%",
    "           layout(title = paste('UMAP Colored by', input$motif, 'Score'),",
    "                  xaxis = list(title = 'UMAP1'),",
    "                  yaxis = list(title = 'UMAP2'))",
    "    }",
    "    ",
    "    return(p)",
    "  })",
    "  ",
    "  # Create a data table of motif scores",
    "  output$motifTable <- renderDT({",
    "    df <- motifData()",
    "    datatable(df, options = list(pageLength = 15, scrollX = TRUE, ",
    "                                 dom = 'lBfrtip',",
    "                                 buttons = c('copy', 'csv')))",
    "  })",
    "  ",
    "  # Update cluster column choices based on available metadata",
    "  observe({",
    "    df <- motifData()",
    "    meta_cols <- setdiff(colnames(df), c('CellID', 'Score', 'UMAP1', 'UMAP2'))",
    "    updateSelectInput(session, 'clusterCol', choices = meta_cols)",
    "  })"
  )
  
  # Insert motif server code before the last bracket
  server_content <- c(
    server_content[1:last_bracket-1],
    motif_server_code,
    server_content[last_bracket:length(server_content)]
  )
  
  # Write the modified server.R file
  writeLines(server_content, server_file)
  message("Modified server.R file to add motif data visualization")
}

message("ShinyCell app with RNA expression and motif scores created at: ", shiny_dir)
message("Run it with: shiny::runApp('", shiny_dir, "')")
message("Starting app...")
shiny::runApp(shiny_dir)

# sc1meta <- readRDS("/Users/tanushswaminathan/b_cell_shiny/sc1meta.rds")
# sc1conf <- readRDS("/Users/tanushswaminathan/b_cell_shiny/sc1conf.rds")
