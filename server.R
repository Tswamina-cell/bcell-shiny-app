library(shiny) 
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
library(ggplot2) 
library(ggrepel) 
library(hdf5r) 
library(ggdendro) 
library(gridExtra) 
library(plotly)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(shinyBS) # For enhanced tooltips
library(shinyjs) # For UI interactions

# Function to extract legend 
g_legend <- function(a.gplot) {  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")  
  legend <- tmp$grobs[[leg]]  
  return(legend)
}  

# Plot theme 
sctheme <- function(base_size = 24, XYval = TRUE, Xang = 0, XjusH = 0.5) { 
  oupTheme = theme( 
    text = element_text(size = base_size, family = "Helvetica"), 
    panel.background = element_rect(fill = "white", colour = NA), 
    axis.line = element_line(colour = "black"), 
    axis.ticks = element_line(colour = "black", size = base_size / 20), 
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(size = base_size), 
    axis.text.x = element_text(angle = Xang, hjust = XjusH), 
    legend.position = "bottom", 
    legend.key = element_rect(colour = NA, fill = NA) 
  ) 
  if(!XYval) { 
    oupTheme = oupTheme + theme( 
      axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
      axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
  } 
  return(oupTheme) 
}
# Plot cell information on dimred 
scDRcell <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2, 
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt, inplab) { 
  if(is.null(inpsub1)) {
    inpsub1 = inpConf$UI[1]
  } 
  
  # Check if the requested columns exist in inpConf
  if(!(inpdrX %in% inpConf$UI) || !(inpdrY %in% inpConf$UI) || 
     !(inp1 %in% inpConf$UI) || !(inpsub1 %in% inpConf$UI)) {
    # Return a warning plot
    warning("One or more required UI elements not found in configuration")
    return(ggplot() + annotate("text", x = 0, y = 0, label = "Missing configuration") + theme_void())
  }
  
  # Get column IDs
  col_ids <- c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID, 
               inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID)
  
  # Check if these column IDs exist in metadata
  missing_cols <- col_ids[!col_ids %in% names(inpMeta)]
  if(length(missing_cols) > 0) {
    warning("Columns missing from metadata: ", paste(missing_cols, collapse=", "))
    return(ggplot() + annotate("text", x = 0, y = 0, 
                              label = paste("Missing columns:", paste(missing_cols, collapse=", "))) + 
             theme_void())
  }
  
  # Extract data with error handling
  tryCatch({
    ggData = inpMeta[, col_ids, with = FALSE]
    colnames(ggData) = c("X", "Y", "val", "sub")
    
    # Prepare ggData 
    rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y)) 
    bgCells = FALSE 
    if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) { 
      bgCells = TRUE 
      ggData2 = ggData[!sub %in% inpsub2] 
      ggData = ggData[sub %in% inpsub2] 
    } 
    if(inpord == "Max-1st") { 
      ggData = ggData[order(val)] 
    } else if(inpord == "Min-1st") { 
      ggData = ggData[order(-val)] 
    } else if(inpord == "Random") { 
      ggData = ggData[sample(nrow(ggData))] 
    } 
    
    # Do factoring if required 
    if(!is.na(inpConf[UI == inp1]$fCL)) { 
      ggCol = strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]] 
      names(ggCol) = levels(ggData$val) 
      ggLvl = levels(ggData$val)[levels(ggData$val) %in% unique(ggData$val)] 
      ggData$val = factor(ggData$val, levels = ggLvl) 
      ggCol = ggCol[ggLvl] 
    } 
   
    # Actual ggplot 
    ggOut = ggplot(ggData, aes(X, Y, color = val)) 
    if(bgCells) { 
      ggOut = ggOut + 
        geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16) 
    } 
    ggOut = ggOut + 
      geom_point(size = inpsiz, shape = 16) + xlab(inpdrX) + ylab(inpdrY) + 
      sctheme(base_size = sList[inpfsz], XYval = inptxt) 
    if(is.na(inpConf[UI == inp1]$fCL)) { 
      ggOut = ggOut + scale_color_gradientn("", colours = cList[[inpcol]]) + 
        guides(color = guide_colorbar(barwidth = 15)) 
    } else { 
      sListX = min(nchar(paste0(levels(ggData$val), collapse = "")), 200) 
      sListX = 0.75 * (sList - (1.5 * floor(sListX/50))) 
      ggOut = ggOut + scale_color_manual("", values = ggCol) + 
        guides(color = guide_legend(override.aes = list(size = 5),  
                                    nrow = inpConf[UI == inp1]$fRow)) + 
        theme(legend.text = element_text(size = sListX[inpfsz])) 
      if(inplab) { 
        ggData3 = ggData[, .(X = mean(X), Y = mean(Y)), by = "val"] 
        lListX = min(nchar(paste0(ggData3$val, collapse = "")), 200) 
        lListX = lList - (0.25 * floor(lListX/50)) 
        ggOut = ggOut + 
          geom_text_repel(data = ggData3, aes(X, Y, label = val), 
                          color = "grey10", bg.color = "grey95", bg.r = 0.15, 
                          size = lListX[inpfsz], seed = 42) 
      } 
    } 
    if(inpasp == "Square") { 
      ggOut = ggOut + coord_fixed(ratio = rat) 
    } else if(inpasp == "Fixed") { 
      ggOut = ggOut + coord_fixed() 
    } 
    return(ggOut)
  }, error = function(e) {
    # Handle the error
    warning("Error in data extraction: ", e$message)
    return(ggplot() + annotate("text", x = 0, y = 0, label = paste("Error:", e$message)) + theme_void())
  })
} 

# Function for numeric data
scDRnum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, 
                    inpH5, inpGene, inpsplt) { 
  if(is.null(inpsub1)) {
    inpsub1 = inpConf$UI[1]
  } 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID), 
                   with = FALSE] 
  colnames(ggData) = c("group", "sub") 
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
  ggData[val2 < 0]$val2 = 0 
  h5file$close_all() 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) { 
    ggData = ggData[sub %in% inpsub2] 
  } 
  
  # Split inp1 if necessary 
  if(is.na(inpConf[UI == inp1]$fCL)) { 
    if(inpsplt == "Quartile") {
      nBk = 4
    } 
    if(inpsplt == "Decile") {
      nBk = 10
    } 
    ggData$group = cut(ggData$group, breaks = nBk) 
  } 
  
  # Actual data.table 
  ggData$express = FALSE 
  ggData[val2 > 0]$express = TRUE 
  ggData1 = ggData[express == TRUE, .(nExpress = .N), by = "group"] 
  ggData = ggData[, .(nCells = .N), by = "group"] 
  ggData = ggData1[ggData, on = "group"] 
  ggData = ggData[, c("group", "nCells", "nExpress"), with = FALSE] 
  ggData[is.na(nExpress)]$nExpress = 0 
  ggData$pctExpress = 100 * ggData$nExpress / ggData$nCells 
  ggData = ggData[order(group)] 
  colnames(ggData)[3] = paste0(colnames(ggData)[3], "_", inp2) 
  return(ggData) 
}
# Plot gene expression on dimred 
scDRgene <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2, 
                     inpH5, inpGene, 
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt) { 
  tryCatch({
    if(is.null(inpsub1)) {
      inpsub1 = inpConf$UI[1]
    } 
    
    # Check if columns exist
    if(!(inpdrX %in% inpConf$UI) || !(inpdrY %in% inpConf$UI) || !(inpsub1 %in% inpConf$UI)) {
      warning("Missing UI configuration")
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Missing configuration") + theme_void())
    }
    
    # Get column IDs
    drX_id <- inpConf[UI == inpdrX]$ID
    drY_id <- inpConf[UI == inpdrY]$ID
    sub1_id <- inpConf[UI == inpsub1]$ID
    
    # Check if columns exist in metadata
    required_cols <- c(drX_id, drY_id, sub1_id)
    missing_cols <- required_cols[!required_cols %in% names(inpMeta)]
    
    if(length(missing_cols) > 0) {
      warning("Columns missing from metadata: ", paste(missing_cols, collapse=", "))
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = paste("Missing columns:", paste(missing_cols, collapse=", "))) + 
               theme_void())
    }
    
    # Extract data
    ggData = inpMeta[, required_cols, with = FALSE]
    colnames(ggData) = c("X", "Y", "sub")
    
    # Continue with original function
    rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
    
    # Check if gene exists
    if(!inp1 %in% names(inpGene)) {
      warning("Gene not found: ", inp1)
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = paste("Gene not found:", inp1)) + 
               theme_void())
    }
    
    # Read gene expression
    h5file <- tryCatch({
      H5File$new(inpH5, mode = "r")
    }, error = function(e) {
      warning("Error opening H5 file: ", e$message)
      return(NULL)
    })
    
    if(is.null(h5file)) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = "Error opening gene expression file") + 
               theme_void())
    }
    
    h5data <- h5file[["grp"]][["data"]]
    ggData$val = h5data$read(args = list(inpGene[inp1], quote(expr=)))
    ggData[val < 0]$val = 0
    h5file$close_all()
    
    # Rest of original function
    bgCells = FALSE
    if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {
      bgCells = TRUE
      ggData2 = ggData[!sub %in% inpsub2]
      ggData = ggData[sub %in% inpsub2]
    }
    
    if(inpord == "Max-1st") {
      ggData = ggData[order(val)]
    } else if(inpord == "Min-1st") {
      ggData = ggData[order(-val)]
    } else if(inpord == "Random") {
      ggData = ggData[sample(nrow(ggData))]
    }
    
    # Actual ggplot
    ggOut = ggplot(ggData, aes(X, Y, color = val))
    if(bgCells) {
      ggOut = ggOut +
        geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16)
    }
    # In your scDRgene function, modify the guides section:
    ggOut = ggOut +
      geom_point(size = inpsiz, shape = 16) + xlab(inpdrX) + ylab(inpdrY) +
      sctheme(base_size = sList[inpfsz], XYval = inptxt) +
      scale_color_gradientn(inp1, colours = cList[[inpcol]]) +
      guides(color = guide_colorbar(barwidth = 15, title.position = "top")) + # Changed title.position to "top"
      theme(legend.title = element_text(margin = margin(0, 0, 15, 0))) # Add margin below title
    
    if(inpasp == "Square") {
      ggOut = ggOut + coord_fixed(ratio = rat)
    } else if(inpasp == "Fixed") {
      ggOut = ggOut + coord_fixed()
    }
    
    return(ggOut)
  }, error = function(e) {
    warning("Error in scDRgene: ", e$message)
    return(ggplot() + 
             annotate("text", x = 0, y = 0, label = paste("Error:", e$message)) + 
             theme_void())
  })
} 

# Helper for bilinear calculation
bilinear <- function(x, y, xy, Q11, Q21, Q12, Q22) { 
  oup = (xy-x)*(xy-y)*Q11 + x*(xy-y)*Q21 + (xy-x)*y*Q12 + x*y*Q22 
  oup = oup / (xy*xy) 
  return(oup) 
}
# Plot gene coexpression on dimred 
scDRcoex <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inp2, 
                     inpsub1, inpsub2, inpH5, inpGene, 
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt) { 
  if(is.null(inpsub1)) {
    inpsub1 = inpConf$UI[1]
  } 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID, 
                       inpConf[UI == inpsub1]$ID),  
                   with = FALSE] 
  colnames(ggData) = c("X", "Y", "sub") 
  rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y)) 
  
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData$val1 = h5data$read(args = list(inpGene[inp1], quote(expr=))) 
  ggData[val1 < 0]$val1 = 0 
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
  ggData[val2 < 0]$val2 = 0 
  h5file$close_all() 
  bgCells = FALSE 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) { 
    bgCells = TRUE 
    ggData2 = ggData[!sub %in% inpsub2] 
    ggData = ggData[sub %in% inpsub2] 
  } 
  
  # Generate coex color palette 
  cInp = strsplit(inpcol, "; ")[[1]] 
  if(cInp[1] == "Red (Gene1)") { 
    c10 = c(255,0,0) 
  } else if(cInp[1] == "Orange (Gene1)") { 
    c10 = c(255,140,0) 
  } else { 
    c10 = c(0,255,0) 
  } 
  if(cInp[2] == "Green (Gene2)") { 
    c01 = c(0,255,0) 
  } else { 
    c01 = c(0,0,255) 
  } 
  c00 = c(217,217,217) 
  c11 = c10 + c01 
  nGrid = 16
  nPad = 2
  nTot = nGrid + nPad * 2 
  gg = data.table(v1 = rep(0:nTot,nTot+1), v2 = sort(rep(0:nTot,nTot+1))) 
  gg$vv1 = gg$v1 - nPad
  gg[vv1 < 0]$vv1 = 0
  gg[vv1 > nGrid]$vv1 = nGrid 
  gg$vv2 = gg$v2 - nPad
  gg[vv2 < 0]$vv2 = 0
  gg[vv2 > nGrid]$vv2 = nGrid 
  gg$cR = bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1]) 
  gg$cG = bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2]) 
  gg$cB = bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3]) 
  gg$cMix = rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255) 
  gg = gg[, c("v1", "v2", "cMix")] 
  
  # Map colours 
  ggData$v1 = round(nTot * ggData$val1 / max(ggData$val1)) 
  ggData$v2 = round(nTot * ggData$val2 / max(ggData$val2)) 
  ggData$v0 = ggData$v1 + ggData$v2 
  ggData = gg[ggData, on = c("v1", "v2")] 
  if(inpord == "Max-1st") { 
    ggData = ggData[order(v0)] 
  } else if(inpord == "Min-1st") { 
    ggData = ggData[order(-v0)] 
  } else if(inpord == "Random") { 
    ggData = ggData[sample(nrow(ggData))] 
  } 
  
  # Actual ggplot 
  ggOut = ggplot(ggData, aes(X, Y)) 
  if(bgCells) { 
    ggOut = ggOut + 
      geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16) 
  } 
  ggOut = ggOut + 
    geom_point(size = inpsiz, shape = 16, color = ggData$cMix) + 
    xlab(inpdrX) + ylab(inpdrY) + 
    sctheme(base_size = sList[inpfsz], XYval = inptxt) + 
    scale_color_gradientn(inp1, colours = cList[[1]]) + 
    guides(color = guide_colorbar(barwidth = 15)) 
  if(inpasp == "Square") { 
    ggOut = ggOut + coord_fixed(ratio = rat) 
  } else if(inpasp == "Fixed") { 
    ggOut = ggOut + coord_fixed() 
  } 
  return(ggOut) 
} 

# Legend for coexpression
scDRcoexLeg <- function(inp1, inp2, inpcol, inpfsz) { 
  # Generate coex color palette 
  cInp = strsplit(inpcol, "; ")[[1]] 
  if(cInp[1] == "Red (Gene1)") { 
    c10 = c(255,0,0) 
  } else if(cInp[1] == "Orange (Gene1)") { 
    c10 = c(255,140,0) 
  } else { 
    c10 = c(0,255,0) 
  } 
  if(cInp[2] == "Green (Gene2)") { 
    c01 = c(0,255,0) 
  } else { 
    c01 = c(0,0,255) 
  } 
  c00 = c(217,217,217) 
  c11 = c10 + c01 
  nGrid = 16
  nPad = 2
  nTot = nGrid + nPad * 2 
  gg = data.table(v1 = rep(0:nTot,nTot+1), v2 = sort(rep(0:nTot,nTot+1))) 
  gg$vv1 = gg$v1 - nPad
  gg[vv1 < 0]$vv1 = 0
  gg[vv1 > nGrid]$vv1 = nGrid 
  gg$vv2 = gg$v2 - nPad
  gg[vv2 < 0]$vv2 = 0
  gg[vv2 > nGrid]$vv2 = nGrid 
  gg$cR = bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1]) 
  gg$cG = bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2]) 
  gg$cB = bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3]) 
  gg$cMix = rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255) 
  gg = gg[, c("v1", "v2", "cMix")] 
  
  # Actual ggplot 
  ggOut = ggplot(gg, aes(v1, v2)) + 
    geom_tile(fill = gg$cMix) + 
    xlab(inp1) + ylab(inp2) + coord_fixed(ratio = 1) + 
    scale_x_continuous(breaks = c(0, nTot), label = c("low", "high")) + 
    scale_y_continuous(breaks = c(0, nTot), label = c("low", "high")) + 
    sctheme(base_size = sList[inpfsz], XYval = TRUE) 
  return(ggOut) 
} 

# Calculate number of cells with expression
scDRcoexNum <- function(inpConf, inpMeta, inp1, inp2, 
                        inpsub1, inpsub2, inpH5, inpGene) { 
  if(is.null(inpsub1)) {
    inpsub1 = inpConf$UI[1]
  } 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpsub1]$ID), with = FALSE] 
  colnames(ggData) = c("sub") 
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData$val1 = h5data$read(args = list(inpGene[inp1], quote(expr=))) 
  ggData[val1 < 0]$val1 = 0 
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
  ggData[val2 < 0]$val2 = 0 
  h5file$close_all() 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) { 
    ggData = ggData[sub %in% inpsub2] 
  } 
  
  # Actual data.table 
  ggData$express = "none" 
  ggData[val1 > 0]$express = inp1 
  ggData[val2 > 0]$express = inp2 
  ggData[val1 > 0 & val2 > 0]$express = "both" 
  ggData$express = factor(ggData$express, levels = unique(c("both", inp1, inp2, "none"))) 
  ggData = ggData[, .(nCells = .N), by = "express"] 
  ggData$percent = 100 * ggData$nCells / sum(ggData$nCells) 
  ggData = ggData[order(express)] 
  colnames(ggData)[1] = "expression > 0" 
  return(ggData) 
} 

# Gene-Gene scatter plot function
scGenePlot <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, 
                     inpH5, inpGene, inpsiz, colorBy = NULL, colorVar = NULL, inpfsz) {
  tryCatch({
    # Read gene expression data for both genes
    h5file <- H5File$new(inpH5, mode = "r")
    h5data <- h5file[["grp"]][["data"]]
    
    # Create data frame with both gene expressions
    ggData <- data.frame(
      gene1 = h5data$read(args = list(inpGene[inp1], quote(expr=))),
      gene2 = h5data$read(args = list(inpGene[inp2], quote(expr=)))
    )
    
    # Set negative values to 0
    ggData$gene1[ggData$gene1 < 0] <- 0
    ggData$gene2[ggData$gene2 < 0] <- 0
    
    h5file$close_all()
    
    # Add subset information if available
    # Gene-Gene scatter plot function (continued)
    # Add subset information if available
    if(!is.null(inpsub1) && inpsub1 != "") {
      sub_col <- inpConf[UI == inpsub1]$ID
      if(!is.na(sub_col)) {
        ggData$sub <- inpMeta[[sub_col]]
        if(!is.null(inpsub2) && length(inpsub2) > 0) {
          # Only subset if there are actually options selected
          if(length(inpsub2) < length(levels(factor(ggData$sub)))) {
            ggData <- ggData[ggData$sub %in% inpsub2, ]
          }
        }
      }
    }
    
    # Calculate correlation only if we have data
    if(nrow(ggData) > 1) {
      corr_value <- cor(ggData$gene1, ggData$gene2, method = "spearman", use = "pairwise.complete.obs")
    } else {
      corr_value <- NA
    }
    
    # Color by cell info if requested
    if(!is.null(colorBy) && colorBy == TRUE && !is.null(colorVar) && colorVar != "") {
      # Check if the color variable exists in metadata
      color_col <- inpConf[UI == colorVar]$ID
      if(!is.na(color_col) && color_col %in% names(inpMeta)) {
        # Add the cell info as color variable
        ggData$color_var <- inpMeta[[color_col]]
        
        # Check if the color variable is categorical or continuous
        if(is.factor(ggData$color_var) || is.character(ggData$color_var)) {
          # For categorical variables
          p <- ggplot(ggData, aes(x = gene1, y = gene2, color = color_var)) +
               geom_point(size = inpsiz, alpha = 0.7) +
               labs(x = paste0(inp1, " expression"), 
                    y = paste0(inp2, " expression"),
                    color = colorVar,
                    title = paste0("Expression of ", inp1, " vs ", inp2),
                    subtitle = paste0("Spearman correlation: ", round(corr_value, 3)))
          
          # If we have factor colors from config, use them
          if(!is.na(inpConf[UI == colorVar]$fCL)) {
            ggCol = strsplit(inpConf[UI == colorVar]$fCL, "\\|")[[1]]
            names(ggCol) = levels(factor(ggData$color_var))
            p <- p + scale_color_manual(values = ggCol)
          }
        } else {
          # For continuous variables
          p <- ggplot(ggData, aes(x = gene1, y = gene2, color = color_var)) +
               geom_point(size = inpsiz, alpha = 0.7) +
               scale_color_gradientn(colors = cList[["Blue-Yellow-Red"]]) +
               labs(x = paste0(inp1, " expression"), 
                    y = paste0(inp2, " expression"),
                    color = colorVar,
                    title = paste0("Expression of ", inp1, " vs ", inp2),
                    subtitle = paste0("Spearman correlation: ", round(corr_value, 3)))
        }
      } else {
        # Fallback to simple coloring if color variable not found
        p <- ggplot(ggData, aes(x = gene1, y = gene2)) +
             geom_point(size = inpsiz, alpha = 0.7, color = "steelblue") +
             labs(x = paste0(inp1, " expression"), 
                  y = paste0(inp2, " expression"),
                  title = paste0("Expression of ", inp1, " vs ", inp2),
                  subtitle = paste0("Spearman correlation: ", round(corr_value, 3)))
      }
    } else {
      # No coloring by cell info - use a simple color
      p <- ggplot(ggData, aes(x = gene1, y = gene2)) +
           geom_point(size = inpsiz, alpha = 0.7, color = "steelblue") +
           labs(x = paste0(inp1, " expression"), 
                y = paste0(inp2, " expression"),
                title = paste0("Expression of ", inp1, " vs ", inp2),
                subtitle = paste0("Spearman correlation: ", round(corr_value, 3)))
    }
    
    # Add common elements
    p <- p + 
      geom_smooth(method = "lm", color = "red", se = TRUE, size = 1, alpha = 0.2) +
      sctheme(base_size = sList[inpfsz]) +
      theme(
        legend.position = "right",
        plot.title = element_text(size = sList[inpfsz], face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = sList[inpfsz] * 0.8, hjust = 0.5),
        axis.title.y = element_text(margin = margin(0, 15, 0, 0))
      )
    return(p)
  }, error = function(e) {
    warning("Error in scGenePlot: ", e$message)
    return(ggplot() + 
            annotate("text", x = 0, y = 0, label = paste("Error:", e$message)) + 
            theme_void())
  })
}

# Calculate correlation statistics between two genes
getCorrelationStats <- function(inpMeta, inp1, inp2, inpsub1, inpsub2, inpH5, inpGene) {
  tryCatch({
    # Read gene expression data for both genes
    h5file <- H5File$new(inpH5, mode = "r")
    h5data <- h5file[["grp"]][["data"]]
    
    # Create vectors for both gene expressions
    gene1_expr <- h5data$read(args = list(inpGene[inp1], quote(expr=)))
    gene2_expr <- h5data$read(args = list(inpGene[inp2], quote(expr=)))
    
    # Set negative values to 0
    gene1_expr[gene1_expr < 0] <- 0
    gene2_expr[gene2_expr < 0] <- 0
    
    h5file$close_all()
    
    # Filter by subset if needed
    if(!is.null(inpsub1) && !is.null(inpsub2)) {
      sub_info <- inpMeta[[sc1conf[UI == inpsub1]$ID]]
      if(length(inpsub2) > 0 && length(inpsub2) != length(levels(sub_info))) {
        include_idx <- which(sub_info %in% inpsub2)
        gene1_expr <- gene1_expr[include_idx]
        gene2_expr <- gene2_expr[include_idx]
      }
    }
    
    # Calculate various correlation metrics
    # Use try-catch to handle potential errors
    pearson_cor <- try(cor.test(gene1_expr, gene2_expr, method = "pearson"), silent = TRUE)
    spearman_cor <- try(cor.test(gene1_expr, gene2_expr, method = "spearman"), silent = TRUE)
    
    # Extract values safely
    pearson_value <- if(inherits(pearson_cor, "try-error")) NA else pearson_cor$estimate
    pearson_pval <- if(inherits(pearson_cor, "try-error")) NA else pearson_cor$p.value
    spearman_value <- if(inherits(spearman_cor, "try-error")) NA else spearman_cor$estimate
    spearman_pval <- if(inherits(spearman_cor, "try-error")) NA else spearman_cor$p.value
    
    # Format results into a text string
    result <- paste0(
      "Correlation Statistics between ", inp1, " and ", inp2, ":\n\n",
      "Pearson correlation: ", round(as.numeric(pearson_value), 4), 
      " (p-value: ", format.pval(pearson_pval, digits = 3), ")\n",
      "Spearman correlation: ", round(as.numeric(spearman_value), 4),
      " (p-value: ", format.pval(spearman_pval, digits = 3), ")\n\n",
      "Number of cells analyzed: ", length(gene1_expr), "\n",
      "Cells expressing ", inp1, ": ", sum(gene1_expr > 0), " (", 
        round(100 * sum(gene1_expr > 0) / length(gene1_expr), 1), "%)\n",
      "Cells expressing ", inp2, ": ", sum(gene2_expr > 0), " (", 
        round(100 * sum(gene2_expr > 0) / length(gene2_expr), 1), "%)\n",
      "Co-expressing cells: ", sum(gene1_expr > 0 & gene2_expr > 0), " (", 
        round(100 * sum(gene1_expr > 0 & gene2_expr > 0) / length(gene1_expr), 1), "%)\n"
    )
    
    return(result)
  }, error = function(e) {
    return(paste("Error calculating correlation statistics:", e$message))
  })
}
# Plot violin / boxplot 
scVioBox <- function(inpConf, inpMeta, inp1, inp2, 
                     inpsub1, inpsub2, inpH5, inpGene, 
                     inptyp, inppts, inpsiz, inpfsz) { 
  if(is.null(inpsub1)) {
    inpsub1 = inpConf$UI[1]
  } 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID), 
                   with = FALSE] 
  colnames(ggData) = c("X", "sub") 
  
  # Load in either cell meta or gene expr
  if(inp2 %in% inpConf$UI) { 
    ggData$val = inpMeta[[inpConf[UI == inp2]$ID]] 
  } else { 
    h5file <- H5File$new(inpH5, mode = "r") 
    h5data <- h5file[["grp"]][["data"]] 
    ggData$val = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
    ggData[val < 0]$val = 0 
    set.seed(42) 
    tmpNoise = rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000 
    ggData$val = ggData$val + tmpNoise 
    h5file$close_all() 
  } 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) { 
    ggData = ggData[sub %in% inpsub2] 
  } 
  
  # Do factoring 
  ggCol = strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]] 
  names(ggCol) = levels(ggData$X) 
  ggLvl = levels(ggData$X)[levels(ggData$X) %in% unique(ggData$X)] 
  ggData$X = factor(ggData$X, levels = ggLvl) 
  ggCol = ggCol[ggLvl] 
  
  # Actual ggplot 
  if(inptyp == "violin") { 
    ggOut = ggplot(ggData, aes(X, val, fill = X)) + geom_violin(scale = "width") 
  } else { 
    ggOut = ggplot(ggData, aes(X, val, fill = X)) + geom_boxplot() 
  } 
  if(inppts) { 
    ggOut = ggOut + geom_jitter(size = inpsiz, shape = 16) 
  } 
  
  # Add adjusted axis labels
  ggOut = ggOut + 
    xlab(inp1) + 
    ylab(inp2) + 
    sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +  
    scale_fill_manual("", values = ggCol) +
    theme(
      legend.position = "none",
      # Move y-axis label further from axis
      axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
      # Move x-axis label closer to axis
      axis.title.x = element_text(margin = margin(10, 0, 0, 0))
    )
  
  return(ggOut) 
}

# Get gene list 
scGeneList <- function(inp, inpGene) { 
  geneList = data.table(gene = unique(trimws(strsplit(inp, ",|;|\n")[[1]])), 
                        present = TRUE) 
  geneList[!gene %in% names(inpGene)]$present = FALSE 
  return(geneList) 
} 

# Plot gene expression bubbleplot / heatmap 
scBubbHeat <- function(inpConf, inpMeta, inp, inpGrp, inpPlt, 
                       inpsub1, inpsub2, inpH5, inpGene, inpScl, inpRow, inpCol, 
                       inpcols, inpfsz, save = FALSE) { 
  if(is.null(inpsub1)) {
    inpsub1 = inpConf$UI[1]
  } 
  # Identify genes that are in our dataset 
  geneList = scGeneList(inp, inpGene) 
  geneList = geneList[present == TRUE] 
  shiny::validate(need(nrow(geneList) <= 50, "More than 50 genes to plot! Please reduce the gene list!")) 
  shiny::validate(need(nrow(geneList) > 1, "Please input at least 2 genes to plot!")) 
   
  # Prepare ggData 
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData = data.table() 
  for(iGene in geneList$gene) { 
    tmp = inpMeta[, c("sampleID", inpConf[UI == inpsub1]$ID), with = FALSE] 
    colnames(tmp) = c("sampleID", "sub") 
    tmp$grpBy = inpMeta[[inpConf[UI == inpGrp]$ID]] 
    tmp$geneName = iGene 
    tmp$val = h5data$read(args = list(inpGene[iGene], quote(expr=))) 
    ggData = rbindlist(list(ggData, tmp)) 
  } 
  h5file$close_all() 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) { 
    ggData = ggData[sub %in% inpsub2] 
  } 
  shiny::validate(need(uniqueN(ggData$grpBy) > 1, "Only 1 group present, unable to plot!")) 
   
  # Aggregate 
  ggData$val = expm1(ggData$val) 
  ggData = ggData[, .(val = mean(val), prop = sum(val>0) / length(sampleID)), 
                  by = c("geneName", "grpBy")] 
  ggData$val = log1p(ggData$val) 
   
  # Scale if required 
  colRange = range(ggData$val) 
  if(inpScl) { 
    ggData[, val:= scale(val), keyby = "geneName"] 
    colRange = c(-max(abs(range(ggData$val))), max(abs(range(ggData$val)))) 
  } 
   
  # hclust row/col if necessary 
  ggMat = dcast.data.table(ggData, geneName~grpBy, value.var = "val") 
  tmp = ggMat$geneName 
  ggMat = as.matrix(ggMat[, -1]) 
  rownames(ggMat) = tmp 
  if(inpRow) { 
    hcRow = dendro_data(as.dendrogram(hclust(dist(ggMat)))) 
    ggRow = ggplot() + coord_flip() + 
      geom_segment(data = hcRow$segments, aes(x=x,y=y,xend=xend,yend=yend)) + 
      scale_y_continuous(breaks = rep(0, uniqueN(ggData$grpBy)), 
                         labels = unique(ggData$grpBy), expand = c(0, 0)) + 
      scale_x_continuous(breaks = seq_along(hcRow$labels$label), 
                         labels = hcRow$labels$label, expand = c(0, 0.5)) + 
      sctheme(base_size = sList[inpfsz]) + 
      theme(axis.title = element_blank(), axis.line = element_blank(), 
            axis.ticks = element_blank(), axis.text.y = element_blank(), 
            axis.text.x = element_text(color="white", angle = 45, hjust = 1)) 
    ggData$geneName = factor(ggData$geneName, levels = hcRow$labels$label) 
  } else { 
    ggData$geneName = factor(ggData$geneName, levels = rev(geneList$gene)) 
  } 
  if(inpCol) { 
    hcCol = dendro_data(as.dendrogram(hclust(dist(t(ggMat))))) 
    ggCol = ggplot() + 
      geom_segment(data = hcCol$segments, aes(x=x,y=y,xend=xend,yend=yend)) + 
      scale_x_continuous(breaks = seq_along(hcCol$labels$label), 
                         labels = hcCol$labels$label, expand = c(0.05, 0)) + 
      scale_y_continuous(breaks = rep(0, uniqueN(ggData$geneName)), 
                         labels = unique(ggData$geneName), expand=c(0,0)) + 
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) + 
      theme(axis.title = element_blank(), axis.line = element_blank(), 
            axis.ticks = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_text(color = "white")) 
    ggData$grpBy = factor(ggData$grpBy, levels = hcCol$labels$label) 
  } 
   
  # Actual plot according to plottype 
  if(inpPlt == "Bubbleplot") { 
    # Bubbleplot 
    ggOut = ggplot(ggData, aes(grpBy, geneName, color = val, size = prop)) + 
      geom_point() +  
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +  
      scale_x_discrete(expand = c(0.05, 0)) +  
      scale_y_discrete(expand = c(0, 0.5)) + 
      scale_size_continuous("proportion", range = c(0, 8), 
                            limits = c(0, 1), breaks = c(0.00,0.25,0.50,0.75,1.00)) + 
      scale_color_gradientn("expression", limits = colRange, colours = cList[[inpcols]]) + 
      guides(color = guide_colorbar(barwidth = 15)) + 
      theme(axis.title = element_blank(), legend.box = "vertical") 
  } else { 
    # Heatmap 
    ggOut = ggplot(ggData, aes(grpBy, geneName, fill = val)) + 
      geom_tile() +  
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) + 
      scale_x_discrete(expand = c(0.05, 0)) +  
      scale_y_discrete(expand = c(0, 0.5)) + 
      scale_fill_gradientn("expression", limits = colRange, colours = cList[[inpcols]]) + 
      guides(fill = guide_colorbar(barwidth = 15)) + 
      theme(axis.title = element_blank()) 
  } 
     
  # Final tidy 
  ggLeg = g_legend(ggOut) 
  ggOut = ggOut + theme(legend.position = "none") 
  if(!save) { 
    if(inpRow & inpCol) {
      ggOut = grid.arrange(ggOut, ggLeg, ggCol, ggRow, widths = c(7,1), 
                  heights = c(1,7,2), layout_matrix = rbind(c(3,NA),c(1,4),c(2,NA)))  
    } else if(inpRow) {
      ggOut = grid.arrange(ggOut, ggLeg, ggRow, widths = c(7,1), 
                  heights = c(7,2), layout_matrix = rbind(c(1,3),c(2,NA)))  
    } else if(inpCol) {
      ggOut = grid.arrange(ggOut, ggLeg, ggCol, heights = c(1,7,2),  
                  layout_matrix = rbind(c(3),c(1),c(2)))  
    } else {
      ggOut = grid.arrange(ggOut, ggLeg, heights = c(7,2),  
                  layout_matrix = rbind(c(1),c(2)))  
    }  
  } else { 
    if(inpRow & inpCol) {
      ggOut = arrangeGrob(ggOut, ggLeg, ggCol, ggRow, widths = c(7,1), 
                heights = c(1,7,2), layout_matrix = rbind(c(3,NA),c(1,4),c(2,NA)))  
    } else if(inpRow) {
      ggOut = arrangeGrob(ggOut, ggLeg, ggRow, widths = c(7,1), 
                heights = c(7,2), layout_matrix = rbind(c(1,3),c(2,NA)))  
    } else if(inpCol) {
      ggOut = arrangeGrob(ggOut, ggLeg, ggCol, heights = c(1,7,2),  
                layout_matrix = rbind(c(3),c(1),c(2)))  
    } else {
      ggOut = arrangeGrob(ggOut, ggLeg, heights = c(7,2),  
                layout_matrix = rbind(c(1),c(2)))  
    }  
  } 
  return(ggOut) 
}
# Dashboard summary statistics
getDashboardSummary <- function(inpMeta) {
  # Calculate summary statistics
  total_cells <- nrow(inpMeta)
  
  # Get cluster counts
  if("cluster" %in% names(inpMeta)) {
    cluster_count <- length(unique(inpMeta$cluster))
  } else {
    # Try to find the main clustering column
    cluster_cols <- grep("cluster|seurat_clusters", names(inpMeta), value = TRUE, ignore.case = TRUE)
    if(length(cluster_cols) > 0) {
      cluster_count <- length(unique(inpMeta[[cluster_cols[1]]]))
    } else {
      cluster_count <- "N/A"
    }
  }
  
  # Format summary text
  summary_text <- paste0(
    "<b>Total Cells:</b> ", format(total_cells, big.mark = ","), "<br>",
    "<b>Number of Clusters:</b> ", cluster_count, "<br>"
  )
  
  # If we have specific QC metrics, include them
  qc_cols <- grep("nUMI|nFeature|percent.mt", names(inpMeta), value = TRUE, ignore.case = TRUE)
  if(length(qc_cols) > 0) {
    for(col in qc_cols) {
      col_mean <- round(mean(inpMeta[[col]], na.rm = TRUE), 1)
      col_median <- round(median(inpMeta[[col]], na.rm = TRUE), 1)
      col_name <- gsub("_", " ", gsub("\\.", " ", col))
      col_name <- paste0(toupper(substr(col_name, 1, 1)), substr(col_name, 2, nchar(col_name)))
      
      summary_text <- paste0(
        summary_text,
        "<b>", col_name, ":</b> Mean: ", col_mean, ", Median: ", col_median, "<br>"
      )
    }
  }
  
  return(summary_text)
}

# Dashboard UMAP plot
getDashboardUMAP <- function(inpMeta, inpConf) {
  # Get cluster or cell type column
  grouping_cols <- grep("cluster|cell.type|celltype", names(inpMeta), value = TRUE, ignore.case = TRUE)
  
  if(length(grouping_cols) > 0) {
    grouping_col <- grouping_cols[1]
    
    # Get UMAP columns
    umap_cols <- grep("umap|tsne", names(inpMeta), value = TRUE, ignore.case = TRUE)
    
    if(length(umap_cols) >= 2) {
      # Create plot data
      plot_data <- data.frame(
        UMAP_1 = inpMeta[[umap_cols[1]]],
        UMAP_2 = inpMeta[[umap_cols[2]]],
        Group = inpMeta[[grouping_col]]
      )
      
      # Create plot with prettier colors
      p <- ggplot(plot_data, aes(x = UMAP_1, y = UMAP_2, color = Group)) +
        geom_point(size = 0.8, alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Cells colored by", gsub("\\.", " ", grouping_col)), 
             color = gsub("\\.", " ", grouping_col)) +
        theme(
          legend.position = "right",
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12)
        )
      
      # Try to get colors from config if available
      group_ui <- grep(paste0("^", grouping_col, "$"), inpConf$ID, value = FALSE)
      if(length(group_ui) > 0) {
        ui_name <- inpConf$UI[group_ui]
        if(!is.na(inpConf[UI == ui_name]$fCL)) {
          ggCol = strsplit(inpConf[UI == ui_name]$fCL, "\\|")[[1]]
          names(ggCol) = levels(plot_data$Group)
          p <- p + scale_color_manual(values = ggCol)
        }
      }
      
      return(p)
    } else {
      # No UMAP dimensions found
      return(ggplot() + 
               annotate("text", x = 0, y = 0, 
                        label = "No dimensionality reduction coordinates found in the dataset") + 
               theme_void())
    }
  } else {
    # No grouping column found
    return(ggplot() + 
             annotate("text", x = 0, y = 0, 
                      label = "No cluster or cell type information found in the dataset") + 
             theme_void())
  }
}

# Dashboard Top Genes
getTopGenes <- function(inpH5, inpGene, n = 10) {
  tryCatch({
    # Load a sample of gene expression data
    h5file <- H5File$new(inpH5, mode = "r")
    h5data <- h5file[["grp"]][["data"]]
    
    # Get mean expression for each gene
    gene_means <- data.frame(
      gene = names(inpGene),
      mean_expr = sapply(names(inpGene), function(gene) {
        expr_vals <- h5data$read(args = list(inpGene[gene], quote(expr=)))
        expr_vals[expr_vals < 0] <- 0
        mean(expr_vals)
      })
    )
    
    h5file$close_all()
    
    # Order and get top genes
    gene_means <- gene_means[order(-gene_means$mean_expr),]
    top_genes <- head(gene_means, n)
    
    # Plot
    p <- ggplot(top_genes, aes(x = reorder(gene, mean_expr), y = mean_expr)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(x = "", y = "Mean Expression", 
           title = paste("Top", n, "Expressed Genes")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 9)
      )
    
    return(p)
  }, error = function(e) {
    warning("Error calculating top genes: ", e$message)
    return(ggplot() + 
             annotate("text", x = 0, y = 0, 
                      label = paste("Error calculating top genes:", e$message)) + 
             theme_void())
  })
}

# Dashboard QC metrics
getQCMetrics <- function(inpMeta) {
  # Find QC metrics
  qc_cols <- grep("nUMI|nFeature|percent.mt", names(inpMeta), value = TRUE, ignore.case = TRUE)
  
  if(length(qc_cols) >= 2) {
    # Pick two metrics to plot against each other
    # Dashboard QC metrics (continued)
    x_col <- qc_cols[1]
    y_col <- qc_cols[2]
    
    # Find a grouping column if available
    grouping_cols <- grep("cluster|cell.type|celltype", names(inpMeta), value = TRUE, ignore.case = TRUE)
    if(length(grouping_cols) > 0) {
      grouping_col <- grouping_cols[1]
      
      # Create plot
      qc_data <- data.frame(
        x = inpMeta[[x_col]],
        y = inpMeta[[y_col]],
        group = inpMeta[[grouping_col]]
      )
      
      p <- ggplot(qc_data, aes(x = x, y = y, color = group)) +
        geom_point(size = 1, alpha = 0.7) +
        labs(
          x = gsub("_", " ", gsub("\\.", " ", x_col)),
          y = gsub("_", " ", gsub("\\.", " ", y_col)),
          title = "QC Metrics",
          color = gsub("_", " ", gsub("\\.", " ", grouping_col))
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12)
        )
    } else {
      # No grouping information, make a simple scatter plot
      qc_data <- data.frame(
        x = inpMeta[[x_col]],
        y = inpMeta[[y_col]]
      )
      
      p <- ggplot(qc_data, aes(x = x, y = y)) +
        geom_point(size = 1, alpha = 0.7, color = "steelblue") +
        labs(
          x = gsub("_", " ", gsub("\\.", " ", x_col)),
          y = gsub("_", " ", gsub("\\.", " ", y_col)),
          title = "QC Metrics"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12)
        )
    }
    
    return(p)
  } else if(length(qc_cols) == 1) {
    # Make a histogram of the single QC metric
    qc_data <- data.frame(
      value = inpMeta[[qc_cols[1]]]
    )
    
    p <- ggplot(qc_data, aes(x = value)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(
        x = gsub("_", " ", gsub("\\.", " ", qc_cols[1])),
        y = "Cell Count",
        title = paste("Distribution of", gsub("_", " ", gsub("\\.", " ", qc_cols[1])))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
    
    return(p)
  } else {
    # No QC metrics found
    return(ggplot() + 
             annotate("text", x = 0, y = 0, 
                     label = "No QC metrics found in the dataset") + 
             theme_void())
  }
}

# Dashboard Cell Type Proportions
getCellTypeProportions <- function(inpMeta) {
  # Find cell type or cluster column
  grouping_cols <- grep("cluster|cell.type|celltype", names(inpMeta), value = TRUE, ignore.case = TRUE)
  
  if(length(grouping_cols) > 0) {
    grouping_col <- grouping_cols[1]
    
    # Calculate proportions
    props <- as.data.frame(table(inpMeta[[grouping_col]]))
    colnames(props) <- c("Group", "Count")
    props$Percentage <- 100 * props$Count / sum(props$Count)
    
    # Ensure we don't have too many groups (combine small ones if needed)
    if(nrow(props) > 15) {
      # Sort by count
      props <- props[order(-props$Count),]
      # Keep top 14, combine others
      top_props <- props[1:14,]
      others <- data.frame(
        Group = "Other",
        Count = sum(props$Count[15:nrow(props)]),
        Percentage = sum(props$Percentage[15:nrow(props)])
      )
      props <- rbind(top_props, others)
    }
    
    # Order by percentage
    props <- props[order(-props$Percentage),]
    props$Group <- factor(props$Group, levels = props$Group)
    
    # Create plot
    p <- ggplot(props, aes(x = Group, y = Percentage, fill = Group)) +
      geom_bar(stat = "identity") +
      labs(
        x = "",
        y = "Percentage",
        title = paste("Cell Distribution by", gsub("_", " ", gsub("\\.", " ", grouping_col)))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.position = "none"
      )
    
    return(p)
  } else {
    # No grouping column found
    return(ggplot() + 
             annotate("text", x = 0, y = 0, 
                     label = "No cluster or cell type information found in the dataset") + 
             theme_void())
  }
}
### Start server code 
shinyServer(function(input, output, session) {
  # Initialize navigation and handle setup
  # Enable shinyjs
  useShinyjs()
  
  # Check and create www directory if it doesn't exist  
  if (!dir.exists("www")) {
    dir.create("www")
  }
  
  # Generate a placeholder cell background image if it doesn't exist
  if (!file.exists("www/cell_background.jpg")) {
    # Create a placeholder image with gradient
    png("www/cell_background.jpg", width = 1200, height = 800)
    par(mar = c(0, 0, 0, 0))
    plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
    rect(0, 0, 1, 1, col = colorRampPalette(c("#f0f8ff", "#4682b4"))(100), border = NA)
    text(0.5, 0.5, "B Cell Development", cex = 3, col = "white", font = 2)
    dev.off()
  }
  
  # Colour and size palettes
  cList = list(c("grey85","#FFF7EC","#FEE8C8","#FDD49E","#FDBB84", 
               "#FC8D59","#EF6548","#D7301F","#B30000","#7F0000"), 
             c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF", 
               "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)], 
             c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C", 
               "#2C728E","#3B528B","#472D7B","#440154")) 
  names(cList) = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple") 
  
  # Panel sizes 
  pList = c("400px", "600px", "800px") 
  names(pList) = c("Small", "Medium", "Large") 
  pList2 = c("500px", "700px", "900px") 
  names(pList2) = c("Small", "Medium", "Large") 
  pList3 = c("600px", "800px", "1000px") 
  names(pList3) = c("Small", "Medium", "Large") 
  sList = c(18,24,30) 
  names(sList) = c("Small", "Medium", "Large") 
  lList = c(5,6,7) 
  names(lList) = c("Small", "Medium", "Large") 
  
  # Add UMAP entries to configuration
  addUMAPtoConfig <- function(sc1conf) {
    xumap_entries <- data.table(
      ID = c("Xumap_1", "Xumap_2"),
      UI = c("X_umap1", "X_umap2"),
      fID = NA_character_,  # Use NA_character_ instead of NA for character columns
      fCL = NA_character_,
      fRow = NA_integer_,   # Use NA_integer_ for numeric columns
      grp = FALSE,
      dimred = TRUE
    )
    
    # Check if these UI values already exist in sc1conf
    if (!any(sc1conf$UI %in% c("X_umap1", "X_umap2"))) {
      sc1conf <- rbindlist(list(sc1conf, xumap_entries), fill = TRUE)
    }
    
    return(sc1conf)
  }
  
  # Update the configuration with UMAP
  sc1conf <- addUMAPtoConfig(sc1conf)
  
  # Show loading indicator and hide it
  showLoading <- function() {
    show("loading-indicator")
  }
  
  hideLoading <- function() {
    hide("loading-indicator")
  }
  
  # Handle navigation between tabs based on user input
  observeEvent(input$nav_selection, {
    if(input$nav_selection == "cellinfo") {
      updateTabsetPanel(session, "main_navbar", selected = "cellinfo")
    } else if(input$nav_selection == "coexpression") {
      updateTabsetPanel(session, "main_navbar", selected = "coexpression")
    } else if(input$nav_selection == "stats") {
      updateTabsetPanel(session, "main_navbar", selected = "stats")
    }
  })
  
  	# Add this code to your server function to handle dataset selection and routing

# Handle dataset selection changes and tab navigation
observeEvent(input$nav_selection, {
  if(input$nav_selection == "cellinfo") {
    updateTabsetPanel(session, "main_navbar", selected = "gene_expr")
    updateTabsetPanel(session, "gene_expr_tabs", selected = "cellinfo_subtab")
  } else if(input$nav_selection == "coexpression") {
    updateTabsetPanel(session, "main_navbar", selected = "gene_expr")
    updateTabsetPanel(session, "gene_expr_tabs", selected = "coexpression_subtab")
  } else if(input$nav_selection == "stats") {
    updateTabsetPanel(session, "main_navbar", selected = "gene_expr")
    updateTabsetPanel(session, "gene_expr_tabs", selected = "stats_subtab")
  } else if(input$nav_selection == "chrom_access") {
    updateTabsetPanel(session, "main_navbar", selected = "chrom_access")
  }
})

# 2. Modify the dataset choice handler for the new structure
observeEvent(input$datasetChoice, {
  if(input$datasetChoice == "chromatin_accessibility") {
    # When switching to motif data, route to the motif visualization tab
    #updateTabsetPanel(session, "main_navbar", selected = "chrom_access")
    
    # Update motif dropdown if needed
    if(file.exists("motif_data")) {
      motif_files <- list.files("motif_data", pattern = "*.csv")
      if(length(motif_files) > 0) {
        motif_names <- sub(".csv$", "", motif_files)
        updateSelectInput(session, "motif", choices = motif_names, selected = motif_names[1])
      } else {
        showNotification("No motif data files found in motif_data directory", type = "warning")
      }
    } else {
      showNotification("Motif data directory not found", type = "error")
    }
  } else {
    # When switching to gene expression data, go to gene expression tab
    current_tab <- input$main_navbar
    if(current_tab == "motif_analysis") {
      # If currently on motif tab, go to gene expression tab
      updateTabsetPanel(session, "main_navbar", selected = "gene_expr")
      updateTabsetPanel(session, "gene_expr_tabs", selected = "cellinfo_subtab")
    }
  }
})

# 3. Add observer to prevent accessing gene expression subtabs when motif data is selected
observe({
  if(input$datasetChoice == "chromatin_accessibility" && 
     input$main_navbar == "gene_expr") {
    # Redirect to motif visualization tab
    updateTabsetPanel(session, "main_navbar", selected = "home")
    showNotification("Gene Expression visualization is not applicable for Chrom Access.", 
                    type = "warning", duration = 3)
  }
})

# Add observer to prevent accessing motif analysis tab when gene expression data is selected
observe({
  if(input$datasetChoice == "gene_expression" && 
     input$main_navbar == "chrom_access") {
    # Redirect to home tab and show notification
    updateTabsetPanel(session, "main_navbar", selected = "home")
    showNotification("Chrom Access is not available with Gene Expression data. Please select Chromatin Accessibility data to access this feature.", 
                    type = "warning", duration = 4)
  }
})

# Motif Data Table with dynamic motif selection
observeEvent(input$motifTable_motif, {
  # Update the table when motif selection changes in the Data Table tab
  output$motifTable <- renderDataTable({
    req(input$motifTable_motif)
    motif_data_path <- file.path('motif_data', paste0(input$motifTable_motif, '.csv'))
    
    tryCatch({
      df <- read.csv(motif_data_path)
      datatable(df, 
                options = list(
                  pageLength = 10,
                  lengthMenu = list(c(10, 25, 50, 100), c('10', '25', '50', '100')), 
                  scrollX = TRUE,
                  dom = 'Blfrtip', 
                  buttons = c('csv', 'excel')
                ),
                rownames = FALSE, 
                filter = "top",
                extensions = "Buttons")
    }, error = function(e) {
      return(datatable(data.frame(Error = paste("Could not load motif data:", e$message))))
    })
  })
})

# Download handlers for motif data
output$downloadMotifCSV <- downloadHandler(
  filename = function() {
    paste0("motif_", input$motifTable_motif, "_", format(Sys.time(), "%Y%m%d"), ".csv")
  },
  content = function(file) {
    motif_data_path <- file.path('motif_data', paste0(input$motifTable_motif, '.csv'))
    df <- read.csv(motif_data_path)
    write.csv(df, file, row.names = FALSE)
  }
)

output$downloadMotifExcel <- downloadHandler(
  filename = function() {
    paste0("motif_", input$motifTable_motif, "_", format(Sys.time(), "%Y%m%d"), ".xlsx")
  },
  content = function(file) {
    motif_data_path <- file.path('motif_data', paste0(input$motifTable_motif, '.csv'))
    df <- read.csv(motif_data_path)
    
    # Check if openxlsx package is available
    if(requireNamespace("openxlsx", quietly = TRUE)) {
      openxlsx::write.xlsx(df, file)
    } else {
      # Fallback to csv if openxlsx is not available
      write.csv(df, sub("\\.xlsx$", ".csv", file), row.names = FALSE)
    }
  }
)

# Keep motif selection synchronized between tabs
observeEvent(input$motif, {
  updateSelectInput(session, "motifTable_motif", selected = input$motif)
})

observeEvent(input$motifTable_motif, {
  updateSelectInput(session, "motif", selected = input$motifTable_motif)
})
  
  # Return to home button handler
observeEvent(input$returnToHome, {
  updateTabsetPanel(session, "main_navbar", selected = "home")
})
  
  # Add tooltips using shinyBS
  observe({
    # Add tooltips for home page elements
    addTooltip(session, "homePageDataset", 
              "Select the type of data you want to explore",
              placement = "top", trigger = "hover")
    
    # Add tooltips for navigation elements
    addTooltip(session, "datasetChoice", 
              "Switch between different types of data",
              placement = "bottom", trigger = "hover")
    
    addTooltip(session, "returnToHome", 
              "Return to the home page",
              placement = "bottom", trigger = "hover")
    
    addTooltip(session, "saveSession", 
              "Save your current analysis settings",
              placement = "bottom", trigger = "hover")
    
    addTooltip(session, "loadSession", 
              "Load a previously saved session",
              placement = "bottom", trigger = "hover")
  })
  
  observe({
  if(input$main_navbar == "motif_analysis") {
    # This handles any old references to "motif_analysis"
    updateTabsetPanel(session, "main_navbar", selected = "chrom_access")
  }
})
  
  # Save/Load Session functionality
observeEvent(input$saveSession, {
  # Get all current input values
  all_inputs <- reactiveValuesToList(input)
  # Filter out inputs that don't need to be saved
  filtered_inputs <- all_inputs[!names(all_inputs) %in% c("main_navbar", "nav_selection")]
  
  # Create a temporary file
  tmp <- tempfile(fileext = ".rds")
  saveRDS(filtered_inputs, file = tmp)
  
  # Let the user download the session file
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  session$sendCustomMessage(type = "downloadData", list(
    filename = paste0("session_", timestamp, ".rds"),
    content = base64enc::base64encode(tmp),
    contentType = "application/octet-stream"
  ))
  
  # Show notification
  showNotification("Session file ready for download", type = "message", duration = 5)
})
  
observeEvent(input$loadSession, {
  # Show a modal dialog with file upload input
  showModal(modalDialog(
    title = "Load Session",
    fileInput("sessionFile", "Upload session file (.rds):",
              accept = ".rds"),
    footer = tagList(
      actionButton("loadSelectedSession", "Load"),
      modalButton("Cancel")
    )
  ))
})


# Handle the file upload and loading
observeEvent(input$loadSelectedSession, {
  req(input$sessionFile)
  
  tryCatch({
    showLoading()
    loaded_inputs <- readRDS(input$sessionFile$datapath)
    
    # Update all inputs with values from the loaded session
    for(name in names(loaded_inputs)) {
      if(name %in% names(input)) {
        # Use appropriate update function based on input type
        input_type <- class(input[[name]])
        
        if("character" %in% input_type || "numeric" %in% input_type) {
          updateTextInput(session, name, value = loaded_inputs[[name]])
        } else if("logical" %in% input_type) {
          updateCheckboxInput(session, name, value = loaded_inputs[[name]])
        } else if(length(loaded_inputs[[name]]) > 1) {
          # Assume it's a checkboxGroupInput
          updateCheckboxGroupInput(session, name, selected = loaded_inputs[[name]])
        } else {
          # Assume it's a selectInput, radioButtons, etc.
          updateSelectInput(session, name, selected = loaded_inputs[[name]])
        }
      }
    }
    
    hideLoading()
    showNotification("Session loaded successfully!", type = "message", duration = 5)
    removeModal()
  }, error = function(e) {
    hideLoading()
    showNotification(paste("Error loading session:", e$message), type = "error", duration = 5)
  })
})
  
  observe_helpers() 
  
  # Set up selectize inputs with create option
  optCrt = "{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }" 
  
  updateSelectizeInput(session, "sc1a1inp2", choices = names(sc1gene), server = TRUE, 
                       selected = sc1def$gene1, options = list( 
                         maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt))) 
  
  updateSelectizeInput(session, "sc1b2inp1", choices = names(sc1gene), server = TRUE, 
                       selected = sc1def$gene1, options = list( 
                         maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt))) 
  
  updateSelectizeInput(session, "sc1b2inp2", choices = names(sc1gene), server = TRUE, 
                       selected = sc1def$gene2, options = list( 
                         maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt))) 
  
  # Add new Gene-Gene Scatter Plot tab selectize inputs
  updateSelectizeInput(session, "sc1b2_scatter_inp1", choices = names(sc1gene), server = TRUE, 
                      selected = "BATF", options = list( 
                        maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
  
  updateSelectizeInput(session, "sc1b2_scatter_inp2", choices = names(sc1gene), server = TRUE, 
                      selected = "CD19", options = list( 
                        maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
  
  updateSelectizeInput(session, "sc1c1inp2", server = TRUE, 
                       choices = c(sc1conf[dimred == FALSE]$UI, names(sc1gene)), 
                       selected = sc1conf[is.na(fID) & dimred == FALSE]$UI[1], options = list( 
                         maxOptions = length(sc1conf[is.na(fID)]$UI) + 3, 
                         create = TRUE, persist = TRUE, render = I(optCrt)))
  # Make sure dimension reduction options are restricted to X_umap1 and X_umap2
  observe({
    # Create list of only UMAP dimension reduction options
    umap_choices <- c("X_umap1", "X_umap2")
    
    # Filter out dimension reduction columns from cell information dropdowns
    cell_info_choices <- sc1conf[dimred == FALSE]$UI
    
    # Update dimension reduction dropdowns for all relevant tabs
    # Tab a1: cellInfo vs geneExpr
    updateSelectInput(session, "sc1a1drX", choices = umap_choices, selected = "X_umap1")
    updateSelectInput(session, "sc1a1drY", choices = umap_choices, selected = "X_umap2")
    
    # Tab b2: Gene coexpression
    updateSelectInput(session, "sc1b2drX", choices = umap_choices, selected = "X_umap1")
    updateSelectInput(session, "sc1b2drY", choices = umap_choices, selected = "X_umap2")
    
    # Tab Motif: Motif scores
    updateSelectInput(session, "motifdrX", choices = umap_choices, selected = "X_umap1")
    updateSelectInput(session, "motifdrY", choices = umap_choices, selected = "X_umap2")
    
    # Update all cell information dropdowns to exclude dimension reductions
    updateSelectInput(session, "sc1a1inp1", choices = sc1conf[grp == TRUE]$UI,
                     selected = if(sc1def$meta1 %in% sc1conf[grp == TRUE]$UI) {
                       sc1def$meta1 
                     } else {
                       sc1conf[grp == TRUE]$UI[1]
                     })
  })
  
  # Handle dataset selection changes
  observeEvent(input$datasetChoice, {
    if(input$datasetChoice == "chromatin_accessibility") {
      # When switching to motif data, update motif dropdown if needed
      if(file.exists("motif_data")) {
        motif_files <- list.files("motif_data", pattern = "*.csv")
        if(length(motif_files) > 0) {
          motif_names <- sub(".csv$", "", motif_files)
          updateSelectInput(session, "motif", choices = motif_names, selected = motif_names[1])
        } else {
          showNotification("No chromatin accessibility files found in motif_data directory", type = "warning")
        }
      } else {
        showNotification("Motif data directory not found", type = "error")
      }
    }
  })
  
  # Handle home page dataset selection (sync with navbar dataset choice)
  observeEvent(input$homePageDataset, {
    updateRadioButtons(session, "datasetChoice", selected = input$homePageDataset)
  })
  
  # Sync navbar dataset choice with home page selection
  observeEvent(input$datasetChoice, {
    updateSelectInput(session, "homePageDataset", selected = input$datasetChoice)
  })
  ### Plots for tab a1 - Cell Info / Gene Expr
  output$sc1a1sub1.ui <- renderUI({ 
    sub = strsplit(sc1conf[UI == input$sc1a1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("sc1a1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  
  observeEvent(input$sc1a1sub1non, { 
    sub = strsplit(sc1conf[UI == input$sc1a1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1a1sub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  
  observeEvent(input$sc1a1sub1all, { 
    sub = strsplit(sc1conf[UI == input$sc1a1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1a1sub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  }) 
  
  output$sc1a1oup1 <- renderPlot({ 
    scDRcell(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp1,  
             input$sc1a1sub1, input$sc1a1sub2, 
             input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1, 
             input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$sc1a1lab1) 
  }) 
  
  output$sc1a1oup1.ui <- renderUI({ 
    plotOutput("sc1a1oup1", height = pList[input$sc1a1psz]) 
  }) 
  
  output$sc1a1oup1.pdf <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1a1drX, "_", input$sc1a1drY, "_", input$sc1a1inp1, ".pdf") 
    }, 
    content = function(file) { 
      ggsave(file, device = "pdf", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w, 
             useDingbats = FALSE, 
             plot = scDRcell(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, 
                           input$sc1a1inp1, input$sc1a1sub1, input$sc1a1sub2, 
                           input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1,  
                           input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, 
                           input$sc1a1lab1)) 
    }
  ) 
  
  output$sc1a1oup1.png <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1a1drX, "_", input$sc1a1drY, "_", input$sc1a1inp1, ".png") 
    }, 
    content = function(file) { 
      ggsave(file, device = "png", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w, 
             plot = scDRcell(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, 
                           input$sc1a1inp1, input$sc1a1sub1, input$sc1a1sub2, 
                           input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1,  
                           input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, 
                           input$sc1a1lab1)) 
    }
  ) 
  
  output$sc1a1.dt <- renderDataTable({ 
    ggData = scDRnum(sc1conf, sc1meta, input$sc1a1inp1, input$sc1a1inp2, 
                     input$sc1a1sub1, input$sc1a1sub2, 
                     "sc1gexpr.h5", sc1gene, input$sc1a1splt) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("pctExpress"), digits = 2) 
  }) 
   
  output$sc1a1oup2 <- renderPlot({ 
    scDRgene(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp2,  
             input$sc1a1sub1, input$sc1a1sub2, 
             "sc1gexpr.h5", sc1gene, 
             input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2, 
             input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt) 
  })
  
  output$sc1a1oup2.ui <- renderUI({ 
    plotOutput("sc1a1oup2", height = pList[input$sc1a1psz]) 
  }) 
  
  output$sc1a1oup2.pdf <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1a1drX, "_", input$sc1a1drY, "_", input$sc1a1inp2, ".pdf") 
    }, 
    content = function(file) { 
      ggsave(file, device = "pdf", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w, 
             useDingbats = FALSE, 
             plot = scDRgene(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, 
                           input$sc1a1inp2, input$sc1a1sub1, input$sc1a1sub2, 
                           "sc1gexpr.h5", sc1gene, 
                           input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2, 
                           input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt)) 
    }
  ) 
  
  output$sc1a1oup2.png <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1a1drX, "_", input$sc1a1drY, "_", input$sc1a1inp2, ".png") 
    }, 
    content = function(file) { 
      ggsave(file, device = "png", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w, 
             plot = scDRgene(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, 
                           input$sc1a1inp2, input$sc1a1sub1, input$sc1a1sub2, 
                           "sc1gexpr.h5", sc1gene, 
                           input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2, 
                           input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt)) 
    }
  )
  ### Plots for tab b2 - Gene Coexpression
  output$sc1b2sub1.ui <- renderUI({ 
    sub = strsplit(sc1conf[UI == input$sc1b2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("sc1b2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  
  observeEvent(input$sc1b2sub1non, { 
    sub = strsplit(sc1conf[UI == input$sc1b2sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1b2sub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  
  observeEvent(input$sc1b2sub1all, { 
    sub = strsplit(sc1conf[UI == input$sc1b2sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1b2sub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  }) 
  
  # Add handlers for the Gene-Gene Scatter Plot tab
  output$sc1b2_scatter_sub1.ui <- renderUI({ 
    sub = strsplit(sc1conf[UI == input$sc1b2_scatter_sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("sc1b2_scatter_sub2", "Select which cells to show", inline = TRUE, 
                      choices = sub, selected = sub) 
  }) 
  
  observeEvent(input$sc1b2_scatter_sub1non, { 
    sub = strsplit(sc1conf[UI == input$sc1b2_scatter_sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1b2_scatter_sub2", label = "Select which cells to show", 
                            choices = sub, selected = NULL, inline = TRUE) 
  }) 
  
  observeEvent(input$sc1b2_scatter_sub1all, { 
    sub = strsplit(sc1conf[UI == input$sc1b2_scatter_sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1b2_scatter_sub2", label = "Select which cells to show", 
                            choices = sub, selected = sub, inline = TRUE) 
  })
  
  output$sc1b2oup1 <- renderPlot({ 
    scDRcoex(sc1conf, sc1meta, input$sc1b2drX, input$sc1b2drY,   
             input$sc1b2inp1, input$sc1b2inp2, input$sc1b2sub1, input$sc1b2sub2, 
             "sc1gexpr.h5", sc1gene, 
             input$sc1b2siz, input$sc1b2col1, input$sc1b2ord1, 
             input$sc1b2fsz, input$sc1b2asp, input$sc1b2txt)
  })
  
  output$sc1b2oup1.ui <- renderUI({ 
    plotOutput("sc1b2oup1", height = pList2[input$sc1b2psz]) 
  }) 
  
  output$sc1b2oup1.pdf <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1b2drX, "_", input$sc1b2drY, "_",  
             input$sc1b2inp1, "_", input$sc1b2inp2, ".pdf") 
    }, 
    content = function(file) { 
      ggsave(file, device = "pdf", height = input$sc1b2oup1.h, width = input$sc1b2oup1.w, 
             useDingbats = FALSE, 
             plot = scDRcoex(sc1conf, sc1meta, input$sc1b2drX, input$sc1b2drY,  
                       input$sc1b2inp1, input$sc1b2inp2, input$sc1b2sub1, 
                       input$sc1b2sub2, "sc1gexpr.h5", sc1gene, 
                       input$sc1b2siz, input$sc1b2col1, input$sc1b2ord1, 
                       input$sc1b2fsz, input$sc1b2asp, input$sc1b2txt))
    }
  ) 
  
  output$sc1b2oup1.png <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1b2drX, "_", input$sc1b2drY, "_",  
             input$sc1b2inp1, "_", input$sc1b2inp2, ".png") 
    }, 
    content = function(file) { 
      ggsave(file, device = "png", height = input$sc1b2oup1.h, width = input$sc1b2oup1.w, 
             plot = scDRcoex(sc1conf, sc1meta, input$sc1b2drX, input$sc1b2drY,  
                       input$sc1b2inp1, input$sc1b2inp2, input$sc1b2sub1, 
                       input$sc1b2sub2, "sc1gexpr.h5", sc1gene, 
                       input$sc1b2siz, input$sc1b2col1, input$sc1b2ord1, 
                       input$sc1b2fsz, input$sc1b2asp, input$sc1b2txt))
    }
  )
  
  output$sc1b2oup2 <- renderPlot({ 
    scDRcoexLeg(input$sc1b2inp1, input$sc1b2inp2, input$sc1b2col1, input$sc1b2fsz) 
  }) 
  
  output$sc1b2oup2.ui <- renderUI({ 
    plotOutput("sc1b2oup2", height = "300px") 
  }) 
  
  output$sc1b2oup2.pdf <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1b2drX, "_", input$sc1b2drY, "_",  
             input$sc1b2inp1, "_", input$sc1b2inp2, "_leg.pdf") 
    }, 
    content = function(file) { 
      ggsave(file, device = "pdf", height = 3, width = 4, useDingbats = FALSE,
             plot = scDRcoexLeg(input$sc1b2inp1, input$sc1b2inp2, input$sc1b2col1, input$sc1b2fsz)) 
    }
  ) 
  
  output$sc1b2oup2.png <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1b2drX, "_", input$sc1b2drY, "_",  
             input$sc1b2inp1, "_", input$sc1b2inp2, "_leg.png") 
    }, 
    content = function(file) { 
      ggsave(file, device = "png", height = 3, width = 4, 
             plot = scDRcoexLeg(input$sc1b2inp1, input$sc1b2inp2, input$sc1b2col1, input$sc1b2fsz)) 
    }
  ) 
  
  output$sc1b2.dt <- renderDataTable({ 
    ggData = scDRcoexNum(sc1conf, sc1meta, input$sc1b2inp1, input$sc1b2inp2, 
                         input$sc1b2sub1, input$sc1b2sub2, "sc1gexpr.h5", sc1gene) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("percent"), digits = 2) 
  }) 
   
  # Add the Gene-Gene scatter plot output
  output$sc1b2_scatter_plot <- renderPlot({
    # Create the gene-gene scatter plot
    scGenePlot(
      sc1conf,
      sc1meta,
      input$sc1b2_scatter_inp1, 
      input$sc1b2_scatter_inp2,
      input$sc1b2_scatter_sub1, 
      input$sc1b2_scatter_sub2,
      "sc1gexpr.h5", 
      sc1gene,
      input$sc1b2_scatter_siz,
      input$sc1b2_scatter_color,
      input$sc1b2_scatter_colorby,
      input$sc1b2_scatter_fsz
    )
  })
  
  output$sc1b2_scatter_plot.ui <- renderUI({
    plotOutput("sc1b2_scatter_plot", height = pList2[input$sc1b2_scatter_psz])
  })
  
  output$sc1b2_scatter_stats <- renderText({
    getCorrelationStats(
      sc1meta,
      input$sc1b2_scatter_inp1,
      input$sc1b2_scatter_inp2,
      input$sc1b2_scatter_sub1,
      input$sc1b2_scatter_sub2,
      "sc1gexpr.h5",
      sc1gene
    )
  })
  # Add download handlers for gene-gene scatter plot
  output$sc1b2_scatter_plot.pdf <- downloadHandler(
    filename = function() {
      # Clean and validate gene names for filename
      gene1 <- ifelse(is.null(input$sc1b2_scatter_inp1) || is.na(input$sc1b2_scatter_inp1), 
                     "gene1", input$sc1b2_scatter_inp1)
      gene2 <- ifelse(is.null(input$sc1b2_scatter_inp2) || is.na(input$sc1b2_scatter_inp2), 
                     "gene2", input$sc1b2_scatter_inp2)
      
      # Replace any invalid filename characters
      gene1 <- gsub("[^a-zA-Z0-9_-]", "", gene1)
      gene2 <- gsub("[^a-zA-Z0-9_-]", "", gene2)
      
      # Generate filename
      paste0("gene_scatter_", gene1, "_vs_", gene2, ".pdf")
    },
    content = function(file) {
      p <- scGenePlot(
        sc1conf,
        sc1meta,
        input$sc1b2_scatter_inp1,
        input$sc1b2_scatter_inp2,
        input$sc1b2_scatter_sub1,
        input$sc1b2_scatter_sub2,
        "sc1gexpr.h5",
        sc1gene,
        input$sc1b2_scatter_siz,
        input$sc1b2_scatter_color,
        input$sc1b2_scatter_colorby,
        input$sc1b2_scatter_fsz
      )
      ggsave(file, plot = p, device = "pdf", 
             height = input$sc1b2_scatter_plot.h, 
             width = input$sc1b2_scatter_plot.w)
    }
  )
  
  output$sc1b2_scatter_plot.png <- downloadHandler(
    filename = function() {
      # Clean and validate gene names for filename
      gene1 <- ifelse(is.null(input$sc1b2_scatter_inp1) || is.na(input$sc1b2_scatter_inp1), 
                     "gene1", input$sc1b2_scatter_inp1)
      gene2 <- ifelse(is.null(input$sc1b2_scatter_inp2) || is.na(input$sc1b2_scatter_inp2), 
                     "gene2", input$sc1b2_scatter_inp2)
      
      # Replace any invalid filename characters
      gene1 <- gsub("[^a-zA-Z0-9_-]", "", gene1)
      gene2 <- gsub("[^a-zA-Z0-9_-]", "", gene2)
      
      # Generate filename
      paste0("gene_scatter_", gene1, "_vs_", gene2, ".png")
    },
    content = function(file) {
      p <- scGenePlot(
        sc1conf,
        sc1meta,
        input$sc1b2_scatter_inp1,
        input$sc1b2_scatter_inp2,
        input$sc1b2_scatter_sub1,
        input$sc1b2_scatter_sub2,
        "sc1gexpr.h5",
        sc1gene,
        input$sc1b2_scatter_siz,
        input$sc1b2_scatter_color,
        input$sc1b2_scatter_colorby,
        input$sc1b2_scatter_fsz
      )
      ggsave(file, plot = p, device = "png", 
             height = input$sc1b2_scatter_plot.h, 
             width = input$sc1b2_scatter_plot.w)
    }
  )
  
  ### Plots for tab c1 - Violin/Box plot
  output$sc1c1sub1.ui <- renderUI({ 
    sub = strsplit(sc1conf[UI == input$sc1c1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("sc1c1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  
  observeEvent(input$sc1c1sub1non, { 
    sub = strsplit(sc1conf[UI == input$sc1c1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1c1sub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  
  observeEvent(input$sc1c1sub1all, { 
    sub = strsplit(sc1conf[UI == input$sc1c1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1c1sub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  }) 
  
  output$sc1c1oup <- renderPlot({ 
    scVioBox(sc1conf, sc1meta, input$sc1c1inp1, input$sc1c1inp2, 
             input$sc1c1sub1, input$sc1c1sub2, 
             "sc1gexpr.h5", sc1gene, input$sc1c1typ, input$sc1c1pts, 
             input$sc1c1siz, input$sc1c1fsz) 
  }) 
  
  output$sc1c1oup.ui <- renderUI({ 
    plotOutput("sc1c1oup", height = pList2[input$sc1c1psz]) 
  }) 
  
  output$sc1c1oup.pdf <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1c1typ, "_", input$sc1c1inp1, "_", input$sc1c1inp2, ".pdf") 
    }, 
    content = function(file) { 
      ggsave(file, device = "pdf", height = input$sc1c1oup.h, width = input$sc1c1oup.w, 
             useDingbats = FALSE, 
             plot = scVioBox(sc1conf, sc1meta, input$sc1c1inp1, input$sc1c1inp2, 
                           input$sc1c1sub1, input$sc1c1sub2, 
                           "sc1gexpr.h5", sc1gene, input$sc1c1typ, input$sc1c1pts, 
                           input$sc1c1siz, input$sc1c1fsz)) 
    }
  ) 
  
  output$sc1c1oup.png <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1c1typ, "_", input$sc1c1inp1, "_", input$sc1c1inp2, ".png") 
    }, 
    content = function(file) { 
      ggsave(file, device = "png", height = input$sc1c1oup.h, width = input$sc1c1oup.w, 
             plot = scVioBox(sc1conf, sc1meta, input$sc1c1inp1, input$sc1c1inp2, 
                           input$sc1c1sub1, input$sc1c1sub2, 
                           "sc1gexpr.h5", sc1gene, input$sc1c1typ, input$sc1c1pts, 
                           input$sc1c1siz, input$sc1c1fsz)) 
    }
  )
  ### Plots for tab d1 - Bubbleplot / Heatmap
  output$sc1d1sub1.ui <- renderUI({ 
    sub = strsplit(sc1conf[UI == input$sc1d1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("sc1d1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  
  observeEvent(input$sc1d1sub1non, { 
    sub = strsplit(sc1conf[UI == input$sc1d1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1d1sub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  
  observeEvent(input$sc1d1sub1all, { 
    sub = strsplit(sc1conf[UI == input$sc1d1sub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "sc1d1sub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  })
  
  output$sc1d1oupTxt <- renderUI({ 
    geneList = scGeneList(input$sc1d1inp, sc1gene) 
    if(nrow(geneList) > 50){ 
      HTML("More than 50 input genes! Please reduce the gene list!") 
    } else { 
      oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted") 
      if(nrow(geneList[present == FALSE]) > 0){ 
        oup = paste0(oup, "<br/>", 
                     nrow(geneList[present == FALSE]), " genes not found (", 
                     paste0(geneList[present == FALSE]$gene, collapse = ", "), ")") 
      } 
      HTML(oup) 
    } 
  }) 
  
  output$sc1d1oup <- renderPlot({ 
    scBubbHeat(sc1conf, sc1meta, input$sc1d1inp, input$sc1d1grp, input$sc1d1plt, 
               input$sc1d1sub1, input$sc1d1sub2, "sc1gexpr.h5", sc1gene, 
               input$sc1d1scl, input$sc1d1row, input$sc1d1col, 
               input$sc1d1cols, input$sc1d1fsz) 
  }) 
  
  output$sc1d1oup.ui <- renderUI({ 
    plotOutput("sc1d1oup", height = pList3[input$sc1d1psz]) 
  }) 
  
  output$sc1d1oup.pdf <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1d1plt, "_", input$sc1d1grp, ".pdf") 
    }, 
    content = function(file) { 
      ggsave(file, device = "pdf", height = input$sc1d1oup.h, width = input$sc1d1oup.w, 
             plot = scBubbHeat(sc1conf, sc1meta, input$sc1d1inp, input$sc1d1grp, input$sc1d1plt, 
                             input$sc1d1sub1, input$sc1d1sub2, "sc1gexpr.h5", sc1gene, 
                             input$sc1d1scl, input$sc1d1row, input$sc1d1col, 
                             input$sc1d1cols, input$sc1d1fsz, save = TRUE)) 
    }
  ) 
  
  output$sc1d1oup.png <- downloadHandler( 
    filename = function() { 
      paste0("sc1", input$sc1d1plt, "_", input$sc1d1grp, ".png") 
    }, 
    content = function(file) { 
      ggsave(file, device = "png", height = input$sc1d1oup.h, width = input$sc1d1oup.w, 
             plot = scBubbHeat(sc1conf, sc1meta, input$sc1d1inp, input$sc1d1grp, input$sc1d1plt, 
                             input$sc1d1sub1, input$sc1d1sub2, "sc1gexpr.h5", sc1gene, 
                             input$sc1d1scl, input$sc1d1row, input$sc1d1col, 
                             input$sc1d1cols, input$sc1d1fsz, save = TRUE)) 
    }
  )
  # Motif data UI handlers
  output$motifsub1.ui <- renderUI({ 
    sub = strsplit(sc1conf[UI == input$motifsub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("motifsub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  
  observeEvent(input$motifsub1non, { 
    sub = strsplit(sc1conf[UI == input$motifsub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "motifsub2", label = "Select which cells to show", 
                             choices = sub, selected = NULL, inline = TRUE) 
  }) 
  
  observeEvent(input$motifsub1all, { 
    sub = strsplit(sc1conf[UI == input$motifsub1]$fID, "\\|")[[1]] 
    updateCheckboxGroupInput(session, inputId = "motifsub2", label = "Select which cells to show", 
                             choices = sub, selected = sub, inline = TRUE) 
  })
  
  # Motif data reactive functions
  motifData <- reactive({
    req(input$motif)
    tryCatch({
      df <- read.csv(file.path('motif_data', paste0(input$motif, '.csv')))
      return(df)
    }, error = function(e) {
      return(data.frame(Error = "Could not load motif data"))
    })
  })
  
  # Motif plot outputs
# Motif visualization plot - use data directly from the motif CSV
# Motif visualization plot - more robust error handling
# Motif visualization with UMAP coordinates
output$motifoup1 <- renderPlot({
  req(input$motif)
  
  # Load motif data
  motif_df <- tryCatch({
    motif_data_path <- file.path('motif_data', paste0(input$motif, '.csv'))
    read.csv(motif_data_path)
  }, error = function(e) {
    showNotification(paste("Error reading file:", e$message), type = "error")
    return(NULL)
  })
  
  validate(need(!is.null(motif_df), "Failed to load motif data"))
  validate(need("CellID" %in% colnames(motif_df), "Motif data must contain CellID column"))
  validate(need("Score" %in% colnames(motif_df), "Motif data must contain Score column"))
  
  # Get UMAP coordinates from metadata
  umap_x_col <- sc1conf[UI == "X_umap1"]$ID
  umap_y_col <- sc1conf[UI == "X_umap2"]$ID
  
  # Create mapping dataframe from metadata
  cell_umap_mapping <- data.frame(
    CellID = sc1meta$sampleID,  # Adjust this to match your actual cell ID column in metadata
    UMAP_1 = sc1meta[[umap_x_col]],
    UMAP_2 = sc1meta[[umap_y_col]]
  )
  
  # Merge motif data with UMAP coordinates
  merged_df <- merge(motif_df, cell_umap_mapping, by = "CellID", all.x = TRUE)
  
  # Count and report any unmatched cells
  unmatched_count <- sum(is.na(merged_df$UMAP_1) | is.na(merged_df$UMAP_2))
  if(unmatched_count > 0) {
    showNotification(paste("Note:", unmatched_count, "cells could not be matched with UMAP coordinates"), 
                    type = "warning", duration = 5)
  }
  
  # Clean data - remove rows with missing coordinates
  clean_df <- merged_df[complete.cases(merged_df[, c("UMAP_1", "UMAP_2", "Score")]), ]
  
  # Create the plot
  ggplot(clean_df, aes(x = UMAP_1, y = UMAP_2, color = Score)) +
    geom_point(size = input$motifsiz, alpha = 0.7) +
    scale_color_gradientn(colours = cList[[input$motifcol1]]) +
    labs(title = paste("Motif scores for", input$motif),
         subtitle = paste("Showing", nrow(clean_df), "cells out of", nrow(motif_df), "total"),
         x = input$motifdrX, y = input$motifdrY) +
    sctheme(base_size = sList[input$motiffsz], XYval = input$motiftxt) +
    theme(legend.position = "right") +
    coord_fixed()
})

# Similar approach for cell information plot
output$motifoup2 <- renderPlot({
  req(input$motif, input$motifcel)
  
  # Load motif data
  motif_df <- tryCatch({
    motif_data_path <- file.path('motif_data', paste0(input$motif, '.csv'))
    read.csv(motif_data_path)
  }, error = function(e) {
    showNotification(paste("Error reading file:", e$message), type = "error")
    return(NULL)
  })
  
  validate(need(!is.null(motif_df), "Failed to load motif data"))
  validate(need("CellID" %in% colnames(motif_df), "Motif data must contain CellID column"))
  validate(need(input$motifcel %in% colnames(motif_df), 
               paste("Selected cell information", input$motifcel, "not found in motif data")))
  
  # Get UMAP coordinates from metadata
  umap_x_col <- sc1conf[UI == "X_umap1"]$ID
  umap_y_col <- sc1conf[UI == "X_umap2"]$ID
  
  # Create mapping dataframe from metadata
  cell_umap_mapping <- data.frame(
    CellID = sc1meta$sampleID,  # Adjust this to match your actual cell ID column in metadata
    UMAP_1 = sc1meta[[umap_x_col]],
    UMAP_2 = sc1meta[[umap_y_col]]
  )
  
  # Merge motif data with UMAP coordinates
  merged_df <- merge(motif_df, cell_umap_mapping, by = "CellID", all.x = TRUE)
  
  # Count and report any unmatched cells
  unmatched_count <- sum(is.na(merged_df$UMAP_1) | is.na(merged_df$UMAP_2))
  if(unmatched_count > 0) {
    showNotification(paste("Note:", unmatched_count, "cells could not be matched with UMAP coordinates"), 
                    type = "warning", duration = 5)
  }
  
  # Clean data - remove rows with missing coordinates
  clean_df <- merged_df[complete.cases(merged_df[, c("UMAP_1", "UMAP_2", input$motifcel)]), ]
  
  # Create the plot
  ggplot(clean_df, aes_string(x = "UMAP_1", y = "UMAP_2", color = input$motifcel)) +
    geom_point(size = input$motifsiz, alpha = 0.7) +
    labs(title = paste("Cell info:", input$motifcel),
         subtitle = paste("Showing", nrow(clean_df), "cells out of", nrow(motif_df), "total"),
         x = input$motifdrX, y = input$motifdrY) +
    sctheme(base_size = sList[input$motiffsz], XYval = input$motiftxt) +
    theme(legend.position = "right") +
    coord_fixed()
})
  
  output$motifoup1.ui <- renderUI({ 
    plotOutput("motifoup1", height = pList[input$motifpsz]) 
  })
  
  output$motifoup1.pdf <- downloadHandler( 
    filename = function() { 
      paste0("motif_", input$motif, ".pdf") 
    }, 
    content = function(file) { 
      p <- ggplot(motifData(), aes(x = UMAP_1, y = UMAP_2, color = Score)) +
        geom_point(size = input$motifsiz, alpha = 0.7) +
        scale_color_gradientn(colours = cList[[input$motifcol1]]) +
        labs(title = paste("Motif scores for", input$motif),
             x = input$motifdrX, y = input$motifdrY) +
        sctheme(base_size = sList[input$motiffsz], XYval = input$motiftxt) +
        theme(legend.position = "right")
      
      ggsave(file, plot = p, device = "pdf", height = input$motifoup1.h, width = input$motifoup1.w)
    }
  )
  
  output$motifoup1.png <- downloadHandler( 
    filename = function() { 
      paste0("motif_", input$motif, ".png") 
    }, 
    content = function(file) { 
      p <- ggplot(motifData(), aes(x = UMAP_1, y = UMAP_2, color = Score)) +
        geom_point(size = input$motifsiz, alpha = 0.7) +
        scale_color_gradientn(colours = cList[[input$motifcol1]]) +
        labs(title = paste("Motif scores for", input$motif),
             x = input$motifdrX, y = input$motifdrY) +
        sctheme(base_size = sList[input$motiffsz], XYval = input$motiftxt) +
        theme(legend.position = "right")
      
      ggsave(file, plot = p, device = "png", height = input$motifoup1.h, width = input$motifoup1.w)
    }
  )
  
  output$motifoup2.ui <- renderUI({ 
    plotOutput("motifoup2", height = pList[input$motifpsz]) 
  })
  
  output$motifoup2.pdf <- downloadHandler( 
    filename = function() { 
      paste0("cellinfo_", input$motifcel, ".pdf") 
    }, 
    content = function(file) { 
      ggData <- motifData()
      cellInfo <- sc1meta[[sc1conf[UI == input$motifcel]$ID]]
      ggData$CellInfo <- cellInfo
      
      p <- ggplot(ggData, aes(x = UMAP_1, y = UMAP_2, color = CellInfo)) +
        geom_point(size = input$motifsiz, alpha = 0.7) +
        labs(title = paste("Cell info:", input$motifcel),
             x = input$motifdrX, y = input$motifdrY) +
        sctheme(base_size = sList[input$motiffsz], XYval = input$motiftxt) +
        theme(legend.position = "right")
      
      ggsave(file, plot = p, device = "pdf", height = input$motifoup2.h, width = input$motifoup2.w)
    }
  )
  
  output$motifoup2.png <- downloadHandler( 
    filename = function() { 
      paste0("cellinfo_", input$motifcel, ".png") 
    }, 
    content = function(file) { 
      ggData <- motifData()
      cellInfo <- sc1meta[[sc1conf[UI == input$motifcel]$ID]]
      ggData$CellInfo <- cellInfo
      
      p <- ggplot(ggData, aes(x = UMAP_1, y = UMAP_2, color = CellInfo)) +
        geom_point(size = input$motifsiz, alpha = 0.7) +
        labs(title = paste("Cell info:", input$motifcel),
             x = input$motifdrX, y = input$motifdrY) +
        sctheme(base_size = sList[input$motiffsz], XYval = input$motiftxt) +
        theme(legend.position = "right")
      
      ggsave(file, plot = p, device = "png", height = input$motifoup2.h, width = input$motifoup2.w)
    }
  )
  
# Fix for the motif data table
output$motifTable <- renderDataTable({
  req(input$motifTable_motif)
  motif_data_path <- file.path('motif_data', paste0(input$motifTable_motif, '.csv'))
  
  tryCatch({
    df <- read.csv(motif_data_path)
    datatable(df)  # Simplest version with default options
  }, error = function(e) {
    datatable(data.frame(Error = paste("Could not load motif data:", e$message)))
  })
})
  
  # Help system handlers
  observeEvent(input$showTutorial, {
    # Create a step-by-step tour using custom modals
    tour_steps <- list(
      list(
        title = "Welcome to ShinyCell!",
        content = "This tour will guide you through the main features of the application. Click 'Next' to continue or 'Close' to exit the tour at any time.",
        button_label = "Next"
      ),
      list(
        title = "Dataset Selection",
        content = "Use the dataset dropdown menu to switch between different data types.",
        target_id = "datasetDropdown",
        button_label = "Next"
      ),
      list(
        title = "Navigation Cards",
        content = "The cards at the bottom provide quick access to different visualization types. Click on any card to navigate to that section.",
        placement = "top",
        button_label = "Next"
      ),
      list(
        title = "Session Management",
        content = "You can save your current analysis settings and load them later using the save and load buttons in the top right.",
        target_id = "saveSession",
        placement = "bottom",
        button_label = "Next"
      )
    )
    
    # Initialize tour
    showModal(modalDialog(
      title = tour_steps[[1]]$title,
      tour_steps[[1]]$content,
      footer = tagList(
        actionButton("tour_next", "Next", class = "btn-primary"),
        modalButton("Close")
      ),
      easyClose = TRUE
    ))
    
    # Set current step
    tour_step <- reactiveVal(1)
    
    # Handle next button click
    observeEvent(input$tour_next, {
      current_step <- tour_step()
      if(current_step < length(tour_steps)) {
        # Move to next step
        tour_step(current_step + 1)
        step <- tour_steps[[current_step + 1]]
        
        # Show modal for next step
        showModal(modalDialog(
          title = step$title,
          step$content,
          footer = tagList(
            if(current_step + 1 < length(tour_steps)) {
              actionButton("tour_next", "Next", class = "btn-primary")
            } else {
              actionButton("tour_finish", "Finish", class = "btn-success")
            },
            modalButton("Close")
          ),
          easyClose = TRUE
        ))
        
        # Highlight target element if specified
        if(!is.null(step$target_id)) {
          # Add highlighting CSS
          runjs(paste0("$('#", step$target_id, "').addClass('tour-highlight');"))
        }
      }
    })
    
    # Handle finish button click
    observeEvent(input$tour_finish, {
      removeModal()
      showNotification("Tour completed! Explore the app to discover more features.", 
                     type = "message", duration = 5)
      
      # Reset any highlighting
      runjs("$('.tour-highlight').removeClass('tour-highlight');")
    })
    
    # Cleanup when modal is closed
    observeEvent(input$tour_close, {
      runjs("$('.tour-highlight').removeClass('tour-highlight');")
    })
  })
  
  # Help button handlers for each section
  observeEvent(input$help_cellinfo, {
    showModal(modalDialog(
      title = "Cell Info / Gene Expression Visualization",
      HTML(paste0(
        "<p>This section allows you to visualize cell metadata and gene expression on dimensionality reduction plots like UMAP or t-SNE.</p>",
        "<h4>Main Features:</h4>",
        "<ul>",
        "<li><strong>Dimension Reduction:</strong> Select which dimensions to display on X and Y axes</li>",
        "<li><strong>Cell Information:</strong> Color cells by categorical or continuous metadata</li>",
        "<li><strong>Gene Expression:</strong> Color cells by expression level of a selected gene</li>",
        "<li><strong>Cell Subsetting:</strong> Filter cells based on metadata categories</li>",
        "<li><strong>Plot Controls:</strong> Adjust point size, color scheme, and other visual parameters</li>",
        "</ul>",
        "<p>You can download plots as PDF or PNG using the buttons below each plot.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_coexpression, {
    showModal(modalDialog(
      title = "Gene Coexpression Visualization",
      HTML(paste0(
        "<p>This tab allows you to visualize the coexpression of two genes on dimensionality reduction plots.</p>",
        "<h4>Main Features:</h4>",
        "<ul>",
        "<li><strong>Gene Selection:</strong> Choose two genes to visualize together</li>",
        "<li><strong>Color Scheme:</strong> Cells are colored based on the expression levels of both genes</li>",
        "<li><strong>Legend:</strong> Helps interpret the coexpression coloring</li>",
        "<li><strong>Cell Statistics:</strong> Shows counts and percentages of cells expressing each gene</li>",
        "</ul>",
        "<p>This visualization is useful for identifying populations where both genes are co-expressed or mutually exclusive.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_gene_scatter, {
    showModal(modalDialog(
      title = "Gene-Gene Scatter Plot",
      HTML(paste0(
        "<p>This visualization shows the relationship between expression levels of two genes using a scatter plot.</p>",
        "<h4>Main Features:</h4>",
        "<ul>",
        "<li><strong>Gene Selection:</strong> Choose two genes to compare</li>",
        "<li><strong>Cell Coloring:</strong> Optionally color points by cell metadata</li>",
        "<li><strong>Correlation Statistics:</strong> View Pearson and Spearman correlations between the genes</li>",
        "<li><strong>Trend Line:</strong> A linear regression line shows the overall relationship</li>",
        "</ul>",
        "<p>This plot is useful for quantifying the correlation between two genes across all cells.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_violin, {
    showModal(modalDialog(
      title = "Violin Plot / Box Plot",
      HTML(paste0(
        "<p>This visualization shows the distribution of gene expression or cell metadata across groups of cells.</p>",
        "<h4>Main Features:</h4>",
        "<ul>",
        "<li><strong>X-axis:</strong> Categorical cell information (e.g., clusters, cell types)</li>",
        "<li><strong>Y-axis:</strong> Gene expression or continuous cell metadata</li>",
        "<li><strong>Plot Type:</strong> Choose between violin plot or box plot</li>",
        "<li><strong>Data Points:</strong> Option to show individual cell data points</li>",
        "</ul>",
        "<p>This visualization is useful for comparing expression distributions across different cell populations.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_heatmap, {
    showModal(modalDialog(
      title = "Bubbleplot / Heatmap",
      HTML(paste0(
        "<p>This visualization shows expression patterns of multiple genes across cell groups.</p>",
        "<h4>Main Features:</h4>",
        "<ul>",
        "<li><strong>Gene List:</strong> Enter multiple genes to visualize</li>",
        "<li><strong>Grouping:</strong> Select categorical cell information for grouping</li>",
        "<li><strong>Plot Type:</strong> Choose between bubbleplot (size indicates proportion of expressing cells) or heatmap</li>",
        "<li><strong>Clustering:</strong> Option to cluster rows (genes) and columns (samples)</li>",
        "</ul>",
        "<p>This visualization is useful for identifying gene expression patterns across different cell populations.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_motif, {
    showModal(modalDialog(
      title = "Motif Score Visualization",
      HTML(paste0(
        "<p>This section allows you to visualize transcription factor motif scores on dimensionality reduction plots.</p>",
        "<h4>Main Features:</h4>",
        "<ul>",
        "<li><strong>Motif Selection:</strong> Choose which transcription factor motif to visualize</li>",
        "<li><strong>Color Scheme:</strong> Cells are colored based on motif enrichment scores</li>",
        "<li><strong>Cell Information:</strong> View the same cells colored by metadata for comparison</li>",
        "<li><strong>Data Table:</strong> Access the raw motif score data</li>",
        "</ul>",
        "<p>This visualization is useful for identifying cell populations with specific transcription factor activity.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
})