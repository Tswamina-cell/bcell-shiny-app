library(shiny)
library(shinyhelper)
library(data.table)
library(Matrix)
library(DT)
library(magrittr)
library(plotly)
library(ggplot2)
library(ggrepel)
library(hdf5r)
library(ggdendro)
library(gridExtra)
library(shinydashboard)
library(shinythemes)
library(dplyr)

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

# Load initial configuration data
sc1conf = readRDS("sc1conf.rds")
sc1def = readRDS("sc1def.rds")
sc1gene = readRDS("sc1gene.rds")
sc1meta = readRDS("sc1meta.rds")

# Update configuration with UMAP entries
sc1conf <- addUMAPtoConfig(sc1conf)

# Define color palettes
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