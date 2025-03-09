library(reticulate)
library(SeuratDisk)
library(zellkonverter)
library(SingleCellExperiment)
library(Seurat)
library(ggplot2)

adata <- readH5AD("/Users/tanushswaminathan/Downloads/donor1_multiome_gex_final.h5ad")
seurat_obj <- as.Seurat(adata, counts = "counts", data = "X")
View(seurat_obj)

adata_new <- readH5AD("/Users/tanushswaminathan/Downloads/donor1_multiome_atac_motif_scores.h5ad")
motif_data <- adata_new@assays@data$X
motif_data

seurat_cells <- colnames(seurat_obj)
motif_cells <- colnames(motif_data)

motif_assay <- CreateAssayObject(counts = motif_data)
motif_assay

common_cells <- intersect(seurat_cells, motif_cells)

seurat_obj <- subset(seurat_obj, cells = common_cells)
motif_data <- subset(motif_assay, cells = common_cells)

seurat_obj[["MotifScores"]] <- motif_data

seurat_obj <- FindVariableFeatures(seurat_obj)


# 1. Convert data to matrix format if it isn't already
motif_data <- as.matrix(adata_new@assays@data$X)

# 2. Create assay with proper matrix format
motif_assay <- CreateAssayObject(
  counts = motif_data,
)

# 3. Make sure cell names match
common_cells <- intersect(colnames(seurat_obj), colnames(motif_data))

# 4. Subset and add to Seurat object
seurat_obj <- subset(seurat_obj, cells = common_cells)
motif_assay <- subset(motif_assay, cells = common_cells)
seurat_obj[["MotifScores"]] <- motif_assay

saveRDS(seurat_obj, file = "seurat_object.rds")
