# expressyouRcell Shiny Viewer

Interactive Shiny application for visualizing subcellular gene localization dynamics using the `expressyouRcell` R package.

The application provides an interactive interface for exploring cellular pictographs across multiple timepoints, supporting enrichment- and value-based coloring strategies, with export-ready outputs.

---

## 🔬 Overview

This tool wraps the `expressyouRcell` computational pipeline into an interactive visualization interface.

It enables users to:

- Explore subcellular localization dynamics across timepoints
- Switch between enrichment and value-based visualization modes
- Navigate results interactively
- Export figures and animations

---

## 🧬 Core Engine

All computations are handled by:

**expressyouRcell (>= 1.0.0)**

It provides:

- Gene-to-subcellular localization mapping  
- Enrichment-based scoring (FDR)  
- Value aggregation (mean / median)  
- Cellular pictograph rendering  
- Multi-timepoint visualization  
- Animation generation  

---

## 🖥️ Shiny Application

The Shiny interface provides:

### Input controls
- Demo dataset (`example_list`)
- Cell type selection
- Coloring mode:
  - Enrichment (FDR-based)
  - Mean of selected column
- Legend toggle

### Visualization
- Interactive pictograph viewer
- Timepoint navigation (previous / next)
- Optional play/pause (planned)
- Consistent rendering across conditions

### Export
- PNG per timepoint
- Animation (GIF / MP4)
- ZIP download of results

---

## 📊 Demo Dataset

The app currently uses:

- `example_list`

This is a built-in dataset containing multiple timepoints (e.g. P3, P5), structured as a list of expression tables.

---

## 📥 Input Data Format (Planned)

Custom upload is not yet available.

Expected (minimal) format:

- gene_symbol
- value

---

## 🧬 Gene Localization Mapping

Function:

map_gene_localization(gene_set, organism)

Supported organisms:

- Homo sapiens  
- Mus musculus  
- Rattus norvegicus  
- Danio rerio  
- Saccharomyces cerevisiae  

(This version does not support the creation of gene localization mapping table as far as now)

---

## ⚙️ Dependencies

### Core engine (expressyouRcell)

- data.table  
- ggplot2  
- clusterProfiler  
- DOSE  
- IRanges  
- rtracklayer  
- rsvg  
- grImport2  
- stringr  
- gridExtra  
- RColorBrewer  
- rlist  
- ggpubr  

### Export features

- gifski (GIF generation)  
- av (video generation)  

### Shiny app

- shiny  
- bslib (recommended for UI styling)  

---

## 🚧 Known Limitations

- No custom dataset upload yet  
- No possibility of creating alternative localization mapping
- Legend may affect plot layout consistency  
- PNG export aspect ratio not fully standardized  
- Viewer width depends on layout configuration  

---

## 🧭 Roadmap

### Data input
- Custom dataset upload (CSV/RDS)
- Input validation
- Schema checking

### Visualization
- Fixed aspect ratio rendering
- Decoupled legend layout
- Improved timepoint consistency
- Full-width responsive viewer

### Interaction
- Slider-based navigation
- Play/pause animation mode
- Improved timepoint labeling

### Export
- Standardized PNG output
- Unified export pipeline
- Optional PDF report generation  

---

## 🧠 Design Philosophy

- Interactive exploration over static plots  
- Reproducible backend computation  
- Clear separation between analysis, visualization, and export  
- Scientific interpretability over automation  

---

## 🚀 Getting Started

Run the app:

shiny::runApp("path/to/app")

---

## 📄 License

This project is licensed under the MIT License.

## 📚 Citation

If you use this tool, please cite:

### Primary software
expressyouRcell R package (backend engine)

- Repository: https://github.com/LabTranslationalArchitectomics/expressyouRcell.git

### Associated publication

[Paganin M, Tebaldi T, Lauria F, Viero G.]. (2023).  
*Visualizing gene expression changes in time, space, and single cells with expressyouRcell*.  
iScience.  
DOI: 10.1016/j.isci.2023.106853

This Shiny application provides an interactive visualization layer built on top of the expressyouRcell framework.