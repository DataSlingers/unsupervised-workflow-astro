# Unsupervised Workflow Case Study: Finding Common Origins of Milky Way Stars

## Overview

TBD

## Reproducibility

To reproduce the full case study:

1. Download `allstars_gc.csv` data from [TBD](), and move `allstars_gc.csv` into `unsupervised-workflow-astro/data/` folder.

2. For RStudio users, open up the R project `unsupervised-workflow-astro.Rproj` in RStudio. For non-RStudio users, change directories to the root repository directory (i.e., `unsupervised-workflow-astro/`) and open up R.

3. Restore the reproducible R environment, containing all necessary dependencies, by typing in the R console:
   ```r
   renv::restore()
   ```
   *Note*: If `renv` was not previously installed, you may need to install the `renv` R package manually via `install.packages("renv")`.

4. Render the quarto notebooks in the `unsupervised-workflow-astro/notebooks/` folder (in numeric order). This can be done by either clicking the "Render" button in RStudio for each notebook, or by running the following commands (sequentially) in command line:
    ```bash
    # set number of cores to use for parallel processing
    export NCORES=4
    # to render data preparation notebook
    quarto render notebooks/01-astro-case-study-data.qmd
    # to render exploratory data analysis notebook
    quarto render notebooks/02-astro-case-study-eda.qmd
    # to render dimension reduction notebook
    quarto render notebooks/03-astro-case-study-dimension-reduction.qmd
    # to render clustering training notebook
    quarto render notebooks/04-astro-case-study-clustering-train.qmd
    # to render clustering validation notebook
    quarto render notebooks/05-astro-case-study-clustering-validation.qmd
    # to render clustering interpretation notebook
    quarto render notebooks/06-astro-case-study-clustering-interpretation.qmd
    # to render all notebooks in the folder
    quarto render
    ```
    *Note:* If you have not installed quarto, you must do so (see https://quarto.org/docs/get-started/) before rendering these notebooks.

5. Open `unsupervised-workflow-astro/docs/index.html` in your web browser to view the results.
