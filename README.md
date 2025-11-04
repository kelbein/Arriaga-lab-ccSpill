# ccSpill

### This package is for creating a spillover matrix from the characterization data of metal-tagged antibodies.

## Step 0, part 1: Install the package and load the library
devtools::install_github(https://github.umn.edu/Arriaga-lab/ccSpill)

library(ccSpill)


## Step 0, part 2: Organize data
Before using this package, organize solution mode data into folders according to the attached file structure. Each date folder (date-1, date-2) that 
has at least one isotope (isotope-1) must also have metals. The concentrations for the levels (level-1, level-2, etc.) do not need to match. 
Fill in meta-data.csv with the isotopes and their concentration levels, in picomolars. Leave empty folders blank (no need to delete).

Run this code to see the data file structure:

browseVignettes("ccSpill")


## Step 1: Load the meta-data.csv file
meta <- concentration_lookup(file_path = ) #define the file path

This gives meta$lookup_table and meta$valid_isotopes, both of which are needed for the next step


## Step 2: Load and process the raw data
all_data <- load_solution_data(base_data_dir = , #define the path to the folder that contains date-1, date-2

conc_lookup = meta$lookup_table, 

valid_isotope_names = meta$valid_isotopes)


## Step 3: Create the spillover matrix
results <- create_ccSpill_matrix(all_data) #uses output from step 2

results$spillover_matrix is the matrix that can be used to correct cell data


## Step 4: Visualize matrices (optional)
plot_spillover_values(results$ratios_dataframe) #gives metals/antibody with error

plot_spillover_normalized(results$ratios_dataframe) #gives spillover matrix (visually)


### Folder Structure
#### Original file names can be kept, but the folder names should follow this format (date-1, isotope-1, level-1, etc.). You'll fill in what that information is (the isotope and concentration for each level) in meta-data.csv.
Folder level 1: data/

Folder level 2: meta-data.csv, date-1, date-2, etc.

Folder level 2: metals, isotope-1, isotope-2, etc.

Folder level 3: level-1, level-2, level-3, etc.

Within folder level 3, store your files. You can keep the original file names.





