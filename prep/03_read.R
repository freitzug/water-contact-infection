
# Read data ---------------------------------------------------------------
df <- readRDS(paste0(path.dbl,"df_demo.RDS")) #survey data
wctmp <- readRDS(paste0(path.dbl,"dfwc_demo.RDS")) #load water contact observation data
dict <- read.csv("dict/data-dictionary.csv") #data dictionary

