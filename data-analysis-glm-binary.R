# load the packages
library(Pmisc)
library(tidyverse)

# load datafile
File = "smokeDownload.RData"
load(File)

# what does the data look like:
smoke %>% 
  select(Age, Sex, Grade, RuralUrban, Race, Tried_cigarette_smkg_even) %>% 
  glimpse()

# what does the data look like:
smoke[1:5,c('Age','Sex','Grade','RuralUrban','Race', 'Tried_cigarette_smkg_even')]

# Check the data format of the variable that to be predicted:
smokeFormats[smokeFormats$colName == 'Tried_cigarette_smkg_even', ]
