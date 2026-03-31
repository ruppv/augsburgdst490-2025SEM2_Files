library(tidyverse)

### Step 0: Preprocessing the data for analysis
### Read in the USDA unemployment and education data, join, and save

## Read
employment <- readxl::read_excel('day16/data/Unemployment.xlsx',
                                 skip = 4,
                                 sheet = 'UnemploymentMedianIncome')


education <- readxl::read_excel('day16/data/Education.xlsx',
                                skip = 3,
                                sheet = 'Education 1970 to 2022') |>
  rename(FIPS_Code = 1,
         rural_urban_continuum = `2013 Rural-urban Continuum Code`)  # Just do some cleaning



## Pivot each dataset
employment2 <- employment |>
  pivot_longer(cols = matches("^Unemployment_rate")) |>
  select(FIPS_Code,name,value) |>
  mutate(Year = as.numeric(str_extract(name,pattern="[:digit:]{4}$")),
         variable = str_extract(name,pattern="[^[:digit:]]+")) |> select(-name,-variable) |>
  rename(unemployment_rate = value)

education2 <- education |>
  pivot_longer(cols = -c("FIPS_Code","State","Area name")) |>
  filter(str_detect(name,"Percent of adults")) |>
  select(FIPS_Code,name,value) |>
  mutate(Year = (str_extract(name,pattern="[:digit:]{4}-[:digit:]{2}|[:digit:]{4}$")),
         Year = as.numeric(str_extract(Year,pattern = "^[:digit:]{4}" )),
         variable = str_extract(name,pattern="[^[:digit:]]+") ) |> select(-name) |>
  mutate(variable = if_else(str_detect(variable,pattern = "with less than a high school diploma"),"less_than_hs",variable),
         variable = if_else(str_detect(variable,pattern = "with a high school diploma"),"hs_only",variable) ,
         variable = if_else(str_detect(variable,pattern = "some college \\("),"some_college_associates",variable),
         variable = if_else(str_detect(variable,pattern = "some college or"),"some_college_associates",variable),
         variable = if_else(str_detect(variable,pattern = "with a bachelor's"),"bachelors_plus",variable),
         variable = if_else(str_detect(variable,pattern = "completing four years"),"bachelors_plus",variable) ) |>
  pivot_wider(names_from = "variable")

### Now join
unemployment_education_all_data <- employment2 |>
  inner_join(education2,by=c("FIPS_Code","Year"))

### Save the data
save(unemployment_education_all_data,file ='day16/data/employment_education_all.Rda')
