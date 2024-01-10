len_len_convert <- function( dat, params, raw_length, length_type, length_units, desired_type = "FORK LENGTH"){

# @dat - dataframe - will be returned with addtional vars
# @params - "./sandbox/LLconversions.csv"
# @raw_length - column name containing lengths to be converted
# @length_type - column name containg length types. Deafult "FORK LENGTH". alternate "TOTAL LENGTH"
# @length_units - column name containg length units
  
  # This function will return length in the desired type (default Fork length) in centimeters. It will convert units (mm and inches) and type to a standard format. It matches withe parameters based on REGION, ITIS CODE, and LENGTH TYPE (original and desired output). To update the parameters, change the values in the csv file. 
 require(tidyverse)
     
t = dat %>%
    #select({{raw_length}}, {{length_type}}, {{length_units}}, ITIS_CODE, REGION_NAME) %>%
    mutate(ORIGINAL_LENGTH_TYPE = {{length_type}}) %>%
  type_convert() %>%
    left_join(pivot_wider(params, names_from =  param , values_from = value),
              by = c("ITIS_CODE", "ORIGINAL_LENGTH_TYPE")) %>%   #"REGION_NAME", "ORIGINAL_LENGTH_TYPE")) %>% #++REMOVING REGION
    filter(desired == desired_type) %>%
    mutate(LENGTH_CM = if_else({{length_units}} == "INCHES", {{raw_length}}*2.54,
                               if_else({{length_units}} == "MILLIMETERS", {{raw_length}}*0.1,
                                       {{raw_length}})),
           FL_CM = a + b*LENGTH_CM,
           TL_CM = a/b + FL_CM/b,
           converted_length = ifelse(ORIGINAL_LENGTH_TYPE == desired_type, 0, 1))
if (desired_type == "FORK LENGTH"){
  return(bind_cols(dat, select(t, FL_CM, converted_length)))
} else if (desired_type == "TOTAL LENGTH"){
  return(bind_cols(dat, select(t, TL_CM, converted_length))) 
} else {
  
  print("Your desired length type is not supported. Please select either 'FORK LENGTH' or 'TOTAL LENGTH' ")
}


  #print("WARNING: this function parses class type for all variables. See documentation for 'type_convert()'")
}
