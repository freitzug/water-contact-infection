#Author: Fabian Reitzug
#Date: 2022-05-13
#Purpose: Converts multiple readings of KK and 

#Method:
# For all ppl with <= 2 readings, take mean across all exams
# For all ppl with > 2 readings take mean weighted by technicians (to account for double readings of same slide by same technician)

#Function takes five input arguments:

# 1. df = data frame to transform (one obs. per exam)
# 2. barcode = participant barcode column
# 3. test = diagnostic exam whose results should be transformed 
# 4. technician = technciian name (to group exmans with more than two readings)
# 5. m.factor = multiplication factor (ex. 24 for Kato-Katz), enter 1 if no multiplication is needed


require(magrittr)
require(dplyr)

multi.exams <-  function(df,barcode,test,technician,m.factor){

df2 <- df  
  
# for exams with 1 or 2 readings
df %<>% add_count({{barcode}}) %>% filter(n<3, !is.na({{test}})) %>% #filter exams reading nr.
  group_by({{barcode}}) %>% dplyr::summarise(
    exam_number = n(),
    out = mean({{test}}, na.rm = T) * m.factor
  )

# for exams with 3 readings
df2 %<>% add_count({{barcode}}) %>% filter(n>2,!is.na({{test}})) %>% #filter exams reading nr.
  group_by_at(vars({{barcode}},{{technician}})) %>% dplyr::summarise(
    n= first(n),
    exam_number = n(),
    out = mean({{test}}, na.rm = T) * m.factor
  ) %>% ungroup() %>% group_by({{barcode}}) %>%
  dplyr::summarise(
    exam_number = first(n),
    out = mean(out, na.rm = T))

df <- bind_rows(df,df2)

df$out[is.nan(df$out)] <- NA
df %<>% dplyr::rename("{{test}}" := out)

rm(df2)

return(df)

}




