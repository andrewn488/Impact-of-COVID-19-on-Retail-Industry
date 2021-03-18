class_conversion <- function(df, x, y){
  'str_prefix' = x
  'new_class' = y
  df %>% mutate(across(starts_with(str_prefix), new_class))
}