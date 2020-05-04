extract_phone_number = function(string){
  ### input strings like "Cell#: 555-555-5555"
  
  x = gsub("[^0-9]", "", string)

  if(!is.na(x) & str_length(x) == 11 & str_sub(x, 1, 1) == "1"){
  	 x <- str_sub(x, 2, 11)
  }

  return(x)
}

quoalesce = function(df, cols, name){
  nm = sym(name)

  df %>% mutate(!!nm := coalesce(!!!syms(cols)))
}

remove_na_cols = function(df){
   copy = df

   names(copy)<- 1:length(copy)

   s = copy%>%summarise_all(~all(is.na(.)))

   s = map_lgl(s, identity)

   df[, !s]

}

get_date = function(x){
  y = tryCatch(as.POSIXct(x, tz = "EST", tryFormats = c(
                           "%B %d, %Y",
                           "%B %d %Y",
                           "%m/%d/%Y",
                           "%m-%d-%Y",
                           "%m %d %Y",
                         "%Y-%m-%d %H:%M:%OS",
                           "%Y/%m/%d %H:%M:%OS",
                           "%Y-%m-%d %H:%M",
                           "%Y/%m/%d %H:%M",
                           "%Y-%m-%d",
                           "%Y/%m/%d")), error = function(e){NA})

  if(!is.na(y) & year(y)<100){
    y <- y + years(1900)
  }

  if(!is.na(y) & year(y) > 2020) {
    y <- y - years(100)
  }

  y = as_date(y)
  return(y)
}