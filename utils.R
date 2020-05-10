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

  if(!is.na(y)){
    res = year(y) %% 100
    cent = floor(year(y)/100)

    ### Modify birth date so that it's between 1920 and 2020

    centuries_to_add = (19 - cent) + as.numeric(res<20)
    y <- y + years(100*centuries_to_add)
  }
  y = as_date(y)
  return(y)
}