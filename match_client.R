match_client = function(client_db, name, dob, phone_number){
    dob = get_date(dob)

    phone_number = extract_phone_number(phone_number) %>% as.numeric()

	phone_number_matches = as.numeric(client_db$phone_number) == phone_number

	dob_matches = client_db$dob == dob

	name_matches = tolower(client_db$name) == tolower(name)

	phone_number_matches = coalesce(phone_number_matches, FALSE)
	dob_matches = coalesce(dob_matches, FALSE)
	name_matches = coalesce(name_matches, FALSE)

	if(any(phone_number_matches & dob_matches & name_matches)){
	
		x = client_db[phone_number_matches & dob_matches & name_matches,]
	
	} else if(any(phone_number_matches & dob_matches)){
    
        x = client_db[phone_number_matches & dob_matches,]
    
    } else if(any(phone_number_matches & name_matches)){
        
        x = client_db[phone_number_matches & name_matches,]

    } else if(any(dob_matches & name_matches)){

	    x = client_db[name_matches & dob_matches,]

    } else if(any(phone_number_matches)){

    	x = client_db[phone_number_matches,]

    } else if(any(dob_matches)){

    	x = client_db[dob_matches,]

    } else if(any(name_matches)){

    	x = client_db[name_matches,]

    } else {
        name = coalesce(name, "None")

    	x = tibble(name = name, dob = dob, phone_number = phone_number)
    
    }

	    x = x %>% group_by() %>% 
	              summarise_all(~{ x = na.omit(.)
	              	               if(is.character(x)){
	              	               	x = x[x != "NULL"]
                                    x = x[x != "X"]
	              	               }
                                   first(x)
	              	              })

  return(x)
}

add_client_data = function(){
	
}
