match_client = function(client_db, name, dob, phone_number, email){
    if(!is.na(name) & name == "NULL"){
        name = NA
    }

    if(!is.na(email) &  email == "NULL"){
        email = NA
    }

    if(!is.na(phone_number) & phone_number == "NULL"){
        phone_number = NA
    }

    dob = get_date(dob)

    phone_number = extract_phone_number(phone_number) %>% as.numeric()

	phone_number_matches = as.numeric(client_db$phone_number) == phone_number

	dob_matches = client_db$dob == dob

    email_matches = tolower(client_db$email) == tolower(email)

    dob_close_matches = (year(client_db$dob) == year(dob)) &
                        (abs(month(client_db$dob) - month(client_db$dob))<=1) &
                        (abs(day(client_db$dob) - day(client_db$dob))<=1)

	name_matches = tolower(client_db$name) == tolower(name)

	phone_number_matches = coalesce(phone_number_matches, FALSE)
	dob_matches = coalesce(dob_matches, FALSE)
	name_matches = coalesce(name_matches, FALSE)
    dob_close_matches = coalesce(dob_close_matches, FALSE)
    email_matches = coalesce(email_matches, FALSE)

	if(any(phone_number_matches & dob_matches & name_matches & email_matches)){
	
		x = client_db[phone_number_matches & dob_matches & name_matches & email_matches,]
	
	} else if(any(phone_number_matches & dob_matches & email_matches)){
    
        x = client_db[phone_number_matches & dob_matches & email_matches,]
    
    } else if(any(phone_number_matches & name_matches & email_matches)){
        
        x = client_db[phone_number_matches & name_matches & email_matches,]

    } else if(any(dob_matches & name_matches & email_matches)){

	    x = client_db[name_matches & dob_matches & email_matches,]

    } else if(any(phone_number_matches & dob_close_matches & email_matches)){

    	x = client_db[phone_number_matches & dob_close_matches & email_matches,]

    }   else if(any(phone_number_matches & dob_matches)){

        x = client_db[phone_number_matches & dob_matches, ]

    }  else if(any(phone_number_matches & email_matches)){

        x = client_db[phone_number_matches & email_matches, ]

    } else if(any(name_matches & phone_number_matches)){

        x = client_db[name_matches & phone_number_matches,]

    } else if(any(dob_matches & email_matches)){

    	x = client_db[dob_matches & email_matches,]

    } else if(any(name_matches & email_matches)){

    	x = client_db[name_matches & email_matches,]

    } else if(any(dob_matches & name_matches)){

        x = client_db[dob_matches & name_matches,]

    } else if(any(email_matches)){
    
        x = client_db[email_matches,]
    
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

    z = x %>% group_by() %>%
          #    arrange(name, phone_number, dob, email)%>%
              summarise_all(~{ x = na.omit(.)
              	               if(is.character(x)){
              	               	x = x[x != "NULL"]
                                   x = x[x != "X"]
              	               }
                                  first(x)
              	              })

  return(z)
}

add_client_data = function(client_db, new_responses){
    ncd = new_responses

    name_cols = names(ncd)[grep(pattern = "Please state your name", names(ncd))]
    dob_cols = names(ncd)[grep(pattern = "What's your birth date", names(ncd))]
    phone_cols = names(ncd)[grep(pattern = "Please list your phone number", names(ncd))]
    email_cols = names(ncd)[grep(pattern = "list your email address", names(ncd))]
    chv_cols = names(ncd)[grep(pattern = "Do you live in Columbia Heights Village?", names(ncd))]

    ncd = ncd %>% mutate_at(c(name_cols, dob_cols, phone_cols, email_cols, chv_cols), as.character)

    ncd = quoalesce(ncd, name_cols, "name")
    ncd = quoalesce(ncd, dob_cols, "dob")
    ncd = quoalesce(ncd, phone_cols, "phone_number")
    ncd = quoalesce(ncd, email_cols, "email")
    ncd = quoalesce(ncd, chv_cols, "CHV")

    ncd = ncd %>% select(name, phone_number, dob, email, 
	                                         address = `What's your address?`,
	                                         CHV,
	                                         adults = `How many adults are on the lease for your unit (including you)?`,
	                                         children = `How many children live in your unit?`,
	                                         nhd = `What neighborhood do you live in?  (can be broad, we're just trying to gauge geographic distance and how to assign a volunteer)`,
	                                         grocery_assistance = `Do you need assistance getting groceries?`,
	                                         grocery_reason= `I need assistance because...`,
	                                         snap_benefits = `If you need assistance for groceries because you are unable to physically go yourself, do you have SNAP benefits or other money that can be used to pay for groceries?`)

    ncd = ncd %>% mutate(phone_number = extract_phone_number(phone_number))

    ncd = ncd %>% rowwise() %>%
                                mutate(dob = get_date(dob)) %>%
                                ungroup()


    list = ncd %>% as.list() %>% `[`(.,c("name", "dob", "phone_number", "email"))

    matches = pmap_dfr(list, function(name, dob, phone_number, email){
                                match_client(client_db, name, dob, phone_number, email)
    	                     })

    matches = matches %>% select(name, dob, phone_number, email, address, 
    	                         CHV, nhd, adults, children, snap_benefits)

    return(bind_cols(new_responses, matches))
}
