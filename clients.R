get_client_data_v1 = function(ssid, sheet){
  	old_form_data = read_sheet(ssid, sheet = sheet)

    old_client_data = old_form_data %>% select(name = `Full Name`,
                                       phone_number =  `Phone Number`,
                                       email = `Email Address`, 
	                                   nhd = `What neighborhood or building do you live in? (i.e. Columbia Heights, Park View, Columbia Heights Village)`,
                                       address = `Address (optional, but would be helpful)`,
                                       orig_dob = `What is your date of birth`) %>%
                                     mutate(email = as.character(email),
                                     	    phone_number = as.character(phone_number),
                                     	    phone_number = extract_phone_number(phone_number),
                                     	    nhd = as.character(nhd),
                                     	    orig_dob = as.character(orig_dob))%>%
                                     as_tibble()

    old_client_data = old_client_data %>% rowwise() %>%
                                          mutate(dob = get_date(orig_dob)) %>%
                                          ungroup()

    old_client_data = old_client_data %>% mutate_if(is.character, ~ifelse(.x == "NULL", NA, .x))

    list = old_client_data %>% as.list() %>% `[`(.,c("name", "dob", "phone_number", "email"))

    clients = pmap_dfr(list, function(name, dob, phone_number, email){
    	                y= match_client(old_client_data, name, dob, phone_number, email)
    	                y = mutate_at(y, c("phone_number"), as.numeric)
    	              })

    unique_clients = clients %>% group_by(dob, phone_number) %>%
                          filter(row_number() == 1) %>%
                          ungroup() %>%
                          mutate(client_id = paste0("X", row_number()),
                          	     phone_number = as.numeric(phone_number))

    possible_duplicates = unique_clients %>% group_by(phone_number) %>% 
                                             filter(n()>1) %>%
                                             arrange(phone_number)

    return(unique_clients)
}

initialize_old_client_db = function(ssid, sheet){
  ocd = get_client_data_v1(ssid, sheet)

  write_csv(ocd, path = OLD_CLIENT_FILE)
  flog.info("Overwrote %s rows to file %s", nrow(ocd), OLD_CLIENT_FILE)
}

get_client_data_v2 = function(ssid, sheet = NULL, data = NULL){

	if(is.null(data)){
      new_form_data = read_sheet(ssid, sheet = sheet)
    } else {
    	new_form_data = data
    }

    ncd = new_form_data %>% filter(`Is this the first time you are filling out this form with us?` == "Yes")

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

    ncd_pretty = ncd %>% select(name, phone_number, dob, email, 
	                                         address = `What's your address?`,
	                                         CHV,
	                                         adults = `How many adults are on the lease for your unit (including you)?`,
	                                         children = `How many children live in your unit?`,
	                                         nhd = `What neighborhood do you live in?  (can be broad, we're just trying to gauge geographic distance and how to assign a volunteer)`,
	                                         grocery_assistance = `Do you need assistance getting groceries?`,
	                                         grocery_reason= `I need assistance because...`,
	                                         snap_benefits = `If you need assistance for groceries because you are unable to physically go yourself, do you have SNAP benefits or other money that can be used to pay for groceries?`)

    ncd_pretty = ncd_pretty %>% mutate(phone_number = extract_phone_number(phone_number))

    ncd_pretty = ncd_pretty %>% rowwise() %>%
                                mutate(dob = get_date(dob)) %>%
                                ungroup()

    ncd_pretty = ncd_pretty %>% group_by(name, dob, phone_number) %>% 
                                filter(row_number()==1)

    ncd_pretty = ncd_pretty %>% mutate_at(c("nhd", "grocery_assistance",
                                             "grocery_reason", "snap_benefits"),
                                              as.character) %>%
                                mutate_at(c("phone_number", "adults", "children"),
                                          as.numeric)
    	
    return(ncd_pretty)                               
}


update_client_db = function(ssid, new_sheet_names = NULL, data = NULL, write = FALSE){
    
    # Careful, this will rebuild client DB! Please take care before using.

	# stop("Please be careful using this function. Comment this error out if you really want to.")

	# Leave this in so that this function doesn't get called except when
	         # really intended
  flog.info("Updating client db")
	ocd = read_csv(CLIENT_FILE)

	flog.info("Made old client db")
    
  if(is.null(data)){
    ncd = tibble()
    for(nm in new_sheet_names){
      flog.info("Making new client db for sheet %s", nm)
      new_data = get_client_data_v2(ssid, nm)
      ncd = bind_rows(ncd, new_data)
    }
  } else {
    ncd = data
  }

  ncd = unique(ncd)

  # for new data, want to check if they're already in old database

  list = ncd %>% as.list() %>% `[`(.,c("name", "dob", "phone_number", "email"))

  matches = pmap_dfr(list, function(name, dob, phone_number, email){
  	                y= match_client(ocd, name, dob, phone_number, email)
  	                y = mutate(y, phone_number =  as.numeric(phone_number))
  	              })

    ## if they're in old database, we still want to add on newer information if available to their DB entry

  matched = bind_cols(ncd, matches %>% magrittr::set_colnames(paste0(names(.), "_x"))) %>%
            filter(!is.na(client_id_x)) %>%
            mutate(name = coalesce(name, name_x),
            	     phone_number = coalesce(phone_number, phone_number_x),
            	     dob = coalesce(dob, dob_x),
            	     email = coalesce(email, email_x),
            	     address = coalesce(address, address_x),
            	     nhd = coalesce(nhd, nhd_x)) %>%
            select(all_of(names(ncd)), client_id = client_id_x)

  matched = matched %>% group_by(client_id) %>% 
                        filter(row_number()==1) %>%
                        ungroup()

    ##### Now find unique entries out of unmatched people and give them ids

    unmatched = matches %>% filter(is.na(client_id)) %>%
                            group_by(phone_number, dob) %>%
                            summarise_all(~{ x = na.omit(.)
	              	               if(is.character(x)){
	              	               	x = x[x != "NULL"]
                                    x = x[x != "X"]
	              	               }
                                   first(x)
	              	              })

    list = unmatched %>% as.list() %>% `[`(.,c("name", "dob", "phone_number", "email"))

    new_clients = pmap_dfr(list, function(name, dob, phone_number, email){
    	                y= match_client(unmatched, name, dob, phone_number, email)
    	                y = mutate(y, phone_number =  as.numeric(phone_number))
    	              }) %>%
                  unique()

    id_ticker = ocd$client_id[grepl("Y", ocd$client_id)] %>%
                              gsub("Y", "", .) %>% 
                              max(., na.rm = T)

    id_ticker = coalesce(as.numeric(id_ticker), as.numeric(0))

    new_clients = new_clients %>% mutate(client_id = paste0("Y", row_number() + id_ticker))

    non_dupe_ocd = ocd %>% anti_join(matched, by = "client_id")

    new_rows = bind_rows(new_clients, matched)

    flog.info("%s new clients, %s return clients", nrow(new_clients), nrow(matched))

    full_db =  bind_rows(new_rows, as.data.frame(non_dupe_ocd))

    assertion = nrow(full_db) == length(unique(full_db$client_id))

    assertthat::assert_that(assertion, 
    	                    msg = "Clients appearing multiple times")

    if(write == TRUE){
    	 write_csv(full_db, path = CLIENT_FILE)
    }

    return(full_db)
}