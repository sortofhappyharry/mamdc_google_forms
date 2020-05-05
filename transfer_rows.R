transfer_rows = function(ssid, intake_sheet, sheet_in_use, completed_tasks){
  unfettered_intake_responses = read_sheet(ssid, sheet = intake_sheet, .name_repair = "minimal")
  orig_names = names(unfettered_intake_responses)

  intake_responses = read_sheet(ssid, sheet = intake_sheet)
  intake_responses= intake_responses %>% mutate_all(as.character)
  repaired_names = names(intake_responses)

  responses_in_use = read_sheet(ssid, sheet = sheet_in_use) %>% mutate_all(as.character)
  completed_responses = read_sheet(ssid, sheet = completed_tasks) %>% mutate_all(as.character)

  all_transferred_responses = bind_rows(responses_in_use, completed_responses)

  phone_cols = names(all_transferred_responses)[grep(pattern = "Please list your phone number", names(all_transferred_responses))]
  
  all_transferred_responses = all_transferred_responses %>% mutate_at(phone_cols, as.character)
                               
  all_transferred_responses= quoalesce(all_transferred_responses, phone_cols, "phone_number")
  
  all_transferred_responses = all_transferred_responses %>% 
                                     mutate(phone_number = as.character(phone_number),
  	                                        phone_number = extract_phone_number(phone_number))

  phone_cols = names(intake_responses)[grep(pattern = "Please list your phone number", names(intake_responses))]
  
  intake_responses = intake_responses %>% mutate_at(phone_cols, as.character)

  intake_responses = quoalesce(intake_responses, phone_cols, "phone_number")
  intake_responses = intake_responses %>% mutate(phone_number = as.character(phone_number),
  	                                             phone_number = extract_phone_number(phone_number))

  new_responses = intake_responses %>% anti_join(all_transferred_responses, by = c("Timestamp", "phone_number")) %>%
                                       select(repaired_names)

  new_responses = new_responses %>% mutate_if(is.list, as.character)

  if(nrow(new_responses)>0){

  	 flog.info("Adding %s new rows to active sheet", nrow(new_responses))

     x = get_client_data_v2(copy_ssid, data = new_responses)
     y = update_client_db(copy_ssid, data = x, write = T)

     a = add_client_data(y, new_responses)

     names(a)[1:length(orig_names)] = orig_names

     sheet_append(ss = ssid, data = a, sheet = sheet_in_use)
     } else {
     	flog.info("No new data, returning from function")
     }

  return()
}