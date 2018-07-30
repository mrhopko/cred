library(cred)
library(testthat)

# create a key
key <- cred::make_key("C:/Data/test_key")

# setup credentials file
cred_file <- "C:/Data/test_do_not_delete_or_move"
cred::setup_cred_file(cred_file, key)

# add some credentials
cred::add_cred("myserver", "user", "password", key)

cred::read_cred(key)

cred::get_server_cred("myserver", key)


