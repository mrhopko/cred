#' add credentials
#'
#' @param server name from vmodbc pacakge
#' @param un User name of credential
#' @param pw password of credential
#' @param key encryption key
#' @return TRUE on completion
#' @export
add_cred <- function(server, un, pw, key) {

  cred <- read_cred(key)
  cred[[paste0(server, "_un")]] <- un
  cred[[paste0(server, "_pw")]] <- pw
  save_cred(cred, key)
  TRUE

}


#' read credentials from pacakge data
#'
#' @param key encryption key
#' @return credentials
#' @export
read_cred <- function(key) {
  cred_location <- get_cred_location()
  dat_enc <- readRDS(cred_location)
  dat <- readBin(dat_enc,"raw",n=1000)
  aes <- digest::AES(key,mode="ECB")
  raw <- aes$decrypt(dat, raw=TRUE)
  txt <- rawToChar(raw[raw>0])
  read.csv(text=txt, stringsAsFactors = FALSE)
}


#' encrypt and save credentials using key
#'
#' @param key encryption key
#' @param cred credentials data.frame
#' @return TRUE on completion
#' @export
save_cred <- function(cred, key) {
  zz <- textConnection("out","w")
  write.csv(cred, zz, row.names=F)
  close(zz)
  out <- paste(out,collapse="\n")
  raw <- charToRaw(out)
  raw <- c(raw,as.raw(rep(0,16-length(raw)%%16)))
  aes <- digest::AES(key,mode="ECB")
  dat_enc <- aes$encrypt(raw)
  cred_location <- get_cred_location()
  saveRDS(dat_enc, cred_location)
  TRUE
}


#' make an encryption key
#'
#' @param output_file file in which to save key
#' @return encryption key
#' @export
make_key <- function(output_file) {
  key <- as.raw(sample(1:16, 16))
  saveRDS(key, output_file)
  warning(paste0("Key saved to ", output_file))
  key
}


#' read a key file
#'
#' @param key_file file name where key is stored
#' @return key
#' @export
read_key <- function(key_file) {
  readRDS(key_file)
}


#' delete all credentials
#'
#' @return TRUE on completion
#' @export
setup_empty_cred <- function(key) {
  empty_cred <- data.frame(cred = "cred")
  save_cred(empty_cred, key)
  TRUE
}


#' setup empty cred file at file_name
#'
#' @param file_name to save encrypted credentials
#' @param key encryption key to use
#' @return file_name
#' @export
setup_cred_file <- function(file_name, key) {

  update_cred_location(file_name)
  setup_empty_cred(key)

}


#' stores the file_name internally in the package
#'
#' @param file_name cred file name
#' @return file_name
#' @export
update_cred_location <- function(file_name) {
  cred_location <- internal_file()
  saveRDS(file_name, cred_location)
  print("file name saved")
  file_name
}

#' gets the file name containing credentials location
#'
#' @return credential file location
#' @export
get_cred_location <- function() {
  cred_location <- internal_file()
  readRDS(cred_location)
}

internal_file <- function() {
  system.file("data", "data_enc.RDS", package = "cred")
}

#' get credentials for a specific server
#'
#' @param server_name name of the server in the credentials file
#' @param key encryption key
#' @return list with un, pw
#' @export
get_server_cred <- function(server_name, key) {
  cred <- read_cred(key)
  list(un = cred[[paste0(server_name, "_un")]],
       pw = cred[[paste0(server_name, "_pw")]])
}

# create initialisation
# saveRDS(data.frame(cred = "cred"), "inst/data/data_enc.RDS")

