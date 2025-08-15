# Define a device class
_device <- setClass(
  "Device",
  slots = c(
    id = "character",
    type = "character",
    ip = "character",
    port = "integer",
    online = "logical"
  )
)

# Define a device list
devices <- list()

# Define a function to add a device
add_device <- function(id, type, ip, port) {
  new_device <- new("_device", id = id, type = type, ip = ip, port = port, online = FALSE)
  devices <<- c(devices, list(new_device))
}

# Define a function to remove a device
remove_device <- function(id) {
  devices <<- devices[sapply(devices, function(device) device@id != id)]
}

# Define a function to toggle device online status
toggle-online <- function(id) {
  device <- devices[[which(sapply(devices, function(device) device@id == id))]]
  if (is.null(device)) {
    return(NULL)
  }
  device@online <<- !device@online
}

# Define a function to integrate devices
integrate_devices <- function() {
  integrated_devices <- data.frame(
    id = sapply(devices, function(device) device@id),
    type = sapply(devices, function(device) device&type),
    ip = sapply(devices, function(device) device@ip),
    port = sapply(devices, function(device) device@port),
    online = sapply(devices, function(device) device@online)
  )
  return(integrated_devices)
}