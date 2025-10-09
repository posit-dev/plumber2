pa <- Plumber2$new(
  host = "0.0.0.0",
  port = 8001
)

pa$add_api_doc(list(info = list(title = "Constructor API", version = "1.0.0")))

pa
