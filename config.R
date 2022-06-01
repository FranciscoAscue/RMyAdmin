### local host and port
options( shiny.host = '192.168.128.110' )
options( shiny.port = 8585 )
#options( shiny.maxRequestSize = 100*1024^2 ) 

user_base <- tibble::tibble(
  user = c("admin","veronica"),
  password = sapply(c("ins2022ins", "veronica123"), sodium::password_store),
  permissions = c("admin", "admin"),
  name = c("ADMINISTRADOR", "VERONICA")
)