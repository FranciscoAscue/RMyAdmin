if( !is.element("RMySQL",rownames(installed.packages() ) ) ){
  install.packages("RMySQL")
}

library(RMySQL)

## data from MySQL
metadata_sql <- function(oficio = NULL){
  
  con <- dbConnect(MySQL(),
                   user = 'ingreso',
                   password = '123ingreso321',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  
  
  query = paste0("SELECT NUMERACION_PLACA,NETLAB,OFICIO,DNI_CE,CORRIDA,PLACA FROM `metadata` WHERE `OFICIO` LIKE '",
                 oficio,"' AND `CORRIDA` IS NULL AND `DNI_CE` IS NOT NULL ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

metadataOrder <- function(corrida, placa, reasig = FALSE){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb') ### cambiar a seqcoviddb
  if(isTRUE(reasig)){
    query = paste0("SELECT NUMERACION_PLACA,NETLAB,OFICIO,DNI_CE,CORRIDA,PLACA FROM `metadata` WHERE `CORRIDA` = ",corrida," AND `PLACA` = '",placa,"' ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")  
  }else{
    query = paste0("SELECT NUMERACION_PLACA FROM `metadata` WHERE `CORRIDA` = ",corrida," AND `PLACA` = '",placa,"' ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")
  }
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

# metadataRechazado <- function(oficio){
#   
#   con <- dbConnect(MySQL(),
#                    user = 'root',
#                    password = 'maq12345',
#                    host = 'localhost',
#                    dbname = 'seqcoviddb')
#   query = paste0("SELECT NETLAB, OFICIO, PROCEDENCIA, CT, FECHA_TM FROM `metadata` WHERE `OFICIO` LIKE '",
#                  oficio,"' AND `CORRIDA` IS NULL ORDER BY `OFICIO` DESC;")
#   rs = dbSendQuery(con, query);
#   df = fetch(rs, -1);
#   dbDisconnect(con)
#   return(df)
# }
# 
# 
# metadataSeq <- function(mindate, maxdate){
#   
#   con <- dbConnect(MySQL(),
#                    user = 'root',
#                    password = 'maq12345',
#                    host = 'localhost',
#                    dbname = 'seqcoviddb')
#   query = paste0("SELECT NETLAB, PROCEDENCIA FROM `metadata` WHERE `FECHA_TM` BETWEEN '",
#                  mindate,"' AND '",maxdate,"' AND `CORRIDA` IS NOT NULL ORDER BY `metadata`.`CORRIDA` DESC;")
#   rs = dbSendQuery(con, query);
#   df = fetch(rs, -1);
#   dbDisconnect(con)
#   return(df)
# }
# 
# metadataS <- function(base_datos,corrida){
#   
#   con <- dbConnect(MySQL(),
#                    user = 'root',
#                    password = 'maq12345',
#                    host = 'localhost',
#                    dbname = 'SARS_GENOMES')
#   query = paste0("SELECT * FROM ",base_datos," WHERE `CORRIDA` = ",corrida,";")
#   rs = dbSendQuery(con, query);
#   df = fetch(rs, -1);
#   dbDisconnect(con)
#   return(df)
# }
# 
# metadaquery <- function(placa, corrida){
#   
#   con <- dbConnect(MySQL(),
#                    user = 'root',
#                    password = 'maq12345',
#                    host = 'localhost',
#                    dbname = 'seqcoviddb')
#   query = paste0("SELECT NUMERACION_PLACA, NETLAB, OFICIO, REGION, DNI_CE, CT, FECHA_TM, MARCA_PRIMERAS_DOSIS, HOSPITALIZACION  FROM `metadata`  WHERE `PLACA` = '",
#                  placa,"' AND `CORRIDA` = ",corrida," ORDER BY `metadata`.`FECHA_TM` DESC;")
#   rs = dbSendQuery(con, query);
#   df = fetch(rs, -1);
#   dbDisconnect(con)
#   return(df)
# }


metadataSendquery <- function(netlab, oficio){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query = paste0("INSERT INTO `metadata` (`NETLAB`, `OFICIO`, `CT`, `CT2`, `FECHA_TM`, `REGION`, `PROCEDENCIA`, `PROVINCIA`, `DISTRITO`, `APELLIDO_NOMBRE`, `DNI_CE`, `EDAD`, `SEXO`, `VACUNADO`, `MARCA_PRIMERAS_DOSIS`, `1DOSIS`, `2DOSIS`, `MARCA_3DOSIS`, `3DOSIS`, `HOSPITALIZACION`, `MOTIVO`, `FALLECIDO`, `NUMERACION_PLACA`, `PLACA`, `CORRIDA`, `RECEPCIONADA`, `CODIGO`, `VERIFICADO`, `FECHA_INGRESO_BASE`) VALUES ('",
                 netlab,"','",oficio,"', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, CURRENT_TIMESTAMP);")
  rs = dbSendQuery(con, query);
  #df = fetch(rs, -1);
  dbDisconnect(con)
  #return(df)
}


metadaupdate <- function(oficio){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query = paste0("SELECT NETLAB, OFICIO, CT, CT2, FECHA_TM, MOTIVO, DNI_CE FROM `metadata`  WHERE `OFICIO` = '",oficio,"' ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

# metadadelete <- function(oficio){
#   
#   con <- dbConnect(MySQL(),
#                    user = 'root',
#                    password = 'maq12345',
#                    host = 'localhost',
#                    dbname = 'seqcoviddb')
#   query = paste0("SELECT NETLAB, OFICIO, CT, CT2, FECHA_TM, MOTIVO, CODIGO  FROM `metadata`  WHERE `OFICIO` = '",
#                  oficio,"' ORDER BY `metadata`.`FECHA_INGRESO_BASE` DESC;")
#   rs = dbSendQuery(con, query);
#   df = fetch(rs, -1);
#   dbDisconnect(con)
#   return(df)
# }


delete_sql <- function(query){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  #query = paste0("SELECT NETLAB, OFICIO, CT, CT2, FECHA_TM, MOTIVO, CODIGO  FROM `metadata`  WHERE `OFICIO` = '",oficio,"' ORDER BY `metadata`.`FECHA_TM` DESC;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

library(utils)

update_sql <- function(sql_id, ct, ct2, fecha_tm, motivo, dni){
  
  if(is.null(ct) | is.na(ct)){
    ct <- 'NULL'
  }
  
  if(is.null(ct2) | is.na(ct2)){
    ct2 <- 'NULL'
  }
  
  if(length(fecha_tm) == 0){
    fecha_tm <- 'NULL'
  }
  
  if(is.null(dni) | is.na(dni)){
    dni <- 'NULL'
  }
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query <- paste0("UPDATE `metadata` SET `CT` = '",
                  ct,"', `CT2` = '",ct2,"', `FECHA_TM` = '",
                  fecha_tm,"', `MOTIVO` = '",motivo,"', `DNI_CE` = '",
                  dni,"' WHERE `metadata`.`NETLAB` = \'",sql_id,"\'")
  query <- gsub("'NULL'", "NULL", query, fixed = TRUE)
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}


metadata_setnumber <- function(netlab, number){
  con <- dbConnect(MySQL(),
                   user = 'ingreso',
                   password = '123ingreso321',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query <- paste0("UPDATE `metadata` SET `NUMERACION_PLACA` = '",
                  number,"' WHERE `metadata`.`NETLAB` = \'",netlab,"\'")
  query <- gsub("'NULL'", "NULL", query, fixed = TRUE)
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

enumerar <- function(netlab_list, max){
  n = max + 1
  for( i in netlab_list){
    metadata_setnumber(i, n)
    n = n + 1
  }
}

metadataAsignar <- function(Corrida, Placa, Oficio){
  
  con <- dbConnect(MySQL(),
                   user = 'ingreso',
                   password = '123ingreso321',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query <- paste0("UPDATE `metadata` SET `CORRIDA` = '",
                  Corrida,"', `PLACA` = '",Placa,"' WHERE `OFICIO` = '",Oficio,
                  "' AND `DNI_CE` IS NOT NULL ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")
  query <- gsub("'NULL'", "NULL", query, fixed = TRUE)
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}


#DELETE FROM `metadata2` WHERE `metadata2`.`NETLAB` = \'AAA2\'"
#data <- metadata_sql(corrida = 1028, placa = 'placa1')
