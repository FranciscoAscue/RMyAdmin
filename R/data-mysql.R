if( !is.element("RMySQL",rownames(installed.packages() ) ) ){
  install.packages("RMySQL")
}

library(RMySQL)

## data from MySQL
metadata_sql <- function(corrida, placa, mindate = NULL, maxdate = NULL){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  
  if( !is.null(mindate) & !is.null(maxdate)){
    query = paste0("SELECT NUMERACION_PLACA,NETLAB,OFICIO,CT,FECHA_TM,PROCEDENCIA,APELLIDO_NOMBRE,DNI_CE,VACUNADO,MOTIVO FROM `metadata` WHERE `FECHA_TM` BETWEEN '",mindate,"' AND '",maxdate,"';")
  }else{
    if(is.null(placa)){
      query = paste0("SELECT NUMERACION_PLACA,NETLAB,OFICIO,CT,FECHA_TM,PROCEDENCIA,APELLIDO_NOMBRE,DNI_CE,VACUNADO,MOTIVO FROM `metadata` WHERE `CORRIDA` = ",corrida," ;")
    }else{
      query = paste0("SELECT NUMERACION_PLACA,NETLAB,OFICIO,CT,FECHA_TM,PROCEDENCIA,APELLIDO_NOMBRE,DNI_CE,VACUNADO,MOTIVO FROM `metadata` WHERE `PLACA` = '",placa,"' AND `CORRIDA` = ",corrida," ;")
    }
  }
  dbSendQuery(con, "SET NAMES utf8mb4;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

metadataOrder <- function(corrida){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb') ### cambiar a seqcoviddb
  query = paste0("SELECT * FROM `metadata` WHERE `CORRIDA` = ",corrida," ORDER BY `metadata`.`PLACA` ASC;")
  dbSendQuery(con, "SET NAMES utf8mb4;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

metadataRechazado <- function(oficio){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query = paste0("SELECT NETLAB, OFICIO, PROCEDENCIA, CT, FECHA_TM FROM `metadata` WHERE `OFICIO` LIKE '",oficio,"' AND `CORRIDA` IS NULL ORDER BY `OFICIO` DESC;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}


metadataSeq <- function(mindate, maxdate){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query = paste0("SELECT NETLAB, PROCEDENCIA FROM `metadata` WHERE `FECHA_TM` BETWEEN '",mindate,"' AND '",maxdate,"' AND `CORRIDA` IS NOT NULL ORDER BY `metadata`.`CORRIDA` DESC;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

metadataS <- function(base_datos,corrida){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'SARS_GENOMES')
  query = paste0("SELECT * FROM ",base_datos," WHERE `CORRIDA` = ",corrida,";")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

metadaquery <- function(placa, corrida){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query = paste0("SELECT NUMERACION_PLACA, NETLAB, OFICIO, REGION, DNI_CE, CT, FECHA_TM, MARCA_PRIMERAS_DOSIS, HOSPITALIZACION  FROM `metadata`  WHERE `PLACA` = '",placa,"' AND `CORRIDA` = ",corrida," ORDER BY `metadata`.`FECHA_TM` DESC;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}


metadataSendquery <- function(netlab, oficio){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query = paste0("INSERT INTO `metadata` (`NETLAB`, `OFICIO`, `CT`, `CT2`, `FECHA_TM`, `REGION`, `PROCEDENCIA`, `PROVINCIA`, `DISTRITO`, `APELLIDO_NOMBRE`, `DNI_CE`, `EDAD`, `SEXO`, `VACUNADO`, `MARCA_PRIMERAS_DOSIS`, `1DOSIS`, `2DOSIS`, `MARCA _3DOSIS`, `3DOSIS`, `HOSPITALIZACION`, `MOTIVO`, `FALLECIDO`, `NUMERACION_PLACA`, `PLACA`, `CORRIDA`, `RECEPCIONADA`, `CODIGO`, `VERIFICADO`, `FECHA_INGRESO_BASE`) VALUES ('",
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
  query = paste0("SELECT NETLAB, OFICIO, CT, CT2, FECHA_TM, MOTIVO FROM `metadata`  WHERE `OFICIO` = '",oficio,"' ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

metadadelete <- function(oficio){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query = paste0("SELECT NETLAB, OFICIO, CT, CT2, FECHA_TM, MOTIVO, CODIGO  FROM `metadata`  WHERE `OFICIO` = '",oficio,"' ORDER BY `metadata`.`FECHA_INGRESO_BASE` DESC;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}


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

update_sql <- function(sql_id, ct, ct2, fecha_tm, check, motivo){
  
  if(is.null(ct) | is.na(ct)){
    ct <- 'NULL'
  }
  
  if(is.null(ct2) | is.na(ct2)){
    ct2 <- 'NULL'
  }
  
  if(isTRUE(check)){
    fecha_tm <- 'NULL'
  }
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query <- paste0("UPDATE `metadata` SET `CT` = '",
                  ct,"', `CT2` = '",ct2,"', `FECHA_TM` = '",
                  fecha_tm,"', `MOTIVO` = '",motivo,
                  "' WHERE `metadata`.`NETLAB` = \'",sql_id,"\'")
  query <- gsub("'NULL'", "NULL", query, fixed = TRUE)
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

#DELETE FROM `metadata2` WHERE `metadata2`.`NETLAB` = \'AAA2\'"
#data <- metadata_sql(corrida = 1028, placa = 'placa1')
