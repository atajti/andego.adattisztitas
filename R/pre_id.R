
#ID-zó függvény ami a céginfó alapján andego kódot ad az egyes rekordoknak majd updateli az érintett mezőket az adatbázisban

pre_id<- function(napi_os){
#——— PACKAGES, FUNCTIONS —————————————————————————————————————————————————————
if(any(!require(data.table) |
       !require(RCurl) |
       !require(RMySQL) |
       !require(bit64) |
       !require(igraph) |
       !require(stringr))){
  stop("One of the pakages could not be loaded!")
}

source( "./scr/functions/chartimes.R" )
source( "./scr/functions/read.R"      )
source( "./scr/functions/sepfix.R"    )
source( "./scr/functions/ekezet.R"    )
source( "./scr/functions/db_os.R"     )
source( "./scr/functions/db_napi.R"   )
source( "./scr/functions/assign_id.R"   )
source( "./scr/functions/clean_addresses.R"   )
source( "./scr/functions/clean_dates.R"   )
source( "./scr/functions/clean_names2.R"   )
source( "./scr/functions/clean_phones.R"   )
source( "./scr/functions/clean_dates.R"   )
source( "./scr/functions/detect_firm.R"   )
source( "./scr/functions/clean_email.R"   )
source( "./scr/functions/clean_firms.R"   )
message("csomagok, függvények betöltése sikerült")
#——————————————————————————————————————————————————————————————————————————————



#——— DB CONNECTION ————————————————————————————————————————————————————————————
connect_kobe <- function(){
  kobe <<- dbConnect(RMySQL::MySQL(),
                   dbname="kobe3",
                   user="andego",
                   host="176.63.150.15",
                   port=3306,
                   password="BudapestAndego")
  dbGetQuery(kobe, "SET NAMES utf8")
}
connect_kobe()
on.exit(dbDisconnect(kobe))
#——————————————————————————————————————————————————————————————————————————————

if(!exists("kobe")){
  stop("kobe csatlakozás nem sikerült (pre_id)")
} else {
  message("köbe csatlakozás sikerült")
}

message(napi_os)
# ID
if((as.POSIXlt(Sys.Date())$wday %in% c(0,6)) |
   napi_os=="os"){
  CEGINFO <- dbConnect(RMySQL::MySQL(),
                       dbname="CEGINFO2",
                       user="andego",
                       host="127.0.0.1",
                       port=33060,
                       password="Eita6iro")
  dbGetQuery(CEGINFO, "SET NAMES utf8")
  on.exit(dbDisconnect(CEGINFO))

  if(!exists("CEGINFO")){
    stop("CEGINFO csatlakozás nem sikerült (pre_id)")
  } else {
    message("CEGINFO kapcsolat sikerült")
  }



  # CEGINFO összepároztatás

  ceginfo_alapok <- unique(rbindlist(list(
    dbGetQuery(CEGINFO,
      paste0("select andego_kod, attribute_id, normalized_value ",
             "from CEGINFO2.ATTRIBUTE_VALUES_TEXT ",
             "where active and ",
             "attribute_id in (12,15,22,23,24,34,187)")),
      dbGetQuery(CEGINFO,
      paste0("select andego_kod, attribute_id,",
               " value as normalized_value ",
             "from CEGINFO2.ATTRIBUTE_VALUES_DATE ",
             "where active and ",
             "attribute_id in (12,15,22,23,24,34,187)")))))
  message("CEGINFO letoltve")
  dbDisconnect(CEGINFO)
  ceginfo_alapok[attribute_id %in% c(12, 22, 34), attr := "DRNAME"]
  ceginfo_alapok[attribute_id %in% c(15, 24), attr := "DRADR1"]
  ceginfo_alapok[attribute_id == 23, attr := "NAMEM"]
  ceginfo_alapok[attribute_id == 187, attr := "GEBDAT"]

  # ceginfo_alapok[,attr := sapply(as.character(attribute_id),
  #   function(attr){
  #     switch(attr,
  #       "12"="DRNAME",
  #       "15"="DRADR1",
  #       "22"="DRNAME",
  #       "23"="NAMEM",
  #       "24"="DRADR1",
  #       "34"="DRNAME",
  #       "187"="GEBDAT")
  #     })]
  # széles tábla, az összes attribútumkombinációval
  ceginfo_wide <- ceginfo_alapok[attr==unique(ceginfo_alapok[, attr])[1],
                      .(andego_kod, normalized_value)]
  for(attrib in unique(ceginfo_alapok[, attr])[2:length(unique(ceginfo_alapok[, attr]))]){
    ceginfo_wide <- merge(ceginfo_wide,
               ceginfo_alapok[attr==attrib,
                              .(andego_kod, normalized_value)],
               by="andego_kod",
               all=TRUE)
  }
  setnames(ceginfo_wide,
    which(
      grepl("normalized",
             names(ceginfo_wide))),
    unique(ceginfo_alapok[, attr]))
gc()
  ceginfo_wide[, `:=`(DRNAME = clean_firms(clean_names2(DRNAME)),
                      DRADR1 = clean_addresses(DRADR1),
                      NAMEM = clean_names2(NAMEM) ,
                      GEBDAT = as.character(clean_dates(GEBDAT))
                      )]

 connect_kobe()
  dbWriteTable(kobe, "CEGINFO_ALAPOK",
    ceginfo_wide,
    row.names=FALSE,
    append=FALSE,
    overwrite=TRUE)

} else {
  connect_kobe()
  ceginfo_wide <- unique(setDT(dbGetQuery(kobe,
    "select * from CEGINFO_ALAPOK")))
  dbDisconnect(kobe)
}


  connect_kobe()
  resztvevok_all <- setDT(dbGetQuery(kobe,
    paste0("select * from RESZTVEVOK_ID",
      if(napi_os=="napi"){
        " where andego_kod is null"
      } else {
        ""
      })))
  dbDisconnect(kobe)


  resztvevok <- resztvevok_all[, `:=`(andego_kod = NULL,
                                      grp =NULL,
                                     rn=1:nrow(resztvevok_all))]

  if(napi_os!="napi"){
    resztvevok <- resztvevok[, andego_id := NA]
  }
  resztvevok <- unique(resztvevok)

# össze kell kapcsolni a céginfós adatot a resztvevokkel
setkeyv(resztvevok, c("DRNAME", "DRADR1", "GEBDAT", "NAMEM"))
 resztvevok_id_ceginfoval <- rbindlist(list(
   ceginfo_wide[!(is.na(andego_kod)|
                  is.na(DRNAME)|
                  is.na(DRADR1)|
                  is.na(GEBDAT)),
                .(andego_kod),
               keyby=.(DRNAME, DRADR1, GEBDAT)][
               resztvevok[,,keyby=.(DRNAME, DRADR1, GEBDAT)], nomatch=0],

   ceginfo_wide[!(is.na(andego_kod)|
                  is.na(DRNAME)|
                  is.na(DRADR1)|
                  is.na(NAMEM)),
                .(andego_kod),
               keyby=.(DRNAME, DRADR1, NAMEM)][
               resztvevok[,,keyby=.(DRNAME, DRADR1, NAMEM)], nomatch=0],

   ceginfo_wide[!(is.na(andego_kod)|
                  is.na(DRNAME)|
                  is.na(GEBDAT)|
                  is.na(NAMEM)),
                .(andego_kod),
               keyby=.(DRNAME, GEBDAT, NAMEM)][
               resztvevok[,,keyby=.(DRNAME, GEBDAT, NAMEM)], nomatch=0],
   resztvevok[!is.na(TAX)&(nchar(TAX) %in% c(8,11))][,
              andego_kod := substr(TAX, 0, 8)]
   ))
 setkey(resztvevok_id_ceginfoval, NULL)
 resztvevok_id_ceginfoval <- unique(resztvevok_id_ceginfoval)
## Csak azok a rekordok kellenek ahol a névben benne van
## valamelyik resztvevo_id név.
system.time(grepl(resztvevok_all$DRNAME[1], ceginfo_wide$DRNAME))
length(unique(resztvevok_all$DRNAME))
ceginfo_wide <- ceginfo_wide[DRNAME %in% resztvevok_all$DRNAME,]
#ceginfo_wide <- 

gc()


# resztvevok_id_ceginfoval <- rbindlist(list(
#   ceginfo_wide[evas(parse(text=paste0(
#     "!is.na(", c("DRNAME", "DRADR1", "GEBDAT"), .(DRNAME)
#   resztvevok, on=c("DRNAME", "DRADR1", "GEBDAT"),
#   .SD,
#   by=.EACHI][
#     resztvevok, on=c("DRNAME", "DRADR1", "NAMEM")][,
#       resztvevok, on=c("DRNAME", "NAMEM", "GEBDAT")]

resztvevok_id_ceginfoval<-  ceginfo_wide[!(is.na(andego_kod)|
                                             is.na(DRNAME)|
                                             is.na(GEBDAT)|
                                             is.na(NAMEM))
                                         , .(andego_kod_3=andego_kod,
                                             DRNAME,
                                             GEBDAT=as.character(GEBDAT),
                                             NAMEM)][ceginfo_wide[!(is.na(andego_kod)|
                                                                      is.na(DRNAME)|
                                                                      is.na(DRADR1)|
                                                                      is.na(NAMEM))
                                                                  , .(andego_kod_2=andego_kod,
                                                                      DRNAME,
                                                                      DRADR1,
                                                                      NAMEM)][ceginfo_wide[!(is.na(andego_kod)|
                                                                                               is.na(DRNAME)|
                                                                                               is.na(DRADR1)|
                                                                                               is.na(GEBDAT))
                                                                                           , .(andego_kod_1=andego_kod,
                                                                                               DRNAME,
                                                                                               DRADR1,
                                                                                               GEBDAT=as.character(GEBDAT))][
                                                                                                 resztvevok, on=c("DRNAME", "DRADR1", "GEBDAT")],
                                                                                                             on=c("DRNAME", "DRADR1", "NAMEM")],
                                                                                                             on=c("DRNAME", "GEBDAT", "NAMEM")]


message("triplamerge pipa")
# adószámból kiszedett adnego-kód:
resztvevok_id_ceginfoval <- rbindlist(list(
  resztvevok_id_ceginfoval,
  resztvevok[!is.na(TAX)&
              (nchar(TAX)%in%c(8,11))][,
             andego_kod_4 := substr(TAX, 0, 8)]),
  fill=TRUE)

# megvan az összes andego_kod
# leszűröm azokat, ahol van egyáltalán, 
# aztán meltelem, hogy mindegyik külön sorba kerüljön.


# az andego_kod.kat össze kell hozni egy oszlopba  

resztvevok_egy_andegokoddal <- unique(melt(
  resztvevok_id_ceginfoval,
  id.vars="rn", # names(resztvevok_id_ceginfoval)[
           #!grepl("andego_kod",
            #      names(resztvevok_id_ceginfoval))],
  measure.vars=names(resztvevok_id_ceginfoval)[
            grepl("andego_kod",
                  names(resztvevok_id_ceginfoval))],
  value.name="andego_kod")[,
   variable:=NULL][!is.na(andego_kod),])
unique(resztvevok_egy_andegokoddal)
message("resztvevok_egy_andegokoddal pipa")

setkey(resztvevok, NULL)

# andego_kod-ot visszamergelem a rn alapján
setkeyv(resztvevok, "rn")
setkeyv(resztvevok_egy_andegokoddal, "rn")

resztvevok_osszes_andegokoddal <- merge(
  resztvevok,
  resztvevok_egy_andegokoddal,
  all=TRUE)

if(napi_os=="napi"){
  resztvevok_all_andegokoddal <- rbindlist(list(
    resztvevok_osszes_andegokoddal,
    resztvevok_all),
  use.names=TRUE,
  fill=TRUE)
} else{
  resztvevok_all_andegokoddal <- resztvevok_osszes_andegokoddal
}

resztvevok_all_andegokoddal<-unique(resztvevok_all_andegokoddal)

resztvevok_all_andegokoddal<-resztvevok_all_andegokoddal[!duplicated(rn)]

resztvevok_uj_andegokoddal<-resztvevok_all_andegokoddal[!is.na(andego_kod)]

message("Resztvevok andegok ódos tábla kész updateljuk adatbázisban az érintett rekordokat")
connect_kobe()
for(i in 1:nrow(resztvevok_uj_andegokoddal)){
  if(nrow(resztvevok_uj_andegokoddal)!=0){
      query<-paste0("UPDATE RESZTVEVOK_ID SET andego_kod = '",resztvevok_uj_andegokoddal[i,andego_kod])
      if(!is.na(resztvevok_uj_andegokoddal[i,DRNAME])){
        query<-paste0(query,"' WHERE DRNAME = '",resztvevok_uj_andegokoddal[i,DRNAME],"'")
      }else{
        query<-paste0(query," WHERE DRNAME IS NULL")
      }
      #GEBDAT mező pastelése a queryhez
      if(!is.na(resztvevok_uj_andegokoddal[i,GEBDAT])){
        query<-paste0(query," AND GEBDAT = '",resztvevok_uj_andegokoddal[i,GEBDAT],"'")
      }else{
        query<-paste0(query," AND GEBDAT IS NULL")
      }
      #NAMEM mező pastelése a queryhez
      if(!is.na(resztvevok_uj_andegokoddal[i,NAMEM])){
        query<-paste0(query," AND NAMEM = '",resztvevok_uj_andegokoddal[i,NAMEM],"'")
      }else{
        query<-paste0(query," AND NAMEM IS NULL")
      }
      #DRADR mező pastelése a queryhez
      if(!is.na(resztvevok_uj_andegokoddal[i,DRADR1])){
        query<-paste0(query," AND DRADR1 = '",resztvevok_uj_andegokoddal[i,DRADR1],"'")
      }else{
        query<-paste0(query," AND DRADR1 IS NULL")
      }
      rs<-dbSendQuery(kobe, query)
      dbClearResult(rs)
    }
}
dbDisconnect(kobe);
}