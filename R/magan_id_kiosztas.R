# Függvény ami résztvevőket vár, merging attribútumokat és egy id oszlopot.
# A résztvevőknek id-t oszt úgy hogy összermergeli őket az attribútumok alapján.
# Ezekkel az id-kkal tér vissza a függvény

magan_id_kiosztas_betoltes<- function(resztvevok_all_andegokoddal,merging_attrs,IDcol){

orig_magan <- resztvevok_all_andegokoddal[CEG==0]

orig_magan[, grp := paste0("grp", .GRP),
  by=c(unique(unlist(merging_attrs)), IDcol)]

magan <- unique(orig_magan[,
  c(unique(unlist(merging_attrs)), IDcol, "grp"),
  with=FALSE])

magan<-magan[!duplicated(andego_id)]


magan[, rn := 1:nrow(magan)]


magan<-rbindlist(lapply(merging_attrs,
                 function(merging_list){
                   print(merging_list)
                   magan[eval(parse(text=paste0(merging_list," %in% magan[,.(db=.N), by=",merging_list,"][db>1, ",merging_list,"]", collapse="&")))]
                 }))[!duplicated(rn)]



# éllista:

  edgelist <- rbindlist(lapply(merging_attrs,
    # megadott változókombók alapján létrehozom az éllistát
    function(v_list){
      print(v_list)
      merge(magan[eval(parse(text=paste0("!is.na(", v_list, ")", collapse="&")))
              ,c(IDcol, v_list, "grp"), with=FALSE],
            magan[eval(parse(text=paste0("!is.na(", v_list, ")", collapse="&")))
              ,c(IDcol, v_list, "grp"), with=FALSE],
            by=v_list,
            all=TRUE,
            allow.cartesian=TRUE,
            suffixes=c("_1", "_2"))[, (v_list) := NULL][grp_1 < grp_2]
    }))
  
# éllistából a hurkokat és a fordítva is szereplő párokaat kiszedem
 edgelist <- edgelist[!(grp_1==grp_2)]
 edgelist <- unique(rbindlist(list(
                                edgelist[grp_1<grp_2,],
                                edgelist[grp_1>grp_2,
                                .(grp_1=grp_2,
                                  grp_2=grp_1,
                                  andego_id_1=andego_id_2,
                                  andego_id_2=andego_id_1)]),
                              use.names=TRUE))
  setcolorder(edgelist, c(which(names(edgelist)=="grp_1"),
                          which(names(edgelist)=="grp_2"),
                          which(names(edgelist)==paste0(IDcol, "_1")),
                          which(names(edgelist)==paste0(IDcol, "_2"))))

  # gráf összerakása
  entity_graph <- graph_from_data_frame(unique(edgelist),
                                        directed=FALSE,
                                        vertices=magan[grp %in%
                                         union(edgelist$grp_1, edgelist$grp_2),
                                         .(grp, grp=grp)])
  entity_graph_comps <- components(entity_graph)$membership

  entity_graph_comps_dt <-rbindlist(list(
     data.table(grp=names(entity_graph_comps),
                comp=unname(entity_graph_comps)),
       data.table(grp=magan$grp[!magan$grp %in% names(entity_graph_comps)],
                comp=max(entity_graph_comps)+
                     seq(sum(!magan$grp %in% names(entity_graph_comps))))
     ))

  # grp-hez komponens és eredeti IDcol érték
  comps <- merge(entity_graph_comps_dt,
    magan[, c("grp", IDcol), with=FALSE],
    all=TRUE,
    by="grp")


  # aggregálás: minden komponensre választok egy IDcol-tagot:
  warn_if_not_unique_id <- function(entity_ids){
                           unique_ids <- unique(entity_ids)
                           unique_ids <- unique_ids[!is.na(unique_ids)]
                           if(length(unique_ids)>1){
                             warning(paste0("IDs ", 
                               paste0(unique_ids, collapse=", "),
                               " are in the same component.\n",
                               "\t", unique_ids[1], " is used."))
                             unique_ids <- unique_ids[1]
                           } else if(length(unique_ids)==0){
                            unique_ids <- as(NA, class(entity_ids))
                           }
                           return(unique_ids)
  }

  comp_id <- comps[, .(id=warn_if_not_unique_id(get(IDcol)),
                       grp=unique(grp)),
                   by=comp]

  if(sum(!is.na(comp_id$id)) & sum(is.na(comp_id$id))){
    comp_id$id[is.na(comp_id$id)] <- paste0("m",
                              max(as.numeric(gsub("[^[:digit:]]",
                                    "",
                                    comp_id$id)), na.rm=TRUE) + 1:
                              sum(is.na(comp_id$id)))

  } else {
    comp_id[, id := paste0("m", comp)]
  }
  
  return (comp_id[,id])
  
}