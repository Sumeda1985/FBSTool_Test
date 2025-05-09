
SUAFBS_t=function(input,output,session){

dbg_print <- function(x) {
  print(paste0("NEWBAL (", COUNTRY, "): ", x))
}


files = dir("SUA-FBS Balancing/R",full.names = TRUE)

for(i in files){
  source(i, local = TRUE)
}

#input$fromyear #####################################3
t=as.numeric(as.numeric(2014) : as.numeric(input$endyear))
startYear = as.numeric(2014)
endYear = as.numeric(input$endyear)
basedir <-getwd()
start_time <- Sys.time()
sapply(dir("SUA-FBS Balancing/R", full.names = TRUE), source)
COUNTRY <- as.character(unique(value_database$data$CountryM49))
dbg_print("parameters")
#BALANCING_METHOD <- swsContext.computationParams$balancing_method
BALANCING_METHOD <- "proportional"
#THRESHOLD_METHOD <- swsContext.computationParams$threshold_method
THRESHOLD_METHOD <- 'share'
# FIX_OUTLIERS <- TRUE
FIX_OUTLIERS <- FALSE
#FILL_EXTRACTION_RATES<-as.logical(swsContext.computationParams$fill_extraction_rates)
FILL_EXTRACTION_RATES <- FALSE
YEARS <- as.character(t)
p <- defaultStandardizationParameters()
p$itemVar <- "measuredItemSuaFbs"
p$mergeKey[p$mergeKey == "measuredItemCPC"] <- "measuredItemSuaFbs"
p$elementVar <- "measuredElementSuaFbs"
p$childVar <- "measuredItemChildCPC"
p$parentVar <- "measuredItemParentCPC"
p$createIntermetiateFile <- "TRUE"
p$protected <- "Protected"
p$official <- "Official"
# shareDownUp_file <-
#   file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_", COUNTRY, ".csv"))
TourismNoIndustrial <- data.table(dbReadTable(concore, "TourismNoIndustrial"))
dbg_print("define functions")

dt_left_join <- function(x, y, by = NA, allow.cartesian = FALSE,
                         nomatch = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }
  
  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }
  
  res <- y[x, on = by, allow.cartesian = allow.cartesian, nomatch = nomatch]
  
  setcolorder(res, c(names(x), setdiff(names(y), names(x))))
  
  res
}

dt_full_join <- function(x, y, by = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }
  
  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }
  
  res <- merge(x, y, by = by, all = TRUE)
  
  # merge sets the key to `by`
  setkey(res, NULL)
  
  res
}

update_opening_stocks <- function(x) {
  x <- x[order(geographicAreaM49, measuredItemSuaFbs, timePointYears)]
  
  groups <- unique(x[, .(geographicAreaM49, measuredItemSuaFbs)])
  
  res <- list()
  
  for (i in seq_len(nrow(groups))) {
    z <- x[groups[i], on = c("geographicAreaM49", "measuredItemSuaFbs")]
    if (nrow(z) > 1) {
      for (j in seq_len(nrow(z))[-1]) {
        # negative delta cannot be more than opening
        if (z$delta[j-1] < 0 & abs(z$delta[j-1]) > z$new_opening[j-1]) {
          z$delta[j-1] <- - z$new_opening[j-1]
        }
        z$new_opening[j] <- z$new_opening[j-1] + z$delta[j-1]
      }
      # negative delta cannot be more than opening
      if (z$delta[j] < 0 & abs(z$delta[j]) > z$new_opening[j]) {
        z$delta[j] <- - z$new_opening[j]
      }
    }
    res[[i]] <- z
  }
  rbindlist(res)
}
# Fill NAs by LOCF/FOCB/interpolation if more than two
# non-missing observations are available, otherwhise just
# replicate the only non-missing observation
na.fill_ <- function(x) {
  if(sum(!is.na(x)) > 1) {
    zoo::na.fill(x, "extend")
  } else {
    rep(x[!is.na(x)], length(x))
  }
}
`%!in%` <- Negate(`%in%`)
# RemainingToProcessedParent() and RemainingProdChildToAssign() will
# be used in the derivation of shareDownUp

RemainingToProcessedParent<-function(data){
  data[, 
       parent_already_processed:=ifelse(is.na(parent_qty_processed),parent_qty_processed,
                                        sum(processed_to_child/extractionRate,na.rm = TRUE)),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
       ]
  data[,remaining_processed_parent:=round(parent_qty_processed-parent_already_processed)]
  data[remaining_processed_parent<0,remaining_processed_parent:=0]
  data[,
       only_child_left:=ifelse(sum(is.na(processed_to_child))==1 & 
                                 is.na(processed_to_child) &
                                 !is.na(production_of_child) &
                                 !is.na(parent_qty_processed) & 
                                 production_of_child>0,TRUE,FALSE),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
       ]
  data[only_child_left==TRUE,processed_to_child:=remaining_processed_parent*extractionRate]
  data[,
       parent_already_processed:=ifelse(is.na(parent_qty_processed), parent_qty_processed,
                                        sum(processed_to_child/extractionRate,na.rm = TRUE)),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
       ]
  data[,remaining_processed_parent:=round(parent_qty_processed-parent_already_processed)]
  data[remaining_processed_parent<0,remaining_processed_parent:=0]
  return(data)
}

RemainingProdChildToAssign<-function(data){
  data[,
       available_processed_child:=sum(processed_to_child,na.rm = TRUE),
       by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")
       ]
  data[,remaining_to_process_child:=round(production_of_child-available_processed_child)]
  data[remaining_to_process_child<0,remaining_to_process_child:=0]
  data[,
       only_parent_left:=ifelse(sum(is.na(processed_to_child))==1 & 
                                  is.na(processed_to_child) &
                                  !is.na(parent_qty_processed) & 
                                  parent_qty_processed>=0,TRUE,FALSE)
       ]
   data[only_parent_left==TRUE,processed_to_child:=0]
  
  data[,available_processed_child:=sum(processed_to_child,na.rm = TRUE),
       by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")
       ]
  
  data[,remaining_to_process_child:=round(production_of_child-available_processed_child)]
  data[remaining_to_process_child<0,remaining_to_process_child:=0]
  return(data)
}
# The fmax function is used when fixing the processingShare of coproducts.
# If TRUE it means that "+" or "or" cases are involved.
fmax <- function(child, main, share, plusor = FALSE) {
  main <- unique(main)
  
  if (plusor) {
    found <- sum(sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE))
    
    if (found == 0) {
      return(max(share, na.rm = TRUE))
    } else if (found == 1) {
      return(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]])
    } else { # should be 2
      return(max(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]], na.rm = TRUE))
    }
  } else {
    if (sum(grepl(main, child)) > 0) {
      share[child == main]
    } else {
      max(share, na.rm = TRUE)
    }
  }
}
# Function that calculates imbalance as supply - utilizations (both calculated
# inside the function, dropped if keep_(supply|utilizations) set to FALSE).
calculateImbalance <- function(data,
                               supply_add = c("production", "imports"),
                               supply_subtract = c("exports", "stockChange"),
                               supply_all = union(supply_add, supply_subtract),
                               item_name = "measuredItemSuaFbs",
                               bygroup = c("geographicAreaM49", "timePointYears", item_name),
                               keep_supply = TRUE,
                               keep_utilizations = TRUE) {
 stopifnot(is.data.table(data))
  data[,
       `:=`(
         supply =
           sum(Value[measuredElementSuaFbs %chin% supply_add],
               - Value[measuredElementSuaFbs %chin% supply_subtract],
               na.rm = TRUE),
         # All elements that are NOT supply elements
         utilizations =
           sum(Value[!(measuredElementSuaFbs %chin% supply_all)],
               na.rm = TRUE)
       ),
       by = bygroup
       ][,
         imbalance := supply - utilizations
         ]
 if (keep_supply == FALSE) {
    data[, supply := NULL]
  }
  
  if (keep_utilizations == FALSE) {
    data[, utilizations := NULL]
  }
  
}

outside <- function(x, lower = NA, upper = NA) {
  x < lower | x > upper
}
################### optim stuff ###########################
my_fun <- function(s) {
  
  zero_if_na <- function(x) {
    ifelse(length(x) == 0, 0, ifelse(is.na(x), 0, x))
  }
  
  prod_i <- x[, measuredElementSuaFbs == 'production']
  impo_i <- x[, measuredElementSuaFbs == 'imports']
  expo_i <- x[, measuredElementSuaFbs == 'exports']
  stoc_i <- x[, measuredElementSuaFbs == 'stockChange']
  food_i <- x[, measuredElementSuaFbs == 'food']
  feed_i <- x[, measuredElementSuaFbs == 'feed']
  seed_i <- x[, measuredElementSuaFbs == 'seed']
  proc_i <- x[, measuredElementSuaFbs == 'foodManufacturing']
  indu_i <- x[, measuredElementSuaFbs == 'industrial']
  loss_i <- x[, measuredElementSuaFbs == 'loss']
  tour_i <- x[, measuredElementSuaFbs == 'tourist']
  resi_i <- x[, measuredElementSuaFbs == 'residual']
  
  prod_v <- zero_if_na(x$Value[prod_i])
  impo_v <- zero_if_na(x$Value[impo_i])
  expo_v <- zero_if_na(x$Value[expo_i])
  stoc_v <- zero_if_na(x$Value[stoc_i])
  food_v <- zero_if_na(x$Value[food_i])
  feed_v <- zero_if_na(x$Value[feed_i])
  seed_v <- zero_if_na(x$Value[seed_i])
  proc_v <- zero_if_na(x$Value[proc_i])
  indu_v <- zero_if_na(x$Value[indu_i])
  loss_v <- zero_if_na(x$Value[loss_i])
  tour_v <- zero_if_na(x$Value[tour_i])
  resi_v <- zero_if_na(x$Value[resi_i])
  supply <- prod_v + impo_v - expo_v - stoc_v
  utilizations <-
    food_v * s[1] +
    feed_v * s[2] +
    seed_v * s[3] +
    proc_v * s[4] +
    indu_v * s[5] +
    loss_v * s[6] +
    tour_v * s[7] +
    resi_v * s[8]
  
  abs(supply - utilizations)
}
do_optim <- function(d) {
  elem_names <- c('food', 'feed', 'seed', 'foodManufacturing',
                  'industrial', 'loss', 'tourist', 'residual')
  my_lower <- d$min_adj
  my_upper <- d$max_adj
  my_protected <- d$Protected
  names(my_lower) <- names(my_upper) <- names(my_protected) <- d$measuredElementSuaFbs
  my_lower <- my_lower[elem_names]
  my_upper <- my_upper[elem_names]
  my_protected <- my_protected[elem_names]
  names(my_lower) <- names(my_upper) <- names(my_protected) <- elem_names
  my_lower[is.na(my_lower) | my_protected == TRUE] <- 0.999999
  my_upper[is.na(my_upper) | my_protected == TRUE] <- 1.000001
  # XXX this used to be &, but we want now all 1s so it chan change
  initial <- (my_lower | my_upper ) * 1.0
  #initial[initial < 1] <- 0.00000001
  opt <- optim(initial, my_fun, method = "L-BFGS-B",
               lower = my_lower, upper = my_upper,
               control = list(pgtol = 0.0001))
  opt$par <- opt$par * !dplyr::near(opt$par, 0, 0.0000001)
  res <- reshape2::melt(as.list(opt$par))
  setDT(res)
  setnames(res, c("value", "L1"), c("adj", "measuredElementSuaFbs"))
  # Round to keep things actually different from 1.
  res[, adj := ifelse(dplyr::near(adj, 1, tol = 0.0001), 1, adj)]
  return(res)
}

balance_optimization <- function(d) {
  myres_optim <- do_optim(d)
  res <- myres_optim[d[, list(measuredElementSuaFbs, Value)],
                     on = c('measuredElementSuaFbs')]
  res[, adj := adj * Value]
  res[dplyr::near(adj, 0) | dplyr::near(adj, 1), adj := NA_real_]
  return(res$adj)
}

################### / optim stuff ###########################

balance_proportional <- function(data) {
  x <- copy(data)
  x <-
    x[
      can_balance == TRUE,
      c("measuredElementSuaFbs", "Value",
        "mov_share", "imbalance", "min_threshold",
        "max_threshold", "can_balance"),
      with = FALSE
      ]
  mov_share_sum <- sum(x$mov_share, na.rm = TRUE)
  if (mov_share_sum > 0) {
    # Recalculate mov_share, as we are excluding protected values
    x[, mov_share := mov_share / mov_share_sum]
  } else {
    # It means that there are no back shares, so use current values
    x[!is.na(Value) & can_balance == TRUE, mov_share := Value / sum(Value, na.rm = TRUE)]
  }
  x[is.na(Value), Value := 0]
  x[, adjusted_value := 0]
  x[Value + mov_share * imbalance >= 0, adjusted_value := Value + mov_share * imbalance]
  x[adjusted_value > Value & adjusted_value > max_threshold, adjusted_value := max_threshold]
  x[adjusted_value < Value & adjusted_value < min_threshold, adjusted_value := min_threshold]
  x <-  x[, c("measuredElementSuaFbs", "adjusted_value"), with = FALSE][data, on = "measuredElementSuaFbs"]
  return(as.numeric(x$adjusted_value))
}
rollavg <- function(x, order = 3) {
  # order should be > 2
  stopifnot(order >= 3)
  non_missing <- sum(!is.na(x))
  # For cases that have just two non-missing observations
  order <- ifelse(order > 2 & non_missing == 2, 2, order)
  if (non_missing == 1) {
    x[is.na(x)] <- na.omit(x)[1]
  } else if (non_missing >= order) {
    n <- 1
    while(any(is.na(x)) & n <= 10) { # 10 is max tries
      movav <- suppressWarnings(RcppRoll::roll_mean(x, order, fill = 'extend', align = 'right'))
      movav <- data.table::shift(movav)
      x[is.na(x)] <- movav[is.na(x)]
      n <- n + 1
    }
     x <- zoo::na.fill(x, 'extend')
  }
  return(x)
}
newBalancing <- function(data, Utilization_Table) {
  # Contains a variable that indicates whether stocks changed
  data[, change_stocks := NA_integer_]
  # XXX define "food residual" those items for which the only utilization
  # is food. Food processing can also be another possible utilization and
  # if there is that does not change its food-residualness, this is why
  # the check is done before assigning food processing.
  # NW: I have commented the conditions out
  # Now it only checks to make sure that food is the only utilization
  # We noticed that some of the food items were missing from the utilization table
  # This is still different from the previous approach of assigning all of the imbalance to 
  # food when "none of the other utilizations are activable"
  data[,
       food_resid :=
         # It's a food item & ...
         (measuredItemSuaFbs %chin% Utilization_Table[food_item == 'X', cpc_code] |
            # food exists & ...
            # !is.na(Value[measuredElementSuaFbs == 'food']) &
            Food_Median > 0 & !is.na(Food_Median)) &
         # ... is the only utilization
         all(is.na(Value[!(measuredElementSuaFbs %chin%
                             c('loss', 'food', 'production', 'imports',
                               'exports', 'stockChange','foodManufacturing', 'tourist'))])),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
       ]
  # Checking if the commodity has past value before assigning the residual
  # imbalance at the end of the balancing procees
  special_list<-c("0143","01442","01443","01446","01449.90","01491.02","01801","01802","01809")
  data[measuredItemSuaFbs %chin% special_list,
       food_resid :=
         # It's a food item & ...
         (measuredItemSuaFbs %chin% Utilization_Table[food_item == 'X', cpc_code] &
            # food exists & ...
            # !is.na(Value[measuredElementSuaFbs == 'food']) &
            Food_Median > 0 & !is.na(Food_Median)) &
         # ... is the only utilization
         all(is.na(Value[!(measuredElementSuaFbs %chin%
                             c('loss', 'food', 'production', 'imports',
                               'exports', 'stockChange','foodManufacturing', 'tourist'))])),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
       ]
  data[,
       `:=`(
         feed_resid =
           # It's a feed item or have past value & ...
           #(measuredItemSuaFbs %in% Utilization_Table[feed == 'X', cpc_code] |
           (Feed_Median > 0 & !is.na(Feed_Median)) &
           #feed is the only utilization....
           all(is.na(Value[!(measuredElementSuaFbs %chin%
                               c('feed', 'production', 'imports', 'exports',
                                 'stockChange','foodManufacturing'))])),
         # It's a industrial item or have past value & ...
         industrial_resid = Industrial_Median > 0 & !is.na(Industrial_Median)),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
       ] 
  data[,
       supply :=
         sum(
           Value[measuredElementSuaFbs %chin% c('production', 'imports')],
           - Value[measuredElementSuaFbs %chin% c('exports')],
           na.rm = TRUE
         ),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
       ]
  # When production needs to be created
  data[
    Protected == FALSE &
      # Only primary
      measuredItemSuaFbs %chin% Utilization_Table[primary_item == "X"]$cpc_code &
      measuredElementSuaFbs == 'production' &
      supply < 0 &
      stockable == FALSE,
    `:=`(
      Value = ifelse(is.na(Value), 0, Value) - supply,
      flagObservationStatus = "I"
      # flagMethod = "c"
    )
    ]
 calculateImbalance(data)
  # Try to assign the maximum of imbalance to stocks
  # NOTE: in the conditions below, 2 was 0.2, indicating that no more than
  # 20% should go to stocks. Now, the condition was relaxed a lot (200%)
  data <-
    dt_left_join(
      data,
      all_opening_stocks[,
                         .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua,
                           timePointYears, opening_stocks = Value)
                         ],
      by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
    )
 data[,
       Value_0 := ifelse(is.na(Value), 0, Value)
       ][
         Protected == FALSE &
           dplyr::near(imbalance, 0) == FALSE &
           measuredElementSuaFbs == "stockChange" &
           stockable == TRUE,
         change_stocks :=
           # The numbers indicate the case. Assignmnet (value and flags) will be done below
           case_when(
             # case 1: we don't want stocks to change sign.
             # This condition is diactivated #10/12/2021
             # sign(Value_0) * sign(Value_0 + imbalance) == -1                                                  ~ 1L,
             
             
             # case 2: if value + imbalance takes LESS than opening stock, take all from stocks
             # Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) <= opening_stocks           ~ 2L,
            (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) <= opening_stocks           ~ 2L,
             
             # case 3: if value + imbalance takes MORE than opening stock, take max opening stocks
             # Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) > opening_stocks            ~ 3L,
             (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) > opening_stocks            ~ 3L,
             
             # case 4: if value + imbalance send LESS than 200% of supply, send all
             # Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks <= supply * 2) ~ 4L,
             (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks <= supply * 2) ~ 4L,
             
             # case 5: if value + imbalance send MORE than 200% of supply, send 200% of supply
             # Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks > supply * 2)  ~ 5L
             (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks > supply * 2)  ~ 5L
           )
         ]
# data[change_stocks == 1L, Value := 0]
  data[change_stocks == 2L, Value := Value_0 + imbalance]
  data[change_stocks == 3L, Value := - opening_stocks]
  data[change_stocks == 4L, Value := Value_0 + imbalance]
  # Only case for which grouping is required
  data[
    change_stocks == 5L,
    Value := max(supply * 2 - opening_stocks, 0),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
    ]
data[
    change_stocks %in% 2L:5L,
    `:=`(flagObservationStatus = "E")
    ]
data[, Value_0 := NULL]
data[, opening_stocks := NULL]
# Recalculate imbalance
calculateImbalance(data)
# Assign imbalance to food if food "only" (not "residual") item
data[
    Protected == FALSE &
      food_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "food",
    `:=`(
      Value = ifelse(is.na(Value) & imbalance > 0, imbalance, ifelse(Value + imbalance >= 0, Value + imbalance, 0)),
      flagObservationStatus = "I"
      # flagMethod = "h"
    )
    ]
for (j in 1:10) {
  # Recalculate imbalance
   calculateImbalance(data)
   data[, can_balance := FALSE]
    data[
      !is.na(Value) &
        Protected == FALSE &
        !(data.table::between(Value, min_threshold, max_threshold, incbounds = FALSE) %in% FALSE) &
        !(measuredElementSuaFbs %chin%
            c("production", "imports", "exports", "stockChange", "foodManufacturing","seed")),
      can_balance := TRUE
      ]
   data[,
         elements_balance := any(can_balance),
         by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
         ]
    
   if (nrow(data[dplyr::near(imbalance, 0) == FALSE & elements_balance == TRUE]) == 0) {
      break()
    } else {
      data[
        dplyr::near(imbalance, 0) == FALSE &
          elements_balance == TRUE,
        adjusted_value := balance_proportional(.SD),
        by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
        ]
     data[
        !is.na(adjusted_value) & !dplyr::near(adjusted_value, Value),
        `:=`(
          Value = adjusted_value,
          flagObservationStatus = "I"
          # flagMethod = "-"
        )
        ]
     data[, adjusted_value := NULL]
    }
  }
# At this point the imbalance (in the best case scenario) should be zero,
# the following re-calculation is useful only for debugging
calculateImbalance(data)
# Assign imbalance to food if food "only" (not "residual") item
  data[
    Protected == FALSE &
      food_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "food",
    `:=`(
      Value =
        ifelse(
          is.na(Value) & imbalance > 0,
          imbalance,
          ifelse(Value + imbalance >= 0, Value + imbalance, 0)
        ),
      flagObservationStatus = "I"
      # flagMethod = "h"
    )
    ]
 calculateImbalance(data)
  # Assign the residual imbalance to industrial if the conditions are met
  data[
    Protected == FALSE &
      industrial_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "industrial",
    `:=`(
      Value =
        ifelse(
          is.na(Value) & imbalance > 0,
          imbalance,
          ifelse(Value + imbalance >= 0, Value + imbalance, Value)
        ),
      flagObservationStatus = "I"
      # flagMethod = "b"
    )
    ]
  
  if (COUNTRY %in% TourismNoIndustrial) {
    adj_tour_ind <-
      data.table::dcast(
        data[
          measuredItemSuaFbs %chin% Utilization_Table[food_item == "X"]$cpc_code &
            measuredElementSuaFbs %chin% c("industrial", "tourist")
          ],
        geographicAreaM49 + timePointYears + measuredItemSuaFbs ~ measuredElementSuaFbs,
        value.var = "Value"
      )
    adj_tour_ind <- adj_tour_ind[industrial > 0]
    adj_tour_ind[, new_tourist := industrial]
    adj_tour_ind[!is.na(tourist), new_tourist := tourist + industrial]
    adj_tour_ind[, c("industrial", "tourist") := NULL]
    by_vars <- c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
   data <- dt_left_join(data, adj_tour_ind, by = by_vars)
   data[
      measuredElementSuaFbs %in% c("industrial", "tourist"),
      any_protected := any(Protected == TRUE),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
      ]
    data[
      !is.na(new_tourist) & measuredElementSuaFbs == "tourist" & any_protected == FALSE,
      `:=`(
        Value = new_tourist,
        flagObservationStatus = "I"
        # flagMethod = "e"
      )
      ]
   data[
      !is.na(new_tourist) & measuredElementSuaFbs == "industrial" & any_protected == FALSE,
      `:=`(
        Value = 0,
        flagObservationStatus = "I"
        # flagMethod = "e"
      )
      ]
   data[, c("any_protected", "new_tourist") := NULL]
  }
  calculateImbalance(data)
  # Assign the residual imbalance to feed if the conditions are met
  data[
    Protected == FALSE &
      feed_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "feed",
    `:=`(
      # XXX: this creates a warning when no assignment is done:
      # Coerced 'logical' RHS to 'double'
      Value =
        ifelse(
          is.na(Value) & imbalance > 0,
          imbalance,
          ifelse(Value + imbalance >= 0, Value + imbalance, Value)
        ),
      flagObservationStatus = "I"
      # flagMethod = "b"
    )
    ]
  calculateImbalance(data)
  data[, c("supply", "utilizations", "imbalance", "mov_share_rebased") := NULL]
  return(data)
}

############################## / FUNCTIONS ##################################
dbg_print("end functions")
#####################################  TREE #################################

dbg_print("download tree")
tree <- data.table(dbReadTable(con, name="tree"))[,StatusFlag := 1]
tree[,c("StatusFlag","LastModified"):= NULL]
tree <- subset(tree, timePointYears %in% c(2014:input$endyear) )
if (COUNTRY == "835"){
  tree[, geographicAreaM49 := as.character(835)]
}

# ## NA ExtractionRates are recorded in the sws dataset as 0
# ## for the standardization, we nee them to be treated as NA
# ## therefore here we are re-changing it
# 
tree[Value == 0, Value := NA]
# 
# #proc_level_exceptions <- ReadDatatable("processing_level_exceptions")
# #
# #if (nrow(proc_level_exceptions) > 0) {
# #  setnames(proc_level_exceptions, c("m49_code", "parent", "child"),
# #           c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC"))
# #
# #  tree <-
# #    tree[!proc_level_exceptions[is.na(level)],
# #         on = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")]
# #
# #  proc_level_exceptions <- proc_level_exceptions[!is.na(level)]
# #}
# 
# tree_to_send <- tree[is.na(Value) & measuredElementSuaFbs=="extractionRate"]
# 
if (FILL_EXTRACTION_RATES == TRUE) {

  expanded_tree <-
    merge(
      data.table(
        geographicAreaM49 = unique(tree$geographicAreaM49),
        timePointYears = sort(unique(tree$timePointYears))
      ),
      unique(tree[, .(geographicAreaM49, measuredElementSuaFbs,
                      measuredItemParentCPC, measuredItemChildCPC)]),
      by = "geographicAreaM49",
      all = TRUE,
      allow.cartesian = TRUE
    )

  tree <- tree[expanded_tree, on = colnames(expanded_tree)]
# flags for carry forward/backward
  tree[is.na(Value), c("flagObservationStatus", "flagMethod") := list("E", "t")]

  tree <-
    tree[!is.na(Value)][
      tree,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemParentCPC", "measuredItemChildCPC",
             "timePointYears"),
      roll = -Inf
      ]

  tree <-
    tree[!is.na(Value)][
      tree,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemParentCPC", "measuredItemChildCPC",
             "timePointYears"),
      roll = Inf
      ]

  # keep orig flags
  tree[, flagObservationStatus := i.i.flagObservationStatus]
  tree[, flagMethod := i.i.flagMethod]

  tree[, names(tree)[grep("^i\\.", names(tree))] := NULL]
}
#  XXX remove NAs
tree <- tree[!is.na(Value)]
uniqueLevels <- unique(tree[, list(geographicAreaM49, timePointYears)])
levels <- list()
treeLevels <- list()
for (i in seq_len(nrow(uniqueLevels))) {
  filter <- uniqueLevels[i, ]
  treeCurrent <- tree[filter, on = c("geographicAreaM49", "timePointYears")]
  levels <- findProcessingLevel(treeCurrent, "measuredItemParentCPC", "measuredItemChildCPC")
  setnames(levels, "temp", "measuredItemParentCPC")
  treeLevels[[i]] <- merge(treeCurrent, levels, by = c("measuredItemParentCPC"), all.x = TRUE)
}
tree <- rbindlist(treeLevels)

# XXX there are no different process levels, but check it
tree[,
     processingLevel := max(processingLevel, na.rm = TRUE),
     by = c("geographicAreaM49", "timePointYears",
            "measuredElementSuaFbs", "measuredItemChildCPC")
     ]
# XXX Check if this one is still good or it can be obtained within the dataset
#processed_item_datatable <- fread_rds("SUA-FBS Balancing/Data/processed_item_datatable.rds")
processed_item_datatable <- data.table(dbReadTable(concore, "processed_item_datatable"))
processedCPC <- processed_item_datatable[, measured_item_cpc]
# XXX what is this for?
#itemMap <- fread_rds("SUA-FBS Balancing/Data/itemMap.rds")
itemMap <- data.table(dbReadTable(concore, "item_map"))
itemMap <- itemMap[, list(measuredItemSuaFbs = code, type)]

##################################### / TREE ################################
#coproduct_table <- fread_rds("SUA-FBS Balancing/Data/zeroweight_coproducts.rds")
coproduct_table <- data.table(dbReadTable(concore, "zeroweight_coproducts"))
############################ POPULATION #####################################
dbg_print("download population")

#popSWS <- fread_rds("SUA-FBS Balancing/Data/popSWS.rds")
popSWS <- data.table(dbReadTable(con, "pop_sws"))[StatusFlag== 1]
popSWS <- popSWS[,c("StatusFlag","LastModified") := NULL]
cols_to_convert <- c("geographicAreaM49", "measuredElement", "timePointYears")
popSWS[, (cols_to_convert) := lapply(.SD, as.character), .SDcols = cols_to_convert]
if (COUNTRY == "835"){
  popSWS[,geographicAreaM49 := as.character(835) ] 
}
popSWS <- subset(popSWS, timePointYears %in% t)
popSWS[geographicAreaM49 == "156", geographicAreaM49 := "1248"]

############################ / POPULATION ##################################
############################################################### DATA #######
# 5510 Production[t]
# 5610 Import Quantity [t]
# 5071 Stock Variation [t]
# 5023 Export Quantity [t]
# 5910 Loss [t]
# 5016 Industrial uses [t]
# 5165 Feed [t]
# 5520 Seed [t]
# 5525 Tourist Consumption [t]
# 5164 Residual other uses [t]
# 5141 Food [t]
# 664 Food Supply (/capita/day) [Kcal]
elemKeys <- c("5510", "5610", "5071", "5113", "5910", "5016",
              "5165", "5520", "5525", "5164", "5166", "5141")

# itemKeys <- GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
# itemKeys <- itemKeys$code


# NOTE: the definition of "food_resid" changed (see inside newBalancing)
# (so, the tables below are not used anymore)

#food_classification_country_specific <-
#  ReadDatatable("food_classification_country_specific",
#                where = paste0("geographic_area_m49 IN ('", COUNTRY, "')"))
#
#food_classification_country_specific <-
#  food_classification_country_specific[geographic_area_m49 == COUNTRY]
#
#food_only_items <- food_classification_country_specific[food_classification == 'Food Residual', measured_item_cpc]
#Utilization_Table <- fread_rds("SUA-FBS Balancing/Data/utilization_table_2018.rds")
Utilization_Table <- data.table(dbReadTable(concore, "utilization_table"))
stockable_items <- Utilization_Table[stock == 'X', cpc_code]
#zeroWeight <- fread_rds("SUA-FBS Balancing/Data/zeroWeight.rds")$x
zeroWeight <- data.table(dbReadTable(concore,"zero_weight"))$x
# flagValidTable <- ReadDatatable("valid_flags")
#flagValidTable=fread_rds("SUA-FBS Balancing/Data/flagValidTable.rds")
flagValidTable <- data.table(dbReadTable(concore, "flagValidTable"))
flagValidTable[, flagObservationStatus := as.factor(flagObservationStatus)]

#nutrientData <-fread_rds("SUA-FBS Balancing/Data/nutrientData.rds")

nutrientData <- data.table(dbReadTable(con, "nutrient_data"))[,StatusFlag :=  1 ]
nutrientData[,c("StatusFlag","LastModified") := NULL]

nutrientData[, geographicAreaM49 := as.character(geographicAreaM49)]
nutrientData[, timePointYearsSP := as.character(timePointYearsSP)]
nutrientData[, measuredElement   := as.character(measuredElement  )]

if (COUNTRY == "835"){
nutrientData[, geographicAreaM49 := as.character(835)]
  
}

dbg_print("download data")
data_2010 <- data.table(dbReadTable(con, "dbcountry"))[StatusFlag == 1][,c("StatusFlag","LastModified") := NULL]
data_2010[!is.na(Value) & is.na(Flag), Flag := ""]
#commented for Zanzibar
data_2000 <- get(load("Data/countrySUA_2000_2009.RData"))
# data <- rbind(data_2010)
#commented for zanzibar
data <- rbind(data_2010,data_2000)
data[,c("Country","Commodity","Element") := NULL]
setnames(data,c("CountryM49","ElementCode","CPCCode","Year","Value","Flag"),
         c("geographicAreaM49","measuredElementSuaFbs","measuredItemFbsSua","timePointYears","Value","flagObservationStatus")
         )
#################### FODDER CROPS ##########################################
# Some of these items may be missing in reference files,
# thus we carry forward the last observation.
fodder_crops_items <-
  tibble::tribble(
    ~description, ~code,
    "Maize for forage", "01911",
    "Alfalfa for forage", "01912",
    "Sorghum for forage", "01919.01",
    "Rye grass for forage", "01919.02",
    "Other grasses for forage", "01919.91",
    "Clover for forage", "01919.03",
    "Other oilseeds for forage", "01919.94",
    "Other legumes for  forage", "01919.92",
    "Cabbage for fodder", "01919.04",
    "Mixed grass and legumes for forage", "01919.93",
    "Turnips for fodder", "01919.05",
    "Beets for fodder", "01919.06",
    "Carrots for fodder", "01919.07",
    "Swedes for fodder", "01919.08",
    "Other forage products, nes", "01919.96",
    "Other forage crops, nes", "01919.95",
    "Hay for forage, from legumes", "01919.10",
    "Hay for forage, from grasses", "01919.09",
    "Hay for forage, from other crops nes", "01919.11"
  )
fodder_crops_availab <-
  data[
    measuredItemFbsSua %in% fodder_crops_items$code &
      measuredElementSuaFbs == "5510"
    ]

if (nrow(fodder_crops_availab) > 0) {
  
  fodder_crops_complete <-
    CJ(
      geographicAreaM49 = unique(fodder_crops_availab$geographicAreaM49),
      measuredElementSuaFbs = "5510",
      timePointYears = unique(data$timePointYears),
      measuredItemFbsSua = unique(fodder_crops_availab$measuredItemFbsSua)
    )
  
  fodder_crops_complete <-
    fodder_crops_complete[order(geographicAreaM49, measuredItemFbsSua, timePointYears)]
  
  fodder_crops <-
    merge(
      fodder_crops_complete,
      fodder_crops_availab[, .(geographicAreaM49, measuredElementSuaFbs,
                               measuredItemFbsSua, timePointYears, Value)],
      by = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears"),
      all.x = TRUE
    )
  
  fodder_crops[,
               Value := zoo::na.locf(Value),
               by = c("geographicAreaM49", "measuredItemFbsSua")
               ]
  
  fodder_crops_new <-
    fodder_crops[
      !fodder_crops_availab,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears")
      ]
if (nrow(fodder_crops_new) > 0) {
    # fodder_crops_new[, `:=`(flagObservationStatus = "E", flagMethod = "t")]
    fodder_crops_new[, `:=`(flagObservationStatus = "E")]
    data <- rbind(data, fodder_crops_new)
    
  }
}
#################### / FODDER CROPS ########################################

original_opening_stocks <- data[measuredElementSuaFbs == "5113"]
original_opening_stocks[, flagMethod := NULL]
original_opening_stocks <-
  flagValidTable[
    original_opening_stocks,
    on = c("flagObservationStatus")
    ][,
      Valid := NULL
      ]
all_opening_stocks<- copy(original_opening_stocks)
all_opening_stocks <- all_opening_stocks[!is.na(timePointYears)]
data <- merge(data, flagValidTable, by = c("flagObservationStatus"), all.x = TRUE)
data[flagObservationStatus %in% c("", "T"), `:=`(Official = TRUE, Protected = TRUE)]
data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]
# We remove "5113" (opening stocks) as will be stored in separate table.
data <- data[measuredElementSuaFbs != "5113"]
dbg_print("elementToCodeNames")
# XXX FIXME: the elementCodesToNames below is
# not working proberply, see issue #38
codes <- tibble::tribble(
  ~measuredElementSuaFbs,  ~name,
  "5910", "exports",
  "5520", "feed",
  "5141", "food",
  "5023", "foodmanufacturing",
  "5610", "imports",
  "5165", "industrial",
  "5016", "loss",
  "5510", "production",
  "5525", "seed",
  "5164", "tourist",
  "5071", "stock_change"
)
data <- merge(data, codes, by = "measuredElementSuaFbs", all.x = TRUE)
data[, measuredElementSuaFbs := name]
data[, name := NULL]
#data <-
#  elementCodesToNames(
#    data,
#    itemCol = "measuredItemFbsSua",
#    elementCol = "measuredElementSuaFbs"
#  )


# XXX: there are some NAs here, but probably there shouldn't
data <- data[!is.na(measuredElementSuaFbs)]
setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
dbg_print("convert sugar")

# XXX
##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
data <- convertSugarCodes(data)

########## / Remove feed if new element and negative imbalance is huge
treeRestricted <- tree[, .(measuredItemParentCPC, measuredItemChildCPC, processingLevel)]
treeRestricted <- unique(treeRestricted[order(measuredItemChildCPC)])
primaryInvolved <- getPrimary(processedCPC, treeRestricted, p)
dbg_print("primary involved descendents")

# XXX: check here, 0111 is in results
primaryInvolvedDescendents <-
  getChildren(
    commodityTree = treeRestricted,
    parentColname = "measuredItemParentCPC",
    childColname = "measuredItemChildCPC",
    topNodes = primaryInvolved
  )
# stocks need to be generated for those items for
# which "opening stocks" are available
items_to_generate_stocks <-
  unique(all_opening_stocks$measuredItemFbsSua)
# XXX what is primaryInvolvedDescendents ?????????
#deriv <- CJ(measuredItemSuaFbs = primaryInvolvedDescendents, measuredElementSuaFbs = 'production', geographicAreaM49 = unique(data$geographicAreaM49), timePointYears = unique(data$timePointYears))
deriv <-
  CJ(
    measuredItemSuaFbs    = unique(tree$measuredItemChildCPC),
    measuredElementSuaFbs = 'production',
    geographicAreaM49     = unique(data$geographicAreaM49),
    timePointYears        = unique(data$timePointYears)
  )
# rbind with anti_join
data <-
  rbind(
    data,
    deriv[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                        'geographicAreaM49', 'timePointYears')],
    fill = TRUE
  )
data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]
data[, stockable := measuredItemSuaFbs %chin% stockable_items]
# stockable items for which a historical series of at least
# 5 non-missing/non-null data points exist
historical_avail_stocks <-
  data[
    measuredElementSuaFbs == "stock_change" &
      timePointYears <= 2013 &
      !is.na(Value) &
      stockable == TRUE,
    .(n = sum(!dplyr::near(Value, 0))),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
    ][
      n >= 5
      ][,
        n := NULL
        ]
# Keep processingShare and shareDownUp
computed_shares <- list()
computed_shares_send <- list()
# Keep negative availability
negative_availability <- list()
#keep shareuPdOWN
updated_shareUpDOwn<-list()
fixed_proc_shares <- list()
original_stock_variation <-
  data[
    measuredElementSuaFbs == "stock_change" & timePointYears >= 2014 & !is.na(Value),
    .(
      geographicAreaM49, measuredItemSuaFbs, timePointYears, Value, flagObservationStatus
    )
    ]
dbg_print("Calculate Food Processing ")
treeProc <-
      tree[
        !is.na(Value) &
          # processingLevel == lev &
          measuredElementSuaFbs == 'extractionRate',
        list(
          measuredItemParentCPC,
          geographicAreaM49,
          measuredItemChildCPC,
          timePointYears,
          extractionRate = Value
        )
        ]
    #     
    #     #tree containing children of all parent of current level
    tree_parent_Level<-
      tree[
        !is.na(Value) &
          measuredElementSuaFbs == 'extractionRate' &
          measuredItemParentCPC %in% treeProc[, get(p$parentVar)],
        list(
          measuredItemParentCPC,
          geographicAreaM49,
          measuredItemChildCPC,
          timePointYears,
          extractionRate = Value
          # processingLevel
        )
        ]
   
   dataMergeTree <- data[measuredElementSuaFbs %chin% c('production', 'imports', 'exports', 'stock_change')]
   data_proc <- copy(dataMergeTree)
   setnames(dataMergeTree, "measuredItemSuaFbs", "measuredItemParentCPC")
   dataMergeTree[, geographicAreaM49 := as.character(geographicAreaM49)]
   tree_parent_Level[, geographicAreaM49 := as.character(geographicAreaM49)]
   tree_parent_Level[, timePointYears := as.character(timePointYears)]
   dataMergeTree <-
      merge(
        dataMergeTree,
        tree_parent_Level,
        by = c(p$parentVar, p$geoVar, p$yearVar),
        allow.cartesian = TRUE
      )
    dataMergeTree[,
                  availability :=
                    sum(
                      Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
                      # XXX p$stockCode is "stockChange", not "stock_change"
                      - Value[get(p$elementVar) %in% c(p$exportCode, "stock_change")],
                      na.rm = TRUE
                    ),
                  by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
                  ]
    
    #     
    #     # XXX: Replace negative availability, so we get zero production, instead of negative. This, , however, should be fixed in advance, somehow.
    dataMergeTree[availability < 0, availability := 0]
    dataMergeTree[,
                  availabilitieChildEquivalent := availability * extractionRate #,
                  #by = c(params$geoVar, params$yearVar, params$parentVar, params$childVar, "measuredElementSuaFbs")
                  ]
    dataMergeTree[,
                  sumAvail := sum(unique(na.omit(availabilitieChildEquivalent))),
                  by = c(p$childVar, p$yearVar, p$geoVar)  #, "measuredElementSuaFbs"
                  ]
    dataMergeTree[, shareDownUp := availabilitieChildEquivalent / sumAvail]
    dataMergeTree[shareDownUp > 1, shareDownUp := 1]
    dataMergeTree[shareDownUp < 0, shareDownUp := 0]
    dataMergeTree[is.nan(shareDownUp), shareDownUp := 0]
    setkey(dataMergeTree, NULL)
    dataMergeTree <-
      unique(
        dataMergeTree[,
                      list(geographicAreaM49, timePointYears, measuredItemParentCPC,
                           measuredItemChildCPC, extractionRate, availability, shareDownUp)
                      ]
      )
    tree_with_shareDWN <- copy(dataMergeTree)
    #data_proc includes data for the current level
    setnames(data_proc, p$itemVar, p$childVar)
    tree_with_shareDWN = merge(tree_with_shareDWN, data_proc, by = c(p$geoVar,p$yearVar, p$childVar), all.x = TRUE)
    tree_with_shareDWN[, weight := ifelse(get(p$childVar) %in% zeroWeight,0,1)]
    # dataMergeTree[timePointYears >= 2014 & Protected == FALSE, Value := NA][, Protected := NULL]
    tree_with_shareDWN[, foodProcElement:= ((Value/extractionRate)*shareDownUp)*weight]
    tree_with_shareDWN <- tree_with_shareDWN[measuredElementSuaFbs == "production"]
    food_proc_table=tree_with_shareDWN[,list(measuredItemSuaFbs=get(p$parentVar),geographicAreaM49 = get(p$geoVar) ,timePointYears = get(p$yearVar)
                                        ,foodProcElement)]
    food_proc_table <-  food_proc_table[, 
                                        list(Value = sum(foodProcElement, na.rm = TRUE)), 
                                        by = list(geographicAreaM49,measuredItemSuaFbs, timePointYears)]
    food_proc_table <- food_proc_table[Value > 0]
    food_proc_table <- food_proc_table[!is.na(Value)]
    if (dim(food_proc_table)[1] != 0){
    
    proc_to_bind <- food_proc_table[, list(geographicAreaM49,measuredItemSuaFbs,timePointYears,Value,
                                           measuredElementSuaFbs = p$foodProcCode, flagObservationStatus ="E", Valid = TRUE
                                              , Protected = TRUE, Official = FALSE)]
    proc_to_bind[, stockable := measuredItemSuaFbs %chin% stockable_items]
    proc_to_bind <- unique(proc_to_bind)
    data <- rbind(data, proc_to_bind)
}
dbg_print("end Food Processing")
######################## OUTLIERS #################################
# re-writing of Cristina's outliers plugin with data.table syntax #

if (FIX_OUTLIERS == TRUE) {
  commDef <- ReadDatatable("fbs_commodity_definitions") # XXX: updated?
  if(file.exists(paste0(basedir,"/SUA-FBS Balancing/Data/commDef.csv"))){
    file.remove(paste0(basedir,"/SUA-FBS Balancing/Data/commDef.csv"))
    write.csv(commDef,"SUA-FBS Balancing/Data/commDef.csv",row.names = FALSE)
  }
  primaryProxyPrimary_items <- commDef[proxy_primary == "X" | primary_commodity == "X"]$cpc
  food_items <- commDef[food_item == "X"]$cpc
  dout <-
    CJ(
      measuredItemSuaFbs = unique(data$measuredItemSuaFbs),
      measuredElementSuaFbs = unique(data$measuredElementSuaFbs),
      geographicAreaM49 = unique(data$geographicAreaM49),
      timePointYears = unique(data$timePointYears)
    )
  dout <-
    merge(
      dout,
      data,
      by = c('geographicAreaM49', 'timePointYears',
             'measuredItemSuaFbs', 'measuredElementSuaFbs'),
      all.x = TRUE
    )
  dout[is.na(Protected), Protected := FALSE]
  dout[,
       `:=`(
         production = Value[measuredElementSuaFbs  == "production"],
         supply     = sum(Value[measuredElementSuaFbs %in% c("production", "imports")],
                          - Value[measuredElementSuaFbs %in% c("exports", "stock_change")],
                          na.rm = TRUE),
         domsupply  = sum(Value[measuredElementSuaFbs %in% c("production", "imports")],
                          na.rm = TRUE)
       ),
       by = c('geographicAreaM49', 'measuredItemSuaFbs', 'timePointYears')
       ]
  dout[measuredElementSuaFbs %in% c("feed", "industrial"), element_supply := supply]
  dout[measuredElementSuaFbs == "seed", element_supply := production]
  dout[measuredElementSuaFbs == "loss", element_supply := domsupply]
  dout[element_supply < 0, element_supply := NA_real_]
  dout[, ratio := Value / element_supply]
  dout[,
       `:=`(
         mean_ratio = mean(ratio[timePointYears %in% (startYear-3):(startYear-1)], na.rm = TRUE),
         Meanold    = mean(Value[timePointYears (startYear-3):(startYear-1)], na.rm = TRUE)
       ),
       by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs')
       ]
  
  # If the element is new, there will be no `mean_ratio` and `Meanold`, thus we
  # use the new values to re-calculate them.
  dout[
    timePointYears >= startYear & (is.na(mean_ratio) | is.nan(mean_ratio) | is.infinite(mean_ratio)),
    mean_ratio := mean(ratio[timePointYears >= startYear], na.rm = TRUE),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs')
    ]
  dout[
    timePointYears >= startYear  & (is.na(Meanold) | is.nan(Meanold) | is.infinite(Meanold)),
    Meanold := mean(Meanold[timePointYears >= startYear ], na.rm = TRUE),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs')
    ]
  dout[mean_ratio > 1, mean_ratio := 1]
  dout[, abs_diff_threshold := ifelse(measuredElementSuaFbs == "feed", 0.1, 0.05)]
  dout[
    Protected == FALSE &
      timePointYears %in% 2014:2017 & # XXX: parameterise
      mean_ratio > 0 &
      abs(element_supply) > 0 &
      measuredElementSuaFbs %in% c("feed", "seed", "loss", "industrial") &
      # the conditions below define the outlier, or cases that were NA
      (is.na(Value) |
         abs(ratio - mean_ratio) > abs_diff_threshold |
         mean_ratio == 1 |
         (mean_ratio != 0 & dplyr::near(ratio, 0)) |
         (outside(Value / Meanold, 0.5, 2) & outside(Value - Meanold, -10000, 10000))),
    impute := element_supply * mean_ratio
    ]
  # Remove imputation for loss that is non-food and non-primaryProxyPrimary
  dout[
    measuredElementSuaFbs == "loss" &
      !(measuredItemSuaFbs %in% food_items &
          measuredItemSuaFbs %in% primaryProxyPrimary_items),
    impute := NA_real_
    ]
  
  dout <-
    dout[
      !is.na(impute) & (is.na(Value) | round(Value, 1) != round(impute, 1)),
      list(
        geographicAreaM49,
        measuredItemSuaFbs,
        measuredElementSuaFbs,
        timePointYears,
        Value_imputed = impute
      )
      ]
  if (nrow(dout) > 0) {
    data <-
      merge(
        data,
        dout,
        by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs', 'timePointYears'),
        all = TRUE
      )
    data[
      !is.na(Value_imputed),
      `:=`(
        Value = Value_imputed,
        flagObservationStatus = "E"
      )
      ]
    data[, Value_imputed := NULL]
  }
}
dbg_print("end outliers")
######################## / rm new loss in dairy/meat #################
# Protect all loss data, to keep it consistent with SDG indicator,
# whatever the flag is.
data[measuredElementSuaFbs == "loss", Protected := TRUE]
data <- merge(data, itemMap, by = "measuredItemSuaFbs")

## Split data based on the two factors we need to loop over
uniqueLevels <- unique(data[, list(geographicAreaM49, timePointYears)])
uniqueLevels <- uniqueLevels[order(geographicAreaM49, timePointYears)]
#parentNodes <- getCommodityLevel(tree, parentColname = "measuredItemParentCPC", childColname = "measuredItemChildCPC")
#parentNodes <- parentNodes[level == 0, node]

# FIXME: some names are set lowercase, but should be now camelCase
data[measuredElementSuaFbs == 'foodmanufacturing', measuredElementSuaFbs := 'foodManufacturing']
data[measuredElementSuaFbs == 'stock_change', measuredElementSuaFbs := 'stockChange']
data <-
  plyr::ddply(
    data,
    .variables = c('geographicAreaM49', 'timePointYears'),
    .fun = function(x) addMissingElements(as.data.table(x), p)
  )

setDT(data)
data[, stockable := measuredItemSuaFbs %chin% stockable_items]

# NOTE: the definition of "food_resid" changed (see inside newBalancing)
#data[, food_resid := measuredItemSuaFbs %chin% food_only_items]
# Remove stocks for non stockable items
data <- data[!(measuredElementSuaFbs == 'stock_change' & stockable == 'FALSE')]
data <- data[measuredElementSuaFbs != 'residual']
# Tourism consumption will be in the data only for selected countries
# and needs to be protected
data[measuredElementSuaFbs == "tourist", Protected := TRUE]
######################### Save UNB for validation #######################

sua_unbalanced <- data[, .(geographicAreaM49, timePointYears,
                           measuredItemSuaFbs, measuredElementSuaFbs,
                           Value, flagObservationStatus)]
sua_unbalanced_aux <-
  sua_unbalanced[,
                 .(
                   supply =
                     sum(Value[measuredElementSuaFbs %chin% c("production", "imports")],
                         - Value[measuredElementSuaFbs %chin% c("exports", "stockChange")],
                         na.rm = TRUE),
                   utilizations =
                     sum(Value[!(measuredElementSuaFbs %chin%
                                   c("production", "imports", "exports", "stockChange"))],
                         na.rm = TRUE)
                 ),
                 by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
                 ][,
                   imbalance := supply - utilizations
                   ][
                     supply > 0,
                     imbalance_pct := imbalance / supply * 100
                     ]

sua_unbalanced_aux <-
  data.table(melt(
    sua_unbalanced_aux,
    c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs"),
    variable.name = "measuredElementSuaFbs",
    value.name = "Value"
  ))

sua_unbalanced_aux[, `:=`(flagObservationStatus = "I")]
sua_unbalanced <- rbind(sua_unbalanced, sua_unbalanced_aux)

######################### Save UNB for validation #######################


data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed", "tourist"),
  movsum_value := suppressWarnings(RcppRoll::roll_sum(shift(Value), 3, fill = 'extend', align = 'right')),
  by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
  ]
data[
    measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed", "tourist"),
    mov_share := movsum_value / sum(movsum_value, na.rm = TRUE),
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]
# Impute share if missing
data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed", "tourist"),
  mov_share := rollavg(mov_share),
  by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
  ]

###when moving share is completely missing (no past data also future data), then take the current values to calculate the moving share. 
data[
  is.na(mov_share) & measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed", "tourist"),
  mov_share := Value / sum(Value, na.rm = TRUE),
  by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
  ]
# Set sum of shares = 1
data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed", "tourist"),
  mov_share := mov_share / sum(mov_share, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]
data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]
# Filter elements that appear for the first time
data_complete <-
  data.table(
    geographicAreaM49 = unique(data$geographicAreaM49),
    timePointYears = sort(unique(data$timePointYears)))[
      unique(data[, .(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs)]),
      on = "geographicAreaM49",
      allow.cartesian = TRUE
      ]
data_complete <-
  merge(
    data_complete,
    data[Value > 0, .(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears, Value)],
    by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs", "timePointYears"),
    all.x = TRUE
  )

data_complete[, y := 1]
data_complete <-
  data_complete[,
                .(
                  t_pre   = sum(y[timePointYears <= 2013]),
                  t_post  = sum(y[timePointYears >= 2014]),
                  na_pre  = sum(is.na(Value[timePointYears <= 2013])),
                  na_post = sum(is.na(Value[timePointYears >= 2014]))
                ),
                by = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs")
                ]
new_elements <-
  data_complete[
    na_pre == t_pre & na_post < t_post
    ][,
      .(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua = measuredItemSuaFbs)
      ][
        order(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua)
        ]
new_loss <-
  new_elements[
    measuredElementSuaFbs == "loss",
    .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua, new_loss = TRUE)
    ]

dbg_print("set thresholds")
if (THRESHOLD_METHOD == 'share') {
 dbg_print("thresholds, share")
  data[,
       supply :=
         sum(
           Value[measuredElementSuaFbs %in% c('production', 'imports')],
           - Value[measuredElementSuaFbs %in% c('exports', 'stockChange')],
           na.rm = TRUE
         ),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
       ]
# NOTE: here we redefine what "supply" is just for seed.
data[,
       supply := ifelse(measuredElementSuaFbs == "seed", Value[measuredElementSuaFbs == "production"], supply),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
       ]
data[supply < 0, supply := 0]
data[
    !(measuredElementSuaFbs %chin% c("production", "imports", "exports", "stockChange")),
    util_share := Value / supply
    ]
data[is.infinite(util_share) | is.nan(util_share), util_share := NA_real_]
# share < 0 shouldn't happen. Also, tourist can be negative.
data[util_share < 0 & measuredElementSuaFbs != "tourist", util_share := 0]
data[util_share > 1, util_share := 1]
data[,
       `:=`(
         min_util_share = suppressWarnings(min(util_share[timePointYears %in% 2000:(startYear-1)], na.rm = TRUE)),
         max_util_share = suppressWarnings(max(util_share[timePointYears %in% 2000:(startYear-1)], na.rm = TRUE))
       ),
       by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
       ]
data[is.infinite(min_util_share) | is.nan(min_util_share), min_util_share := NA_real_]
data[is.infinite(max_util_share) | is.nan(max_util_share), max_util_share := NA_real_]
data[max_util_share > 1, max_util_share := 1]
data[,
       `:=`(
         min_threshold = supply * min_util_share,
         max_threshold = supply * max_util_share
       )
       ]
data[, supply := NULL]
  
 } else {
   stop("Invalid method.")
 }

data[,
     `:=`(
       min_adj = min_threshold / Value,
       max_adj = max_threshold / Value
     )
     ]
data[min_adj > 1, min_adj := 0.9]
data[min_adj < 0, min_adj := 0.01]
data[max_adj < 1, max_adj := 1.1]
data[max_adj > 10, max_adj := 10] # XXX Too much?

# Fix for new "loss" element. If loss gets in as a new utilization,
# remove the thresholds for food.
data <-
  merge(
    data,
    new_loss,
    by = c('geographicAreaM49', 'measuredItemSuaFbs'),
    all.x = TRUE
  )
data[
  new_loss == TRUE & measuredElementSuaFbs == "food",
  `:=`(min_adj = 0, max_adj = 10)
  ] # XXX is 10 enough?
data[, new_loss := NULL]

# / Fix for new "loss" element. If loss gets in as a new utilization,
# / remove the thresholds for food.
# Recalculate the levels, given the recalculation of adj factors
data[, min_threshold := Value * min_adj]
data[, max_threshold := Value * max_adj]
# We need the min and max to be "near" one (but not too near) in case of
# Protected figures, so that they are not changed
data[Protected == TRUE, min_adj := 0.999]
data[Protected == TRUE, max_adj := 1.001]
# This fix should also be done in optim function?
data[
  dplyr::near(min_adj, 1) & dplyr::near(max_adj, 1),
  `:=`(
    min_adj = 0.999,
    max_adj = 1.001
  )
  ]
data[,
     `:=`(
       Food_Median       = median(Value[measuredElementSuaFbs=="food" & timePointYears %in% 2000:(startYear-1)], na.rm=TRUE),
       Feed_Median       = median(Value[measuredElementSuaFbs=="feed" & timePointYears %in% 2000:(startYear-1)], na.rm=TRUE),
       Industrial_Median = median(Value[measuredElementSuaFbs=="industrial" & timePointYears %in% 2000:(startYear-1)], na.rm=TRUE)
     ),
     by = c("geographicAreaM49", "measuredItemSuaFbs")
     ]
## 1 => year = 2014
i <- 1
dbg_print("starting balancing loop")
# XXX Only from 2004 onwards
uniqueLevels <- uniqueLevels[timePointYears >= 2014][order(timePointYears)]
standData <- vector(mode = "list", length = nrow(uniqueLevels))
for (i in seq_len(nrow(uniqueLevels))) {
  
  # For stocks, the first year no need to see back in time. After the first year was done,
  # stocks may have changed, so opening need to be changed in "data".
  dbg_print(paste("in balancing loop, start", i))
  # For stocks, the first year no need to see back in time. After the first year was done,
  # stocks may have changed, so opening need to be changed in "data".
##updating opening  
  if (i > 1) {
    dbg_print(paste("check stocks change in balancing", i))
    items_stocks_changed <-
      unique(standData[[i-1]][!is.na(change_stocks)]$measuredItemSuaFbs)
    if (length(items_stocks_changed) > 0) {
      dbg_print(paste("recalculate stocks changed", i))
      stocks_modif <-
        rbind(
          # Previous data (balanced)
          rbindlist(standData)[
            !is.na(Value) &
              measuredElementSuaFbs == 'stockChange' &
              measuredItemSuaFbs %chin% items_stocks_changed,
            list(geographicAreaM49, timePointYears, measuredItemSuaFbs, delta = Value)
            ],
          # New data (unbalanced)
          data[
            !is.na(Value) &
              timePointYears > unique(standData[[i-1]]$timePointYears) &
              measuredElementSuaFbs == 'stockChange' &
              measuredItemSuaFbs %chin% items_stocks_changed,
            list(geographicAreaM49, timePointYears, measuredItemSuaFbs, delta = Value)
            ]
        )
      
      data_for_opening <-
        dt_left_join(
          all_opening_stocks[
            timePointYears >= 2014 &
              measuredItemFbsSua %chin% items_stocks_changed,
            .(geographicAreaM49,  measuredItemSuaFbs =  measuredItemFbsSua,
              timePointYears, new_opening = Value)
            ],
          stocks_modif,
          by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
        )
      
      data_for_opening[is.na(delta), delta := 0]
      data_for_opening <- data_for_opening[order(geographicAreaM49, measuredItemSuaFbs, timePointYears)]
      dbg_print(paste("update opening stocks", i))
      data_for_opening <- update_opening_stocks(data_for_opening)
      dbg_print(paste("merge data in opening stocks", i))
      all_opening_stocks <-
        dt_left_join(
          all_opening_stocks,
          data_for_opening[,
                           .(
                             geographicAreaM49,
                             measuredItemFbsSua = measuredItemSuaFbs,
                             timePointYears,
                             new_opening
                           )
                           ],
          by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")
        )
     dbg_print(paste("new_opening", i))
     all_opening_stocks[
        !is.na(new_opening) & (round(new_opening) != round(Value) | is.na(Value)),
        `:=`(
          Value = new_opening,
          flagObservationStatus = "E",
          # flagMethod = "u"
          Protected = FALSE
        )
        ]
      all_opening_stocks[, new_opening := NULL]
    }
  }
######update opening stock
tree[, geographicAreaM49 := as.character(geographicAreaM49)]   
tree[, timePointYears := as.character(timePointYears)]     
filter <- uniqueLevels[i, ]
sel_vars <- c("geographicAreaM49", "timePointYears")
treeSubset <- tree[filter, on = sel_vars]
#treeSubset[, sel_vars := NULL, with = FALSE]
treeSubset[, sel_vars := NULL]
dataSubset <- data[filter, on = sel_vars]
dbg_print(paste("actual balancing", i))
standData[[i]] <- 
    newBalancing(
      data = dataSubset,
      #nutrientData = subNutrientData,
      #batchnumber = batchnumber,
      Utilization_Table = Utilization_Table
    )
  #issues[[length(issues) + 1]] <- newBal_result[["issues"]]
  # FIXME: we are now assigning the "Protected" flag to ALL processing as
  # after the first loop it should have been computed and that value SHOULD
  # never be touched again.
  standData[[i]][measuredElementSuaFbs == "foodManufacturing", Protected := TRUE]
  dbg_print(paste("in balancing loop, end", i))
  
}
all_opening_stocks[, measuredElementSuaFbs := "5113"]
dbg_print("end of balancing loop")
standData <- rbindlist(standData)
calculateImbalance(standData)
standData[
  supply > 0,
  imbalance_percent := imbalance / supply * 100
  ]

# If the imbalance is relatively small (less than 5% in absoulte value)
# a new allocation is done, this time with no limits.
# Here we need to protect seed, because of its lik to production.

standData[measuredElementSuaFbs == "seed", Protected := TRUE]
dbg_print("balancing of imbalances < 5%")
standData[, small_imb := FALSE]
standData[
  data.table::between(imbalance_percent, -5, -0.01) |
    data.table::between(imbalance_percent, 0.01, 5),
  small_imb := TRUE
  ]
if (nrow(standData[small_imb == TRUE]) > 0) {
  standData[, can_balance := FALSE]
  # The following two instructions basically imply to assign the
  # (small) imbalance with no limits (except food, among utilizations)
  sel_vars <- c("production", "imports", "exports", "stockChange", "food")
  standData[
    measuredElementSuaFbs %!in% sel_vars &
      Protected == FALSE &
      !is.na(min_threshold),
    min_threshold := 0
    ]
  standData[
    measuredElementSuaFbs %!in% sel_vars &
      Protected == FALSE &
      !is.na(max_threshold),
    max_threshold := Inf
    ]
  standData[
    !is.na(Value) &
      Protected == FALSE &
      !(data.table::between(Value, min_threshold, max_threshold, incbounds = FALSE) %in% FALSE) &
      !(measuredElementSuaFbs %chin%
          c("production", "imports", "exports", "stockChange", "foodManufacturing")),
    can_balance := TRUE
    ]
  standData[,
            elements_balance := any(can_balance),
            by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
            ]
  
  if (nrow(standData[small_imb == TRUE & elements_balance == TRUE]) > 0) {
    
    standData[
      small_imb == TRUE & elements_balance == TRUE,
      adjusted_value := balance_proportional(.SD),
      by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
      ]
    standData[
      !is.na(adjusted_value) & adjusted_value != Value,
      `:=`(
        Value = adjusted_value,
        flagObservationStatus = "I"
        # flagMethod = "n"
      )
      ]
    standData[, adjusted_value := NULL]
  }
}
calculateImbalance(standData)
standData[
  supply > 0,
  imbalance_percent := imbalance / supply * 100
  ]
######################### Save BAL for validation #######################
dbg_print("sua_balanced for validation")
sel_vars <-
  c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs",
    "measuredElementSuaFbs", "Value", "flagObservationStatus")
sua_balanced <-
  rbind(
    data[timePointYears < 2014, sel_vars, with = FALSE],
    standData[, sel_vars, with = FALSE]
  )
sua_balanced_aux <-
  sua_balanced[,
               .(
                 supply =
                   sum(Value[measuredElementSuaFbs %chin% c("production", "imports")],
                       - Value[measuredElementSuaFbs %chin% c("exports", "stockChange")],
                       na.rm = TRUE),
                 utilizations =
                   sum(Value[!(measuredElementSuaFbs %chin%
                                 c("production", "imports", "exports", "stockChange"))],
                       na.rm = TRUE)
               ),
               by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
               ][,
                 imbalance := supply - utilizations
                 ][
                   supply > 0,
                   imbalance_pct := imbalance / supply * 100
                   ]
sua_balanced_aux <-
  data.table(melt(
    sua_balanced_aux,
    c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs"),
    variable.name = "measuredElementSuaFbs",
    value.name = "Value"
  ))
sua_balanced_aux[, `:=`(flagObservationStatus = "I")]
sua_balanced <- rbind(sua_balanced, sua_balanced_aux)
imbalances <-
  unique(
    standData[,
              list(
                geographicAreaM49,
                measuredElementSuaFbs = "5166",
                measuredItemFbsSua = measuredItemSuaFbs,
                timePointYears,
                Value = imbalance,
                flagObservationStatus = "I",
                # flagMethod = "i",
                Protected = FALSE
              )
              ]
  )
# FIXME: see also the one done for elementToCodeNames
codes <- as.data.table(tibble::tribble(
  ~code,  ~name,
  "5910", "exports",
  "5520", "feed",
  "5141", "food",
  "5023", "foodManufacturing",
  "5610", "imports",
  "5165", "industrial",
  "5016", "loss",
  "5510", "production",
  "5525", "seed",
  "5164", "tourist",
  "5071", "stockChange"
))

standData <- standData[codes, on = c('measuredElementSuaFbs' = 'name')]
standData <-
  standData[,
            c("geographicAreaM49", "code", "measuredItemSuaFbs", "timePointYears",
              "Value", "flagObservationStatus", "Protected"),
            with = FALSE
            ]
setnames(
  standData,
  c("code", "measuredItemSuaFbs"),
  c("measuredElementSuaFbs", "measuredItemFbsSua")
)
standData <- standData[!is.na(Value)]
# These cases should not happen (i.e., all flags should already be
# set), but in any case, add specific flags so to check.
standData[is.na(flagObservationStatus), flagObservationStatus := "M"]
# standData[is.na(flagMethod), flagMethod := "q"]
# Calculate calories
calories_per_capita <-
  dt_left_join(
    # Food
    standData[
      measuredElementSuaFbs == '5141',
      list(
        geographicAreaM49,
        # measuredElementSuaFbs = "664",
        measuredItemFbsSua,
        timePointYears,
        food = Value,
        flagObservationStatus = "T"
        # flagMethod = "i"
      )
      ],
nutrientData[,
                 list(
                   geographicAreaM49,
                   measuredItemFbsSua = measuredItemCPC,
                   measuredElementSuaFbs = measuredElement,
                   timePointYears = timePointYearsSP,
                   nutrient = Value
                 )
                 ],
    by = c('geographicAreaM49', 'timePointYears', 'measuredItemFbsSua')
  )
calories_per_capita <-
  dt_left_join(
    calories_per_capita,
    popSWS[, list(geographicAreaM49, timePointYears, population = Value)],
    by = c('geographicAreaM49', 'timePointYears')
  )
data_for_foodGrams<-calories_per_capita[measuredElementSuaFbs=="664"]
data_for_foodGrams[,Value:=(food*1000000)/(365*population*1000)]
data_for_foodGrams[, c("food", "nutrient", "population") := NULL]
data_for_foodGrams[,measuredElementSuaFbs:="665"]
data_for_foodGrams[, Protected := FALSE]
calories_per_capita<- as.data.table(calories_per_capita)
calories_per_capita<- calories_per_capita[ , edible:= nutrient[measuredElementSuaFbs=="1061"], 
                                           by=c("geographicAreaM49", "timePointYears", "measuredItemFbsSua")]
calories_per_capita_total<-copy(calories_per_capita)
calories_per_capita[, Value := food *edible* nutrient / population / 365 * 10]
calories_per_capita[, Protected := FALSE]
calories_per_capita[, c("food", "nutrient", "population", "edible") := NULL]
calories_per_capita_total[, Value := food *edible * nutrient/100]
calories_per_capita_total[, Protected := FALSE]
calories_per_capita_total[, c("food", "nutrient", "population", "edible") := NULL]
calories_per_capita_total[measuredElementSuaFbs=="664",
                          measuredElementSuaFbs:="261"]
calories_per_capita_total[measuredElementSuaFbs=="674",
                          measuredElementSuaFbs:="271"]
calories_per_capita_total[measuredElementSuaFbs=="684",
                          measuredElementSuaFbs:="281"]
calories_per_capita_total <- calories_per_capita_total[measuredElementSuaFbs %in% c("261","271","281")]
calories_per_capita <- merge(calories_per_capita, nutrientEle[,c("ElementCode","measuredElement"), with= F],
                                                              by.x = "measuredElementSuaFbs",
                                                              by.y= "measuredElement", all.x = TRUE)
calories_per_capita[,ElementCode := as.character(ElementCode)]
calories_per_capita[is.na(ElementCode), ElementCode := measuredElementSuaFbs]
calories_per_capita[, measuredElementSuaFbs := NULL]
setnames(calories_per_capita, "ElementCode","measuredElementSuaFbs")
calories_per_capita<-rbind(calories_per_capita,calories_per_capita_total,data_for_foodGrams)
# calories_per_capita<- calories_per_capita[!measuredElementSuaFbs=="1061"]
standData <-
  rbind(
    standData,
    imbalances,
    all_opening_stocks,
    calories_per_capita
  )
standData <- standData[timePointYears >= 2014 & !is.na(Value)]
standData[, Protected := NULL]
# #save only the reference year
# standData <- standData[timePointYears %in% startYear:endYear]

value$sua_balanced_plugin <- copy(standData)
#dbWriteTable(concore, name="sua_balanced",  value=standData , overwrite = TRUE) #save SUA Balanced

#updating opening stock, stock change and other elements in the data base of the years of compilation (t)
standData <- standData[timePointYears %in% startYear:endYear]
data <- standData[measuredElementSuaFbs %in% c("5113","5071")]
longData <- copy(value$stockDataLong)
#write.csv(data,"data_Stock.csv",row.names =  F)
#write.csv(value$stockDataLong, "longData.csv", row.names = F)
col_format <- c("ElementCode", "Year", "CPCCode")
setnames(data,names(data), c("CountryM49","ElementCode","CPCCode","Year","Value","Flag"))
data[, (col_format) := lapply(.SD, as.character), .SDcols = (col_format)]
setnames(data,names(data), c("CountryM49","ElementCode","CPCCode","Year","Value","Flag"))
new_data <- merge(
  longData[, (col_format) := lapply(.SD, as.character),
           .SDcols = (col_format)],
  data,
  by = c("CPCCode", "ElementCode", "Year"),
  all = TRUE
)
new_data <- new_data[Value.y != Value.x | Flag.y != Flag.x |is.na(Value.y != Value.x)|is.na(Flag.y != Flag.x),
                     .(CountryM49 = as.character(
                       countrycode("Lesotho",
                                   origin = "country.name",
                                   destination = "un")),
                       Country = "Lesotho",
                       CPCCode,
                       ElementCode,
                       Year,
                       StatusFlag = 1,
                       LastModified = as.numeric(Sys.time()),
                       Value = Value.y,
                       Flag = Flag.y)]
# Merge with CPC and element codes
#old_data <- merge(old_data, all_cpc, by = "CPCCode", all.x = TRUE)
new_data <- merge(new_data, all_cpc, by = "CPCCode", all.x = TRUE)
#old_data <- merge(old_data, all_elements, by = "ElementCode", all.x = TRUE)
new_data <- merge(new_data, all_elements, by = "ElementCode", all.x = TRUE)

#save only data from year 2014
new_data <- new_data[!Year %in% c(2010:2013)]
data_2010_2013 <- longData[Year %in% c(2010:2013)]
new_data <- new_data[,c("Country","Commodity","Element") := NULL]
new_data <- rbind(new_data,data_2010_2013)

rows_update(
  tbl(con, "dbcountry"),
  as_tibble(new_data),
  by = c("CountryM49",
         "CPCCode",
         "ElementCode", 
         "Year"),
  in_place = TRUE,
  copy = TRUE,
  unmatched = "ignore"
)
#dynamically update the stock domain with stock values in sua balanced stock values 
update_stock_domain <- update_stock_domain(input,output,session)


}