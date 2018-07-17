#-------------------------------------------#
#------ Simulate future sugarcane G&D ------#
#-------------------------------------------#

#---Three methodologies:
#------ 1) Alter the future wth data with all past wth data available and simulate DSSAT/CANEGRO
#------ 2) Simulate all years without modifing wth and pick the best match between the current year simulations and past years
#------ 3) Uses best fit wth data between rainfall normal and all other wth

#--- Murilo Vianna (Jul-2018)

wd            ="C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/Modeling/Future_Sim/Future_sim_v1"

xfile_mwth    = "FUWE0001.SCX"      # Modified WTH
xfile_unmod   = "FUWE0002.SCX"      # Unmodified WTH
xfile_bm      = "FUWE0003.SCX"      # Best fit (BF) normal rainfall
ds_v          = 47                  # DSSAT version
crop          = "Sugarcane"         # Crop used
plantgro_fh   = "PlantGro_Head.csv" # A csv file with original PlantGro output names and converted names for R (This is necessary because R do not accept some special characters as vector label, such as # and %)
perfchart     = T                   # Save models performace charts?
plot_best_wth = T                   # Save charts of perfromace (normal year ~ Best year) for Solomon's method? 
wth_orig      = "SPPI"              # Name of original WTH files
wth_nm        = "PIFW"              # Name of modified WTH files 
wth_bm        = "PIBM"              # Name of best best fit normal rainfall (Solomon method)
l_sc_out      = c("smfmd")#,        # Name of the outputs to be analysed (as given in the plantgro_fh)
                #  "su.fmd",
                #  "shtd")
planting_year_init_f  = 2008        # Planting Year
harvesting_year_init_f= 2009        # Harvesting Year
planting_doy  = 225                 # Planting doy
harvest_doy   = 225                 # Harvesting doy
nyears        = 10                  # Number of years to be used for the "normal" year method
outidx        = "rmse"              # Statistical index to select the best fit (see mperf function)
l_today_dap   = seq(10,             # list of days after planting steps that will be considered the "today dap" for methods evaluation
                    360,
                    by = 10)
wth_dcomp = "RAIN"                  # The meteorological variable to be used in the Solomon's method Warning:CASE SENSITIVE 

#--- Replicate for past years? 
#--- Reduce one year and consider the reduced year as the current year
#--- Note that the series starts on 1979 so to ensure that the entire range "nyears" are included
#--- in all replications do not set init_wth_series_yr before your series possible harvesting!
#--- If you do not want to replicate across years set the below logical (replicate_py) to False
replicate_py        = T
init_wth_series_yr  = 1997

#------------------------------------------------------------------
#-----------------------------------
#---debug
o = "smfmd"
py = l_py[2]
tdap = 150
plim = c(0, 160)

plot(deb_out_orig$TMAX[deb_out_orig$year==2007 |deb_out_orig$year==2008])
points(deb_out_rbf$TMAX, col = "red")

#-----------------------------------


#--- code init
#--- replicate across years?
if(replicate_py){
  replicate_across_years_until = init_wth_series_yr + nyears   #
}else{
  replicate_across_years_until = planting_year_init_f
}

#--- load functions
source(paste(wd,"/Future_sim_f.R",sep=""))

for(o in l_sc_out){
  
  sc_out = o
  
  l_py = seq(planting_year_init_f,replicate_across_years_until)

  for(py in l_py) {
    
    dyr_f = harvesting_year_init_f - planting_year_init_f # nyears of crop season (sugarcane can have more than 1 year or in BRA crop season is in the end of year)
    planting_year     = py
    harvesting_year   = planting_year + dyr_f
    
    planting_year_init    = planting_year - nyears
    harvesting_year_init  = harvesting_year - nyears
    
    for (tdap in l_today_dap) {
      
      message(paste("Running ", ":", tdap, "Planting year", py))
      today_dap       = tdap
      today_dap_date  = as.Date(paste0(planting_year, "-01-01")) + planting_doy + today_dap - 1
      planting_date   = as.Date(paste0(planting_year, "-01-01")) + planting_doy - 1
      
      #----------------------------#
      #--- Read WTH base file   ---#
      #----------------------------#
      
      #--- list original wth files
      l_wth = dir("C:/DSSAT47/Weather")
      lwth_df = data.frame(wthfile = l_wth[substr(l_wth, 1, 4) == wth_orig],
                           year    = as.numeric(paste("20", substr(l_wth[substr(l_wth, 1, 4) ==wth_orig], 5, 6), sep = "")))
      
      #--- considering that WTH filenames follows DSSAT format and years that are higher than today 
      #--- are data from last century (e.g. SPPI7801, the 78 state for 1978 and not 2078)
      lwth_df$year[lwth_df$year > format(Sys.Date(), "%Y")] = lwth_df$year[lwth_df$year > format(Sys.Date(), "%Y")] - 100
      lwth_df$wthfile_new = gsub(wth_orig, wth_nm, lwth_df$wthfile)
      lwth_df$wthfile_bm  = gsub(wth_orig, wth_bm, lwth_df$wthfile)
      
      #--- sort by year
      lwth_df = lwth_df[order(lwth_df$year), ]
      
      #--- find the header rownumber end (as "@DATE") and the number of meteorological variables
      rdhead = data.frame(data = readLines(paste("C:/DSSAT47/Weather/", lwth_df$wthfile[1], sep ="")),
                         h    = F)
      rdhead$h[substr(rdhead$data,1,5)=="@DATE"] = T
      rhead     = as.numeric(rownames(rdhead[rdhead$h,]))
      nwth_var  = length(read.table(text = as.character(rdhead$data[rhead+1])))
      var_lab   = read.table(text = as.character(rdhead$data[rhead]))
      var_lab[,1] = gsub("@","",var_lab[,1])
      vlabels = ""
      
      for(v in 1:nwth_var) {vlabels[v]   = as.character(var_lab[1,v])}
      
      #--- wth file head
      wth_head = readLines(paste0("C:/DSSAT47/Weather/", lwth_df$wthfile[1]),
                           n = rhead)
      
      #--- replace with the new aws name ID
      wth_head = gsub(wth_orig, wth_nm, wth_head)
      
      #--- read original wth files
      for (fwth in lwth_df$wthfile) {
        wth = read.table(paste("C:/DSSAT47/Weather/", fwth, sep = ""),
                         skip = rhead,
                         colClasses = c("character",rep("numeric",nwth_var-1)))
        wth = wth[, 1:nwth_var]
        colnames(wth) = vlabels
        
        wth$year = lwth_df$year[lwth_df$wthfile==fwth]
        
        if (fwth == lwth_df$wthfile[1]) {
          wth_df = data.frame(wth)
          
        } else{
          wth_df = rbind(wth_df, wth)
          
        }
      }
      
      #--- compute doy from date
      wth_df$doy = as.numeric(substr(wth_df$DATE,3,5))
      
      #--- compute Gregorian date using function YYDOY
      wth_df$date_greg = YYDOY(wth_df$DATE,F,1900)
      wth_df$date_greg[wth_df$year>1999] = YYDOY(wth_df$DATE[wth_df$year>1999],F,2000)
      
      #-----------------------------------------------#
      #--- Separate this year simulations wth data ---#
      #-----------------------------------------------#
      
      wth_cy = wth_df[wth_df$date_greg >= planting_date &
                      wth_df$date_greg <= today_dap_date, ]
      
      #-----------------------------------------------#
      
      #------------------------------------------------------------------------------------#
      #--- Create "future" wth using past wth data from today_dap untill havesting date ---#
      #------------------------------------------------------------------------------------
      
      #--- replace in time-series 
      l_yrs = unique(wth_df$year[wth_df$year>=planting_year_init & wth_df$year<=harvesting_year])
      
      #--- Note that this replacement only works for crop seasons <= 365
      for (yr in l_yrs) {
        wth_df_yr = wth_df[wth_df$year == yr,]
        wth_df_yr$DATE_FMT = wth_df_yr$DATE
        
        m_doy = data.frame(doy = wth_df_yr$doy)
        m_doy = merge(m_doy, wth_cy, by = "doy")
        
        wth_df_yr[wth_df_yr$doy %in% m_doy$doy, vlabels] = m_doy[, vlabels]
        
        out_wth = data.frame(
          date = wth_df_yr$DATE_FMT,
          srad = sprintf("%5.1f", wth_df_yr$SRAD),
          tmax = sprintf("%5.1f", wth_df_yr$TMAX),
          tmin = sprintf("%5.1f", wth_df_yr$TMIN),
          rain = sprintf("%5.1f", wth_df_yr$RAIN)
        )
        
        
        write(
          wth_head,
          file = paste("C:/DSSAT47/Weather/", lwth_df$wthfile_new[lwth_df$year ==
                                                                    yr], sep = ""),
          append = F,
          sep = ""
        )
        
        write.table(
          out_wth,
          file =  paste("C:/DSSAT47/Weather/", lwth_df$wthfile_new[lwth_df$year ==
                                                                     yr], sep = ""),
          append = T,
          row.names = F,
          col.names = F,
          quote = F
        )
        
        if (yr == l_yrs[1]) {
          deb_out_orig = wth_df_yr
        } else{
          deb_out_orig = rbind(deb_out_orig, wth_df_yr)
        }
        
        message(
          paste(
            "WTH file:",
            lwth_df$wthfile_new[lwth_df$year == yr],
            "Created",
            ":",
            tdap,
            "Planting year",
            py
          )
        )
        
      }
      #------------------------------------------------------------------------------------
      
      #------------------------------------------------------------------------------------#
      #------------------------------ Run for all modified wth ----------------------------#
      #------------------------------------------------------------------------------------
      
      
      #--- create Xfile
      xfile_all_mwth  = readLines(paste(wd, "/FUWE0001_master.SCX", sep = ""))
      
      xfile_all_mwth = gsub("<pyr>" , substr(planting_year_init, 3, 4)   , xfile_all_mwth)
      xfile_all_mwth = gsub("<pdoy>",
                            sprintf("%003.0f", planting_doy)  ,
                            xfile_all_mwth)
      
      xfile_all_mwth = gsub("<hyr>" ,
                            substr(harvesting_year_init, 3, 4) ,
                            xfile_all_mwth)
      
      xfile_all_mwth = gsub("<hdoy>", sprintf("%003.0f", harvest_doy)   , xfile_all_mwth)
      
      xfile_all_mwth = gsub("<station>", wth_nm, xfile_all_mwth)
      
      nyr = (planting_year + 1) - (planting_year_init)
      if (nyr < 10) {
        nyr = paste(" ", nyr, sep = "")
      }
      xfile_all_mwth = gsub("<nyr>", nyr, xfile_all_mwth)
      
      write(xfile_all_mwth,
            paste("C:/DSSAT", ds_v, "/", crop, "/", xfile_mwth, sep = ""))
      
      message(paste("Xfile created", ":", tdap, "Planting year", py))
      
      #--- prepare batch call
      bfile = readLines(paste(wd, "/DSSBatch_Master.v47", sep = ""))
      bfile[4] = gsub("<calib_xfile>", xfile_mwth, bfile[4])
      
      #--- write in Crop folder
      write(bfile,
            file = paste("C:/DSSAT", ds_v, "/", crop, "/", "DSSBatch.v", ds_v, sep = ""))
      
      #--- set wd to run
      setwd(paste("C:/DSSAT", ds_v, "/", crop, "/", sep = ""))
      
      #--- Call DSSAT047.exe and run X files list within DSSBatch.v47
      system(paste(
        "C:/DSSAT",
        ds_v,
        "/DSCSM0",
        ds_v,
        ".EXE SCCAN0",
        ds_v,
        " B ",
        paste("DSSBatch.v", ds_v, sep = ""),
        sep = ""
      ))
      
      message(paste("Reading PlantGro", ":", tdap, "Planting year", py))
      
      #--- Read simulated data
      plant_lines = readLines("PlantGro.OUT")
      
      message(paste("Plantgro Read", ":", tdap, "Planting year", py))
      
      setwd(wd)
      
      #--- PlantGro Head
      pgro_head = read.csv(plantgro_fh)
      
      #--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
      write.table(
        plant_lines[substr(plant_lines, 2, 3) == "19" |
                      substr(plant_lines, 2, 3) == "20"],
        file = "PlantGro_numeric.OUT",
        row.names = F,
        col.names = F,
        quote = F
      )
      plant = read.table(file = "PlantGro_numeric.OUT")                   #Read numeric lines as data.frame
      
      #--- Columns name accordingly to DSSAT output name
      colnames(plant) = pgro_head$R_head
      
      message(paste("Plantgro indexing Runs", ":", tdap, "Planting year", py))
      
      #--- Read Runs (last year series)
      run = trimws(substr(plant_lines[substr(plant_lines, 2, 4) == "RUN"], 6, 12))
      
      #--- Index outputs with treatments
      plant$run = ""
      j = 0
      for (i in 1:length(plant$dap)) {
        if (plant$dap[i] == 0) {
          j = j + 1
        }
        
        plant$run[i] = run[j]
        
      }
      
      message(paste("Plantgro ready", ":", tdap, "Planting year", py))
      
      message(
        paste(
          "Separate current year and past years simulations",
          ":",
          tdap,
          "Planting year",
          py
        )
      )
      
      #--- separate current year and past years simulations
      obs_run = unique(plant$run)[length(unique(plant$run))]
      plantgro_cy  = plant[plant$run == obs_run, ]
      plantgro_s   = plant[plant$run != obs_run, ]
      
      #--- store future "observed" data
      plant_orig = plantgro_cy
      
      #--- compute bp limits for future data
      bp = boxplot(plantgro_s[plantgro_s$dap > today_dap, sc_out] ~
                     plantgro_s$dap[plantgro_s$dap > today_dap],
                   plot = F)
      
      fut_pred = data.frame(
        bp1 = bp$stats[1, ],
        bp2 = bp$stats[2, ],
        bp3 = bp$stats[3, ],
        bp4 = bp$stats[4, ],
        bp5 = bp$stats[5, ],
        dap = as.numeric(bp$names)
      )
      
      #------------------------------------------------------------------------------------
      
      
      #------------------------------------------------------------------------------------#
      #--------------------- Run for all year without modifying wth -----------------------#
      #------------------------------------------------------------------------------------
      
      message(paste(
        "Run for all year without modifying wth",
        ":",
        tdap,
        "Planting year",
        py
      ))
      
      #--- create Xfile
      xfile_all_wth  = readLines(paste(wd, "/FUWE0001_master.SCX", sep = ""))
      
      xfile_all_wth = gsub("<pyr>" , substr(planting_year_init, 3, 4)   , xfile_all_wth)
      xfile_all_wth = gsub("<pdoy>", sprintf("%003.0f", planting_doy)  , xfile_all_wth)
      
      xfile_all_wth = gsub("<hyr>" , substr(harvesting_year_init, 3, 4) , xfile_all_wth)
      xfile_all_wth = gsub("<hdoy>", sprintf("%003.0f", harvest_doy)   , xfile_all_wth)
      
      xfile_all_wth = gsub("<station>", wth_orig, xfile_all_wth)
      
      nyr = (planting_year + 1) - (planting_year_init)
      if (nyr < 10) {
        nyr = paste(" ", nyr, sep = "")
      }
      xfile_all_wth = gsub("<nyr>", nyr, xfile_all_wth)
      
      write(xfile_all_wth,
            paste("C:/DSSAT", ds_v, "/", crop, "/", xfile_unmod, sep = ""))
      
      message(paste("Xfile created", ":", tdap, "Planting year", py))
      
      #--- prepare batch call
      bfile = readLines(paste(wd, "/DSSBatch_Master.v47", sep = ""))
      bfile[4] = gsub("<calib_xfile>", xfile_unmod, bfile[4])
      
      #--- write in Crop folder
      write(bfile,
            file = paste("C:/DSSAT", ds_v, "/", crop, "/", "DSSBatch.v", ds_v, sep = ""))
      
      #--- set wd to run
      setwd(paste("C:/DSSAT", ds_v, "/", crop, "/", sep = ""))
      
      #--- Call DSSAT047.exe and run X files list within DSSBatch.v47
      system(paste(
        "C:/DSSAT",
        ds_v,
        "/DSCSM0",
        ds_v,
        ".EXE SCCAN0",
        ds_v,
        " B ",
        paste("DSSBatch.v", ds_v, sep = ""),
        sep = ""
      ))
      
      message(paste("Reading PlantGro", ":", tdap, "Planting year", py))
      
      #--- Read simulated data
      plant_lines = readLines("PlantGro.OUT")
      
      message(paste("PlantGro Read", ":", tdap, "Planting year", py))
      
      setwd(wd)
      
      #--- PlantGro Head
      pgro_head = read.csv(plantgro_fh)
      
      #--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
      write.table(
        plant_lines[substr(plant_lines, 2, 3) == "19" |
                      substr(plant_lines, 2, 3) == "20"],
        file = "PlantGro_numeric_unmod.OUT",
        row.names = F,
        col.names = F,
        quote = F
      )
      plant_unmod = read.table(file = "PlantGro_numeric_unmod.OUT")                   #Read numeric lines as data.frame
      
      #--- Columns name accordingly to DSSAT output name
      colnames(plant_unmod) = pgro_head$R_head
      
      message(paste("Plantgro indexing Runs", ":", tdap, "Planting year", py))
      
      #--- Read Runs (last year series)
      run = trimws(substr(plant_lines[substr(plant_lines, 2, 4) == "RUN"], 6, 12))
      
      #--- Index outputs with treatments
      plant_unmod$run = ""
      j = 0
      for (i in 1:length(plant_unmod$dap)) {
        if (plant_unmod$dap[i] == 0) {
          j = j + 1
        }
        
        plant_unmod$run[i] = run[j]
        
      }
      
      message(paste("Plantgro ready", ":", tdap, "Planting year", py))
      
      message(
        paste(
          "Separate current year and past years simulations",
          ":",
          tdap,
          "Planting year",
          py
        )
      )
      
      obs_run = unique(plant_unmod$run)[length(unique(plant_unmod$run))]
      
      #--- compute beste agreement between output past years simulations and current year until today_dap
      obs = plantgro_cy[plantgro_cy$dap <= today_dap, sc_out]
      
      message(
        paste(
          "Compute best agreement between output past years simulations and current year until today_dap",
          ":",
          tdap,
          "Planting year",
          py
        )
      )
      
      p_idx = c(outidx, "bias")
      for (run in unique(plantgro_s$run)) {
        sim = plantgro_s[plantgro_s$dap <= today_dap &
                           plantgro_s$run == run, sc_out]
        
        vnam = paste(run)
        
        if (run == unique(plant_unmod$run)[1]) {
          perf = mperf(sim, obs, vnam, F, p_idx)
        } else{
          perf = rbind(perf, mperf(sim, obs, vnam, F, p_idx))
        }
        
        message(
          paste(
            "Compute best agreement between",
            run,
            "  and this year RUN",
            ":",
            tdap,
            "Planting year",
            py
          )
        )
      }
      
      #--- sort by outidx
      perf = perf[order(perf[, outidx]), ]
      
      #--- plant best match
      plant_unmod_bm = plant_unmod[plant_unmod$run == perf$vnam[1], ]
      
      message(paste("Best RUN is", perf$vnam[1], ":", tdap, "Planting year", py))
      
      #--- plant best match average
      perf_b1 = perf[perf$bias > 0, ]
      perf_b2 = perf[perf$bias < 0, ]
      
      if (length(perf_b1$bias) == 0) {
        perf_b1 = perf
      }
      if (length(perf_b2$bias) == 0) {
        perf_b2 = perf
      }
      
      perf_b1 = perf_b1[order(perf_b1[, outidx]), ]
      perf_b2 = perf_b2[order(perf_b2[, outidx]), ]
      
      yr_b1 = plant_unmod[plant_unmod$run == perf_b1$vnam[1], "year"][1]
      yr_b2 = plant_unmod[plant_unmod$run == perf_b2$vnam[1], "year"][1]
      
      plant_b1 = plant_unmod[plant_unmod$run == perf_b1$vnam[1], c("dap", sc_out)]
      plant_b2 = plant_unmod[plant_unmod$run == perf_b2$vnam[1], c("dap", sc_out)]
      
      #--- necessary to correct for leap years
      plant_b1b2 = merge(plant_b1, plant_b2, by = "dap")
      plant_b1b2$avg = (plant_b1b2[, 2] + plant_b1b2[, 3]) / 2
      
      plant_unmod_bm_avg = plant_unmod_bm
      plant_unmod_bm_avg = merge(plant_b1b2, plant_unmod_bm_avg, by = "dap")
      plant_unmod_bm_avg[, sc_out] = plant_unmod_bm_avg$avg
      message(paste(
        "Average between",
        yr_b1,
        yr_b2,
        ":",
        tdap,
        "Planting year",
        py
      ))
      
      #------------------------------------------------------------------------------------
      
      
      
      #------------------------------------------------------------------------------------#
      #------- Run for the best match between normal rainfall year and all years ----------#
      #------------------------------------------------------------------------------------#
      
      message(paste("Running best normal rainfall", ":", tdap, "Planting year", py))
      #--- use the best fit of normal rain and previous years (proposed by solomon)
      
      #--- select only past years within the WSI range (10 years -> nyears)
      wth_df_bm = wth_df
      wth_df_bm$month = format(as.Date(wth_df_bm$date_greg), "%m")
      
      #using only data before the planting year (Maybe using data before "today" for the comparing could yield better results)
      wth_df_bm = wth_df_bm[wth_df_bm$date_greg < (as.Date(paste(planting_year, "-01-01", sep =""))) &
                              wth_df_bm$year >= (planting_year - nyears), ]
      
      #--- using data before "today" date
      #today_dap_date = as.Date(paste(planting_year, "-01-01", sep ="")) + planting_doy + today_dap - 1
      
      
      #--- selecting only data that are from full months to avoid incomplete monthly rainfall values
      #wth_df_bm = wth_df_bm[wth_df_bm$date_greg < (as.Date(paste(format(as.Date(today_dap_date), "%Y-%m"), "-01", sep =
      #                                                             ""))), ]
      
      wth_df_bm_feb29 = data.frame(wth_df_bm,m_d = format(wth_df_bm$date_greg,"%m-%d"))
      wth_df_bm_feb29 = wth_df_bm_feb29[wth_df_bm_feb29$m_d=="02-29",]
        
      feb29 = data.frame(TMAX  = mean(wth_df_bm_feb29$TMAX),
                         TMIN  = mean(wth_df_bm_feb29$TMIN),
                         SRAD  = mean(wth_df_bm_feb29$SRAD),
                         RAIN  = mean(wth_df_bm_feb29$RAIN))
      
      #--- monthly means
      month_wth = data.frame(
        month = unique(wth_df_bm[, c("month", "year")])[1],
        year  = unique(wth_df_bm[, c("month", "year")])[2],
        TMAX  = aggregate(TMAX ~ month + year, data = wth_df_bm, mean)$TMAX,
        TMIN  = aggregate(TMIN ~ month + year, data = wth_df_bm, mean)$TMIN,
        SRAD  = aggregate(SRAD ~ month + year, data = wth_df_bm, mean)$SRAD,
        RAIN  = aggregate(RAIN ~ month + year, data = wth_df_bm, sum)$RAIN
      )
      
      #--- normal
      normal_wth = data.frame(
        month = unique(month_wth$month),
        TMAX  = aggregate(TMAX ~ month, data = wth_df_bm, mean)$TMAX,
        TMIN  = aggregate(TMIN ~ month, data = wth_df_bm, mean)$TMIN,
        SRAD  = aggregate(SRAD ~ month, data = wth_df_bm, mean)$SRAD,
        RAIN  = aggregate(RAIN ~ month, data = month_wth, mean)$RAIN
      )
        
      #--- uses Solomon method but with RMSE as the best agreement indicator
        
        #--- use rain in the comparison
        month_wth$month = as.numeric(month_wth$month)
        
        obs_normal = normal_wth[,c("month",wth_dcomp)]
        for(y in unique(wth_df_bm$year)){
          
          sim_match = merge(obs_normal,month_wth[month_wth$year==y,c("month",wth_dcomp)], by = "month")
          colnames(sim_match) = c("month","obs","sim")  
          
          if(y == unique(wth_df_bm$year)[1]){
          perf_wth = mperf(sim_match$sim,
                           sim_match$obs,
                           y,
                           F,
                           outidx)
          }else{
            perf_wth = rbind(perf_wth,mperf(sim_match$sim,
                             sim_match$obs,
                             y,
                             F,
                             outidx))
            
          }
        }
        
      
      
      #--- sort by outidx
      perf_wth = perf_wth[order(perf_wth$rmse),]
      
      #--- Select the best fit year (min(rmse))
      best_wth = month_wth[month_wth$year == perf_wth$vnam[1],]
      
      if(plot_best_wth){
        png(
          paste("Perf_Normal_BFyear_",sc_out,"_todaydap_",today_dap,"_pyr_",planting_year,".png",sep = ""),
          units = "in",
          width = 12,
          height = 12,
          pointsize = 18,
          res = 300)
        
        par(mfrow = c(1, 1),
          mar = c(4.5, 4.5, 0.5, 0.5),
          oma = c(0, 0, 0, 0))
        
        mperf(best_wth$RAIN,
            obs_normal$RAIN,
            paste0(wth_dcomp,": Year ",unique(best_wth$year)),
            T,
            outidx)
      
        dev.off()
      }
      
      message(
        paste(
          "The year with best agreement with normal rainfall is ",
          best_wth$year[1],
          ":",
          tdap,
          "Planting year",
          py
        )
      )
      
      #--- wth file head
      wth_head = readLines(paste("C:/DSSAT47/Weather/", lwth_df$wthfile[1], sep =
                                   ""),
                           n = rhead) # wth file head
      
      wth_head = gsub(wth_orig, wth_bm, wth_head)
      
      #--- replace in all time-series
      best_wth_data = wth_df_bm[wth_df_bm$year == unique(best_wth$year),]
      
      #--- replace in time-series 
      l_yrs = seq(planting_year,harvesting_year)
      
      #--- Note that this replacement only works for crop seasons <= 365
      for (yr in l_yrs) {
        wth_df_yr = wth_df[wth_df$year == yr,]
        if(length(wth_df_yr$DATE) == length(best_wth_data$DATE)){
          wth_df_yr = best_wth_data[,colnames(wth_df)]
        }else{
          #leap year in one of selected years
          if(length(wth_df_yr$DATE) == 366){
            wth_lp_yr = data.frame(m_d = format(wth_df_yr$date_greg,"%m-%d"))
            wth_df_yr = merge(wth_lp_yr,data.frame(best_wth_data,m_d=format(best_wth_data$date_greg,"%m-%d")), by = "m_d", all.x = T)
            wth_df_yr[is.na(wth_df_yr$DATE),vlabels[2:length(vlabels)]] = feb29[1,vlabels[2:length(vlabels)]]
            wth_df_yr[,c("DATE","year","doy","date_greg")] = wth_df[wth_df$year == yr,c("DATE","year","doy","date_greg")]
            wth_df_yr$month[60] = "02"
            wth_df_yr$m_d = NULL
          }else{
            wth_lp_yr = data.frame(m_d = format(wth_df_yr$date_greg,"%m-%d"))
            wth_df_yr = merge(wth_lp_yr,data.frame(best_wth_data,m_d=format(best_wth_data$date_greg,"%m-%d")), by = "m_d")
            wth_df_yr[,c("DATE","year","doy","date_greg")] = wth_df[wth_df$year == yr,c("DATE","year","doy","date_greg")]
            wth_df_yr$m_d = NULL
          }
        }
        
        wth_df_yr$DATE_FMT = wth_df$DATE[wth_df$year == yr]
        
        m_doy = data.frame(doy = wth_df_yr$doy)
        m_doy = merge(m_doy, wth_cy, by = "doy")
        
        wth_df_yr[wth_df_yr$doy %in% m_doy$doy, vlabels] = m_doy[, vlabels]
        
        out_wth = data.frame(
          date = wth_df_yr$DATE_FMT,
          srad = sprintf("%5.1f", wth_df_yr$SRAD),
          tmax = sprintf("%5.1f", wth_df_yr$TMAX),
          tmin = sprintf("%5.1f", wth_df_yr$TMIN),
          rain = sprintf("%5.1f", wth_df_yr$RAIN)
        )
        
        
        write(
          wth_head,
          file = paste("C:/DSSAT47/Weather/", lwth_df$wthfile_bm[lwth_df$year ==
                                                                    yr], sep = ""),
          append = F,
          sep = ""
        )
        
        write.table(
          out_wth,
          file =  paste("C:/DSSAT47/Weather/", lwth_df$wthfile_bm[lwth_df$year ==
                                                                     yr], sep = ""),
          append = T,
          row.names = F,
          col.names = F,
          quote = F
        )
        
        message(
          paste(
            "WTH file:",
            lwth_df$wthfile_bm[lwth_df$year == yr],
            "Created",
            ":",
            tdap,
            "Planting year",
            py
          )
        )
        
      }
      
      message(paste("Create Xfile", ":", tdap, "Planting year", py))
      
      #--- create Xfile
      xfile_bm_full  = readLines(paste(wd, "/FUWE0001_master.SCX", sep = ""))
      
      pyr = planting_year
      
      xfile_bm_full = gsub("<pyr>" , substr(pyr, 3, 4)    , xfile_bm_full)
      xfile_bm_full = gsub("<pdoy>", sprintf("%003.0f", planting_doy) , xfile_bm_full)
      
      dy  = harvesting_year_init - planting_year_init
      hyr = pyr + dy
      
      xfile_bm_full = gsub("<hyr>", substr(hyr, 3, 4)  , xfile_bm_full)
      xfile_bm_full = gsub("<hdoy>", sprintf("%003.0f", harvest_doy)  , xfile_bm_full)
      
      xfile_bm_full = gsub("<station>", wth_bm, xfile_bm_full)
      nyr = 1
      if (nyr < 10) {
        nyr = paste(" ", nyr, sep = "")
      }
      xfile_bm_full = gsub("<nyr>", nyr, xfile_bm_full)
      
      write(xfile_bm_full,
            paste("C:/DSSAT", ds_v, "/", crop, "/", xfile_bm, sep = ""))
      
      #--- prepare batch call
      bfile = readLines(paste(wd, "/DSSBatch_Master.v47", sep = ""))
      bfile[4] = gsub("<calib_xfile>", xfile_bm, bfile[4])
      
      #--- write in Crop folder
      write(bfile,
            file = paste("C:/DSSAT", ds_v, "/", crop, "/", "DSSBatch.v", ds_v, sep = ""))
      
      #--- set wd to run
      setwd(paste("C:/DSSAT", ds_v, "/", crop, "/", sep = ""))
      
      #--- Call DSSAT047.exe and run X files list within DSSBatch.v47
      system(paste(
        "C:/DSSAT",
        ds_v,
        "/DSCSM0",
        ds_v,
        ".EXE SCCAN0",
        ds_v,
        " B ",
        paste("DSSBatch.v", ds_v, sep = ""),
        sep = ""
      ))
      
      message(paste("Reading PlantGro", ":", tdap, "Planting year", py))
      
      #--- Read simulated data
      plant_lines = readLines("PlantGro.OUT")
      
      message(paste("PlantGro Read", ":", tdap, "Planting year", py))
      
      setwd(wd)
      
      #--- PlantGro Head
      pgro_head = read.csv(plantgro_fh)
      
      #--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
      write.table(
        plant_lines[substr(plant_lines, 2, 3) == "19" |
                      substr(plant_lines, 2, 3) == "20"],
        file = "PlantGro_numeric_bm.OUT",
        row.names = F,
        col.names = F,
        quote = F
      )
      plant_bm = read.table(file = "PlantGro_numeric_bm.OUT")                   #Read numeric lines as data.frame
      
      #--- Columns name accordingly to DSSAT output name
      colnames(plant_bm) = pgro_head$R_head
      
      #------------------------------------------------------------------------------------
      
      
      #------------------------------------------------------------------------------------#
      #--------------------- Evaluate what is the best approach ---------------------------#
      #------------------------------------------------------------------------------------#
      #------------------------------------------------------------------------------------
      
      message(paste(
        "Evaluate what is the best approach",
        ":",
        tdap,
        "Planting year",
        py
      ))
      
      
      sim_data = plantgro_cy[plantgro_cy$dap > today_dap, c("dap", sc_out)]
      colnames(sim_data)[colnames(sim_data) == sc_out] = paste(sc_out, "_obs", sep =
                                                                 "")
      
      #--- add modified wth future predictions (median [bp3] as the predictor)
      sim_data = merge(sim_data, fut_pred[fut_pred$dap > today_dap, c("dap", "bp3")], by = "dap")
      colnames(sim_data)[colnames(sim_data) == "bp3"] = paste(sc_out, "_bp3", sep =
                                                                "")
      
      #--- add unmodified wth best match predictions (output sim x obs)
      sim_data = merge(sim_data, plant_unmod_bm[plant_unmod_bm$dap > today_dap, c("dap", sc_out)], by = "dap")
      colnames(sim_data)[colnames(sim_data) == sc_out] = paste(sc_out, "_unmodbm_", plant_unmod_bm$year[1], sep =
                                                                 "")
      
      #--- add unmodified wth average best match predictions (output sim x obs)
      sim_data = merge(sim_data, plant_unmod_bm_avg[plant_unmod_bm_avg$dap > today_dap, c("dap", sc_out)], by = "dap")
      colnames(sim_data)[colnames(sim_data) == sc_out] = paste(sc_out, "_unmodbmavg_", yr_b1, "_", yr_b2, sep =
                                                                 "")
      
      #--- add the wth that best match to current year normal rainfall (solomon approach)
      sim_data = merge(sim_data, plant_bm[plant_bm$dap > today_dap, c("dap", sc_out)], by = "dap")
      colnames(sim_data)[colnames(sim_data) == sc_out] = paste(sc_out, "_rainbm_", plant_bm$year[1], sep =
                                                                 "")
      
      l_sim = unique(colnames(sim_data))[unique(colnames(sim_data)) != "dap" &
                                           unique(colnames(sim_data)) != paste(sc_out, "_obs", sep = "")]
      
      message(paste("Plot performances", ":", tdap, "Planting year", py))
      
      png(
        paste(
          "Sim_perf_",
          sc_out,
          "_today_dap_",
          today_dap,
          "_pyr_",
          planting_year,
          ".png",
          sep = ""
        ),
        units = "in",
        width = 12,
        height = 12,
        pointsize = 18,
        res = 300
      )
      
      nchart1 = length(l_sim)
      nchart2 = 1
      if (nchart1 > 7) {
        nchart1 = nchart1 / 2
        nchart2 = 2
        
        if (nchart1 %% nchart2 > 0) {
          nchart1 = nchart1 + 1
        }
      }
      
      par(
        mfrow = c(nchart1, nchart2),
        mar = c(4.5, 4.5, 0.5, 0.5),
        oma = c(0, 0, 0, 0)
      )
      draw = T
      
      
      for (s in l_sim) {
        sim  = sim_data[, s]
        obs  = sim_data[, paste(sc_out, "_obs", sep = "")]
        vnam = s
        
        if (s == l_sim[1]) {
          perf_models = mperf(sim, obs, vnam, draw, c(outidx, "r2", "d"))
        } else{
          perf_models = rbind(perf_models, mperf(sim, obs, vnam, draw, c(outidx, "r2", "d")))
        }
        
      }
      
      dev.off()
      
      perf_models$tdap = tdap
      perf_models$pyr  = planting_year
      perf_models$nyr  = (planting_year + 1) - (planting_year_init)
      write.csv(
        perf_models,
        file = paste(
          "SimPerf_",
          sc_out,
          "_tdap_",
          today_dap,
          "_py_",
          planting_year,
          ".csv",
          sep = ""
        )
      )
      
      #--- plot outdata ~ dap
      
      message(paste("Plot Out~dap", ":", tdap, "Planting year", py))
      
      #--- separate current year and past years simulations
      
      plantgro_cy  = plant[plant$run == obs_run, ]
      plantgro_s   = plant[plant$run != obs_run, ]
      
      #--- store "future data" ("observed")
      plant_orig = plantgro_cy
      
      #--- plot until today data~dap
      png(
        paste(
          "Sim_DAP_",
          sc_out,
          "_today_dap_",
          today_dap,
          "_pyr_",
          planting_year,
          ".png",
          sep = ""
        ),
        units = "in",
        width = 12,
        height = 12,
        pointsize = 18,
        res = 300
      )
      par(
        mfrow = c(1, 1),
        mar = c(4.5, 4.5, 0.5, 0.5),
        oma = c(0, 0, 0, 0)
      )
      
      if (tdap == l_today_dap[1]) {
        plim = c(0, max(plant[, sc_out]))
      }
      
      plot(
        plantgro_cy[plantgro_cy$dap <= today_dap, sc_out] ~ plantgro_cy$dap[plantgro_cy$dap <=
                                                                              today_dap],
        xlim = c(0, max(plant$dap)),
        ylim = plim,
        lty  = 1,
        col  = "red",
        lwd  = 2.5,
        type = "l",
        ylab = sc_out,
        xlab = "DAP"
      )
      
      #--- plot predictions based on past wth (past series data~dap)
      for (run in unique(plantgro_s$run)) {
        lines(plantgro_s[plantgro_s$run == run &
                           plantgro_s$dap > today_dap, sc_out] ~
                plantgro_s$dap[plantgro_s$run == run &
                                 plantgro_s$dap > today_dap],
              col = "grey",
              lwd = 0.5)
        
      }
      
      
      #--- plot median of wth modified simulations
      lines(fut_pred$bp3 ~ fut_pred$dap, lwd  = 1.5)
      
      #--- plot best match of past simulations
      lines(plant_unmod_bm[plant_unmod$dap > today_dap, sc_out] ~
              plant_unmod_bm[plant_unmod$dap > today_dap, "dap"],
            col = "blue",
            lwd  = 1.5)
      
      #--- plot best match of past simulations
      lines(
        plant_unmod_bm_avg[plant_unmod_bm_avg$dap > today_dap, sc_out] ~
          plant_unmod_bm_avg[plant_unmod_bm_avg$dap > today_dap, "dap"],
        col = "blue",
        lwd  = 1.5,
        lty = 3
      )
      
      
      #--- plot best match of rainfall normal and last years wth
      lines(plant_bm[plant_bm$dap > today_dap, sc_out] ~
              plant_bm[plant_bm$dap > today_dap, "dap"], col = "green", lwd  = 1.5)
      
      #--- plot "observed" future data with wth omited (dap>today_dap)
      lines(
        plant_orig[plant_orig$dap > today_dap, sc_out] ~ plant_orig$dap[plant_orig$dap >
                                                                          today_dap],
        col = "red",
        lty = 3,
        lwd  = 1.5
      )
      
      legend(
        "bottomright",
        inset   = 0.02,
        legend  = c(
          paste("Current Data", sep = ""),
          paste("Future Data (Observed)", sep = ""),
          "All Past Years (modified WTH)",
          "Future predictions (Median of past years)",
          paste(
            "Future predictions (BF Normal Rainfall, Yr = ",
            best_wth$year[1],
            ")",
            sep = ""
          ),
          paste(
            "Future predictions (BF Current Output, Yr = ",
            plant_unmod_bm$year[1],
            ")",
            sep = ""
          ),
          paste(
            "Future predictions (BF Avg Current Output, Avg(",
            yr_b1,
            ",",
            yr_b2,
            ")",
            sep = ""
          )
        ),
        col     = c("red",
                    "red",
                    "grey",
                    "black",
                    "green",
                    "blue",
                    "blue"),
        lt      = c(1,
                    3,
                    1,
                    1,
                    1,
                    1,
                    3),
        lwd     = c(2.5,
                    1.5,
                    0.5,
                    1.5,
                    1.5,
                    1.5,
                    1.5),
        bg      = "white",
        cex     = 0.7,
        box.lty = 1
      )
      
      legend(
        "topleft",
        inset   = 0.02,
        legend  = c(paste(
          "Today DAP = ", tdap, ", Year = ", py, ")", sep = ""
        )),
        bg      = "white",
        cex     = 0.7,
        box.lty = 1
      )
      
      dev.off()
      
      
      
    } #--- end of tdap loop
    
  } #--- end of py loop
  
} #--- end of o loop

#------------------------------------------------------------------------------------




l_perf = dir(path = paste(wd,sep=""),pattern = "SimPerf")

l_perf_df = read.table(text = l_perf, sep = "_")
l_perf_df$V1 = NULL
l_perf_df$V3 = NULL
l_perf_df$V5 = NULL
l_perf_df$fname = l_perf
l_perf_df$V6 = as.numeric(gsub(".csv","",l_perf_df$V6))
colnames(l_perf_df) = c("sscan_out","tdap","pyr","fname")

#--- Gather all data in same DF
for(r in l_perf_df$fname){
  
  if(r == l_perf_df$fname[1]){
    results = read.csv(file = paste(wd,"/",r,sep=""))
  }else{
    results = rbind(results,read.csv(file = paste(wd,"/",r,sep="")))
  }
}

#--- index to method
results$method = 1

#--- replace repetitive vnam
results$vnam = gsub("unmodbmavg","m4",results$vnam)
results$vnam = gsub("unmodbm"   ,"m3",results$vnam)
results$vnam = gsub("rainbm"    ,"m2",results$vnam)
results$vnam = gsub("bp3"       ,"m1",results$vnam)

#--- index methods 1 = bp3, 2 = rainfall, 3 =  unmod, 4 = avg_unmod 
results$method[grep(results$vnam, pattern = "m1")] = 1
results$method[grep(results$vnam, pattern = "m2")] = 2
results$method[grep(results$vnam, pattern = "m3")] = 3
results$method[grep(results$vnam, pattern = "m4")] = 4

#--- Separate data
m1_df = results[results$method == 1,]
m2_df = results[results$method == 2,]
m3_df = results[results$method == 3,]
m4_df = results[results$method == 4,]

#----------------------------------------------------
idx   = read.table(text = m1_df$vnam, sep = "_")
colnames(idx) = c("sccan_out","method_ID")

m1_df = cbind(m1_df,idx)
m1_df$X     = NULL
m1_df$vnam  = NULL

#----------------------------------------------------
idx   = read.table(text = m2_df$vnam, sep = "_")
colnames(idx) = c("sccan_out","method_ID","sel_yr")

m2_df = cbind(m2_df,idx)
m2_df$X     = NULL
m2_df$vnam  = NULL

#----------------------------------------------------
idx   = read.table(text = m3_df$vnam, sep = "_")
colnames(idx) = c("sccan_out","method_ID","sel_yr")

m3_df = cbind(m3_df,idx)
m3_df$X     = NULL
m3_df$vnam  = NULL
#----------------------------------------------------
idx   = read.table(text = m4_df$vnam, sep = "_")
colnames(idx) = c("sccan_out","method_ID","sel_yr1","sel_yr2")

m4_df = cbind(m4_df,idx)
m4_df$X     = NULL
m4_df$vnam  = NULL
#----------------------------------------------------

#--- all data in same df
m_df = rbind(m1_df[,c("rmse","tdap","pyr","nyr","sccan_out","method_ID")],
             m2_df[,c("rmse","tdap","pyr","nyr","sccan_out","method_ID")],
             m3_df[,c("rmse","tdap","pyr","nyr","sccan_out","method_ID")],
             m4_df[,c("rmse","tdap","pyr","nyr","sccan_out","method_ID")])

o   = "smfmd"
yl  = "Stalk Fresh Mass"
ul  = "(t ha-1)"

lab_df = data.frame(sccan = c("smfmd","su.fmd"),
                    yl = c("Stalk Fresh Mass", "Sucrose Content"),
                    ul = c("(t ha-1)","(%)"))

lab_df_m = data.frame(m  = c("m1","m2","m3","m4"),
                      yl = c("Median of past years",
                             "BF Normal Rainfall",
                             "BF Current Output",
                             "BF Avg Current Output"))

for(out in unique(m_df$sccan_out)){

  for(m in unique(m_df$method_ID)){
  
    png(paste("BP_RMSE_",m,"_",out,".png",sep=""),units="in",width=20,height=12,pointsize=18,res=300)
    
    bp =    boxplot(m_df$rmse[m_df$method_ID==m & m_df$sccan_out==out]~m_df$tdap[m_df$method_ID==m & m_df$sccan_out==out],
                    ylim = c(0,max(m_df$rmse)),
                    ylab = paste(lab_df$yl[lab_df$sccan==out]," Error ",lab_df$ul[lab_df$sccan==out],sep=""),
                    xlab = "Season Day of Prediction Start (DAP)",
                    plot = T,
                    col  = "grey",
                    las = 2)
    
    lines(aggregate(rmse ~ tdap, data = m_df[m_df$method_ID == m & m_df$sccan_out == out,], mean)$rmse, type = "b", col = "red")
    
    legend("topright",
           inset   = 0.02,
           legend  = c("Average Error","Outliers"),
           col     = c("red","black"),
           lt      = c(1,NA),
           pch     = c(1,1),
           bg      = "white",
           cex     = 1.0,
           box.lty = 1,
           title = lab_df_m$yl[lab_df_m$m==m])
    
    dev.off()
    
    if(m == unique(m_df$method_ID)[1] & out == unique(m_df$sccan_out)){
      bp_m_df = data.frame(tdap     = as.numeric(bp$names),
                         output     = out,
                         method     = m,
                         method_lab = lab_df_m$yl[lab_df_m$m==m],
                         rmse_bp1   = bp$stats[1,],
                         rmse_bp2   = bp$stats[2,],
                         rmse_med   = bp$stats[3,],
                         rmse_bp4   = bp$stats[4,],
                         rmse_bp5   = bp$stats[5,],
                         rmse_avg   = aggregate(rmse ~ tdap, data = m_df[m_df$method_ID == m & m_df$sccan_out == out,], mean)$rmse,
                         rmse_min   = aggregate(rmse ~ tdap, data = m_df[m_df$method_ID == m & m_df$sccan_out == out,], min )$rmse,
                         rmse_max   = aggregate(rmse ~ tdap, data = m_df[m_df$method_ID == m & m_df$sccan_out == out,], max )$rmse,
                         rmse_80    = sapply(unique(m_df$tdap),function(x) quantile(m_df$rmse[m_df$method_ID== m & m_df$tdap == x],0.8)))
    }else{
    
      bp_m_df = rbind(bp_m_df,data.frame(tdap = as.numeric(bp$names),
                           output     = out,
                           method     = m,
                           method_lab = lab_df_m$yl[lab_df_m$m==m],
                           rmse_bp1   = bp$stats[1,],
                           rmse_bp2   = bp$stats[2,],
                           rmse_med   = bp$stats[3,],
                           rmse_bp4   = bp$stats[4,],
                           rmse_bp5   = bp$stats[5,],
                           rmse_avg   = aggregate(rmse ~ tdap, data = m_df[m_df$method_ID == m & m_df$sccan_out == out,], mean)$rmse,
                           rmse_min   = aggregate(rmse ~ tdap, data = m_df[m_df$method_ID == m & m_df$sccan_out == out,], min )$rmse,
                           rmse_max   = aggregate(rmse ~ tdap, data = m_df[m_df$method_ID == m & m_df$sccan_out == out,], max )$rmse,
                           rmse_80    = sapply(unique(m_df$tdap),function(x) quantile(m_df$rmse[m_df$method_ID== m & m_df$tdap == x],0.8))))
      
    }
  }
}

#--- write RMSE csv
write.csv(bp_m_df, file = paste("RMSE_all_methods.csv",sep = ""),quote = F, row.names = F)

for(out in unique(m_df$sccan_out)){

  png(paste("Median_RMSE_allmethods_",out,".png",sep=""),units="in",width=12,height=12,pointsize=18,res=300)
  
  plot(bp_m_df$rmse_med[bp_m_df$output==out & bp_m_df$method=="m1"]~bp_m_df$tdap[bp_m_df$output==out & bp_m_df$method=="m1"],
       type = "l",
       ylim = c(0,max(bp_m_df$rmse_med[bp_m_df$output==out])),
       ylab = paste(lab_df$yl[lab_df$sccan==out]," Median Error ",lab_df$ul[lab_df$sccan==out],sep=""),
       xlab = "Season Day of Start Prediction (DAP)")
  
lines(bp_m_df$rmse_med[bp_m_df$output==out & bp_m_df$method=="m2"]~bp_m_df$tdap[bp_m_df$output==out & bp_m_df$method=="m2"],
      type = "l",
      col = "green")

lines(bp_m_df$rmse_med[bp_m_df$output==out & bp_m_df$method=="m3"]~bp_m_df$tdap[bp_m_df$output==out & bp_m_df$method=="m3"],
      type = "l",
      col = "blue")

lines(bp_m_df$rmse_med[bp_m_df$output==out & bp_m_df$method=="m4"]~bp_m_df$tdap[bp_m_df$output==out & bp_m_df$method=="m4"],
      type = "l",
      lty = 3,
      col = "blue")

legend("bottomleft",
       inset   = 0.02,
       legend  = lab_df_m$yl,
       col     = c("black","green","blue","blue"),
       lt      = c(1,1,1,3),
       bg      = "white",
       cex     = 1.0,
       box.lty = 1)

dev.off()

}

for(out in unique(m_df$sccan_out)){
  
  png(paste("Average_RMSE_allmethods_",out,".png",sep=""),units="in",width=12,height=12,pointsize=18,res=300)
  
  plot(bp_m_df$rmse_avg[bp_m_df$output==out & bp_m_df$method=="m1"]~bp_m_df$tdap[bp_m_df$output==out & bp_m_df$method=="m1"],
       type = "l",
       ylim = c(0,max(bp_m_df$rmse_avg[bp_m_df$output==out])),
       ylab = paste(lab_df$yl[lab_df$sccan==out]," Average Error ",lab_df$ul[lab_df$sccan==out],sep=""),
       xlab = "Season Day of Start Prediction (DAP)")
  
  lines(bp_m_df$rmse_avg[bp_m_df$output==out & bp_m_df$method=="m2"]~bp_m_df$tdap[bp_m_df$output==out & bp_m_df$method=="m2"],
        type = "l",
        col = "green")
  
  lines(bp_m_df$rmse_avg[bp_m_df$output==out & bp_m_df$method=="m3"]~bp_m_df$tdap[bp_m_df$output==out & bp_m_df$method=="m3"],
        type = "l",
        col = "blue")
  
  lines(bp_m_df$rmse_avg[bp_m_df$output==out & bp_m_df$method=="m4"]~bp_m_df$tdap[bp_m_df$output==out & bp_m_df$method=="m4"],
        type = "l",
        lty = 3,
        col = "blue")
  
  legend("bottomleft",
         inset   = 0.02,
         legend  = lab_df_m$yl,
         col     = c("black","green","blue","blue"),
         lt      = c(1,1,1,3),
         bg      = "white",
         cex     = 1.0,
         box.lty = 1)
  
  dev.off()
  
}


