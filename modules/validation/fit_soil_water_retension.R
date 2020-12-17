fit_soil_water_retension <- function() {
    
    
    myDF <- read.csv("tmp_data/soil_water_content.csv")


    with(myDF, plot(VWC~GWC))
    
    
    data('dataSHP')
    
    # -------------------------------------------------------------------
    # fit Soil Hydraulic Properties (SHP)
    # -------------------------------------------------------------------
    
    ans <- fitSHP(obs = list(th = dataSHP$th, K = dataSHP$Ku),
                  suc = list(th = dataSHP$suc, K = dataSHP$suc),
                  FUN.shp = 'vg',
                  modality = 'uni',
                  par.shp = NULL,
                  fit = 'both',
                  weighting = 'var',
                  log = c('alfa', 'n', 'ks'),
                  control = list(ncomplex = 15, reltol = 1e-07,tolsteps = 7),
                  suc.negativ = TRUE,
                  integral = FALSE,
                  L = 0,
                  print.info = TRUE
    )
    
    ans$par
    plot(ans)
    

}