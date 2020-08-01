### calculate RH
saturate_vp_func <- function(Tc,a=6.12,m=7.59,Tn=240.73){
    # T = Temperature (°C)
    VPS <- a*10^(m*Tc/(Tc+Tn))
    return(VPS)
}