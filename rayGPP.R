#### Just a quick example for mapping GPP with rayshader (in quick response to requests on twitter...)
### Sorry for the semi-bad code... (hope it works)
library(rayshader)
library(raster)
library(rgl)

  dem <- raster("ETOPO1.halfdegree.nc")
  gpp  <-  raster("GPPdata_Beer_etal_2010_Science.nc")

  elmat  <-  dem %>%  raster_to_matrix() 
  gpp  <- gpp %>% raster_to_matrix()

 ### Now plot map of gpp where gpp also gives the elevation...
  rgl.clear(); rs_surface(gpp, zscale = 50, gpp, water=FALSE, shadowReachFactor = 2, shadowintens = 0.8)
  ### Some tweaking to make plot nicer (not these steep grey cliffs)
  gppFilt <- imagine:: quantileFilter(gpp, 3,0.001)
  gpp2Plot <- gpp
  gpp2Plot[is.na(gpp)]  <- gppFilt[is.na(gpp)] * 0.0
  
  rgl.clear(); rs_surface(gpp2Plot, zscale = 50, gpp, water=FALSE, shadowReachFactor = 2, shadowintens = 0.8, 
                          zoom=0.7, solid=T, soliddepth=-10, theta=0, windowsize = c(2400,1600))
  render_snapshot()
   
  
  ### Now plot map of gpp where gpp als gives the elevation but inlcude ocean...
  ## Missing values kind of a problem still
  elmat4plot <- ifelse(elmat < 0, elmat/5, gpp2Plot )
  rgl.clear(); rs_surface(elmat4plot, zscale = 100, gpp, water=TRUE, shadowReachFactor = 2, shadowintens = 0.8, theta=0)
  render_snapshot()
  
  values2rgb  <- function(values, pal=pals::viridis(64)) {
  
  
  ncol <- length(pal)
  cols <- pal[scales::rescale(values, c(1,ncol))] %>% col2rgb(alpha=TRUE)
  cols  <- cols %>% array(dim=c(4, values %>% dim)) %>% aperm(c(3,2,1))
  cols/255.
}

## Function to rayshade an elevation matrix with specified coloring values, which will be mapped onto a color scale with values2rgb
rs_surface  <- function(elmat, coloring, water = FALSE, alpha_over=0.75, 
                        texture="bw", zscale=250, shadowintens=0.8, shadowReachFactor=1, windowsize=c(1200, 800), zoom=0.5, ...)
{
  elmat %>% 
    
    sphere_shade(texture=texture,  colorintensity = 1, zscale = zscale) %>% 
    #ambient_shade() %>%
    add_shadow(ray_shade(elmat,zscale=zscale/shadowReachFactor),max_darken = 1-shadowintens) %>% 
    add_overlay( coloring %>% values2rgb, alphalayer = alpha_over) %T>%
    plot_map() %>%
    plot_3d(zscale = zscale, heightmap = elmat, wateralpha = 0.7, water=water, windowsize = windowsize, zoom = zoom, ...)
  
  
  }
