library(here)

source(here("_rsource","rmd2md.R"))

site_url<-"C:/Users/JuanVesga/Dropbox/Code/Git/ide-modelling-R-ankara/ide-modelling-R-ankara"
imgs_url<-"C:/Users/JuanVesga/Dropbox/Code/Git/ide-modelling-R-ankara/ide-modelling-R-ankara/_images/"
rmd2md( path_site = getwd(),
                    dir_rmd = "_rmd",
                    dir_md = "_assignments",                              
                    #dir_images = "_images",
                    url_images = "_images/",
                    out_ext='.md', 
                    in_ext='.rmd', 
                    recursive=FALSE)
