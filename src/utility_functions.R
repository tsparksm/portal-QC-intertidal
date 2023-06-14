# Define list of parameters to download and plot, with ParmName
# See discrete_parms from kcmarine for full list
parms <- c("NH3N", 
           "Enterococcus", 
           "FecalColiform", 
           "NNN", 
           "OP", 
           "Salinity", 
           "Temperature", 
           "TotalN")

# Define data paths
Z_drive <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/"
data_fpath_rda <- paste(Z_drive, 
                        "MarinePortal", 
                        "WaterQuality", 
                        "ShinyIntertidal", 
                        "discrete_data.rda", sep = "/")
data_fpath_csv <- paste(Z_drive, 
                        "MarinePortal", 
                        "WaterQuality", 
                        "ShinyIntertidal", 
                        "discrete_data.csv", sep = "/")
site_fpath <- paste(Z_drive, 
                    "MarinePortal", 
                    "WaterQuality", 
                    "ShinyIntertidal", 
                    "intertidal_sites.txt", sep = "/")

# Update site data downloaded from Monitoring Portal
# There is no option to do this from the app; manual update only
update_site <- function() {
  if (is.na(Sys.getenv("site_user", unset = NA))) {
    Sys.setenv(site_user = getPass::getPass(msg = "Enter username"))
    Sys.setenv(site_pw = getPass::getPass(msg = "Enter password"))
  }
  
  webpage <- "http://dnrp-apps2/Monitoring-Portal/Sites?SiteType=1&pageSize=1000"
  
  tt <- RCurl::getURL(webpage, 
                      userpwd = paste(Sys.getenv("site_user"), 
                                      Sys.getenv("site_pw"), 
                                      sep = ":"))
  sites <- XML::readHTMLTable(tt, 
                              stringsAsFactors = FALSE)[[1]]
  colnames(sites) <- c("Details", "SiteID", "SiteName", "Locator", "Latitude", 
                       "Longitude", "Shallow", "SiteType", "Area")
  sites <- sites %>% 
    select(!Details) %>% 
    mutate(Shallow = if_else(is.na(Shallow), FALSE, TRUE))
  write_tsv(sites, site_fpath)
}

# Load site data downloaded from Monitoring Portal
load_site <- function() {
  read_tsv(site_fpath, 
           col_types = cols(SiteID = col_character(), 
                            SiteName = col_character(), 
                            Locator = col_character(), 
                            Latitude = col_double(), 
                            Longitude = col_double(), 
                            Shallow = col_logical(), 
                            SiteType = col_character(), 
                            Area = col_character())) %>% 
    mutate(Shallow = if_else(is.na(Shallow), FALSE, TRUE))
}
