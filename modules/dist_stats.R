buff.Stats <- function(input, output, session, layers, region) {
  outbuffstats <- reactive({
    x <- tibble(
      Attribute = c(
        "Study Area (km2)", 
        "Linear disturbances (km)", 
        "Areal disturbances (km2)", 
        "Fires (%)", 
        "IFL 2000 (%)", 
        "IFL 2020 (%)", 
        "Intactness (%)",
        "Footprint (%)",
        "Footprint + Fires (%)"
      ), 
      Value = NA
    )
    
    # Check if region is not NULL
    if (class(layers$intactness)[1]=="SpatVector") {
      aoi <- expanse(region, unit = "km")
      x$Value[x$Attribute == "Study Area (km2)"] <- round(aoi, 0)
      x$Value[x$Attribute=="Linear disturbances (km)"] <- round(sum(perim(layers$line))/1000, 1)
      x$Value[x$Attribute=="Areal disturbances (km2)"] <- round(sum(expanse(layers$poly, unit = "km")), 1)
      x$Value[x$Attribute=="Fires (%)"] <- round(sum(expanse(layers$fires, unit="km"))/aoi*100, 2)
      x$Value[x$Attribute == "IFL 2000 (%)"] <- round(sum(expanse(layers$ifl_2000, unit = "km")) / aoi*100, 2)
      x$Value[x$Attribute == "IFL 2020 (%)"] <- round(sum(expanse(layers$ifl_2020, unit = "km")) / aoi*100, 2)
      x$Value[x$Attribute=="Intactness (%)"] <- round(sum(expanse(layers$intactness, unit = "km"))/aoi*100, 2)
      x$Value[x$Attribute=="Footprint (%)"] <- round(sum(expanse(layers$footprint, unit = "km")) /aoi*100, 2)
      foot_fires <- layers$footprint %>% terra::union(layers$fires) %>% aggregate()
      x$Value[x$Attribute=="Footprint + Fires (%)"] <- round(sum(expanse(foot_fires, unit = "km")) /aoi*100, 2)
    }else{
      aoi <- expanse(region, unit = "km")
      x$Value[x$Attribute == "Study Area (km2)"] <- round(aoi, 0)
      x$Value[x$Attribute=="Linear disturbances (km)"] <- round(sum(perim(layers$line))/1000, 1)
      x$Value[x$Attribute=="Areal disturbances (km2)"] <- round(sum(expanse(layers$poly, unit = "km")), 1)
      x$Value[x$Attribute=="Fires (%)"] <- round(sum(expanse(layers$fires, unit="km"))/aoi*100, 2)
      x$Value[x$Attribute == "IFL 2000 (%)"] <- round(sum(expanse(layers$ifl_2000, unit = "km")) / aoi*100, 2)
      x$Value[x$Attribute == "IFL 2020 (%)"] <- round(sum(expanse(layers$ifl_2020, unit = "km")) / aoi*100, 2)
    }
    return(x)
  })

}