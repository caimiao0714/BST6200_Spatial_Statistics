pacman::p_load(sf, ggplot2, readxl, dplyr, highcharter)


cn = get_data_from_map(download_map_data("countries/cn/custom/cn-all-sar-taiwan"))
cn1 = cn %>% 
  mutate(value_2_map = rnorm(n(), 0, 1))



p = hcmap("countries/cn/custom/cn-all-sar-taiwan", 
          data = cn1, 
          value = "value_2_map",
          joinBy = c("hc-key", "hc-key"),
          name = "random data",
          borderColor = "grey", 
          borderWidth = 0.5,
          tooltip = list(valueDecimals = 2, 
                         valueSuffix = "%"),
          dataLabels = list(enabled = TRUE, format = '{point.name}')) %>% 
  hc_colorAxis(dataClasses = color_classes(seq(-2, 2, by = 0.5))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>% 
  hc_title(text = "A demonstration of provinces in China") %>% 
  hc_subtitle(text = "Georgia Department of Public Health's Data Warehouse") %>% 
  hc_add_theme(hc_theme_ft())
p
