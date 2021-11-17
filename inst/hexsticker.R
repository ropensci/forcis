#' 
#' Create an Hexagonal Sticker for the Package
#' 

foraminifera <- png::readPNG(here::here("inst", "foraminifera.png"))

p <- ggplot2::ggplot() + 
  rphylopic::add_phylopic(foraminifera, alpha = 1, x = 0.3, y = 0.5, ysize = 1.2, 
                          col = "white") +
  ggplot2::theme_void() + 
  ggpubr::theme_transparent()


hexSticker::sticker(
  
  subplot   = p,
  package   = "forcis",
  filename  = here::here("man", "figures", "hexsticker.png"),
  dpi       = 600,
  
  p_size    = 36.0,         # Title
  u_size    =  6.0,         # URL
  p_family  = "Aller_Rg",
  
  p_color   = "#FFFFFF",   # Title
  h_fill    = "#CA3833",   # Background
  h_color   = "#972B28",   # Border
  u_color   = "#FFFFFF",   # URL
  
  p_x       = 1.00,        # Title
  p_y       = 0.58,        # Title
  s_x       = 0.98,        # Subplot
  s_y       = 1.20,        # Subplot
  
  s_width   = 1.40,        # Subplot
  s_height  = 1.10,        # Subplot
  
  url       = "https://frbcesab.github.io/forcis",
  
  spotlight = TRUE,
  l_alpha   = 0.10,
  l_x       = 0.75,
  l_y       = 1.5,
  l_width   = 2,
  l_height  = 2
)
