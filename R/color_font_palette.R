color_font_palette = function(){
  #' Function for making custom reports with NPC fonts and colors
  #'
  #'
  #' @return list of element c which contains hex color codes and element f which contains ftext fonts
  #' @export

library(officer)

# colors ----
c = list(
  # npc offical colors ----
  npc_official_purple = "#800080",
  npc_official_lightblue = "#1DCAD3",
  npc_official_blue = "#00205C",
  npc_official_orange = "#D3451C",

  # table colors ----
  darkblue  = "#002060",
  lightblue  = "#c0e6f5",
  lightblue2 = "#44b3e1",
  lightblue3 = "#1ccad4",
  lightblue4 = "#4C729D",

  # font colors ----
  font_color = "#404040",
  blue_font = "#003057" ,
  purple_font = "#762F71",
  footnote_font_color = "#7F7F7F"
)

  # fonts ----
  f = list(

    h1_format = fp_text(
      color = c$blue_font,
      font.size = 26,
      bold = TRUE,
      font.family = "Corbel"
    ),

    h2_format = fp_text(
      color = c$blue_font,
      font.size = 16,
      bold = TRUE,
      font.family = "Corbel"
    ),

    h3_format = fp_text(
      color = c$blue_font,
      font.size = 14,
      bold = TRUE,
      font.family = "Corbel"
    ),

    body_format = fp_text(
      color = c$font_color,
      font.size = 12,
      font.family = "Calibri"
    ),

    footer_format = fp_text(
      color = "#000000",
      font.size = 9,
      font.family = "Calibri"
    ),

    hyperlink_format = fp_text(
      font.size = 12,
      color = "#467886",
      font.family = "Calibri",
      underlined = TRUE
    ),

    superscript_format = fp_text(
      font.size = 12,
      color = c$blue_font,
      font.family = "Calibri",
      vertical.align = "superscript"
    ),

    body_italic_format = fp_text(
      color = c$font_color,
      font.size = 12,
      font.family = "Calibri",
      italic = TRUE
    )
  )

npc = list("c" = c,"f" = f)
return(npc)
}
