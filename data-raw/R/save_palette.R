# Defines and stores the color palettes for the Jamaica data

require("RColorBrewer")
require("ddata")

# color_study
color_study <- c(
  brewer.pal(9, "Greens")[8],        # Bangladesh
  brewer.pal(12, "Paired")[c(5, 4)], # Brazil 1/2
  brewer.pal(12, "Paired")[c(3, 7)], # Chile 1/2
  brewer.pal(12, "Paired")[6],       # China
  brewer.pal(8,  "Dark2")[4],        # Columbia 1/2
  brewer.pal(12, "Paired")[2],       # Columbia 2
  brewer.pal(8, "Set2")[1],          # Ecuador
  brewer.pal(8, "YlOrRd")[3],        # Ethiopia
  brewer.pal(8, "Set2")[c(6, 4)],    # Jamaica 1/2
  brewer.pal(12, "Paired")[12],      # Madagascar
  brewer.pal(12, "Paired")[c(8)],    # Netherlands 1
  brewer.pal(12, "Paired")[c(1)],    # Netherlands 2
  brewer.pal(9, "BuPu")[7]           # South Africa
)
names(color_study) <-   c("Bangladesh", "Brazil 1", "Brazil 2", 
                        "Chile 1", "Chile 2", "China", 
                        "Colombia 1", "Columbia 2",
                        "Ecuador", "Ethiopia", "Jamaica 1", "Jamaica 2", 
                        "Madagascar", "Netherlands 1", "Netherlands 2", 
                        "South Africa")

# color_country
color_country <- c(
  brewer.pal(9, "Greens")[8],        # BD
  brewer.pal(12, "Paired")[4],       # BR
  brewer.pal(12, "Paired")[6],       # CL
  brewer.pal(12, "Paired")[12],      # CN
  brewer.pal(12, "Paired")[2],       # CO
  brewer.pal(8, "Set2")[6],          # EC
  brewer.pal(8, "GnBu")[6],          # ET
  brewer.pal(8, "Greys")[8],         # JM
  brewer.pal(8, "Reds")[6],          # MG
  brewer.pal(12, "Paired")[8],       # NL
  brewer.pal(9, "BuPu")[9]           # ZA
)
names(color_country) <-   c("BD", "BR", "CL", "CN", "CO", "EC",
                            "ET", "JM", "MG", "NL", "ZA")


# color_wave
color_wave <- rep(brewer.pal(4, "Set1"), 5)[1:13]
names(color_wave) <- as.character(1:13)


# color_instrument
color_instrument <- c(
  brewer.pal(12, "Paired")[10], # ASQ
  brewer.pal(12, "Paired")[7], # Barrera
  brewer.pal(12, "Paired")[12], # Battelle
  brewer.pal(6, "Blues")[3:5],  # Bayley1/2/3
  brewer.pal(12, "Paired")[6],  # Denver
  brewer.pal(12, "Paired")[8],  # Dutch
  brewer.pal(12, "Paired")[4],  # Griffith
  brewer.pal(12, "Paired")[3],  # McCarthy
  brewer.pal(12, "Paired")[9],  # Stanford
  brewer.pal(12, "Paired")[5]  # WM.Memory
)
names(color_instrument) <- c("ASQ", "Barrera", "Battelle", 
                             "Bayley1", "Bayley2", "Bayley3", 
                             "Denver", "Dutch", "Griffiths", "McCarthy", 
                             "Stanford", "WM.Memory")

# color_domain
color_domain <- c(
  brewer.pal(5, "Set2")[2],   # Cognitive
  brewer.pal(5, "Set2")[5],   # Expressive
  brewer.pal(5, "Set2")[3],   # Fine Motor
  brewer.pal(5, "Set2")[4],   # Gross Motor
  brewer.pal(5, "Set2")[1],   # Receptive
  rgb(0, 0, 0)                # NA
)
names(color_domain) <- 
  c("Cognitive", "Expressive", "Fine Motor", "Gross Motor", "Receptive", "")


# color_item (2332 color)
# Obtain list of all items in gcdg
all_items <- unique(unlist(sapply(gcdg_meta, "[", "item")))
all_items <- gtools::mixedsort(all_items)

colorblind <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")
brewpal <- brewer.pal(7, "Set2")
color_item <- rep(brewpal, length.out = length(all_items))
names(color_item) <- all_items

jamaica_palettes <- list(study = color_study,
                         country = color_country,
                         domain = color_domain,
                         instrument = color_instrument,
                         wave = color_wave,
                         item = color_item)

# save to /data
devtools::use_data(jamaica_palettes, overwrite = TRUE)
