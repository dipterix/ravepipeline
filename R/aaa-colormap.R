preview_color <- function(name, colors) {
  # colors <- seeds[[name]]
  n_colors <- length(colors)
  pad_v <- "#FFFFFF00"
  if (n_colors < 25) {
    raster_img <- matrix(
      c(colors, rep(pad_v, 25 - n_colors)),
      byrow = TRUE, ncol = 5
    )
    nr <- 5
    nc <- 5
  } else {
    nr <- 5
    nc <- ceiling(n_colors / 5)
    raster_img <- matrix(c(colors, rep(pad_v, nc * nr - n_colors)), byrow = TRUE, ncol = nc)
  }

  plot(c(0, nc), c(0, nr), type = "n", xlab = "", ylab = "",
       axes = FALSE, main = sprintf("%s [n=%d]", name, length(colors)))
  graphics::rasterImage(image = raster_img, interpolate = FALSE,
                        xleft = 0, xright = nc, ytop = nr, ybottom = 0)
}

colormap_factory <- function(seeds, min_n = NA) {

  force(seeds)
  force(min_n)

  function(name, seed_colors = NULL, preview = missing(name), error_if_missing = FALSE) {
    if (missing(name)) {
      if (preview) {
        utils::str(seeds)

        oldpar <- graphics::par(mfrow = grDevices::n2mfrow(length(seeds)),
                                mar = c(0.1, 0.1, 2.6, 0.1))
        on.exit({ graphics::par(oldpar) })

        # visualize
        for (seed_name in names(seeds)) {
          preview_color(seed_name, seeds[[seed_name]])
        }

      }
      return(invisible(seeds))
    }

    # `seed_colors` applies to this call only: registering it would rewrite the
    # shared table for every later caller in the session. Only palettes that
    # came from the table are cached back into it.
    cacheable <- !length(seed_colors)

    if (cacheable) {
      seed_names <- names(seeds)
      if (!isTRUE(name %in% seed_names)) {
        err_msg <- paste0(
          "Invalid colormap name: `", name, "`... ",
          "Available choices are ", paste(seed_names, collapse = ", "), ". ",
          "Returning the default colors"
        )

        if (error_if_missing) {
          stop(err_msg)
        }

        logger(err_msg, level = "warning")
        name <- "default"
      }
      colors <- seeds[[name]]
    } else {
      colors <- seed_colors
    }

    if (isTRUE(min_n > 0) && length(colors) < min_n) {
      space <- attr(colors, "space") %OF% c("rgb", "Lab")
      interpolate <- attr(colors, "interpolate") %OF% c("linear", "spline")
      colors <- grDevices::colorRampPalette(colors, space = space, interpolate = interpolate)(min_n)
      attr(colors, "space") <- space
      attr(colors, "interpolate") <- interpolate
      # cache the interpolation so repeated reads do not re-ramp
      if (cacheable) {
        seeds[[name]] <<- colors
      }
    }
    if (preview) {
      preview_color(name, colors)
    }
    colors
  }
}

#' @title Built-in 'RAVE' color palettes
#' @description
#' Look up, preview, and interpolate the color palettes shipped with
#' \pkg{ravepipeline}. \code{DISCRETE_COLORMAPS} holds qualitative palettes,
#' intended for categorical variables such as electrode groups.
#' \code{CONTINUOUS_COLORMAPS} holds sequential and diverging palettes,
#' intended for numeric heat maps; each of its palettes is interpolated to at
#' least 515 stops the first time it is requested, and the interpolation is
#' cached for later calls.
#' @param name palette name; when missing, the entire palette table is returned
#' invisibly
#' @param seed_colors optional character vector of colors used in place of the
#' named palette. It applies to this call only: the shared palette table is
#' left unchanged
#' @param preview whether to draw the palette. The default previews the whole
#' table when \code{name} is missing, and stays quiet otherwise; pass
#' \code{FALSE} explicitly to query the table without drawing
#' @param error_if_missing whether an unknown \code{name} raises an error
#' instead of emitting a warning and falling back to \code{"default"}
#' @returns A character vector of colors in \verb{#RRGGBB} notation; when
#' \code{name} is missing, the invisible named list of every palette
#' @examples
#'
#' names(DISCRETE_COLORMAPS(preview = FALSE))
#'
#' DISCRETE_COLORMAPS("tab10")
#'
#' length(CONTINUOUS_COLORMAPS("viridis"))
#'
#' # ad-hoc colors, interpolated but not registered
#' length(CONTINUOUS_COLORMAPS("scratch", seed_colors = c("#000000", "#FFFFFF")))
#'
#' if(interactive()) {
#'   DISCRETE_COLORMAPS("Set1", preview = TRUE)
#' }
#'
#' @name rave-colormaps
NULL

#' @rdname rave-colormaps
#' @export
DISCRETE_COLORMAPS <- colormap_factory(list(
  # RAVE's default
  "default" = c("#FFA500", "#1874CD", "#006400", "#FF4500", "#A52A2A",
                "#7D26CD", "#FE00FA", "#16FF32", "#FBE426", "#B00068",
                "#1CFFCE", "#90AD1C", "#2ED9FF", "#DEA0FD", "#F8A19F",
                "#325A9B", "#C4451C", "#1C8356", "#85660D", "#B10DA1",
                "#1CBE4F", "#F7E1A0", "#C075A6", "#AAF400", "#BDCDFF",
                "#822E1C", "#B5EFB5", "#7ED7D1", "#1C7F93", "#3B00FB"),

  "BeautifulField" = c("orange", "dodgerblue3", "darkgreen", "orangered", "brown",
                       "purple3"),

  Perm4_0 = c("#2297E6", "#F5C710", "#61D04F", "#DF536B", "#CD0BBC"),

  Accent = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0",
             "#f0027f", "#bf5b17", "#666666"),

  Dark2 = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
            "#e6ab02", "#a6761d", "#666666"),

  Paired = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
             "#e31a1c", "#fdbf6f", "#ff7f00"),

  Pastel1 = c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6",
              "#ffffcc", "#e5d8bd", "#fddaec"),

  Pastel2 = c("#b3e2cd", "#fdcdac", "#cbd5e8", "#f4cae4", "#e6f5c9",
              "#fff2ae", "#f1e2cc", "#cccccc"),

  Set1 = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
           "#ffff33", "#a65628", "#f781bf"),

  Set2 = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854",
           "#ffd92f", "#e5c494", "#b3b3b3"),

  Set3 = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3",
           "#fdb462", "#b3de69", "#fccde5"),

  "tab10" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
              "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),

  "tab20" = c("#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c",
              "#98df8a", "#d62728", "#ff9896", "#9467bd", "#c5b0d5",
              "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#7f7f7f",
              "#c7c7c7", "#bcbd22", "#dbdb8d", "#17becf", "#9edae5"),

  "tab20b" = c("#393b79", "#5254a3", "#6b6ecf", "#9c9ede", "#637939",
               "#8ca252", "#b5cf6b", "#cedb9c", "#8c6d31", "#bd9e39",
               "#e7ba52", "#e7cb94", "#843c39", "#ad494a", "#d6616b",
               "#e7969c", "#7b4173", "#a55194", "#ce6dbd", "#de9ed6"),

  "tab20c" = c("#3182bd", "#6baed6", "#9ecae1", "#c6dbef", "#e6550d",
               "#fd8d3c", "#fdae6b", "#fdd0a2", "#31a354", "#74c476",
               "#a1d99b", "#c7e9c0", "#756bb1", "#9e9ac8", "#bcbddc",
               "#dadaeb", "#636363", "#969696", "#bdbdbd", "#d9d9d9"),

  "tab22" = c("#4682B4", "#CD3E4E", "#781286", "#C43AFA", "#009400",
              "#DCF8A4", "#E69422", "#00760E", "#7ABADC", "#EC0DB0",
              "#0C30FF", "#CCB68E", "#2ACCA4", "#779FB0", "#DCD814",
              "#67FFFF", "#FFA500", "#A52A2A", "#00C8C8", "#C846FF",
              "#EAA91E", "#FAFF32"),

  "tab25" = c("#196428", "#7D64A0", "#641900", "#DC1464", "#DC140A",
              "#B4DC8C", "#DC3CDC", "#B42878", "#8C148C", "#141E8C",
              "#234B32", "#E18C8C", "#C8234B", "#A06432", "#14DC3C",
              "#DCB48C", "#146432", "#DC3C14", "#78643C", "#DCB4DC",
              "#3C14DC", "#A08CB4", "#50148C", "#14DCA0", "#14B48C",
              "#8CDCDC", "#50A014", "#640064", "#464646", "#FFC020"),

  # Discrete but also continuous...
  "turbo20" = c("#3d358b", "#4456c7", "#4778f0", "#4196ff", "#2eb4f2",
                "#1bd0d5", "#1ae4b6", "#35f394", "#61fc6c", "#8fff49",
                "#b4f836", "#d2e935", "#ebd339", "#faba39", "#fe9b2d",
                "#f9781e", "#ed5510", "#dc3b07", "#c12302", "#a11201"),

  # same as turbo20, NOT inherently discrete
  "spectral30" = c("#41004b", "#7a008b", "#850096", "#4300a2", "#0000b1",
                   "#0000d1", "#002fdd", "#0078dd", "#008ddd", "#009ecf",
                   "#00a8af", "#00aa95", "#00a668", "#009b13", "#00aa00",
                   "#00bf00", "#00d400", "#00ea00", "#00ff00", "#75ff00",
                   "#ccf900", "#ecef00", "#f8da00", "#ffc100", "#ffa100",
                   "#ff5100", "#fc0000", "#e60000", "#d60000", "#cc0c0c")
))


#' @rdname rave-colormaps
#' @export
CONTINUOUS_COLORMAPS <- colormap_factory(list(

  # RAVE's builtin

  # default	Lab	linear
  default = structure(
    c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
      "#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
    space = "Lab"
  ),

  BlueWhiteRed = structure(
    c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
      "#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
    space = "Lab"
  ),

  BlueGrayRed = structure(
    c("#053061", "#2166ac", "#4393c3", "#92c5de", "#b4b4b4",
      "#f4a582", "#d6604d", "#b2182b", "#67001f"),
    space = "Lab"
  ),

  Spectral = structure(
    c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598",
      "#ffffbf", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142"),
    space = "Lab"
  ),

  BrownWhiteGreen = structure(
    c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3",
      "#f5f5f5", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"),
    space = "Lab"
  ),

  PinkWhiteGreen = structure(
    c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef",
      "#f7f7f7", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419"),
    space = "Lab"
  ),

  PurpleWhiteGreen = structure(
    c("#40004b", "#762a83", "#9970ab", "#c2a5cf", "#e7d4e8",
                         "#f7f7f7", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837", "#00441b"),
    space = "Lab"
  ),

  OrangeWhitePurple = structure(
  c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6",
                        "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b"),
    space = "Lab"
  ),

  BlackWhiteRed = structure(
  c("#1a1a1a", "#4d4d4d", "#878787", "#bababa", "#e0e0e0",
                    "#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
    space = "Lab"
  ),

  BlueYellowRed = structure(
  c("#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8",
                    "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"),
    space = "Lab"
  ),

  GreenYellowRed = structure(
  c("#006837", "#1a9850", "#66bd63", "#a6d96a", "#d9ef8b",
                     "#ffffbf", "#fee08b", "#fdae61", "#f46d43", "#d73027", "#a50026"),
    space = "Lab"
  ),

  # 9 stops sampled, max dE 3.4
  viridis = structure(
  c("#440154", "#472d7b", "#3b528b", "#2c728e", "#21918c",
              "#28ae80", "#5ec962", "#addc30", "#fde725"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 7 stops sampled, max dE 4.1
  plasma = structure(
  c("#0d0887", "#5c01a6", "#9c179e", "#cc4778", "#ed7953",
             "#fdb42f", "#f0f921"),
    space = "rgb",
    interpolate = "spline"
  ),

  # 13 stops sampled, max dE 4.5
  inferno = structure(
  c("#000004", "#110a30", "#320a5e", "#57106e", "#781c6d",
              "#9a2865", "#bc3754", "#d84c3e", "#ed6925", "#f98e09",
              "#fbb61a", "#f4df53", "#fcffa4"),
    space = "rgb",
    interpolate = "spline"
  ),

  # 11 stops sampled, max dE 4.4
  magma = structure(
  c("#000004", "#140e36", "#3b0f70", "#641a80", "#8c2981",
            "#b73779", "#de4968", "#f7705c", "#fe9f6d", "#fecf92", "#fcfdbf"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 11 stops sampled, max dE 4.6
  cividis = c("#00224e", "#083370", "#35456c", "#4f576c", "#666970",
              "#7d7c78", "#948e77", "#aea371", "#c8b866", "#e5cf52", "#fee838"),

  # 5 stops sampled, max dE 4.5
  coolwarm = structure(
  c("#3b4cc0", "#8db0fe", "#dddcdc", "#f4987a", "#b40426"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 3 exact upstream stops
  bwr = c("#0000ff", "#fffefe", "#ff0000"),

  # 5 exact upstream stops
  seismic = c("#00004c", "#0101ff", "#fffdfd", "#fe0000", "#800000"),

  # 9 exact upstream stops
  Greys = c("#ffffff", "#f0f0f0", "#d9d9d9", "#bdbdbd", "#959595",
            "#727272", "#515151", "#242424", "#000000"),

  # 9 exact upstream stops
  Purples = c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8",
              "#807cba", "#6950a3", "#53268f", "#3f007d"),

  # 9 exact upstream stops
  Blues = c("#f7fbff", "#deebf7", "#c6dbef", "#9dcae1", "#6aaed6",
            "#4191c6", "#2070b4", "#08509b", "#08306b"),

  # 9 exact upstream stops
  Greens = c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a0d99b", "#73c476",
             "#40aa5d", "#228a44", "#006c2c", "#00441b"),

  # 9 exact upstream stops
  Oranges = c("#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6a", "#fd8c3b",
              "#f16813", "#d84801", "#a53603", "#7f2704"),

  # 9 exact upstream stops
  Reds = c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb694a",
           "#ee3a2c", "#ca181d", "#a30f15", "#67000d"),

  # 9 exact upstream stops
  YlOrBr = c("#ffffe5", "#fff7bc", "#fee390", "#fec34f", "#fe9829",
             "#eb6f14", "#cb4b02", "#983404", "#662506"),

  # 9 exact upstream stops
  YlOrRd = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8c3c",
             "#fc4d2a", "#e2191c", "#bb0026", "#800026"),

  # 9 exact upstream stops
  OrRd = c("#fff7ec", "#fee8c8", "#fdd49e", "#fdba83", "#fc8c59",
           "#ef6447", "#d62f1e", "#b20000", "#7f0000"),

  # 9 exact upstream stops
  PuRd = c("#f7f4f9", "#e7e1ef", "#d4b9da", "#c993c7", "#df64af",
           "#e72989", "#cd1256", "#970042", "#67001f"),

  # 9 exact upstream stops
  RdPu = c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9eb5", "#f767a1",
           "#dc3397", "#ad017e", "#790177", "#49006a"),

  # 9 exact upstream stops
  BuPu = c("#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda", "#8c95c6",
           "#8c6ab1", "#88409c", "#800f7b", "#4d004b"),

  # 9 exact upstream stops
  GnBu = c("#f7fcf0", "#e0f3db", "#ccebc5", "#a7ddb5", "#7accc4",
           "#4db2d3", "#2a8bbe", "#0867ab", "#084081"),

  # 9 exact upstream stops
  PuBu = c("#fff7fb", "#ece7f2", "#d0d1e6", "#a5bddb", "#73a9cf",
           "#358fc0", "#056faf", "#04598c", "#023858"),

  # 9 exact upstream stops
  YlGnBu = c("#ffffd9", "#edf8b1", "#c6e9b4", "#7ecdbb", "#40b5c4",
             "#1d90c0", "#225da8", "#243392", "#081d58"),

  # 9 exact upstream stops
  PuBuGn = c("#fff7fb", "#ece2f0", "#d0d1e6", "#a5bddb", "#66a9cf",
             "#3590bf", "#028189", "#016b58", "#014636"),

  # 9 exact upstream stops
  BuGn = c("#f7fcfd", "#e5f5f9", "#ccece6", "#98d8c9", "#65c2a3",
           "#40ad75", "#228a44", "#006c2c", "#00441b"),

  # 9 exact upstream stops
  YlGn = c("#ffffe5", "#f7fcb9", "#d9f0a3", "#acdd8e", "#77c679",
           "#40aa5c", "#228343", "#006737", "#004529"),

  # 2 exact upstream stops
  binary = c("#ffffff", "#000000"),

  # 5 stops sampled, max dE 2.1
  gist_yarg = c("#ffffff", "#bfbfbf", "#7f7f7f", "#3f3f3f", "#000000"),

  # 5 stops sampled, max dE 2.2
  gist_gray = c("#000000", "#404040", "#808080", "#c0c0c0", "#ffffff"),

  # 2 exact upstream stops
  gray = c("#000000", "#ffffff"),

  # 7 stops sampled, max dE 3.7
  bone = structure(
  c("#000000", "#252533", "#4a4a67", "#707b90", "#95aeb5",
           "#c6dada", "#ffffff"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 15 stops sampled, max dE 4.7
  pink = structure(
  c("#1e0000", "#593737", "#7b4e4e", "#956060", "#ac6f6f",
           "#c07c7c", "#c99588", "#d0ac94", "#d8bf9e", "#dfd1a7",
           "#e5e1b0", "#ececc2", "#f3f3d8", "#f9f9ed", "#ffffff"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 2 exact upstream stops
  spring = c("#ff00ff", "#ffff00"),

  # 2 exact upstream stops
  summer = c("#008066", "#ffff66"),

  # 2 exact upstream stops
  autumn = c("#ff0000", "#ffff00"),

  # 2 exact upstream stops
  winter = c("#0000ff", "#00ff80"),

  # 2 exact upstream stops
  cool = c("#00ffff", "#ff00ff"),

  # 5 exact upstream stops
  Wistia = c("#e4ff7a", "#ffe81a", "#ffbd00", "#ffa000", "#fc7f00"),

  # 17 stops sampled, max dE 3.0
  hot = structure(
  c("#0b0000", "#350000", "#5f0000", "#890000", "#b30000",
          "#dd0000", "#ff0800", "#ff3200", "#ff5c00", "#ff8600",
          "#ffb000", "#ffda00", "#ffff07", "#ffff46", "#ffff85",
          "#ffffc4", "#ffffff"),
    interpolate = "spline"
  ),

  # 9 stops sampled, max dE 4.9
  afmhot = c("#000000", "#400000", "#800000", "#c04000", "#ff8001",
             "#ffc041", "#ffff81", "#ffffc1", "#ffffff"),

  # 13 stops sampled, max dE 3.8
  gist_heat = c("#000000", "#200000", "#3f0000", "#600000", "#800000",
                "#9f0000", "#c00100", "#e02b00", "#ff5500", "#ff8103",
                "#ffab57", "#ffd5ab", "#ffffff"),

  # 7 stops sampled, max dE 3.0
  copper = structure(
  c("#000000", "#342115", "#69422a", "#9e6440", "#d28555",
             "#ffa66a", "#ffc77f"),
    interpolate = "spline"
  ),

  # 15 stops sampled, max dE 4.3
  twilight = structure(
  c("#e2d9e2", "#bccbd1", "#89adc5", "#6989be", "#5f61b4",
               "#5c359a", "#491564", "#2f1436", "#501444", "#7f2350",
               "#a54350", "#bc6b59", "#ca997c", "#d6c2b6", "#e2d9e2"),
    interpolate = "spline"
  ),

  # 15 stops sampled, max dE 4.6
  twilight_shifted = structure(
  c("#301437", "#491564", "#5c359a", "#5f61b4", "#6989be",
                       "#89adc5", "#bccbd1", "#e2d9e2", "#d6c2b6", "#ca997c",
                       "#bc6b59", "#a54350", "#7f2350", "#501444", "#2f1436"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 25 stops sampled, max dE 4.6
  hsv = c("#ff0000", "#ff3b00", "#ff7c00", "#ffbd00", "#fcf500",
          "#c5ff00", "#84ff00", "#49ff00", "#08ff00", "#00ff39",
          "#00ff74", "#00ffb5", "#00fff6", "#00cdff", "#008cff",
          "#004bff", "#0010ff", "#3100ff", "#7200ff", "#ad00ff",
          "#ee00ff", "#ff00cf", "#ff0094", "#ff0053", "#ff0018"),

  # 9 stops sampled, max dE 3.8
  berlin = c("#9eb0ff", "#519fd3", "#286886", "#14303e", "#190c09",
             "#411201", "#7d341e", "#be6f63", "#ffadad"),

  # 7 stops sampled, max dE 4.8
  managua = structure(
  c("#ffcf67", "#cc824d", "#92463b", "#572949", "#4e5593",
              "#6498ce", "#81e7ff"),
    interpolate = "spline"
  ),

  # 9 stops sampled, max dE 3.9
  vanimo = structure(
  c("#ffcdfd", "#cd78bd", "#923e80", "#401b37", "#1a1513",
             "#2a3716", "#527227", "#7fae47", "#befda5"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 13 stops sampled, max dE 2.5
  ocean = c("#008000", "#006015", "#00402a", "#002040", "#000055",
            "#00206a", "#004080", "#006095", "#0080aa", "#42a0c0",
            "#81c0d5", "#c0e0ea", "#ffffff"),

  # 21 stops sampled, max dE 1.9
  terrain = c("#333399", "#2353b9", "#1276dc", "#0098fe", "#00b2b2",
              "#01cc66", "#31d670", "#65e07a", "#99eb85", "#cdf58f",
              "#fefe98", "#e6df8b", "#ccbe7d", "#b29c6f", "#987b61",
              "#815e56", "#997c76", "#b39e99", "#cdbfbc", "#e7e0df", "#ffffff"),

  # 29 stops sampled, max dE 3.9
  gnuplot = structure(
  c("#000000", "#300038", "#44006d", "#53009d", "#6001c6",
              "#6b01e4", "#7502f8", "#8004ff", "#8806f8", "#9108e6",
              "#980cc8", "#a00fa0", "#a71470", "#ad193b", "#b52000",
              "#bb2800", "#c13000", "#c73900", "#cc4400", "#d25000",
              "#d75d00", "#dd6d00", "#e27d00", "#e78e00", "#eca200",
              "#f1b600", "#f6cd00", "#fae500", "#ffff00"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 31 stops sampled, max dE 5.0
  gnuplot2 = c("#000000", "#000020", "#000044", "#000064", "#000088",
               "#0000a8", "#0000cc", "#0000ec", "#0d00ff", "#2600ff",
               "#4200ff", "#5b00ff", "#7800ff", "#9106f9", "#ad18e7",
               "#c92ad5", "#e23ac5", "#fe4cb3", "#ff5ca3", "#ff6e91",
               "#ff7e81", "#ff906f", "#ffa05f", "#ffb24d", "#ffc23d",
               "#ffd42b", "#ffe41b", "#fff609", "#ffff2a", "#ffff9b", "#ffffff"),

  # 9 exact upstream stops
  CMRmap = c("#000000", "#262680", "#4d26bf", "#9a337e", "#ff4126",
             "#e68100", "#e6c01c", "#e6e683", "#ffffff"),

  # 13 stops sampled, max dE 4.1
  cubehelix = structure(
  c("#000000", "#181027", "#192d48", "#16534c", "#2b6f39",
                "#607a2f", "#a1794a", "#cc7c86", "#d490c6", "#c6b4ee",
                "#c3d9f3", "#d9f2ef", "#ffffff"),
    interpolate = "spline"
  ),

  # 3 exact upstream stops
  brg = c("#0000ff", "#fe0100", "#00ff00"),

  # 25 stops sampled, max dE 4.0
  gist_rainbow = c("#ff0029", "#ff0d00", "#ff4800", "#ff8400", "#ffba00",
                   "#fff500", "#cdff00", "#97ff00", "#5cff00", "#20ff00",
                   "#00ff16", "#00ff51", "#00ff8c", "#00ffc2", "#00fffd",
                   "#00c6ff", "#008fff", "#0053ff", "#0018ff", "#1f00ff",
                   "#5a00ff", "#9600ff", "#cd00ff", "#ff00f6", "#ff00bf"),

  # 13 stops sampled, max dE 4.9
  rainbow = c("#8000ff", "#5641fd", "#2c7ef7", "#00b5eb", "#2adddd",
              "#54f6cb", "#80ffb4", "#abf69b", "#d4dd80", "#ffb360",
              "#ff7e41", "#ff4121", "#ff0000"),

  # 25 stops sampled, max dE 5.0
  jet = structure(
  c("#000080", "#0000ad", "#0000df", "#0000ff", "#0028ff",
          "#0054ff", "#0080ff", "#00a8ff", "#00d4ff", "#16ffe1",
          "#36ffc1", "#5aff9d", "#7dff7a", "#9dff5a", "#c1ff36",
          "#e4ff13", "#ffe600", "#ffbd00", "#ff9400", "#ff6f00",
          "#ff4700", "#ff1e00", "#df0000", "#ad0000", "#800000"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 15 stops sampled, max dE 4.7
  turbo = structure(
  c("#30123b", "#4146ac", "#4776ee", "#3aa3fc", "#1bd0d5",
            "#25eca7", "#61fc6c", "#a4fc3c", "#d2e935", "#f4c73a",
            "#fe9b2d", "#f36315", "#da3907", "#b21a01", "#7a0403"),
    space = "Lab",
    interpolate = "spline"
  ),

  # 21 exact upstream stops
  nipy_spectral = c("#000000", "#700080", "#870098", "#0300aa", "#0000dd",
                    "#0078dd", "#0098dd", "#00aaab", "#00aa88", "#009a00",
                    "#00bc00", "#00dc00", "#00ff00", "#bcff00", "#efed00",
                    "#ffc900", "#ff9900", "#fe0000", "#dc0000", "#cc0c0c", "#cccccc")

  # At least 2 x 256 colors for 256 color screen
), min_n = 515)
