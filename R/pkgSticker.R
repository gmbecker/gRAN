#' Create hex stickers for packages
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom hexSticker sticker
#' @param pkgName Name of the package
#' @param destination Directory where the sticker will be saved
#' @return None
#' @export
createSticker <- function(pkgName, destination) {
    pkgcrypt <- encode_string(pkgName)

    pkgFontColor <- setNames(as.list(c("#2B1B17", "#4863A0", "#348781",
                                       "#347235","#347235","#B87333","#B87333",
                                       "#F87431","#E41B17","#461B7E")), 0:9)
    pkgFillColor <- setNames(as.list(c("#E5E4E2", "#C2DFFF", "#98FF98",
                                       "#FDD7E4","#FFF5EE","#93FFE8","#FFFF00",
                                       "#FCDFFF","#CCFB5D","#FFCBA4")), 0:9)
    pkgOutlineColor <- setNames(as.list(c("#4B0082", "#000000", "#566D7E",
                                          "#1F45FC", "#008080", "#348017",
                                          "#FFD801", "#ADA96E", "#F62217",
                                          "#FF00FF")), 0:9)

    x <- as.numeric(pkgcrypt) %% length(pkgFontColor)
    y <- sum(as.numeric(unlist(strsplit(pkgcrypt, "")))) %% length(pkgFillColor)
    z <- as.numeric(substr(pkgcrypt, 1,
                           ceiling(nchar(pkgcrypt)/2))) %% length(pkgOutlineColor)

    #Determine font size for package name on sticker
    if (getOS() == "osx") {
      fontsize <- max(2, (39-nchar(pkgName))/5)
    } else if (getOS() == "linux") {
      fontsize <- max(5.5, 18 - 0.4*nchar(pkgName))
    } else {
      fontsize <- 8
    }

    # Create the sticker
    sticker(system.file2("assets", "images", "pkg.png"),
            package = pkgName,
            filename = file.path(destination, paste0(pkgName, ".png")),
            p_size = fontsize,
            s_x = 1, s_y = 0.6,
            s_width = 0.9, s_height = 0.9,
            p_color = as.character(pkgFontColor[as.character(x)]),
            h_fill = as.character(pkgFillColor[as.character(y)]),
            h_color = as.character(pkgOutlineColor[as.character(z)]))
}
