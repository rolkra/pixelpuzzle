#' Show a sprite in console
#'
#' @param img Image of Sprite (vector of strings)
#' @param rownumbers Show rownumbers on the left side?
#' @param cls Clear Screen before drawing sprite?
#' @param bg Background color, default is transparent (".")
#' @import cli
#' @importFrom stringr str_replace_all
#' @return Prints sprite in console 
#' @export
#' @examples 
#' @examples 
#' sprites <- define_sprite()
#' 
#' ## show first sprite
#' show_sprite(sprites[[1]])
#' ## show sprite named "alien"
#' show_sprite(sprites[["sword"]])


show_sprite <- function(img, rownumbers = TRUE, cls = TRUE, bg = ".") {
  
  # row numbers
  numbers <- format(1:20, digits = 2)
  numbers <- numbers[1:length(img)]
  
  # characters
  pixel <- "\U2589" # "\U2589"
  space <- "\U2001"
  
  # set background?
  if(bg != ".") {
    img <- str_replace_all(img, c("[.]" = bg))
  }
  
  # create pixels
  img <- str_replace_all(img, c(
    "G" = col_green(pixel),
    "B" = col_blue(pixel),
    "R" = col_red(pixel),
    "S" = col_silver(pixel),
    "Y" = col_yellow(pixel),
    "M" = col_magenta(pixel),
    "C" = col_cyan(pixel),
    "W" = col_white(pixel),
    "X" = col_black(pixel),
    "[.]" = space,
    " " = space))

  # add row numbers
  if (rownumbers) {
    img <- paste(numbers, img)
  }
  
  # show image
  if (cls)  {
    cat("\014")    
  }
  cat(paste(img, collapse = "\n"))  
        
} # show_sprite


shift_sprite_row <- function(img, row = 1, shift = 1) {
  
  line <- img[row]
  n <- nchar(line)
  img[row] <- str_c(substr(line, n-shift+1, n), substr(line, 1, n-shift))
  img
} # shift_sprite_row


#' Returns a list of predefined sprites
#'
#' @return List of sprites 
#' @export
#' @examples 
#' sprites <- define_sprite()
#' 
#' ## show first sprite
#' show_sprite(sprites[[1]])
#' ## show sprite named "alien"
#' show_sprite(sprites[["sword"]])

define_sprite <- function() {

  sprites <- list(
    
  alien = c(
         "..Y.....Y...",
         "...Y...Y....",
         "..YYYYYYY...",
         ".YY.YYY.YY..",
         "YYYYYYYYYYY.",
         "Y.YYYYYYY.Y.",
         "Y.Y.....Y.Y.",
         "...YY.YY...."),

  ghost = c(
         ".....RRRR.....",
         "...RRRRRRRR...",
         "..RRRRRRRRRR..",
         ".RRRWWRRRRWWR.",
         ".RRWWWWRRWWWW.",
         ".RRWWBBRRWWBB.",
         ".RRWWBBRRWWBBR",
         "RRRRWWRRRRWWRR",
         "RRRRRRRRRRRRRR",
         "RRRRRRRRRRRRRR",
         "RRRRRRRRRRRRRR",
         "RRRRRRRRRRRRRR",
         "RR.RRR..RRR.RR",
         "R...RR..RR...R"),

  mario = c(
         "...RRRRRR....",
         "..RRRRRRRRRR.",
         "..SSSYYYSY...",
         ".SYSYYYYSYYY.",
         ".SYSSYYYYSYYY",
         ".SSYYYYYSSSS.",
         "...YYYYYYYY..",
         "..RRBRRRR....",
         ".RRRBRRBRRR..",
         "RRRRBBBBRRRR.",
         "YYRBYBBYBRYY.",
         "YYYBBBBBBYYY.",
         "YYBBBBBBBBYY.",
         "..BBB..BBB...",
         ".SSS....SSS..",
         "SSSS....SSSS."),

  luigi = c(
    "....GGGGGG...",
    ".GGGGGGGGGG..",
    "...YSYYYSSS..",
    ".YYYSYYYYSYS.",
    "YYYSYYYYSSYS.",
    ".SSSSYYYYYSS.",
    "..YYYYYYYY...",
    "....GGGGBGG..",
    "..GGGBGGBGGG.",
    ".GGGGBBBBGGGG",
    ".YYGBYBBYBGYY",
    ".YYYBBBBBBYYY",
    ".YYBBBBBBBBYY",
    "...BBB..BBB..",
    "..SSS....SSS.",
    ".SSSS....SSSS"),

  buhu = c(
         ".....BBBB.......",
         "...BBWWWWWBB....",
         "..BWWWWWWWWWB...",
         ".BWWWWWWWWWWWB..",
         ".BWBWBWWWWWWWB..",
         "BWWBWBWWWWBBWWB.",
         "BWWWWWWWWBWWBWB.",
         "BWRWRWRWWWWWBWB.",
         "BWRRRRRWWWWWBWWB",
         "BWRRRRRWWWWWWWWB",
         ".BWRRRRRWWWWWWWB",
         ".BWRWRWRWWWWWWB.",
         "..BWWWWWWWWWBB..",
         "...BBBWWWWBB....",
         "......BBBB......"),
  
  mushroom = c(
    ".....SSSSSS.....",
    "...SSWWGGGGSS...",
    "..SWWWWGGGGWWS..",
    ".SWWWWGGGGGGWWS.",
    ".SWWWGGWWWWGGWS.",
    "SGGGGGWWWWWWGGGS",
    "SGWWGGWWWWWWGGGS",
    "SWWWWGWWWWWWGGWS",
    "SWWWWGGWWWWGGWWS",
    "SGWWGGGGGGGGGWWS",
    "SGGGSSSSSSSSGGWS",
    ".SSSWWXWWXWWSSS.",
    "..SWWWXWWXWWWS..",
    "..SWWWWWWWWWWS..",
    "...SWWWWWWWWS...",
    "....SSSSSSSS...."),
  
  yoshi = c(
         ".....SS..SS.....",
         "....SGGSSGGS....",
         "...SGWWWWWWGS...",
         "...SWWXWWXWWS...",
         "...SWWXWWXWWS...",
         "..SSWGGGGGGWSS..",
         ".SGGSSSSSSSSGGS.",
         ".SGSGGGGGGGGSGS.",
         "SGSGGGSGGSGGGSGS",
         "SGSGGGGGGGGGGSGS",
         "SWSGGGGGGGGGGSWS",
         "SWSGGGGGGGGGGSWS",
         ".SSGGGGGGGGGGSS.",
         "...SGGGGGGGGS...",
         "....SGGGGGGS....",
         ".....SSSSSS....."),
  
  kerby = c(
         "..BB.BBBBB......",
         ".BWWBWWWWWBB....",
         "BWWBWWWWWWWWB...",
         "BWWWWWBWBWWWWB..",
         "BWWWWWBWBWWWWB..",
         "BWWWWWBWBWWWWWB.",
         "BWWWRRWWWRRWWWWB",
         "BWWWWWWBWWWWWWWB",
         ".BWWWWWBWWWWWWWB",
         ".BWWWWWWWWWWBBB.",
         ".BWWWWWWWWWBRRRB",
         "..BWWWWWWWBRRRRB",
         "..BBWWWWWWBRRRRB",
         ".BRRBBWWWBRRRRB.",
         "BRRRRRBBBBBRRB..",
         ".BBBBBB...BBB..."),
  
  sword = c(
    "XXX...............",
    "XCBX..............",
    "XBCBX.............",
    ".XBCBX............",
    "..XBCBX............",
    "...XBCBX...........",
    "....XBCBX...XX.....",
    ".....XBCBX.XSX.....",
    "......XBCBXSX......",
    ".......XBCXSX......",
    "........XXYX.......",
    ".......XSSXSX......",
    "......XSXX.XSX.....",
    "......XX....XSXX...",
    ".............XYX...",
    ".............XXX..."),
  
  pickaxe = c(
    "....XXXX....",
    "...XYYYYYXS.",
    "....XXXYYYX.",
    ".......XYYY.",
    "......XSXYYX",
    ".....XSX.XYX",
    "....XSX..XYX",
    "...XSX...XYX",
    "..XSX.....X.",
    ".XSX........",
    "XSX.........",
    "XX..........")
  
  )
  
  sprites
  
} # define_sprites

select_sprite <- function(sprites, bg = ".")  {
  
  index <- 1
  selected <- FALSE
  while(!selected) {
    
    # show sprite
    sprite <- sprites[[index]]
    show_sprite(sprite, bg = bg)
    
    # use it?
    cat("\n")
    input <- readline("use it? [y]es or [n]o: ")
    
    if (tolower(input) %in% c("q", "quit", "e", "end", "stop")) {
      selected <- TRUE
      sprite <- NA
      index <- 0
      break
    }
    
    if(input == "y")  {
      selected <- TRUE
    } else {
      selected <- FALSE
      index <- index + 1
      if(index > length(sprites)) {
        index <- 1
      }
    }
    
  } # while
  
  # return selected sprite
  sprite
  
} # select sprites


combine_sprite <- function(img, img2, gap = 1) {
  
  h1 <- length(img)
  h2 <- length(img2)
  
  if (h1 > h2) {
    empty_line <- paste0(rep(".", nchar(img2[1])), collapse = "")
    empty_lines <- rep(empty_line, h1 - h2)
    img2 <- c(empty_lines, img2)
  }
  
  if (h2 > h1) {
    empty_line <- paste0(rep(".", nchar(img[1])), collapse = "")
    empty_lines <- rep(empty_line, h2 - h1)
    img <- c(empty_lines, img)
  }
  
  img12 <- paste0(img, paste0(rep(".", gap), collapse = ""), img2)
  return(img12)
}


colorize_sprite <- function(img, colors = c("X", "S", "W", "S", "X", "ORI"), sleep = 0.1) {
  
  ori <- img
  
  for(i in seq_along(colors)) {
    
    if (colors[i] != "ORI") {    
      img <- str_replace_all(ori, "[RGBYSWXMC]", colors[i])
    } else {
      img <- ori
    }
    show_sprite(img, rownumbers = FALSE)
    Sys.sleep(sleep)         
    
  }
  
} # colorize cli sprite

shuffle_sprite <- function(img, difficulty = 1) {
  
  for(i in seq_along(1:(difficulty*2))) {
    
    row <- sample(1:length(img), 1)
    shift <- round(runif(1, 1, nchar(img[1])-1 ), 0)
    img <- shift_sprite_row(img, row = row, shift = shift)
    
  }
  
  img
}

flip_sprite <- function(img) {
  
  new <- img
  
  for (i in seq_along(img))  {
    splits <- strsplit(img[i], "")[[1]]
    reversed <- rev(splits)
    final_result <- paste(reversed, collapse = "")
    new[i] <- final_result
  }
  # return result
  new
} # flip_sprite

play_sprite <- function(img, ori, bg = ".")  {

  show_sprite(img, bg = bg)
  end <- FALSE
  last_input <- "1"
  
  while (!end) {
  
    # solved?
    solved <- round(100.0 * sum(img == ori) / length(ori),1)
    if (solved == 100) {
      cat("\nyou solved the puzzle!")
      beepr::beep("mario")
      break
    }
    
    # input
    txt <- paste0("row nr | (h)elp | (q)uit: ")
    input <- readline(txt)
    
    # help?
    if (tolower(input) == "help" | tolower(input) == "h") {
      show_sprite(img, bg = bg)
      cat(paste0("\nnot correct: ", length(ori) - sum(img == ori), " rows"))
      Sys.sleep(5)
      show_sprite(ori, bg = bg)
      Sys.sleep(1)
      show_sprite(img, bg = bg)
    }
    
    # quit?
    if (tolower(input) == "quit" | tolower(input) == "q") {
      break
    } 

    # shift row
    if (input == "") {
      input <- last_input
    }
    last_input <- input
    row <- suppressWarnings(as.integer(input))
    img <- img %>% shift_sprite_row(row)
    show_sprite(img, bg = bg)
  }
} # play 


intro <- function(sleep = 0.5) {
  
  # define PIXEL
  img <- c("RRRR.M.B...B.CCCC.G...",
           "R..R.M..B.B..C....G...",
           "RRRR.M...B...CCC..G...",
           "R....M..B.B..C....G...",
           "R....M.B...B.CCCC.GGGG")
  
  # show in different colors
  colorize_sprite(img, sleep = 0.1, 
                      colors = c("R","M","B","C","G","Y","ORI"))
  
  # add text
  cat(style_bold("\n\n     P   U   Z   Z   L   E "))
  beepr::beep("fanfare")
  cat("\n\n")
  cat("Restore the pixel art by shifting rows.\n")
  cat("enter row number to shift right\n")
  cat("<return> repeats the last command\n")

} 

#' Play pixelpuzzle in the R-console
#'
#' @param img Image of Sprite (vector of strings). If no sprite is provided,
#' the player can choose one of the predefined pixel arts.
#' @param bg Background color, default is transparent (".")
#' @return  
#' @export
#' @examples
#' ## Start game (in interactive R sessions)
#' if (interactive())  {
#'    pixelpuzzle()
#' }

pixelpuzzle <- function(img = NA, bg = ".") {

  # intro
  intro()
  
  # enter difficulty
  cat("\n")
  input <- readline("Difficulty (1-10): ")
  if (input == "") {
    input <- "1"
  }
  
  if (tolower(input) %in% c("q", "quit", "e", "end", "stop")) {
    return("you quit")
  }
  
  difficulty <- suppressWarnings(as.integer(input))
  if(is.na(difficulty)) {
    difficulty <- 1
  }
  
  # sprites
  sprites <- define_sprite()
  
  # select pixel art
  if(!any(is.na(img))) {
    img <- img
  } else {
    img <- select_sprite(sprites, bg = bg)
  }

  if (all(is.na(img))) {
    return("you quit")
  }
  
  # show original
  show_sprite(img, bg = bg)
  cat("\nget ready ...")

  # shuffle
  ori <- img
  img <- shuffle_sprite(img, difficulty)
  
  # play
  Sys.sleep(3)
  play_sprite(img, ori, bg = bg)
}