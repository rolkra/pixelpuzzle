# pixelpuzzle

Puzzle game for the R console. Restore the pixel art by shifting rows!

<img src="man/figures/pixelpuzzle-allstars.png" alt="example pixel art" width="800">

## Installation

CRAN

```r
install.packages("pixelpuzzle")
```

Github

```r
# install from github
if (!require(devtools)) install.packages("devtools")
devtools::install_github("rolkra/pixelpuzzle")
```

# Start Game

```r
library(pixelpuzzle)
pixelpuzzle()
```

# Pixel Art

{pixelpuzzle} comes with predefined pixel art you can play with. Just start the game with `pixelpuzzle()` to select one.

In case you want to define your own pixel art you can use the following color codes for each pixel:

* R = Red
* M = Magenta
* B = Blue
* C = Cyan
* G = Green
* Y = Yellow
* W = White
* S = Silver
* X = Black
* . = No color (transparent)

```r
library(pixelpuzzle)

sprite <- c(
  "RRRRRRRRR",
  "MMMMMMMMR",
  "BBBBBBBMR",
  "CCCCCCBMR",
  "GGGGGCBMR",
  "YYYYGCBMR",
  "WWWYGCBMR",
  "SSWYGCBMR",
  "XSWYGCBMR"
)

show_sprite(sprite)
```

<img src="man/figures/pixelpuzzle-defsprite.png" alt="define pixel art" width="200">

You can use this pixel art by passing it as parameter to `pixelpuzzle()`

```r
pixelpuzzle(sprite)
```
