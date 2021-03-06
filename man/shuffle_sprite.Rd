% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pixelpuzzle.R
\name{shuffle_sprite}
\alias{shuffle_sprite}
\title{Shuffle a sprite randomly}
\usage{
shuffle_sprite(img, difficulty = 1)
}
\arguments{
\item{img}{Sprite}

\item{difficulty}{Difficulty (intensity of shuffeling)}
}
\value{
List of sprites
}
\description{
Shuffle a sprite randomly
}
\examples{
sprites <- define_sprite()
sprite <- sprites[["sword"]]
show_sprite(shuffle_sprite(sprite))
}
