\name{predict.eye_multinomial_transition}
\alias{predict.eye_multinomial_transition}
\title{Predict destination-state probabilities}
\usage{
\method{predict}{eye_multinomial_transition}(object, newdata = NULL,
  type = c("probability", "class", "link"), ...)
}
\arguments{
  \item{object}{Multinomial transition model.}
  \item{newdata}{Optional prepared transition data.}
  \item{type}{Probability, class, or linear predictor.}
  \item{...}{Unused.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{S3 method supporting a research-scale eyeprocess object.}
