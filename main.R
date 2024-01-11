## Welcome!

## This is your project's main script file and together with
## manuscript.Rmd it provides and entry point for you and other people
## coming to the project. The code in this file should give an outline
## of the different steps conducted in your study, from importing data
## to producing results.

## Feel free to remove this introductory text as you get started.

## Before you start writing code you need to install some packages.
## Run the following code to install the packages you need:
install.packages(devtools)
devtools::install_github("martingerdin/noacsr")
devtools::install_github("martingerdin/rofi")

## You can remove the three lines above once the packages have been
## installed, or put ## in front of each line to comment them out, if
## you want to keep them for future reference.

## Load packages
library(dotenv)
library(noacsr)
library(rofi)
noacsr::source_all_functions()

## Import data
data <- import_data(test = TRUE)

## Merge data
merged.data <- merge_data(data)

## Add outcome variable ofi
merged.data$ofi <- create_ofi(merged.data)

## This file should be relatively short, and most of the heavy
## lifting should be done by specialised functions. These functions
## live in the folder functions/ and you create a new function using
## create_function().

## Source all functions (if you tick the box "Source on save" in
## RStudio functions will be automatically sourced when you save
## them). They all need to be sourced however when you compile your
## manuscript file or run this file as a job, as that happens in a
## clean R session.
