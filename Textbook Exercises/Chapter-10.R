# Chapter 10 — Scripts, algorithms and functions


# 10.2 Scripts ------------------------------------------------------------

poly_mat = cbind(
        x = c(0, 0, 9, 9, 0),
        y = c(0, 9, 9, 0, 0)
)
source("https://git.io/10-centroid-alg.R") # short url


# 10.4 Functions -----------------------------------------------

# Sample function
t_centroid = function(x) {
        (x[1, ] + x[2, ] + x[3, ]) / 3
}

# Triangle Area function
t_area = function(x) {
        abs(
                x[1, 1] * (x[2, 2] - x[3, 2]) +
                        x[2, 1] * (x[3, 2] - x[1, 2]) +
                        x[3, 1] * (x[1, 2] - x[2, 2])
        ) / 2
}

# Testing the Traingle Area Function =
t_new = cbind(x = c(0, 3, 3, 0),
              y = c(0, 0, 1, 0))
t_area(t_new)

# Creating the poly_centroid() function
t_centroid = function(x) {
        (x[1, ] + x[2, ] + x[3, ]) / 3
}

t_area = function(x) {
        abs(
                x[1, 1] * (x[2, 2] - x[3, 2]) +
                        x[2, 1] * (x[3, 2] - x[1, 2]) +
                        x[3, 1] * (x[1, 2] - x[2, 2])
        ) / 2
}

poly_centroid = function(x) {
        i = 2:(nrow(x) - 2)
        T_all = lapply(i, function(x) {
                rbind(Origin, poly_mat[x:(x + 1), ], Origin)
        })
        C_list = lapply(T_all, t_centroid)
        C = do.call(rbind, C_list)
        A = vapply(T_all, t_area, FUN.VALUE = double(1))
        c(weighted.mean(C[, 1], A), weighted.mean(C[, 2], A))
}

# Weekly Exercise ---------------------------------------------------------


# Script Part -------------------------------------------------------------

# Aim: To take the existing polt_centroid() function and make it "sf" type stable 

# Step 0 — Sourcing the required libraries
library(sf)
library(dplyr)

# Step 1 — Define poly_centroid() and its dependent functions t_centroid() and t_area()
t_centroid = function(x) {
        (x[1, ] + x[2, ] + x[3, ]) / 3
}

t_area = function(x) {
        abs(
                x[1, 1] * (x[2, 2] - x[3, 2]) +
                        x[2, 1] * (x[3, 2] - x[1, 2]) +
                        x[3, 1] * (x[1, 2] - x[2, 2])
        ) / 2
}

poly_centroid = function(x) {
        i = 2:(nrow(x) - 2)
        Origin = poly_mat[1, ]
        
        T_all = lapply(i, function(x) {
                rbind(Origin, poly_mat[x:(x + 1), ], Origin)
        })
        C_list = lapply(T_all, t_centroid)
        C = do.call(rbind, C_list)
        A = vapply(T_all, t_area, FUN.VALUE = double(1))
        c(weighted.mean(C[, 1], A), weighted.mean(C[, 2], A))
}

# Step 2 — Define poly_centroid_sf() by wrapping poly_centroid()
poly_centroid_sf = function(x) {
        st_coordinates(x) %>%
        poly_centroid() %>%
        st_point() %>%
        st_sfc() %>%
        st_sf()
}

# Testing Part -------------------------------------------------------------
# Define 'poly_sfc' and 'poly_mat' test data
poly_mat = cbind(
        x = c(0, 0, 9, 9, 0),
        y = c(0, 9, 9, 0, 0)
)
poly_sfc = sf::st_polygon(list(poly_mat))

# Running poly_centroid_sf()
Result1 = poly_centroid_sf(sf::st_sf(sf::st_sfc(poly_sfc)))
class(Result1)    # To confirm that it returns an 'sf' object

Result2 = poly_centroid_sf(poly_mat)      # To confirm that only 'sf' inputs are valid