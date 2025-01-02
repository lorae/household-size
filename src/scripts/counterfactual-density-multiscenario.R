# counterfactual-density-multiscenario.R
#
# I hate to have two scripts so similarly named, but I want to build some new counter-
# factuals without affecting the old ones. This is the newer, updated version of 
# src/counterfactual-density.R. The purpose of this script is to calculate what -
# controlling for demographic factors - average person-level household size would be
# in 2022.
#
# Rather than using all the demographic features at once, it layers them on one at
# a time. This allows us to conclude what happens with the introduction of each 
# individual layer and how robust the results are to various controls.
#
