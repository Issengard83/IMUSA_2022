#### LOAD PACKAGES ####
pacman::p_load(samplingbook)

### Population of the city of Santa Fe (2022) ###
pop = 405000

### SAMPLE SIZE CALCULATION (DOGS) ####
# Estimated number of dogs in Santa Fe
dog = pop*0.17

# Seroprevalence of leptospiral antibodies of dogs in the province of 
# Santa Fe (Seghesso Zabala et al., 2013)
prev1 = 22/156

# Precision 5%
s5 = sample.size.prop(e = .05, P = prev1, N = dog)

# Precision 7.5%
s75 = sample.size.prop(e = .075, P = prev1, N = dog)

# Precision 10%
s10 = sample.size.prop(e = .1, P = prev1, N = dog)

# Add non-response rate of 15%
dogs = round(s75$n + s75$n*.15)

### SAMPLE SIZE CALCULATION (CATS) ####
# Estimated number of cats in Santa Fe
cat = pop*0.06

# Seroprevalence of leptospiral antibodies of cats in the province of 
# Santa Fe (Francois et al., 2020)
prev2 = 6/160

# Precision 5%
s5 = sample.size.prop(e = .05, P = prev2, N = cat)

# Precision 7.5%
s75 = sample.size.prop(e = .075, P = prev2, N = cat)

# Precision 10%
s10 = sample.size.prop(e = .1, P = prev2, N = cat)

# Add non-response rate of 15%
cats = round(s75$n + s75$n*.15)
