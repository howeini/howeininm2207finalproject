---
  author: "Ho Wei Ni"
date: "`r Sys.Date()`"
output:
  pdf_document: default
df_print: paged
html_document: null
title: "Week-7: Code-along"
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Slide 6

```{r, eval=TRUE,echo=TRUE}
library(tidyverse)
library(palmerpenguins)
glimpse(penguins)
```
<br>
  
### Plot recreation
  
```{r, eval=TRUE,echo=TRUE}
ggplot(data=penguins, # data layer
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     colour = species,
                     shape = species,
                     size = body_mass_g,
                     alpha = flipper_length_mm)) + # aesthetics layer
    geom_point() + # geometric layer
    labs(title = "Bill depth and length",
         subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins",
         x = "Bill depth (mm)",
         y = "Bill length (mm)", 
         colour = "Species",
         caption = "Source: Palmer Stationi LTER / palmerpenguins package") +
    scale_colour_viridis_d()
```
<br>

### Mapping VS Setting

```{r, eval=TRUE,echo=TRUE}
ggplot(penguins) + 
  aes(x = bill_depth_mm,
      y = bill_length_mm,
      size = body_mass_g,
      alpha = flipper_length_mm) + 
  geom_point()
```

```{r, eval=TRUE,echo=TRUE}
ggplot(penguins) + 
  aes(x = bill_depth_mm,
      y = bill_length_mm) + 
  geom_point(size = 2, alpha = 0.5)
```
<br>
  
### Faceting -> smaller plots that display subsets of data
  
```{r, eval=TRUE,echo=TRUE}
ggplot(penguins) + 
  aes(x = bill_depth_mm,
      y = bill_length_mm) + 
  geom_point() + 
  facet_grid(sex ~ species) # row ~ column 
```

```{r, eval=TRUE,echo=TRUE}
ggplot(penguins) + 
  aes(x = bill_depth_mm,
      y = bill_length_mm) + 
  geom_point() + 
  facet_wrap( ~ species, ncol = 2) # for one variable only
```

```{r, eval=TRUE,echo=TRUE}
ggplot(penguins) + 
  aes(x = bill_depth_mm,
      y = bill_length_mm) + 
  geom_point() + 
  facet_grid(. ~ species) # for one variable only
```

```{r, eval=TRUE,echo=TRUE}
ggplot(penguins) + 
  aes(x = bill_depth_mm,
      y = bill_length_mm,
      color = species) + 
  geom_point() + 
  facet_grid(species ~ sex) + 
  scale_color_viridis_d() +
  guides(color = "none") # to remove legend (which is there by default)
```


# Numeric data

```{r, eval=TRUE,echo=TRUE}
library(openintro)
glimpse(loans_full_schema)
```

```{r, eval=TRUE,echo=TRUE}
loans <- loans_full_schema %>%
    select(loan_amount, interest_rate, term, grade, state, annual_income, homeownership, debt_to_income)
glimpse(loans)
```

### Histogram
```
ggplot(loans) + 
  aes(x = loan_amount, fill = homeownership) + 
  geom_histogram(binwidth = 5000, alpha = 0.5) + 
  labs(x = "Loan amount ($)",
       y = "Frequency",
       title = "Amounts of Lending Club loans") + 
  facet_wrap(~ homeownership, nrow = 3)
```

### Density plot
```
ggplot(loans, aes(x = loan_amount, fill = homeownership)) + 
  geom_density(adjust = 2, alpha = 0.5) + # 1 is the default bandwidth 
  labs(x = "Loan amount ($)",
       y = "Density",
       title = "Amounts of Lending Club loans")
```

### Box plot
```
ggplot(loans, aes(x = interest_rate)) + 
  geom_boxplot() + 
  labs(x = "Interest rate (%)",
       y = NULL,
       title = "Interest rate of Lending Club loans") + 
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
```

### Adding a categorical variable
```
ggplot(loans, aes(x = interest_rate,
                  y = grade)) + 
  geom_boxplot() + 
  labs(x = "Interest rate (%)",
       y = NULL,
       title = "Interest rate of Lending Club loans",
      subtitle = "by grade of loan")
```

### Scatterplot
```
ggplot(loans, aes(x = debt_to_income,
                  y = interest_rate)) + 
  geom_point()
```

### Hex plot
```
ggplot(loans %>% filter(debt_to_income < 100),
                aes(x = debt_to_income,
                  y = interest_rate)) + 
  geom_hex()
```

### Visualising categoric variables 

### Bar plot 
```
ggplot(loans, aes(x = homeownership,
                  fill = grade)) + 
  geom_bar()
```

```
ggplot(loans, aes(x = homeownership,
                  fill = grade)) + 
  geom_bar(position = "fill") # makes the height of all 3 bars equal for comparison 
```

### Bar plots
```
ggplot(loans, aes(y = homeownership,
                  fill = grade)) + 
  geom_bar(position = "fill") + 
  labs(x = "Proportion", 
       y = "Homeownership",
       fill = "Grade",
       title = "Grades of Lending Club loans")
```


### For data of mixed types
### Violin plots
```
ggplot(loans, aes(x = homeownership, y = loan_amount)) + geom_violin()
```

### Ridge plots
```
library(ggridges)
ggplot(loans, aes(x = loan_amount, y = grade, fill = grade, color = grade)) + 
  geom_density_ridges(alpha = 0.5)
```