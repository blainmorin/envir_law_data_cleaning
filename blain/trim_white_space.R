### Blain example

### Trim Extra White Space
### This script removes extra spaces before or after a string
### Stolen from Chris' file

law.df = law.df %>%
  mutate(
    across(
      where(
        is.character
      ),
      str_trim
    )
  )




### Hello World

### This is some work ive done today

### this work oim one day two

### now i am complete
