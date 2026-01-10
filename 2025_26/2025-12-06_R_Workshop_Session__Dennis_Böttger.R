### Roadmap for Learning R ###

# 1. Basic operations and syntax
9+3
5*6
8/2
10-4

3^2
sqrt(16)
log(10)
round(3.1459,8)

round(log(7677922),)

result <- 8-2

a <- 10+5

b <-a*4

str(a)

z<- "15"
str(z)
y <-as.numeric(z)
str(y)


# 2. vectors

v <- c()

names <-c("Dennis","Maryam","Yasin",
          "Ella","Lisa","Trang","Greeshma","Alan","Sagar",
          "Ceylin","Carmen"
          )

names

age <-c(27,24,24,
        21,21,25,21,22,29,
        26,21,NA)
age


countries <- c("Germany","Iran","Iran",
               "Germany","Germany","Vietnam","India","India","Nepal",
               "Turkey","Germany")

#just <-c(Germany,France,Italy)

# 3. indexing a vector

names[8]
names[326]
names[2:4]

names[c(1,3,5)]
names[-5]


age[age>23]

# 4. create a data frame

age_cleaned <-na.omit(age)

age
age_cleaned


data <- data.frame(Name=names,Age=age_cleaned,Country =countries)

data$Country <-factor(data$Country)
str(data)


data$Country

str(data)


head(data)
tail(data)

summary(data)

hist(data$Age)


# 5. subsetting a data frame

germans <- data[data$Country =="Germany",]
indians <- data[data$Country =="India",]

# 6. basic logic


#boolean
TRUE
FALSE

5>3

5==3

a==15

b!=2



f <-FALSE

t <-TRUE

t&t
f&f

f&f

f|f
f|t
t|t

xor(f,t)

xor(t,t)

a<=15
16>=a
14>=a



# 7. loops

# 8. usefull links

##################################
# 1. Basic operations and syntax