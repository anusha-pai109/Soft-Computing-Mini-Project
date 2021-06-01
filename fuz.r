library(readxl)
library(sets)
dataset <- read_excel("D:/Semester 6/Soft computing fuzzy systems project/dataset.xlsx")
View(dataset)
head(dataset)
head(subset(dataset, select = 'quality'))
factor(dataset$quality)
w = table(dataset$quality)
t = as.data.frame(w)
names(t)[1] = 'quality'
t
head(subset(dataset, select = 'price'))
factor(dataset$price)
w1 = table(dataset$price)
t1 = as.data.frame(w1)
names(t1)[1] = 'price'
t1
head(subset(dataset, select = 'seller'))
factor(dataset$seller)
w2 = table(dataset$seller)
t2 = as.data.frame(w2)
names(t2)[1] = 'seller'
t2
head(subset(dataset, select = 'product'))
factor(dataset$product)
w3 = table(dataset$product)
t3 = as.data.frame(w3)
names(t3)[1] = 'product'
t3
variable <- set(
  quality = fuzzy_partition(varnames = c(acceptable = 70, excellent = 80, worst = 50),
                            sd = 5.0),
  seller = fuzzy_partition(varnames = c(bad = 50, excellent = 90, regular = 60), 
                           sd = 3.0),
  price = fuzzy_partition(varnames = c(average = 50, cheap = 70,
                                       costly = 80), sd = 7.5),
  
)
variable
rule <- set(
  fuzzy_rule(quality %is% excellent && seller %is% bad &&
               price %is% average, product %is% perfect),
  fuzzy_rule(quality %is% worst && seller %is% regular &&
               price %is% costly, product %is% bad),
  fuzzy_rule(quality %is% acceptable, product %is% bad),
  fuzzy_rule(quality %is% excellent || seller %is% excellent ||
               price %is% cheap, product %is% ok),
  fuzzy_rule(quality %is% worst && price %is% cheap,
             product %is% ok),
  fuzzy_rule(quality %is% worst && seller %is% bad &&
               price %is% cheap, product %is% ok)
)
rule
models <- fuzzy_system(variable,rule)
print(models)
class(models)
plot(models)
example.1 <- fuzzy_inference(models, list(quality = 80, seller = 0,
                                         price = 50))
gset_defuzzify(example.1, "centroid")
plot(example.1)
example.2 <- fuzzy_inference(models, list(quality = 30, seller = 0,
                                         price = 70))
gset_defuzzify(example.2, "largestofmax")
plot(example.2)