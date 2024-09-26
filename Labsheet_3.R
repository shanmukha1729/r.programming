Data_Frame <- data.frame (
  Training = c("Strength", "Stamina", "Other"),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45)
)
print(Data_Frame) 


vec1 = c(1,2,3)
vec2 = c("R","Scilab","Java")
vec3 = c("For prototyping","For prototyping","For Scaleup")
df = data.frame(vec1,vec2,vec3)
print(df) 
print(df[1:2,]) 
print(df[,1:2]) 
print(df[1:2]) 


pd=data.frame("Name"=c("Senthil","Senthil","Sam","Sam"),
              "Month"=c("Jan","Feb","Jan","Feb"),
              "BS" = c(141.2,139.3,135.2,160.1),
              "BP" = c(90,78,80,81))
pd2 = subset(pd,Name=="Senthil" | BS> 150 )
print("new subset pd2")
print(pd2) 


Data_Frame <- data.frame (
  Training = c("Strength", "Stamina", "Other"),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45) )
Data_Frame_New <- Data_Frame[-c(1), -c(1)]
Data_Frame_New 


vec1 = c(1,2,3)
vec2 = c("R","Scilab","Java")
vec3 = c("For prototyping", "For prototyping","For Scaleup")
print(df)
df = data.frame(vec1,vec2,vec3) 


my_list <- list(a = 1, b = c(2, 3, 4), c = "Hello")
my_list[[1]]
my_list$b
my_list[["c"]]
my_list[c(1,2)]


my_list <- list(a = 1, b = c(2, 3, 4), c = "Hello")
my_list[[1]] <- 100
my_list$b <- c(10, 20, 30)
my_list$d <- "New Element"
print(my_list)


my_matrix <- matrix(1:9, nrow = 3, byrow = TRUE)
my_matrix[2, 3] 
my_matrix[1,] 
my_matrix[,2]


my_matrix <- matrix(1:9, nrow = 3, byrow = TRUE)
my_matrix[2, 3] <- 100
my_matrix[1, ] <- c(10, 20, 30)
my_matrix[, 2] <- c(50, 60, 70)
print(my_matrix)


my_vector <- c(10, 20, 30, 40, 50)
my_vector[3]
my_vector[c(1, 4)]


my_vector <- c(10, 20, 30, 40, 50)
my_vector[3] <- 300
my_vector[my_vector > 30] <- 999
print(my_vector)


my_df <- data.frame(name = c("Alice", "Bob", "Charlie"), age = c(25,30, 35))
my_df$age 
my_df[2, 1] 
my_df[1, ] 
my_df[, c("name", "age")]

my_df[2, 1] <- "Robert"
my_df$age <- c(26, 31, 36)
my_df[3, ] <- c("Charles", 40)
print(my_df)


vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)
combined_cbind <- cbind(vector1, vector2)
print("Combined Columns using cbind():")
print(combined_cbind)

vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)
combined_rbind <- rbind(vector1, vector2)
print("Combined Rows using rbind():")
print(combined_rbind)


list1 <- list(a = 1:3, b = c("A", "B", "C"))
list2 <- list(c = c(4, 5, 6), d = c("X", "Y", "Z"))
combined_cbind <- cbind(list1, list2)
print("Combined Lists using cbind():")
print(combined_cbind)

combined_rbind <- rbind(list1, list2)
print("Combined Lists using rbind():")
print(combined_rbind)


matrix1 <- matrix(1:6, nrow = 3, ncol = 2)
matrix2 <- matrix(7:12, nrow = 3, ncol = 2)
combined_cbind <- cbind(matrix1, matrix2)
print("Combined Matrices using cbind():")
print(combined_cbind)

combined_rbind <- rbind(matrix1, matrix2)
print("Combined Matrices using rbind():")
print(combined_rbind)


df1 <- data.frame(ID = 1:3, Name = c("Alice", "Bob", "Charlie"))
df2 <- data.frame(Age = c(25, 30, 35), Gender = c("F", "M", "M"))
combined_cbind <- cbind(df1, df2)
print("Combined Data Frames using cbind():")
print(combined_cbind)


df1 <- data.frame(ID = 1:3, Name = c("Alice", "Bob", "Charlie"), Age = c(25, 30,35))
df2 <- data.frame(ID = 4:6, Name = c("David", "Eva", "Frank"), Age = c(28, 22,40))
combined_rbind <- rbind(df1, df2)
print("Combined Data Frames using rbind():")
print(combined_rbind)