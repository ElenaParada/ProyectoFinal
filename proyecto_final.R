library(tidyverse)
employee <- read.csv('C:/Users/MACHENIKE/Desktop/Diplomado_Ciencia_de_Datos/EmployeeSampleData.csv')
View(employee)
#Hay un total de 100 registros de empleados
current_employees <- employee %>% 
  filter(employee$dia_despido == "")

print(summary(current_employees))
#actualmente se cuenta con un total de 915 empleados 

job_titles <- select(employee, puesto)

job_titles_count <- job_titles %>%
  group_by(puesto) %>%
  summarise(Workers = n())

view(job_titles_count)
#hay 33 puestos distintos, los puestos más ocupados son los vicepresidentes y el menos ocupado es Network Engineer

#<------------------------------------------------------------------------------------------------->

salary <- select(employee, nombre_completo, puesto, salario_anual)

highest_salary <- salary %>%
  arrange(desc(salario_anual)) %>%
  head(20)

average_salary <- salary %>% 
  summary(salario_anual) %>% 
  head(20)

highest_employee <- salary %>%
  filter(salario_anual == 258498)

lowest_employee <- salary %>%
  filter(salario_anual == 40063)

print(highest_salary)
#quienes mas gana en salario son los vicepresidentes

print(average_salary)
#el salario mas alto es de 258498 dolares anuales, la media es de 96557 y el minimo es de 40063

print(highest_employee)
#el empleado que mas gana es Raelynn Rios como Vice Presidente 

print(lowest_employee)
#el empleado que menos gana es Miles Dang como coordinador de IT

#<-------------------------------------------------------------------------------------------------->
fire_employee <- employee %>%
  filter(!is.na(dia_despido) & dia_despido != "")
view(fire_employee)

total_fire <- summary(fire_employee$dia_despido)
print(total_fire)
#85 empleados han sido despedidos o han renunciado 

total_fire_count <- fire_employee %>% 
  group_by(puesto) %>% 
  summarise(workers = n())

view(total_fire_count)
#Los directores son los que mayormente han renunciado o han sido despedidos

#<-------------------------------------------------------------------------------------------->
salary_gender <- select(employee, sexo, salario_anual)

men_salary <- salary_gender %>% 
  filter(salary_gender$sexo == "Male")

average_men_salary <- men_salary %>% 
  summarise(average = mean(salario_anual, na.rm = TRUE))

print(average_men_salary)
#el salario promedio de los hombres es de 114188 dolares anuales

women_salary <- salary_gender %>% 
  filter(salary_gender$sexo == "Female")

average_women_salary <- women_salary %>% 
  summarise(average = mean(salario_anual, na.rm = TRUE))

print(average_women_salary)
#el salario promedio de las mujeres es de 112314.2 dolares anuales

#en promedio los hombres ganan un poco mas que las mujeres 
#<--------------------------------------------------------------------------->

native <- select(employee, pais, salario_anual)
group_native <- native %>% 
  group_by(pais) %>% 
  summarise(average = mean(salario_anual, na.rm = TRUE))
#hay 3 nacionalidades EE.UU, China y Brasil
print(group_native)
#en Promedio los empleados de China ganan más con un promedio de $113,824, le sigue los empleados de EE.UU con un promedio de $113,205 y por último los originarios de Brasil con un promedio de $112,325