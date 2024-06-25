#Ejercicio 1 - Stefan Rada

#Rutal del archivo Excel descargado en local.
ruta <- file.choose()

#Impresión del listado de hojas del archivo excel.
excel_sheets(ruta)

#Creación de un dataframe a partir del archivo excel.
datos <- read_excel(ruta)

#Comprobación de los datos cargados.
View(datos)
