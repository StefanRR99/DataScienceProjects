#Pr√°ctica 2 - ITBD
#Autor : Stefan Rada

#Paquetes
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import minimize

#Apartado 1 - ACABAR
#Definici√≥n de la funci√≥n

f = lambda x1, x2: 9*x1**2 - 2*x1*x2 + x2**2 - 4*x1**3 + 0.5*x1**4

g = lambda x1, x2: np.array([2*x1**3 - 12*x1**2 + 18*x1 - 2*x2, 
                             -2*x1 + 2*x2])

H = lambda x1, x2: np.array([[6*x1**2 - 24*x1 + 18, -2],
                             [-2, 2]], dtype=float)

"""Los PUNTOS CRITICOS son los que anulan las derivadas parciales

x1, x2 = (0, 0)
x1, x2 = (2, 2)
x1, x2 = (4, 4)

Se puede comprobar mediante g(0, 0) , g(2, 2), g(4, 4)


Verificar que el origen satisface la condicion suficiente de optimalidad de segundo orden.

Para comprobar que se cumple la condicion, debemos confirmar que todos los autovalores de la matriz Hessiana son mayores o iguales a 0 en los puntos criticos.

"""

punto1 = H(0, 0)
punto2 = H(2, 2)
punto3 = H(4, 4)

autovalores1 = np.linalg.eigvals(punto1)
autovalores2 = np.linalg.eigvals(punto2)
autovalores3 = np.linalg.eigvals(punto3)

print(autovalores1)
print(autovalores2)
print(autovalores3)

#Existe un autovalor en el punto 2, 2 que es negativo, por lo tanto no se cumple la condicion suficiente de optimalidad.


#Apartado 2
#Creación de las matrices según el rango del enunciado
X1, X2 = np.meshgrid(np.linspace(-1, 5), np.linspace(-3, 8))

#Cálculo de la función para cada punto
Z = f(X1, X2)

#Figura en 3 dimensiones y etiquetas añadidas
fig = plt.figure(figsize=(10, 8))
ejes = plt.axes(projection='3d')
ejes.plot_surface(X1, X2, Z, cmap = 'viridis')

ejes.set_xlabel('X1')
ejes.set_ylabel('X2')
ejes.set_title('Gr√°fica 3D de la funci√≥n')

plt.show()


#Apartado 3
#Mismo procedimiento que en el anterior. X13, X23, Z3 ya que es el tercer apartado, para no sobreescribir variables. 

X13, X23 = np.meshgrid(np.linspace(-1, 1), np.linspace(-2, 1))
Z3 = f(X13, X23)

plt.figure(figsize=(8, 6))
plt.contour(X13, X23, Z3, 50, cmap='viridis', linestyles="solid")

plt.xlabel('X1')
plt.ylabel('X2')
plt.title('Curvas de Nivel de la Función')

plt.show()

"""  GRAFICO EN 3D
X13, X23 = np.meshgrid(np.linspace(-1, 1), np.linspace(-2, 1))
Z3 = f(X13, X23)

fig = plt.figure(figsize=(10, 8))
ejes = plt.axes(projection='3d')
#Alpha en 0.7 para añadir transparencia a la figura 3D
ejes.plot_surface(X13, X23, Z3, cmap = 'viridis', alpha=0.7)

#Curvas de nivel
ejes.contour(X13, X23, Z3, 50, cmap='viridis', linestyles="solid", offset=0)

ejes.set_xlabel('X1')
ejes.set_ylabel('X2')
ejes.set_title('Gr√°fica 3D de la funci√≥n')

plt.show()
"""

#Apartado 4 - AL ser 2x2 la matriz solo hay dos autovalores, por eso hacemos la suma, que es el maximo y el minimo.
# Método del gradiente
def metodo_gradiente(punto, iteraciones):
    x = punto
    for i in range(iteraciones):
        matriz = H(x[0], x[1])
        autovalores_gradiente = np.linalg.eigvals(matriz)
        alpha = 2 / (sum(autovalores_gradiente))
        gradiente = g(x[0], x[1])
        x = x - alpha * gradiente
    return x

x0 = np.array([0.4, -1.9])
resultado_4 = metodo_gradiente(x0, 40)
print(resultado_4)

#RESULTADO CON EL METODO DEL GRADIENTE - [ 0.00016508 -0.00069616]
#Practicamente el punto (0, 0)

#Apartado 5
def newton_puro(punto, iteraciones):
    x = punto
    for i in range(iteraciones):
        gradiente = g(x[0], x[1])
        matriz = H(x[0], x[1])
        x = x - np.linalg.inv(matriz).dot(gradiente)
    return x

x0 = np.array([0.4, -1.9])
resultado_5 = newton_puro(x0, 5)
print(resultado_5)

#RESULTADO CON EL METODO NEWTON PURO - [-8.35026243e-14 -8.35026243e-14]
#Practicamente el punto (0, 0)


#Apartado 6 - confirmar y cambiar cosas
def metodo_gradiente_lineal(punto, iteraciones):
    x = punto
    for i in range(iteraciones):
        #Funcion lineal, a optimizar
        funcion_lineal = lambda alpha: f(x[0] - alpha * g(x[0], x[1])[0], x[1] - alpha * g(x[0], x[1])[1])
        
        #Tamaño de paso, empezando por 1
        resultado = minimize(funcion_lineal, 1)
        alpha = resultado.x[0]
        
        x = x - alpha * g(x[0], x[1])    
    return x

x0 = np.array([0.4, -1.9])
resultado_6 = metodo_gradiente_lineal(x0, 40)
print(resultado_6)

#RESULTADO CON EL METODO GRADIENTE CON BUSQUEDA LINEAL EXACTA - [-4.48980987e-04  4.89831265e-05]
#Practicamente el punto (0, 0)


#Apartado 7
#Creo nuevos metodos que guarden en un vector todos los puntos, ya que los metodos anteriores unicamente devolvian el punto final.
#La unica modificacion es guardar copias de los puntos en la variable historial y retornar esta en vez del punto final
def metodo_gradiente_historial(punto, iteraciones):
    x = punto
    historial = [x]
    for i in range(iteraciones):
        matriz = H(x[0], x[1])
        autovalores_gradiente = np.linalg.eigvals(matriz)
        alpha = 2 / (sum(autovalores_gradiente))
        gradiente = g(x[0], x[1])
        x = x - alpha * gradiente
        historial.append(x.copy())
    return np.array(historial)

def newton_puro_historial(punto, iteraciones):
    x = punto
    historial = [x]
    for i in range(iteraciones):
        gradiente = g(x[0], x[1])
        matriz = H(x[0], x[1])
        x = x - np.linalg.inv(matriz).dot(gradiente)
        historial.append(x.copy())
    return np.array(historial)

def metodo_gradiente_lineal_historial(punto, iteraciones):
    x = punto
    historial = [x]
    for i in range(iteraciones):
        funcion_lineal = lambda alpha: f(x[0] - alpha * g(x[0], x[1])[0], x[1] - alpha * g(x[0], x[1])[1])        
        resultado = minimize(funcion_lineal, 1)
        alpha = resultado.x[0]        
        x = x - alpha * g(x[0], x[1])
        historial.append(x.copy())
    return np.array(historial)

punto_inicial = [0.4, -1.9]

puntos_gradiente = metodo_gradiente_historial(punto_inicial, 40)
puntos_gradiente_lineal = metodo_gradiente_lineal_historial(punto_inicial, 40)
puntos_newton = newton_puro_historial(punto_inicial, 5)

X17, X27 = np.meshgrid(np.linspace(-1, 1, 800), np.linspace(-2, 1, 800))
Z7 = f(X17, X27)

plt.figure(figsize=(8, 10))

plt.contour(X17, X27, Z7, 50, cmap='viridis')
plt.plot(puntos_gradiente[:, 0], puntos_gradiente[:, 1], marker='o', label='Gradiente')
plt.plot(puntos_gradiente_lineal[:, 0], puntos_gradiente_lineal[:, 1], marker='o', label='Gradiente lineal')
plt.plot(puntos_newton[:, 0], puntos_newton[:, 1], marker='o', label='Newton puro')
plt.legend()
plt.show()


#Apartado 8 
# Punto inicial
x0 = np.array([0.4, -1.9])

#Creo esta funcion para facilitar la escritura de la formula pN
inversa_por_gradiente = lambda x1, x2: np.linalg.inv(H(x1, x2)).dot(g(x1, x2))

#Formulas
pg = -g(x0[0], x0[1]) / np.linalg.norm(g(x0[0], x0[1]))
pN = -inversa_por_gradiente(x0[0], x0[1]) / np.linalg.norm(inversa_por_gradiente(x0[0], x0[1]))

#Nuevas formulas de la funcion
f_pg = lambda alpha: f(x0[0] + alpha * pg[0], x0[1] + alpha * pg[1])
f_pN = lambda alpha: f(x0[0] + alpha * pN[0], x0[1] + alpha * pN[1])


#Intervalo de 100 valores de alpha
valores_alpha = np.linspace(0, 2, 100)

#Grafico
plt.figure(figsize=(8, 6))
plt.plot(valores_alpha, [f_pg(alpha) for alpha in valores_alpha], label='Gradiente', color='blue')
plt.plot(valores_alpha, [f_pN(alpha) for alpha in valores_alpha], label='Newton', color='red')
plt.title('Funciones ϕg(α) y ϕN(α)')
plt.xlabel('α')
plt.legend()
plt.show()

"""

PREGUNTAS

(a) ¿Cu´al de las dos direcciones pg y pN ser´ıa mejor para una b´usqueda lineal exacta?

Para la busqueda lineal exacta, el mejor metodo es el de Newton ya que converge mas rapido y dispone de una curvatura
mas suave o menos pronunciada como se puede ver en el grafico.


(b) ¿Cu´al ser´ıa mejor para tama˜nos de paso muy peque˜nos?

El metodo del gradiente es el mas indicado para tamaños de paso muy pequeños, ya que el de Newton se puede volver inestable
por culpa de la matriz invertida, mientras que el gradiente solo sigue la direccion del gradiente, es decir es mas robusto.

"""


























