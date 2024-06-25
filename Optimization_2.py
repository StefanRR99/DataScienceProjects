#!/usr/bin/env python
# coding: utf-8

# #APARTADO 1

# In[1]:


import numpy as np

# Número de imágenes del dataset
m = 3000
# Número de píxeles
N = 1+784

def sigma(z):
    return (1 / (1 + np.exp(-z)))

def f(zeta,X,Y):
    epsilon = 1e-15
    resultado_sigma = sigma(np.dot(X,zeta))
    sum1 = np.dot(Y, np.log(resultado_sigma))
    sum2 = np.dot((1-Y), np.log(1-resultado_sigma + epsilon))
    return ((-1/m)*sum1 - (1/m)*sum2)

def g(zeta,X,Y):
    sumatorio = np.dot((Y - sigma(np.dot(X,zeta))), X)
    return (-1/m*sumatorio)


def backtracking(f, g, xk, set_x, set_y, alpha=1, beta=0.5, c=1e-4):
    # Evaluar la función y su gradiente en el punto actual
    fk = f(xk,set_x,set_y)
    gk = g(xk,set_x,set_y)
    right_cond = fk + c * alpha * np.dot(gk, -gk)

    # Condición de Armijo para el backtracking
    while f(xk - alpha * gk,set_x,set_y) > right_cond:
        alpha *= beta  # Reducir el tamaño de paso
    return alpha


# #APARTADO 2

# In[2]:


import time
def descenso(f,g,x0,set_x,set_y,alg="back",lr=0.01,tol=1e-3,maxit=10000):
    start_time = time.time()
    warning = True
    xk = x0
    iter = 0
    x_values = []
    y_values = []
    alpha = lr
    gk = g(xk,set_x,set_y)

    while iter < maxit:
        if np.linalg.norm(gk) <= tol:
          warning = False
          break
        if iter % 1000 == 0:
            print(f"Iteración: {iter}")
        x_values.append(f(xk,set_x,set_y))
        y_values.append(np.linalg.norm(gk))

        if alg == "back" and iter != 0:
            alpha = backtracking(f,g,xk,set_x,set_y)

        xk = xk - alpha * gk
        gk = g(xk,set_x,set_y)
        iter += 1

    if warning:
        print(f"Warning - Se han realizado las {maxit} iteraciones")
        iter = maxit
    end_time = time.time()
    return xk, iter, end_time - start_time, np.array(x_values), np.array(y_values)


# #APARTADO 3

# In[3]:


from tensorflow.keras.datasets import mnist


# In[4]:


(train_x,train_y),(test_x,test_y)=mnist.load_data()


# In[5]:


# Filtro 0 hasta 8 entrenamiento
mask = (train_y != 9)
train_x, train_y = train_x[mask], train_y[mask]
# Normalización escala de grises train
train_x = train_x / 255.0

# Filtro 0 hasta 8 test
mask = (test_y != 9)
test_x, test_y = test_x[mask], test_y[mask]
# Normalización escala de grises test
test_x = test_x / 255.0

# Obtenemos los primeros 3000 valores de 6 y 8
m = 3000
mask = (train_y == 6) | (train_y == 8)
indices = np.where(mask)[0][:m]
seleccionados = train_x[indices]

# Bias 1 + 784
x = seleccionados.reshape(m, -1)
x = np.hstack((np.ones((m, 1)), x))
y = train_y[indices]
y[y == 6] = 0
y[y == 8] = 1

#print(x.shape)
#print(y.shape)

# Bias y conjunto test
test_m = 1932
mask = (test_y == 6) | (test_y == 8)
indi = np.where(mask)[0]
selec = test_x[indi]

t_x = selec.reshape(test_m, -1)
t_x = np.hstack((np.ones((test_m, 1)), t_x))
t_y = test_y[indi]
t_y[t_y == 6] = 0
t_y[t_y == 8] = 1


# #APARTADO 4

# In[6]:


theta = np.zeros(785)

resultados_1 = descenso(f,g,theta,x,y,alg="back",lr=10,tol=1e-3,maxit=10000)
np.savez('thetas1.npz', xk=resultados_1[0],iter=resultados_1[1],time=resultados_1[2],x_values=resultados_1[3],y_values=resultados_1[4])
resultados_2 = descenso(f,g,theta,x,y,alg="const",lr=0.01,tol=1e-3,maxit=10000)
np.savez('thetas2.npz', xk=resultados_2[0],iter=resultados_2[1],time=resultados_2[2],x_values=resultados_2[3],y_values=resultados_2[4])


# In[7]:


print("- - - Información de tamaño de paso BACKTRACKING ; alpha0 = 10 - - -")
print(f"Nº de iteraciones: {resultados_1[1]}")
print(f"Tiempo: {resultados_1[2]}")
print(resultados_1[3])
print(resultados_1[4])

print("- - - Información de tamaño de paso CONSTANTE ; alpha0 = 0.01 - - -")
print(f"Nº de iteraciones: {resultados_2[1]}")
print(f"Tiempo: {resultados_2[2]}")
print(resultados_2[3])
print(resultados_2[4])


# In[8]:


datos1 = np.load('thetas1.npz')

xk1 = datos1['xk']
iteraciones1 = datos1['iter']
tiempo1 = datos1['time']
valores_x1 = datos1['x_values']
valores_y1 = datos1['y_values']


datos2 = np.load('thetas2.npz')

xk2 = datos2['xk']
iteraciones2 = datos2['iter']
tiempo2 = datos2['time']
valores_x2 = datos2['x_values']
valores_y2 = datos2['y_values']


# In[9]:


import matplotlib.pyplot as plt
# Creando la figura con dos gráficas
plt.figure(figsize=(10, 8))

# Primera gráfica (superior): valor de xk para ambos métodos
plt.subplot(2, 1, 1)  # 2 filas, 1 columna, posición 1
plt.semilogy(np.linspace(0,iteraciones1,iteraciones1), valores_x1, label='Método Backtracking')
plt.semilogy(np.linspace(0,iteraciones2,iteraciones2), valores_x2, label='Método Constante')
plt.title('Comparación de xk en cada iteración')
plt.xlabel('Iteración')
plt.ylabel('xk (escala logarítmica)')
plt.legend()

# Segunda gráfica (inferior): valor de ||∇f(xk)||
plt.subplot(2, 1, 2)  # 2 filas, 1 columna, posición 2
plt.semilogy(np.linspace(0,iteraciones1,iteraciones1), valores_y1, label='Método Backtracking')
plt.semilogy(np.linspace(0,iteraciones2,iteraciones2), valores_y2, label='Método Constante')
plt.title('Comparación de ||∇f(xk)|| para ambos métodos')
plt.xlabel('Iteración')
plt.ylabel('||∇f(xk)||')
plt.legend()

# Guardando la gráfica en un archivo PDF
plt.tight_layout()
plt.savefig('Compara.pdf')

# Mostrando la gráfica
plt.show()

"""
El método de backtracking tiene una mejor aproximación al mínimo global ya que converge a causa de que la norma es mas baja que la tolerancia.
Mientras que en el método constante no alcanza la tolerancia antes del número máximo de iteraciones.
"""

# #APARTADO 5

# In[10]:


pred_back_train = [np.array([])]
pred_const_train = [np.array([])]

for i in range(m):
  if sigma(np.dot(xk1.T,x[i])) < 0.5:
    pred_back_train = np.append(pred_back_train,0)
  else:
    pred_back_train = np.append(pred_back_train,1)

for i in range(m):
  if sigma(np.dot(xk2.T,x[i])) < 0.5:
    pred_const_train = np.append(pred_const_train,0)
  else:
    pred_const_train = np.append(pred_const_train,1)


# In[14]:


aciertos_back_train = len(pred_back_train[pred_back_train == y])
aciertos_const_train = len(pred_const_train[pred_const_train == y])

print(f"Predicciones correctas, método Bactracking, datos de Entrenamiento: {aciertos_back_train}")
print(f"Predicciones correctas, método Constante, datos de Entrenamiento: {aciertos_const_train}")


# In[12]:


pred_back_test = [np.array([])]
pred_const_test = [np.array([])]
for i in range(test_m):
  if sigma(np.dot(xk1.T,t_x[i])) < 0.5:
    pred_back_test = np.append(pred_back_test,0)
  else:
    pred_back_test = np.append(pred_back_test,1)

for i in range(test_m):
  if sigma(np.dot(xk2.T,t_x[i])) < 0.5:
    pred_const_test = np.append(pred_const_test,0)
  else:
    pred_const_test = np.append(pred_const_test,1)


# In[15]:


aciertos_back_test = len(pred_back_test[pred_back_test == t_y])
aciertos_const_test = len(pred_const_test[pred_const_test == t_y])

print(f"Predicciones correctas, método Bactracking, datos de Test: {aciertos_back_test}")
print(f"Predicciones correctas, método Constante, datos de Test: {aciertos_const_test}")


# ## ¿Qué método consigue más aciertos en el conjunto de entrenamiento y en el conjunto de testeo?
# 
# En el conjunto de entrenamiento, el método que más aciertos consigue es el de Backtracking, que acierta el 100% de los datos de entrenamiento.
# En el conjunto de datos de prueba, ambos métodos consiguen una cantidad de aciertos muy parecida pero de igual manera el método backtracking consigue más acieretos. La longitud del conjunto de datos de test es de 1932.
# Los resultados pueden variar si modificamos el umbral; en este caso, lo tenemos establecido en menor y mayor de 0.5, pero se podría establecer en < 0.3 y > 0.7 para predecir con seguridad los números 6 y 8, respectivamente.
