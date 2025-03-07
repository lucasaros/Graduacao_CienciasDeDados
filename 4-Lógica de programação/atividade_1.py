matriz = []
linha = 2
coluna = 3
indice = 0

while indice < linha:

   r = []

   j = 0

   while j < coluna:

       r.append(indice + j)

       j += 1

   matriz.append(r)

   indice += 1

print(matriz)