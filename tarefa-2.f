      program paralelepipedo
        !Pegando os valores das coordenadas de v1.
        write(*,*) 'Digite as coordenadas de v1:'
        read(*,*) x1, y1, z1
        !Pegando os valores das coordenadas de v2.
        write(*,*) 'Digite as coordenadas de v2:'
        read(*,*) x2, y2, z2
        !Pegando os valores das coordenadas de v3.
        write(*,*) 'Digite as coordenadas de v3:'
        read(*,*) x3, y3, z3
      !----CALCULANDO O VOLUME DO PARALELEPÍPEDO COM PRODUTO MISTO-----
        !Fazendo o produto vetorial de v1 com v2.
        x4 = y1*z2 - y2*z1
        y4 = z1*x2 - z2*x1
        z4 = x1*y2 - x2*y1
        !Utilizando os vetores encontrados para calcular o produto misto
        volume = sqrt((x4*(x3-x2) + y4*(y3-y2) + z4*(z3-z2))**2)
      !----------CALCULANDO A ÁREA DO PARALELEPÍPEDO------------------
        !Tirando o módulo do vetor área e multiplicando por 2, pois são
        !duas faces opostas de mesmo tamanho, temos:
        area1 = 2*sqrt(x4**2 + y4**2 + z4**2)
        !Fazendo o produto vetorial de v1 com (v3-v2).
        x5 = y1*(z3-z2) - (y3-y2)*z1
        y5 = z1*(x3-x2) - (z3-z2)*x1
        z5 = x1*(y3-y2) - (x3-x2)*y1
        !Tirando o módulo do vetor área e multiplicando por 2, pois são
        !duas faces opostas de mesmo tamanho, temos:
        area2 = 2*sqrt(x5**2 + y5**2 + z5**2)
        !Fazendo o produto vetorial de (v3-v2) com v2.
        x6 = (y3-y2)*z2 - y2*(z3-z2)
        y6 = (z3-z2)*x2 - z2*(x3-x2)
        z6 = (x3-x2)*y2 - x2*(y3-y2)
        !Tirando o módulo do vetor área e multiplicando por 2, pois são
        !duas faces opostas de mesmo tamanho, temos:
        area3 = 2*sqrt(x6**2 + y6**2 + z6**2)
        !Calculando a área total temos:
        area = area1 + area2 + area3
        !Imprimindo na tela o valor da área e do volume do
        !paralelepípedo.
        write(*,*) 'A area do paralelepípedo vale:', area
        write(*,*) 'O volume do paralelepípedo vale:', volume
      end program paralelepipedo
