      program torus
        !Atribuindo o valor de pi à variável "pi".
        pi = acos(-1.e0)
        !Coletando o valor do raio interno e do raio externo do torus.
        write(*,*) 'Digite o valor do raio interno e do raio externo do 
     &torus'
        read(*,*) rinterno, rexterno       
        !Calculando a área do torus.
        area = 2*pi*rinterno*2*pi*rexterno
        !Calculando o volume do torus.
        volume = 2*pi*rinterno*pi*rexterno**2
        !Escrevendo na tela o valor da área do torus.
        write(*,*) 'A área do torus com raio interno =',rinterno,
     &'e raio externo =',rexterno, 'vale:', area
        !Escrevendo na tela o valor do volume do torus.
        write(*,*) 'O volume do torus com raio interno =',rinterno,
     &'e raio externo =',rexterno,'vale:',volume
      end program torus
