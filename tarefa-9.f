      !Essa função calcula o valor de gama que aparece na expressão
      !analítica da formula de volume da esfera de id dimensões.
      function gama(id, pi)
      !Atribuindo o valor inicial de gama.
      gama = 1.e0
      !x é o argumento da função gama.
      x =(real(id)/2) + 1
      !Looping que irá calcular o valor de gama.
      do i = 1, 10000
        !Uma das condições de parada da função gama.
        if (x == 1/2.e0) then
          gama = gama*sqrt(pi)
          return
        !Segunda condição de parada da função gama.
        elseif (x == 1.e0) then
          gama = gama*1
          return
        !Condição para continuar o looping e continuar calculando a
        !função gama.
        else
           x = x - 1
           gama = x*gama
        endif
      enddo
      end function gama
!-----------------------------------------------------------------------
      !Essa subroutine calcula o valor do volume da esfera de id
      !dimesões e raio 1.
      subroutine volume_esfera(id)
        !Atribuindo o valor de pi a variável "pi".
        pi = acos(-1.e0)
        !Calculando o volume da esfera de id dimensões através da
        !equação, e chamando a função gama no processo.
        volume = (pi**(id/2.e0))/gama(id, pi)
        !Escrevendo no arquivo de saída o volume da esfera que foi 
        !calculado.
        write(1,*) id, volume
      end subroutine volume_esfera
!-----------------------------------------------------------------------
      program main
        !Abrindo o arquivo de saída onde serão salvos os dados.
        open(unit=1, file = 'dimensões-esferas.')
        !Chamando a subroutine que calcula o volume da esfera de id
        !dimensões para diferentes dimensões.
        call volume_esfera(2)
        call volume_esfera(3)
        call volume_esfera(4)
        call volume_esfera(5)
        call volume_esfera(6)
        call volume_esfera(7)
        call volume_esfera(8)
        call volume_esfera(9)
        call volume_esfera(10)
        call volume_esfera(11)
        call volume_esfera(12)
        call volume_esfera(13)
        call volume_esfera(14)
        call volume_esfera(15)
        call volume_esfera(16)
        call volume_esfera(17)
        call volume_esfera(18)
        call volume_esfera(19)
        call volume_esfera(20)
        !Fechando o arquivo de saída na onde foram salvos os dados.
        close(1)
      end program main
