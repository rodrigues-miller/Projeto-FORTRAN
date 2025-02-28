      !Essa subroutine irá calcular o volume da esfera de id dimensões
      !com o método de monte carlo.
      subroutine volume_esfera_random(m, id, r)
      !Definindo o contador que irá armazenar o número de vezes que o
      !ponto caiu dentro da esfera de id dimensões.
        icont = 0
        !Looping que irá calcular m pontos diferentes.
        do i = 1, m
          !Definindo o valor da soma iniciando em 0.
          soma = 0
          !Looping que irá sortear o valor de cada componente do ponto.
          do j = 1, id
            !Atribuindo um valor aleatório entre 0 e 1 para x.
            x = rand()
            !Somando o valor quadrático de x na variável soma.
            soma = soma + x**2
          end do
          !Calculando o distância do ponto ao centro da esfera, tirando
          !a raiz da soma de suas componentes ao quadrado.
          soma = sqrt(soma)
          !Condição para que o ponto calculado esteja dentro da esfera.
          if (soma <= 1) then
            !Caso o ponto esteja dentro da esfera adicione mais 1 a
            !variável contadora.
            icont = icont + 1
          endif
        enddo
        !Calculando o volume para uma esfera de raio 1 utilizando a
        !fração do número de pontos que caiu dentro da esfera dividido
        !pelo número total de pontos, e  utilizando o volume do cubo de
        !id dimensões.
        volume = ((2*r)**id)*(icont/real(m))
        !Escrevendo no arquivo de saída o volume calculado da esfera.
        write(1,101)
        write(1,100) volume, id, r, m
101     format('|----------------------------------------------|')
100     format('|',f16.8,'|',I9,'|',f8.2,'|',I10,'|')
      end subroutine volume_esfera_random
!-----------------------------------------------------------------------
      !Essa função calcula o valor de gama que aparece na expressão
      !analítica do cáculco do volume da esfera de id dimensões.
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
      !dimesões e raio r de forma analítica.
      subroutine volume_esfera(id,r)
        !Atribuindo o valor de pi à variável "pi".
        pi = acos(-1.e0)
        !Calculando o volume da esfera de id dimensões através da
        !equação, e chamando a função gama no processo.
        volume = ((pi**(id/2.e0))*(r**id))/gama(id, pi)
        !Escrevendo no arquivo de saída o volume da esfera que foi 
        !calculado.
        write(1,101)
        write(1,50) volume, id, r
101     format('|----------------------------------------------|')
50      format('|',f16.8,'|',I9,'|',f8.2,'|','     Exato|')
      end subroutine volume_esfera
!-----------------------------------------------------------------------
      program esfera
        !Pedindo para o usuário informar qual o raio da esfera.
        write(*,*)'Informe o raio da esfera que você deseja calcular o 
     &volume:'
        read(*,*) r
        !Abrindo o arquivo onde ficarão os dados de saída do programa.
        open(1,file='saída-1-tarefa-8.')
        write(1,102)
        write(1,103)
        !Chamando a subroutine que calculará o volume da esfera para
        !diferentes valores de m e para dimensão 2.
        call volume_esfera_random(10000,2,r)
        call volume_esfera_random(100000,2,r)
        call volume_esfera_random(10000000,2,r)
        call volume_esfera_random(100000000,2,r)
        !Chamando a subroutine que irá calcular o volume da esfera de 2
        !dimensões analíticamente a fim de comparar os valores.
        Call volume_esfera(2,r)
        !Chamando a subroutine que calculará o volume da esfera para
        !diferentes valores de m e para dimensão 3.
        call volume_esfera_random(10000,3,r)
        call volume_esfera_random(100000,3,r)
        call volume_esfera_random(10000000,3,r)
        call volume_esfera_random(100000000,3,r)
        !Chamando a subroutine que irá calcular o volume da esfera de 3
        !dimensões analíticamente a fim de comparar os valores.
        call volume_esfera(3,r)
        !Chamando a subroutine que calculará o volume da esfera para
        !diferentes valores de m e para dimensão 4.
        call volume_esfera_random(10000,4,r)
        call volume_esfera_random(100000,4,r)
        call volume_esfera_random(10000000,4,r)
        call volume_esfera_random(100000000,4,r)
        !Chamando a subroutine que irá calcular o volume da esfera de 4
        !dimensões analíticamente a fim de comparar os valores.
        call volume_esfera(4,r)
        write(1,102)
        close(1)
103     format('|     Volume     |Dimensões|  Raio  | Iterções |')
102     format('------------------------------------------------')
      end program esfera
!-----------------------------------------------------------------------

