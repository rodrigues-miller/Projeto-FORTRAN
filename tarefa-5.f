      !Essa subroutine recursiva irá permutar os números do vetor m.
      recursive subroutine permutacao(m, k, n)
        !Definindo o tamanho do vetor m.
        dimension m(1000000)
        !Condição de parada da recursividade, quando a posição de troca
        !do vetor é igual ao tamanho do mesmo.
        if (k.eq.n) then
          !Chamando a subroutine que irá calcular a paridade da
          !permutação, e adicionar o valor da paridade na posição n+1 do
          !vetor m.
          call paridade(m, n)
          !Escrevendo no arquivo de saída todas as permutações
          !calculadas.
          write(2,*) (m(i), i=1, n+1)
        else
          !Realizando todas as trocas possíveis na posição k e chamando
          !a subroutine para calcular todas as trocas possíveis na
          !posição k+1.
          do i = k, n
            itroca = m(k)
            m(k) = m(i)
            m(i) = itroca
            !Chamando recursivamente o subroutine permutação para
            !realizar a permutação do vetor p(n-1).
            call permutacao(m, k+1, n)
            !Desfazendo as trocas que foram realizadas no vetor para
            !que se possa retornar ao passo anterior da recursão sem
            !que os valores estejam trocados.
            itroca = m(k)
            m(k) = m(i)
            m(i) = itroca
          end do
        end if
      end subroutine permutacao
!-----------------------------------------------------------------------
      !Essa subroutine irá calcular a paridade de cada permutação.
      subroutine paridade(m, n)
        !Definindo o tamanho do vetor m e do vetor l.
        dimension m(1000000), l(1000000)
        !Defindo o valor da variável "ipar" na qual será armazenado qual
        !a paridade da permutação.
        ipar = 1
        !Copiando os valores do vetor m já permutado no vetor l.
        do i = 1, n
          l(i) = m(i)
        end do
        !Ordenando o vetor l afim de obter sua paridade.
        do i = 1, n-1
          do j = i+1, n
          !Condição de troca dos valores na posição i do vetor l com a
          !posiçao j.
            if (l(i).gt.l(j)) then
              jtroca = l(i)
              l(i) = l(j)
              l(j) = jtroca
              !Alterando o valor da variável paridade a cada troca que é
              !realizada, pois ipar é -1 para número ímpar de trocas e 1
              !para números pares de trocas.
              ipar = ipar*(-1)
            end if
          end do
        end do
        !adicionando o valor da paridade da permutação na posição n+1 do
        !vetor m.
        m(n+1) = ipar
      end subroutine
!-----------------------------------------------------------------------
      program main
        !Definindo o tamanho do vetor m, na onde serão guardados os
        !dados para realizar a permutação.
        dimension m(1000000)
        !Definindo o valor inicial de k, onde k é a variável que salva
        !em que posição do vetor estão ocorrendo as trocas.
        k = 1
        !Pedindo que o usuário informe até que valor ele deseja
        !permutar e armazenando na variável "n".
        write(*,*) 'Digite o valor de n:'
        read(*,*) n
        !Adicionando os valores solicitados no vetor m.
        do i = 1, n
          m(i) = i
        end do
        !Abrindo o arquivo onde os dados de saída serão salvos.
        open(2,file='saída-1-tarefa-5.')
        !Chamando a subroutine que irá realizar a permutação dos números
        !e imprimi-lás.
        call permutacao(m, k, n)
        close(2)
      end program main
