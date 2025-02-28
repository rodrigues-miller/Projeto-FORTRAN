      !Essa subroutine recursiva irá permutar os números do vetor m e
      !irá chamar a subroutine que irá calcular o valor do determinante
      !da matriz "amatriz".
      recursive subroutine permutacao(m, amatriz, k, n, det)
        !Definindo o tamanho do vetor m , na onde serão guardados os
        !dados para realizar a permutação, e definindo o tamanho da
        !matriz "amatriz", na onde serão armazenados os valores da 
        !matriz.
        dimension m(1000), amatriz(1000,1000)
        !Condição de parada da recursividade.
        if (k.eq.n) then
          !Chamando a subroutine que irá calcular a paridade da
          !permutação, e adicionar o valor da paridade na posição n+1 do
          !vetor m.
          call paridade(m, n)
          !Chamando a subroutine que irá calcular o valor do
          !determinante.
          call determinante(m, amatriz, n, det)
        else
          !Realizando todas as trocas possíveis na posição k e chamando
          !a subroutine para calcular todas as trocas possíveis na
          !posição k+1.
          do i = k, n
            itroca = m(k)
            m(k) = m(i)
            m(i) = itroca
            !Chamando recursivamente o subroutine permutação para
            !realizar a permutação do vetor p(n-1), parando assim de
            !reazilar as trocas na posição k para realizar as trocas 
            !na posição k+1.
            call permutacao(m, amatriz, k+1, n, det)
            !Desfazendo as trocas que foram realizadas no vetor para
            !que se possa retonar ao passo anterior da recursão sem que
            !os valores estejam trocados.
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
        dimension m(1000), l(1000)
        !Defindo o valor da variável na qual será armazenado o tipo de
        !paridade da permutação.
        ipar = 1
        !Copiando os valores do vetor m já permutado, no vetor l.
        do i = 1, n
          l(i) = m(i)
        end do
        !Ordenando o vetor l a fim de obter sua paridade.
        do i = 1, n-1
          do j = i+1, n
          !Condição de troca dos valores na posição i do vetor l com
          !posiçao j do mesmo.
            if (l(i).gt.l(j)) then
              jtroca = l(i)
              l(i) = l(j)
              l(j) = jtroca
              !Alterando o valor da variável paridade a cada troca que é
              !realizada, pois ipar é -1 para número ímpar de trocas e 1
              !para números pares de troca.
              ipar = ipar*(-1)
            end if
          end do
        end do
        !adicionando o valor da paridade da permutação na posição n+1 do
        !vetor m.
        m(n+1) = ipar
      end subroutine paridade
!-----------------------------------------------------------------------
      !Essa subroutine irá calcular o determinante da matriz "amatriz".
      subroutine determinante(m, amatriz, n, det)
        !Definindo o tamanho do vetor m e da matriz "amatriz".
        dimension m(1000), amatriz(1000,1000)
        !A variável valor irá armazenar o valor de cada produto de
        !componentes da matriz já permutados.
        valor = 1
        !Looping na onde se pega os valores corretos de cada matriz e se
        !calcula cada parcela do determinante para depois soma-las.
        do i = 1, n
          !Multiplincando todos os valores da matriz "amatriz" que 
          !fazem parte da permutação de m.
          valor = valor*amatriz(i,m(i))
        end do
        !Somando ao determinando cada parcela que foi calculada no
        !valor.
        det = det + valor*m(n+1)
        !Abrindo o arquivo que armazena os dados de saída.
        open(3,file='saída-1-tarefa-6.')
        !Escrevendo na tela o valor do determinante que foi calculado.
        write(3,*) det
        !Fechando o arquivo que armazena os dados de saída.
        close(3)
      end subroutine determinante
!----------------------------------------------------------------------
      program main
        !Definindo o tamanho do vetor m, na onde serão guardados os
        !dados para realizar a permutação, e definindo o tamanho da
        !matriz "amatriz", na onde serão armazenados os valores da 
        !matriz.
        dimension m(1000), amatriz(1000,1000)
        k = 1
        !Definindo o valor da variável det, na onde será guardado o
        !valor do determinante.
        det = 0
        !Abrindo o aquivo onde serão lidos os dados.
        open(2,file='entrada-1-tarefa-6.')
        !Lendo no arquivo de entrada o tamanho da matriz que será
        !calculado o determinante.
        read(2,*) n
        !Lendo no arquivo de entrada a matriz.
        do i = 1, n
          read(2,*) (amatriz(i,j), j=1,n)
        end do
        !Fechando o arquivo que contém os dados de entrada.
        close(2)
        !Adicionando os valores que serão necessários permutar para
        !calcular o determinante de uma matriz n x n.
        do i = 1, n
          m(i) = i
        end do
        !Chamando a subiroutine que irá calcular realizar a permutação
        !no vetor m, e irá chamar a subroutine que irá calcular o valor
        !do determinante da matriz.
        call permutacao(m, amatriz, k, n, det)
      end program main
