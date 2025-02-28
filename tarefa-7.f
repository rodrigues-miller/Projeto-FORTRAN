      !Essa subroutine recursiva irá permutar os números do vetor m e
      !irá chamar a subroutine que irá calcular o valor do determinante
      !da matriz "amatriz".
      recursive subroutine permutacao(m, amatriz, k, n, det)
        !Definindo o tamanho do vetor m , onde serão guardados os dados
        !para realizar a permutação, e definindo o tamanho da matriz
        !"amatriz", onde serão armazenados os valores da matriz.
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
        !Defindo o valor da variável "ipar", na qual será armazenada a
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
        !A variável "valor" irá armazenar o valor de cada produto de
        !componentes da matriz já permutados.
        valor = 1
        !Looping onde se pega os valores corretos de cada matriz e
        !calcula-se cada parcela do determinante para depois soma-las.
        do i = 1, n
          !Multiplincando todos os valores da matriz "amatriz" que 
          !fazem parte da permutação de m.
          valor = valor*amatriz(i,m(i))
        end do
        !Somando ao determinando cada parcela que foi calculada no
        !valor.
        det = det + valor*m(n+1)
      end subroutine determinante
!-----------------------------------------------------------------------
      program main
        !Definindo o tamanho do vetor m, onde serão guardados os dados
        !para realizar a permutação, definindo o tamanho da matriz
        !"amatriz", na onde serão armazenados os valores da matriz,
        !definindo o tamanho do vetor resultante do sistema "y".
        dimension m(1000), y(1000), troca(1000), amatriz(1000,1000)
        !Abrindo o arquivo que possui os dados que se deseja ler.
        open(1,file='entrada-1-tarefa-7.')
        !Abrindo o arquivo onde serão salvos os dados de saída.
        open(2,file='saída-1-tarefa-7.')
        !Lendo a primeira linha do arquivo de entrada, onde se diz
        !quantos sistemas serão resolvidos.
        read(1,*) l
        !Utilizando o "do" para que se possa correr por todos os 
        !sistemas que estão salvos no aruquivo.
        do il = 1,l
          !k mostra o número de iterações +1 que já ocorreram na
          !recursividade.
          k = 1
          !Definindo o valor da inicial da variável det, onde será
          !guardado o valor do determinante.
          det = 0
          !Lendo no arquivo de entrada qual o tamanho do sistema que
          !será calculado.
          read(1,*) n
          !Lendo no arquivo de entrada a matriz do sitema.
          do i = 1, n
            read(1,*) (amatriz(i,j), j=1,n)
          end do
          !Lendo no arquivo de entrada o vetor resultante do sistema.
          read(1,*) (y(j), j = 1, n)
          !Adicionando no vetor "m" os valores que serão necessários
          !permutar para calcular o determinante de uma matriz n x n.
          do i = 1, n
            m(i) = i
          end do
          !Chamando a subroutine que irá realizar a permutação no vetor
          !m, e irá chamar a subroutine que irá calcular o valor do
          !determinante da matriz "amatriz".
          call permutacao(m, amatriz, k, n, det)
          !Abaixo utilizei a regra de cramer para calcular a solução do
          !sistema.
          !Condição para que haja solução no sistema.
          if (det.NE.0) then
            !Esse loop irá percorrer todas as colunas da matriz
            !"amatriz".
            do i = 1, n
              !Definindo o valor do segundo determinante a ser
              !calculado.
              det2 = 0
              !Trocando o valor da coluna i da matriz "amatriz", pelos 
              !valores do vetor y.
              do j = 1, n
                troca(j) = amatriz(j,i)
                amatriz(j,i) = y(j)
              end do
              !Chamando a subroutine que irá calcular o valor do segundo
              !determinante.
              call permutacao(m, amatriz, k, n, det2)
              !Devolvendo os valores originais da coluna i da matriz
              !"amatriz".
              do j = 1, n
                amatriz(j,i) = troca(j)
              end do
              !Escrevendo no arquivo de saída a solução do sistema.
              write(2,100) i, det2/det
            end do
            !Escrevendo no aruquivo de saída um separador de linha
            !apenas para organizar a solução de cada sistema.
            write(2,50)
          else
            write(2,*) 'Não foi possível calcular a solução do siste
     &ma pois ele é indeterminado ou impossível'
          end if
        end do
        !Fechando os arquivos que foram utilizados.
        close(1)
        close(2)
        !Definindo os formatos utilizados no programa para impressão.
50      format('---------------')
100     format('x',I1,'=',f10.2)
      end program main
