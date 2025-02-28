      program ordenar
        !Dando um tamanho inicial para o vetor que será ordenado e
        !definindo seu nome como ordenado.
        dimension ordenado(1000000)
        !Abrindo o arquivo que contém os dados que serão lidos e
        !ordenados.
        open(unit=1, file='entrada-1-tarefa-3.')
        !Lendo o número de elementos que há na lista e o número de
        !elementos que se deseja ordenar (onde n é o número de elementos
        !que a lista possui).
        read(1,*) n, m
        !Lendo todos os dados do arquivo.
        do i = 1, n
          read(1,*) ordenado(i)
        enddo
        !Abrindo o arquivo que irá armazenar os dados solicitados.
        open(unit=2,file='saída-1-tarefa-3.')
        !Ordenando os m dados que foram solicitados.
        do i = 1, m
          do j = i+1, m
            if (ordenado(i) > ordenado(j)) then
              troca = ordenado(i)
              ordenado(i) = ordenado(j)
              ordenado(j) = troca
            endif
          enddo
          !Escrevendo no arquivo de saída o valor dos dados já
          !ordenados.
          write(2,*) ordenado(i)
        enddo
        close(2)
        close(1)
      end program ordenar
