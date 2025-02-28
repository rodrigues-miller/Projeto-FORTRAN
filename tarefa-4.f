      !Definindo a função que calculará o cosseno através da série e
      !obterá o resultado em simples precisão.
      function cosseno(x)
        !Definindo as varáveis de dupla precisão que serão utilizadas.
        REAL*8 :: dcosseno,dcossenoanterior, dvalor, eprec, x
        !Definindo o valor inicial da série do cosseno.
        cosseno = 1.e0
        !Atribuindo o valor inicial da variável "valor", que receberá o
        !valor de cada termo da série do cosseno a cada iteração..
        valor = 1.e0
        !Fazendo o looping para realizar a soma dos termos da série.
        do i = 1, 10000000
          !A variável "valor" é o valor da cada termo da série, que
          !muda a cada iteração, e é somada no valor do cosseno.
          valor = (valor*(x**2))/((i*2)*(i*2-1))
          !Condição para a parada do looping, pois já foi atingida a
          !precisão desejada.
          if (abs(cosseno - cossenoanterior).lt.0.000001) then
            !Retornando o valor do cosseno calculado pela função.
            return
          end if
          !Atribuindo o valor antigo calculado do cosseno a variável
          !"cossenoanterior".
          cossenoaanterior = cosseno
          !Somando o termo calculado da série do cosseno, ao valor do
          !cosseno.
          cosseno =  cosseno + (-1)**(i)*valor
        end do 
      end function cosseno
!-----------------------------------------------------------------------
      !Definindo a função que calculará o cosseno através da série e
      !obterá o resultado em dupla precisão.
      function dcosseno(x, eprec)
        !Definindo as varáveis de dupla precisão que serão utilizadas.
        REAL*8 :: dcosseno, dcossenoanterior, dvalor, eprec, x
        !Definindo o valor inicial da série do cosseno.
        dcosseno = 1.d0
        !Atribuindo o valor inicial da variável "valor", que receberá o
        !valor de cada termo da série do cosseno a cada iteração.
        dvalor = 1.d0
        !Fazendo o looping para realizar a soma do termos da série.
        do i = 1, 10000000
          !A variável "dvalor" é o valor da cada termo da série, que
          !muda a cada iteração, e é somada no valor do cosseno.
          dvalor = (dvalor*(x**2))/((i*2)*(i*2-1))
          !Condição para a parada do looping, pois já foi atingida a
          !precisão desejada.
          if (abs(dcosseno - dcossenoanterior).lt.eprec) then
            !Retornando o valor do cosseno calculado pela função.
            return
          end if
          !Definindo o valor antigo calculado do cosseno a variável
          !"dcossenoanterior".
          dcossenoanterior = dcosseno
          !Somando o termo calculado da série do cosseno, ao valor do
          !cosseno.
          dcosseno = dcosseno + (-1)**(i)*dvalor
        end do 
      end function dcosseno
!-----------------------------------------------------------------------
      !Essa subroutine irá formatar e imprimir os resultados, calculados
      !em simples precisão, no formato de tabela.
      subroutine tabelaprecisaosimples(x)
        !Definindo as varáveis de dupla precisão que serão utilizadas.
        REAL*8 :: dcosseno,dcossenoanterior, dvalor, eprec, x
        !Imprimindo no arquivo de saída os resultados obtidos, em 
        !formato de tabela.
        write(2,100)
        write(2,50)
        write(2,101)
        write(2,102) x, cosseno(x), cos(x)
        write(2,100)
        !Definindo as formatações em que os dados serão impressos.
50      format('|Argumento|    Função    |    Fortran   |')
100     format('-----------------------------------------')
101     format('|---------|--------------|--------------|')
102     format('|',1x,f7.2,1x,'|',1x,f12.9,1x,'|',1x,f12.9,1x,'|')
      end subroutine tabelaprecisaosimples
!-----------------------------------------------------------------------
      !Essa subroutine irá formatar e imprimir os resultados, calculados
      !em dupla precisão, em formato de tabela.
      subroutine tabelaprecisaodupla(x)
        !Definindo as varáveis de dupla precisão que serão utilizadas.
        REAL*8 :: dcosseno, dcossenoanterior, dvalor, eprec, x
        !Imprimindo no arquivo de saída os resultados obtidos, em
        !formato de tabela.
        write(2,202)
        write(2,150)
        write(2,201)
        write(2,200) 0.000001d0, abs(dcosseno(x, 0.000001d0)-dcos(x))
        write(2,201)
        write(2,200) 0.0000001d0, abs(dcosseno(x, 0.000001d0)-dcos(x))
        write(2,201)
        write(2,200) 0.00000001d0, abs(dcosseno(x, 0.0000001d0)-dcos(x))
        write(2,201)
        write(2,200) 0.000000001d0, abs(dcosseno(x, 0.00000001d0)-
     &dcos(x))
        write(2,201)
        write(2,200) 0.0000000001d0, abs(dcosseno(x, 0.000000001d0)-
     &dcos(x))
        write(2,201)
        write(2,200) 0.00000000001d0, abs(dcosseno(x, 0.0000000001d0)-
     &dcos(x))
        write(2,201)
        write(2,200) 0.000000000001d0, abs(dcosseno(x, 0.00000000001d0)-
     &dcos(x))
        write(2,201)
        write(2,200) 0.0000000000001d0, 
     &abs(dcosseno(x, 0.0000000000001d0)-dcos(x))
        write(2,201)
        write(2,200) 0.00000000000001d0, 
     &abs(dcosseno(x, 0.00000000000001d0)-dcos(x))
        write(2,201)
        write(2,200) 0.000000000000001d0, 
     &abs(dcosseno(x, 0.000000000000001d0)-dcos(x))
        write(2,201)
        write(2,200) 0.0000000000000001d0, 
     &abs(dcosseno(x, 0.0000000000000001d0)-dcos(x))
        write(2,201)
        write(2,200) 0.00000000000000001d0, 
     &abs(dcosseno(x, 0.00000000000000001d0)-dcos(x))
        write(2,201)
        write(2,151) dcos(x)
        write(2,202)
        !Definindo a formatação em que os dados serão impressos.
150     format('|    eprec    |     erro cosseno     |')
151     format('|valor cosseno|',1x,f20.17,1x,'|')
200     format('|',3x,e7.1,3x,'|',5x,e10.1,7x,'|')
201     format('|-------------|----------------------|')
202     format('--------------------------------------')
      end subroutine tabelaprecisaodupla
!-----------------------------------------------------------------------
      program main
        !Definindo as varáveis de dupla precisão que serão utilizadas.
        REAL*8 :: dcosseno,dcossenoanterior, dvalor, eprec, x
        !Pedindo que o usuário informe o valor do ângulo em radianos 
        !para qual ele deseja calcular o cosseno.
        write(*,*) 'Digite o valor de x em radianos no qual deseja calcu
     &lar o cosseno:'
        read(*,*) x
        !Abrindo o arquivo onde os dados serão salvos.
        open(2, file="saída-1-tarefa-4.")
        write(2,*) 'Precisão simples'
        !Chamando a subroutine que irá rodar todo o programa para
        !simples precisão e imprimir os dados em forma de tabela.
        call tabelaprecisaosimples(x)
        write(2,*) ''
        write(2,*) 'Precisão dupla'
        !Chamando a subroutine que irá rodar todo o programa para
        !dupla precisão e imprimir os dados em forma de tabela.
        call tabelaprecisaodupla(x)
        !Fechando o arquivo de saída.
        close(2)
      end program main
