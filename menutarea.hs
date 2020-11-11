main = do
    putStrLn("------------Menú------------")
    putStrLn("Elige un caso: \n1. Serie Fibonacci \n2. Estructura for \n3. Factorial \n4. Desaparece números"
            ++"\n5. Palindromos \n6. Calculadora \n7. Salir")
    putStr("Elección:  ")
    n <- getLine
    casos (read n)
    
casos n= do
    case n of 
        1 -> fib1
        2 -> condicional 1
        3 -> factor
        4 -> des [0,1,2,3,4,5,6,7,8,9,10]
        5 -> pal
        6 -> calcu
        7 -> print "Fin del programa"
        _ -> inv 1
       

fib1 = do
    putStrLn("Ingresa el limite")
    n<-getLine
    print $ fib (read n+1) 
    main


fib n = take  n [fst t|t <- (iterate (\(x,y)->(y,x+y))(0,1))]

condicional n = do
        if n<=10
            then do
                print n
                condicional (n+1)
        else
            main

factor = do
    putStrLn("Ingresa el número")
    num1<- getLine
    print $ "El factorial es: " ++ show(factorial (read num1))


factorial 0 = 1
factorial n = n * factorial (n - 1)

des n= do
    if( length n > 0) then do
        let n2 = (init n)
        print $ n
        des n2
    else 
        main

pal = do
    putStrLn("Ingresa la cadena")
    n <- getLine
    palindromo n
    main

palindromo n = do if n == reverse n then print "Es palindromo" else print "No es palindromo"

calcu = do
    putStrLn("Escribe el primer digito")
    num1 <- getLine
    putStrLn("Escribe el primer digito")
    num2 <- getLine
    putStrLn("------------Menú Calculadora------------")
    putStrLn("Elige la operación a realizar")
    putStrLn("1. Suma")
    putStrLn("2. Resta")
    putStrLn("3. Multiplicacion")
    putStrLn("4. Division")
    putStrLn("5. Salir")
    putStr("Elección:  ")
    n <- getLine
    menu (read n) (read num1) (read num2)
    

menu n num1 num2 = do
    case n of
        1 -> suma num1 num2
        2 -> resta num1 num2
        3 -> multi num1 num2
        4 -> divis num1 num2
        5 -> main
        _ -> inv 2


suma num1 num2 = do
    let res= num1+num2
    putStrLn("El resultado de la suma es: "++ show(res))
    calcu

resta num1 num2 = do
    let res= num1-num2
    putStrLn("El resultado de la resta es: "++ show(res))
    calcu

multi num1 num2 = do
    let res= num1*num2
    putStrLn("El resultado de la multiplicacion es: "++ show(res))
    calcu

divis num1 num2 = do
    let res= num1/num2
    putStrLn("El resultado de la division es: "++ show(res))
    calcu

inv n = do
    print("Esa opcion es invalidad favor de ingresar otra")
    if(n==1) then
        main
    else
        calcu