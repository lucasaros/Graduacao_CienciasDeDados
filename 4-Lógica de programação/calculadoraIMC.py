#valor inicial da variável op
op = "y"

#estrutura de rapetição
while op == "y":
    peso = float(input("Qual seu peso?:\n"))
    altura = float(input("Qual sua altura?:\n"))
    imc = peso / (altura * altura)

    if imc <= 18.5:
        print("Você está abaixo do peso!")
        print(imc)
    else:
        if imc <= 24.9:
            print("Você está no peso ideal!")
            print(imc)
        else:
            print("Você está com sobrepeso!")
            print(imc)
    op = input("Deseja executar novamente? Digite y se sim.\n")