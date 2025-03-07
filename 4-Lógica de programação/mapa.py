
menu = int(input("\nEscolha uma opção:\n1.Inserir um novo cadastro\n2.Mostrar cadastros\n0.Encerrar\n"))
listaLivros = []
tam = 5

if menu == 0:
      print("programa encerrado")
elif menu == 1:
      for i in range(0, tam, 1):
            livros = {
                  'nomeDoLivro': "",
                  'nomeDoAltor': "",
                  'nomeDaEditora':"",
            }
            print("Nome do Livro:")
            livros ['nomeDoLivro'] = input()
            print("Nome do autor:")
            livros ['nomeDoAutor'] = input()
            print("Nome da editora:")
            livros ['nomeDaEditora'] = input()
            listaLivros.append(livros)
            menu = int(input("\nEscolha uma opção:\n1.Inserir um novo cadastro\n2.Mostrar cadastros\n0.Encerrar\n"))
      print("Sistema de cadastro lotado.")
      print("Livros cadastrados:")
      for i in range(0, tam, 1):
            print("Livro:", listaLivros[i]['nomeDoLivro'],"-Autor:", listaLivros[i]['nomeDoAutor'], "-Editora:", listaLivros[i]['nomeDaEditora'])   

else: 
      if menu == 2:
            for i in range(0, tam, 1):
                  print("Lista vazia!")
                  menu = int(input("\nEscolha uma opção:\n1.Inserir um novo cadastro\n2.Mostrar cadastros\n0.Encerrar\n"))
      else:
            print("Opção inválida")
      menu = int(input("\nEscolha uma opção:\n1.Inserir um novo cadastro\n2.Mostrar cadastros\n0.Encerrar\n"))





