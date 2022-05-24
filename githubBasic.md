
# [Manual do Github](https://git-scm.com/book/en/v2)

[] crie um REAMDE.md
[1] crie um arquivo .gitignore a adcione o arquivo Manifest.toml
[2] git init
[3] git add src/ test/ Project.toml LICENSE.md README.md .gitignore
[4] git commit -m "Mensagem do commit""
[5] git status 
[6] git remote add origin https://github.com/maxlemes/NOME.jl
[7] git push -u origin master



## Inicializando um Repositório em um Diretório Existente

Se você tem um diretório de projeto que atualmente não está sob controle de versão e deseja começar a controlá-lo com o Git, primeiro **você precisa ir para o diretório desse projeto**. Se você nunca fez isso, parece um pouco diferente dependendo de qual sistema você está executando:

$ git init

Para adicionar os arquivos existente no diretório faça:

$ git add *.c
$ git add LICENSE
$ git commit -m 'Initial project version'


## Clonagem de um Repositório Existente

$ git clone https://github.com/repositorio ou

$ git clone https://github.com/repositorio pasta_destino


## Registrando Alterações no Repositório

Existem 4 tipos de arquivos:

1) arquivos não rastreados (Untracked): arquivos não lidos pelo Git
2) arquivos não modificado (Unmodified): arquivos lidos pelo Git que não foram modifcados desde o último commit
3) arquivos modificados (Modified): arquivos modificados, mas não adicionados.
4) arquivos preparados (Staged): arquivos adicionados e prontos para commit.

## Verificando o status dos seus arquivos

$ git status

## Rastreando Novos Arquivos / Preparando de Arquivos Modificados
Sempre que você adicionar um novo arquivo ao projeto ou modificar um arquivo existente você deve prepará-lo para commit com o comando

$ git add ARQUIVO

## Ignorando Arquivos

$ cat .gitignore
*.log.   

ignora os arquivos .log

## Visualizando suas alterações encenadas e não esfaqueadas
Esse comando compara o que está no seu diretório de trabalho com o que está na sua área de teste. O resultado indica as alterações que você fez que ainda não preparou.

$ git diff

## Confirmando Suas Alterações

$ git commit -a -m 'adicione a mensagem'

## Removendo Arquivos

$ git rm ARQUIVO

## Movendo Arquivos

$ git mv file_from file_to

## Exibindo o Histórico de Confirmação
Depois de criar vários commits, ou se você clonou um repositório com um histórico de commits existente, você provavelmente vai querer olhar para trás para ver o que aconteceu.

$ git log

## Desfazendo Coisas
Por exemplo, se você fizer o commit e perceber que esqueceu de preparar as alterações em um arquivo que deseja adicionar a esse commit, poderá fazer algo assim:

$ git commit -m 'Initial commit'
$ git add forgotten_file
$ git commit --amend

## Desestageamento de um arquivo em estágio

$ git reset HEAD ARQUIVO

## Desmodificando um Arquivo Modificado

$ git checkout -- ARQUIVO

## Mostrando Seus Controles Remotos

$ git remote -v

## Enviando para seus controles remotos

$ git push origin master

## Inspecionando um controle remoto

$ git remote show origin






