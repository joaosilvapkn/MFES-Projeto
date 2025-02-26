Projeto de Defesa de Nota

Conversão de software concept (no ficheiro "meeting.als") usando refinamento why3 com versões imperativas e funcionais

Cada versão contêm uma implementação com listas e maps

Para ambas as implementações com listas, o código OcamL encontra-se extraído e com alguns exemplos 


1.Verificar o código:

  correr: "why3 prove meetingFuncList.mlw" ou "why prove meetingImpList.mlw"


2.Extraír código OcamL:

  correr: "why3 extract -D ocaml64 meetingFuncList.MeetingFuncListEx -o "filnename".ml -L ." ou "why3 extract -D ocaml64 meetingImpList.MeetingImpListEx -o "filnename".ml -L ." (nas implementações com listas)


3.Compilar exemplos em Ocaml:

  correr: "ocamlbuild -pkg zarith "filename".native" (nas implementações com listas)
