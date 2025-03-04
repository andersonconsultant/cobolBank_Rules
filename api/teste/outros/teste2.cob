#!/bin/bash
# Script para compilar os programas COBOL e criar o executável final

# Nomes dos arquivos
EXEC="sqlcommands"
MAIN_SRC="sqlcommands.cob"

ALLOC_OBJ="allocate.o"
ALLOC_SRC="allocate.cob"

DISCSQL_OBJ="disconnectsql.o"
DISCSQL_SRC="disconnectsql.cob"

# Compilar o objeto allocate.o
if [ -f "$ALLOC_OBJ" ]; then
    read -p "O arquivo '$ALLOC_OBJ' já existe. Deseja substituí-lo? (s/n): " resposta
    if [[ "$resposta" =~ ^[sS] ]]; then
        rm -f "$ALLOC_OBJ"
    else
        echo "Compilação do '$ALLOC_OBJ' cancelada."
        exit 1
    fi
fi
echo "Compilando $ALLOC_OBJ..."
cobc -c -free -o "$ALLOC_OBJ" "$ALLOC_SRC"

# Compilar o objeto disconnectsql.o
if [ -f "$DISCSQL_OBJ" ]; then
    read -p "O arquivo '$DISCSQL_OBJ' já existe. Deseja substituí-lo? (s/n): " resposta
    if [[ "$resposta" =~ ^[sS] ]]; then
        rm -f "$DISCSQL_OBJ"
    else
        echo "Compilação do '$DISCSQL_OBJ' cancelada."
        exit 1
    fi
fi
echo "Compilando $DISCSQL_OBJ..."
cobc -c -free -o "$DISCSQL_OBJ" "$DISCSQL_SRC"

# Etapa final de linkagem para criar o executável final
if [ -f "$EXEC" ]; then
    read -p "O arquivo '$EXEC' já existe. Deseja substituí-lo? (s/n): " resposta
    if [[ "$resposta" =~ ^[sS] ]]; then
        rm -f "$EXEC"
    else
        echo "Linkagem do '$EXEC' cancelada."
        exit 1
    fi
fi
echo "Linkando todos os objetos para criar $EXEC..."
cobc -x -free -o "$EXEC" "$MAIN_SRC" "$ALLOC_OBJ" "$DISCSQL_OBJ" -L/usr/lib/x86_64-linux-gnu -lodbc

echo "Compilação concluída."
