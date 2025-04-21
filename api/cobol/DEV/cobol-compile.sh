#!/bin/bash

# Configurações do ambiente
OCESQL_HOME="/home/server/Open-COBOL-ESQL-1.4"
COPY_DIR="$OCESQL_HOME/copy"
LIB_DIR="/usr/local/lib"

# Cores para output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color
BLUE='\033[0;34m'

# Função para exibir mensagens de status
log_status() {
    echo -e "${BLUE}>>> $1${NC}"
}

# Função para exibir erros
log_error() {
    echo -e "${RED}ERRO: $1${NC}"
    exit 1
}

# Função para exibir sucesso
log_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

# Função que compila e executa
build_and_run() {
    local source_file="$1"
    local output_name="${source_file%.*}" # Remove a extensão

    # Verifica se o arquivo existe
    if [ ! -f "$source_file" ]; then
        log_error "Arquivo $source_file não encontrado!"
    fi

    log_status "Iniciando compilação de $source_file"

    # Passo 1: Pré-processamento com ocesql
    log_status "Executando pré-processador OCESQL..."
    ocesql "$source_file" "$source_file.pre"
    if [ $? -ne 0 ]; then
        log_error "Falha no pré-processamento OCESQL"
    fi
    log_success "Pré-processamento concluído"

    # Passo 2: Compilação com cobc
    log_status "Compilando com GnuCOBOL..."
    cobc -x -o "$output_name" "$source_file.pre" \
        -I"$COPY_DIR" \
        -L"$LIB_DIR" \
        -locesql
    if [ $? -ne 0 ]; then
        log_error "Falha na compilação"
    fi
    log_success "Compilação concluída"

    # Passo 3: Execução com LD_PRELOAD
    log_status "Executando o programa..."
    export LD_PRELOAD="$LIB_DIR/libocesql.so"
    ./"$output_name"
    if [ $? -ne 0 ]; then
        log_error "Erro durante a execução do programa"
    fi
    log_success "Execução concluída com sucesso"
}

# Função para limpar arquivos temporários
clean() {
    log_status "Limpando arquivos temporários..."
    rm -f *.pre
    log_success "Limpeza concluída"
}

# Função para verificar dependências
check_deps() {
    log_status "Verificando dependências..."
    
    if ! command -v ocesql &> /dev/null; then
        log_error "ocesql não encontrado. Verifique a instalação do Open-COBOL-ESQL"
    fi
    
    if ! command -v cobc &> /dev/null; then
        log_error "cobc não encontrado. Verifique a instalação do GnuCOBOL"
    fi
    
    log_success "Todas as dependências encontradas"
}

# Função para exibir ajuda
show_help() {
    echo "Uso: $0 arquivo.cbl [opções]"
    echo
    echo "Opções:"
    echo "  -h, --help     Mostra esta ajuda"
    echo "  -c, --clean    Limpa arquivos temporários antes de compilar"
    echo
    echo "Exemplo:"
    echo "  $0 programa.cbl         # Compila e executa programa.cbl"
    echo "  $0 programa.cbl -c      # Limpa, compila e executa programa.cbl"
}

# Verifica se nenhum argumento foi fornecido
if [ $# -eq 0 ]; then
    show_help
    exit 1
fi

# O primeiro argumento deve ser o arquivo fonte
SOURCE_FILE="$1"
shift  # Remove o primeiro argumento, deixando apenas as opções

# Processa as opções restantes
CLEAN=0
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            exit 0
            ;;
        -c|--clean)
            CLEAN=1
            shift
            ;;
        *)
            log_error "Opção desconhecida: $1"
            ;;
    esac
done

# Verifica se o arquivo tem extensão .cbl ou .cob
if [[ ! "$SOURCE_FILE" =~ \.(cbl|cob)$ ]]; then
    log_error "O arquivo deve ter extensão .cbl ou .cob"
fi

# Execução principal
check_deps

if [ $CLEAN -eq 1 ]; then
    clean
fi

build_and_run "$SOURCE_FILE"