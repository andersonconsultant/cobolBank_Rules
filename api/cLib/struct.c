#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Data{
    int dia;
    int mes;
    int ano;
}Data;

struct Perfil{
    int id;
    char nome[50];
    Data data_nascimento;
};

struct Endereco{
    char rua[50];
    int numero;
    char bairro[50];
    char cidade[50];
    char estado[3];
};


int main (){

    struct Perfil aluno1;
    struct Endereco endereco1;

    aluno1.id = 1;
    strcpy(aluno1.nome, "João da Silva");
    aluno1.data_nascimento.dia = 15;
    aluno1.data_nascimento.mes = 8;
    aluno1.data_nascimento.ano = 1990;
    strcpy(endereco1.rua, "Rua das Flores");
    endereco1.numero = 123;
    strcpy(endereco1.bairro, "Centro");
    strcpy(endereco1.cidade, "São Paulo");
    strcpy(endereco1.estado, "SP");
    printf("ID: %d\n", aluno1.id);
    printf("Nome: %s\n", aluno1.nome);
    printf("Data de Nascimento: %02d/%02d/%04d\n", aluno1.data_nascimento.dia, aluno1.data_nascimento.mes, aluno1.data_nascimento.ano);
    printf("Endereço: %s, %d, %s, %s - %s\n", endereco1.rua, endereco1.numero, endereco1.bairro, endereco1.cidade, endereco1.estado);

}