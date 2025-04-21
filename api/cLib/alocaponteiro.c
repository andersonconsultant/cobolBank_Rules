#include <stdio.h>
#include <stdlib.h>

int* alocaVet(int tam) {
    int *saldo;
    saldo = (int *)malloc(tam * sizeof(int));
    if (saldo == NULL) {
        printf("Erro ao alocar memória.\n");
        return NULL;
    }
    return saldo;
}

int main() {
    int *saldo, tam, tam2;

    // printf("Digite o tamanho do vetor: ");
    // scanf("%d", &tam);
    tam = 1;

    if (tam <= 0) {
        printf("Tamanho inválido.\n");
        return 1;
    }

    saldo = alocaVet(tam);
    if (saldo == NULL) {
        return 1; // Erro ao alocar memória
    }

    *saldo = 10; // Atribuir o valor 10 ao primeiro elemento do array

    tam2 = 5;

    for (int i = 0; i < tam2; i++) {
        printf("Digite o saldo: \n Ou digite 0 para sair: ");
        scanf("%d", &saldo[0]); // Passar o endereço do elemento
        printf("Saldo atual: %d\n", *saldo);

        if  (*saldo == 0) {
            printf("Saindo...\n");
            break;
        }

    }


    // printf("Saldo atual: %d\n", saldo[0]);


    // printf("Saldo anterior: %d\n", saldo[0]);

    // printf("Digite o saldo: ");
    // scanf("%d", &*saldo); // Passar o endereço do primeiro elemento

    // printf("Saldo atual: %d\n", saldo[0]);

    free(saldo); // Liberar a memória alocada
    return 0;
}
