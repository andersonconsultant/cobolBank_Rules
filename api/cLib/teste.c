#include <stdio.h>
#include <stdlib.h>

//aloca um vetor do tamanho pedido

int* alocaVetor(int tam){
    int *vetor;
    vetor = (int*) malloc(tam * sizeof(int));
    return vetor;
}

int main(){
    int *vetor, tam, i;

    //Lendo o tamanho do vetor
    printf("Digite o tamanho do vetor: ");
    scanf("%d", &tam);

    vetor = alocaVetor(tam);

    //colocando alguns valores no vetor
    for(int i = 0; i < tam; i++){
        vetor[i] = i;
    }
    for(int i = 0; i < tam; i++){
        printf("%d\n", vetor[i]);
    }

    // // Libera a memória alocada
    // free(vetor);
    // printf("Memoria liberada\n");


    int index, value;


    // Lendo o índice e o valor para alterar no vetor
    printf("Digite o índice do vetor que deseja alterar: ");
    scanf("%d", &index);

    if (index >= 0 && index < tam) {
        printf("Digite o valor que deseja atribuir: ");
        scanf("%d", &value);

        vetor[index] = value;

        printf("Valor alterado com sucesso!\n");
        printf("Novo valor no índice %d: %d\n", index, vetor[index]);
    } else {
        printf("Índice inválido!\n");
    }

    //Printando cenário de vetores após alterações realizadas.
    printf("Cenario completo de vetores atualizado:\n");

    for(int i = 0; i < tam; i++){
        printf("%d\n", vetor[i]);
    }


    //Libera memória para os vetores/ponteiros definidos 
    free(vetor);
    printf("\nMemoria liberada\n");
    return 0;

}