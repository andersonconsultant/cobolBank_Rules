#include <stdio.h>
#include <stdlib.h>

int main() {
    char texto[256]; // Buffer para armazenar o texto

    FILE *arq;

    arq = fopen("arquivo.txt", "w"); // Abre o arquivo para escrita
    if (arq == NULL) {
        printf("Erro ao abrir o arquivo.\n");
        return 1;
    }

    printf("Arquivo aberto com sucesso.\n");
    printf("Digite o texto a ser editado (com espaços): ");
    fgets(texto, sizeof(texto), stdin); // Lê uma linha inteira, incluindo espaços
    printf("Texto editado: %s\n", texto);
    fprintf(arq, "%s", texto); // Escreve o texto no arquivo
    printf("Texto editado com sucesso.\n");

    fclose(arq);
    return 0;
}