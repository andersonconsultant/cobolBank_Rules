#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h> // Necessário para usar argumentos variáveis

// Função msg atualizada para aceitar formato e argumentos variáveis
void msg(const char *format, ...) {
    char buffer[100];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);
    printf("Mensagem: %s\n", buffer);
}

// Função para alocar um vetor do tamanho solicitado
int* alocaVetor(int tamanhodovetor) {
    int *vetor;

    size_t tamanhoMemoria = tamanhodovetor * sizeof(int);
    msg("Tamanho total da memoria a ser alocada: %zu bytes", tamanhoMemoria);

    vetor = (int*)malloc(tamanhoMemoria);
    if (vetor == NULL) {
        msg("Erro ao alocar memoria.");
        return NULL;
    }

    return vetor;
}

int main() {
    int tamanho;
    msg("Digite o tamanho do vetor: ");
    scanf("%d", &tamanho);

    int *vetor = alocaVetor(tamanho);

    if (vetor != NULL) {
        for (int i = 0; i < tamanho; i++) {
            vetor[i] = i;
            printf("vetor[%d] = %d\n", i, vetor[i]);
        }

        // Libera a memória alocada
        free(vetor);
    }

    return 0;
}
