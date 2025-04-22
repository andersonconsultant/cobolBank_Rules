# COBOL Banking System - Business Rules and Test Scenarios

## Controle de Versão

| Versão | Data       | Descrição das Alterações                   | Autor |
|--------|------------|-------------------------------------------|-------|
| 1.0.0  | 2024-03-25| Versão inicial - Regras de Negócio        | -     |
| 1.1.0  | 2024-04-22| Adicionada estrutura do projeto e API     | -     |

## Sobre este Documento

Este guia documenta as regras de negócio e cenários de teste para o sistema bancário COBOL-PostgreSQL. O documento serve como referência para desenvolvimento, testes e validação do sistema.

## Estrutura do Projeto

```
Rules/
├── api/                # API e Gateway
│   ├── cobol/         # Programas COBOL
│   │   ├── DEV/       # Ambiente de desenvolvimento
│   │   ├── TST/       # Ambiente de teste
│   │   └── bin/       # Binários compilados
│   ├── routes/        # Rotas da API
│   ├── config.js      # Configurações do servidor
│   └── index.js       # Ponto de entrada
├── package.json       # Dependências
└── GUIDE-RULES.md     # Este documento
```

## Integração com a API

### Endpoints Disponíveis

1. **Health Check**
   - `GET /v1/health`
   - Verifica o status da API
   - Retorna ambiente e status

2. **Execução COBOL**
   - `GET /v1/cobol/bin`
   - Inicia o programa COBOL
   - Retorna saída do programa

3. **Autenticação**
   - `POST /v1/cobol/login`
   - Autentica usuário
   - Parâmetros: username, password

4. **Transações**
   - `POST /v1/cobol/transaction`
   - Processa transação
   - Parâmetros: type, amount

### Exemplo de Uso

```javascript
// Exemplo de chamada para login
const response = await fetch('/v1/cobol/login', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ username: 'user123', password: 'pass123' })
});
```

## Ambientes

### Desenvolvimento (DEV)
- Localizado em `api/cobol/DEV/`
- Usado para desenvolvimento ativo
- Acesso direto ao banco de dados de desenvolvimento

### Teste (TST)
- Localizado em `api/cobol/TST/`
- Usado para testes e validação
- Banco de dados separado para testes

## Configurações

### Arquivos de Configuração

1. **config.js**
   - Configurações do servidor
   - Caminhos do projeto
   - Endpoints da API

2. **config.json**
   - Configurações do projeto
   - Caminhos relativos
   - Variáveis de ambiente

### Variáveis de Ambiente

```javascript
{
  "environment": "development",
  "paths": {
    "client": {
      "root": "../../client",
      "styles": "../../client/styles",
      "images": "../../client/images",
      "fonts": "../../client/fonts",
      "html": {
        "index": "../../client/index.html"
      }
    },
    "cobol": {
      "root": "./cobol",
      "programs": {
        "start": "./cobol/bin/start",
        "login": "./cobol/bin/login",
        "transaction": "./cobol/bin/transaction"
      }
    }
  }
}
```

## 1. Fluxo de Login e Autenticação

### Objetivo
Garantir um processo seguro e eficiente de autenticação de usuários no sistema.

### Regras de Negócio
1. Credenciais
   - Usuário deve fornecer ID e senha
   - Senha deve ser armazenada com criptografia
   - Bloqueio após 3 tentativas falhas

2. Recuperação de Senha
   - Verificação em duas etapas
   - Token de recuperação com validade de 15 minutos
   - Notificação por email

### Cenários de Teste

#### 1.1 Login com Sucesso
```sql
EXEC SQL
    SELECT user_id, password_hash, status
    FROM users
    WHERE user_id = :ws-user-id;
```
- **Entrada**: Credenciais válidas
- **Resultado Esperado**: Acesso concedido
- **Validação**: Registro em log de acesso

#### 1.2 Login Inválido
- **Entrada**: Credenciais incorretas
- **Resultado Esperado**: Mensagem "Credenciais inválidas"
- **Validação**: Contador de tentativas incrementado

#### 1.3 Recuperação de Senha
- **Entrada**: Email cadastrado
- **Resultado Esperado**: Email com token enviado
- **Validação**: Token registrado na base

## 2. Cadastro de Novo Cliente

### Objetivo
Permitir o registro de novos clientes com validação adequada de dados.

### Regras de Negócio
1. Dados Obrigatórios
   - Nome completo
   - CPF (com validação)
   - Data de nascimento
   - Endereço completo
   - Email (único)

2. Validações
   - CPF único no sistema
   - Idade mínima de 18 anos
   - Email válido e verificado

### Cenários de Teste

#### 2.1 Cadastro com Sucesso
```sql
EXEC SQL
    INSERT INTO customers
    (cpf, full_name, birth_date, email, address)
    VALUES
    (:ws-cpf, :ws-name, :ws-birth-date, :ws-email, :ws-address);
```
- **Entrada**: Dados válidos completos
- **Resultado Esperado**: Cliente registrado
- **Validação**: Número de conta gerado

#### 2.2 Cadastro Duplicado
- **Entrada**: CPF já existente
- **Resultado Esperado**: Erro de duplicidade
- **Validação**: Mensagem específica de erro

## 3. Processamento de Transações Financeiras

### Objetivo
Garantir a integridade e rastreabilidade das operações financeiras.

### Regras de Negócio
1. Transferências
   - Saldo suficiente
   - Limites por tipo de conta
   - Registro de histórico

2. Validações
   - Conta origem/destino ativas
   - Horário de operação
   - Limites diários

### Cenários de Teste

#### 3.1 Transferência com Sucesso
```sql
EXEC SQL
    UPDATE accounts
    SET balance = balance - :ws-amount
    WHERE account_id = :ws-source-account
    AND balance >= :ws-amount;
```
- **Entrada**: Transferência válida
- **Resultado Esperado**: Operação concluída
- **Validação**: Saldos atualizados

#### 3.2 Saldo Insuficiente
- **Entrada**: Valor > Saldo
- **Resultado Esperado**: Erro de saldo
- **Validação**: Transação não efetivada

## 4. Gerenciamento de Permissões

### Objetivo
Controlar o acesso às funcionalidades do sistema baseado em perfis.

### Regras de Negócio
1. Perfis
   - Administrador
   - Gerente
   - Caixa
   - Cliente

2. Restrições
   - Hierarquia de acessos
   - Registro de operações
   - Aprovações multinível

### Cenários de Teste

#### 4.1 Acesso Administrador
```sql
EXEC SQL
    SELECT permission_level
    FROM user_roles
    WHERE user_id = :ws-user-id;
```
- **Entrada**: Login admin
- **Resultado Esperado**: Acesso total
- **Validação**: Todas funcionalidades

#### 4.2 Acesso Restrito
- **Entrada**: Login cliente
- **Resultado Esperado**: Acesso limitado
- **Validação**: Bloqueio de áreas restritas

## 5. Geração de Relatórios

### Objetivo
Fornecer informações precisas e formatadas para análise.

### Regras de Negócio
1. Tipos de Relatório
   - Extrato de conta
   - Relatório gerencial
   - Análise de transações

2. Formatos
   - PDF
   - Excel
   - CSV

### Cenários de Teste

#### 5.1 Extrato de Conta
```sql
EXEC SQL
    SELECT transaction_date, description, amount
    FROM transactions
    WHERE account_id = :ws-account-id
    AND transaction_date BETWEEN :ws-start-date AND :ws-end-date;
```
- **Entrada**: Período válido
- **Resultado Esperado**: Relatório gerado
- **Validação**: Totais conferem

## Próximos Passos

1. Implementação das regras em COBOL
2. Criação de testes unitários
3. Validação com usuários-chave
4. Documentação de APIs
5. Treinamento da equipe

## Observações

- Todas as operações devem ser registradas em log
- Backup diário da base de dados
- Monitoramento de performance
- Plano de contingência atualizado 

## Implementação de Logs

### 1. Tabela LOG no PostgreSQL

```sql
EXEC SQL
    CREATE TABLE transaction_logs (
        log_id          SERIAL PRIMARY KEY,
        transaction_id  VARCHAR(36),
        user_id         VARCHAR(50),
        operation_type  VARCHAR(30),
        table_affected  VARCHAR(50),
        old_value      TEXT,
        new_value      TEXT,
        ip_address     VARCHAR(45),
        timestamp      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        status         VARCHAR(20),
        error_message  TEXT
    );
```

#### Exemplo de Registro de Log em COBOL:
```cobol
01  WS-LOG-RECORD.
    05 WS-TRANSACTION-ID    PIC X(36).
    05 WS-USER-ID          PIC X(50).
    05 WS-OPERATION-TYPE   PIC X(30).
    05 WS-TABLE-AFFECTED   PIC X(50).
    05 WS-OLD-VALUE        PIC X(1000).
    05 WS-NEW-VALUE        PIC X(1000).
    05 WS-IP-ADDRESS       PIC X(45).
    05 WS-STATUS           PIC X(20).
    05 WS-ERROR-MESSAGE    PIC X(1000).

PROCEDURE DIVISION.
    MOVE function RANDOM to WS-TRANSACTION-ID
    MOVE "INSERT" TO WS-OPERATION-TYPE
    
    EXEC SQL
        INSERT INTO transaction_logs
        (transaction_id, user_id, operation_type, table_affected,
         new_value, ip_address, status)
        VALUES
        (:WS-TRANSACTION-ID, :WS-USER-ID, :WS-OPERATION-TYPE,
         :WS-TABLE-AFFECTED, :WS-NEW-VALUE, :WS-IP-ADDRESS, :WS-STATUS)
    END-EXEC.
```

### 2. Arquivo de Log do Sistema

#### Estrutura do Arquivo de Log:
```text
[TIMESTAMP] [LEVEL] [TRANSACTION-ID] [USER] [OPERATION] [STATUS] [MESSAGE]
```

#### Exemplo de Implementação em COBOL:
```cobol
01  WS-LOG-FILE-NAME    PIC X(50) VALUE "bank_operations.log".
01  WS-LOG-RECORD.
    05 WS-TIMESTAMP     PIC X(23).
    05 FILLER          PIC X VALUE SPACE.
    05 WS-LEVEL        PIC X(5).
    05 FILLER          PIC X VALUE SPACE.
    05 WS-TRANS-ID     PIC X(36).
    05 FILLER          PIC X VALUE SPACE.
    05 WS-MESSAGE      PIC X(100).

PROCEDURE DIVISION.
    OPEN EXTEND WS-LOG-FILE-NAME
    MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
    MOVE "INFO" TO WS-LEVEL
    STRING "Operation completed successfully" 
           DELIMITED BY SIZE 
           INTO WS-MESSAGE
    WRITE WS-LOG-RECORD
    CLOSE WS-LOG-FILE-NAME.
```

### Recomendações de Uso

1. **Log no Banco de Dados**:
   - Operações críticas (transações financeiras)
   - Alterações em dados de clientes
   - Tentativas de login (sucesso/falha)
   - Alterações de permissões

2. **Log em Arquivo**:
   - Performance da aplicação
   - Erros de sistema
   - Problemas de conexão
   - Monitoramento em tempo real

### Política de Retenção

1. **Logs no Banco**:
   - Manter por 5 anos (requisito regulatório)
   - Criar tabelas particionadas por mês
   - Backup diário incremental

2. **Logs em Arquivo**:
   - Rotação diária
   - Compressão após 7 dias
   - Retenção de 90 dias
   - Backup semanal 