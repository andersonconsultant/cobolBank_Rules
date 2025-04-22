# GUIDE-API: Guia de Integração com Backend

## 1. Visão Geral da API

### 1.1 Estrutura do Projeto
```
Rules/api/
├── cobol/           # Programas COBOL
├── routes/          # Rotas da API
├── utils/           # Utilitários
│   └── paths.js    # Sistema de resolução de paths
├── log/            # Logs da aplicação
├── teste/          # Testes automatizados
├── config.js       # Configurações
├── index.js        # Ponto de entrada
└── script.js       # Scripts utilitários
```

### 1.2 Sistema de Paths

#### 1.2.1 Resolução de Paths (utils/paths.js)
```javascript
const path = require('path');

// Obtém o diretório raiz do projeto (2 níveis acima da pasta api)
const PROJECT_ROOT = path.resolve(__dirname, '../../../');

function resolveProjectPath(...segments) {
    return path.join(PROJECT_ROOT, ...segments);
}

module.exports = {
    PROJECT_ROOT,
    resolveProjectPath,
    paths: {
        config: resolveProjectPath('config.json'),
        client: {
            root: resolveProjectPath('client'),
            styles: resolveProjectPath('client/styles'),
            images: resolveProjectPath('client/images'),
            fonts: resolveProjectPath('client/fonts'),
            html: {
                index: resolveProjectPath('client/index.html')
            }
        },
        api: {
            root: resolveProjectPath('Rules/api'),
            cobol: resolveProjectPath('Rules/api/cobol')
        }
    }
};
```

#### 1.2.2 Uso dos Paths
```javascript
// Em qualquer arquivo da API
const { paths } = require('./utils/paths');

// Acessando paths do cliente
console.log('Root:', paths.client.root);
console.log('Index HTML:', paths.client.html.index);

// Acessando paths da API
console.log('API Root:', paths.api.root);
console.log('COBOL Path:', paths.api.cobol);

// Usando em configurações do Express
app.use(express.static(paths.client.root));
app.use('/styles', express.static(paths.client.styles));
```

### 1.3 Configuração do Servidor (index.js)
```javascript
const express = require('express');
const cors = require('cors');
const bodyParser = require('body-parser');
const { paths } = require('./utils/paths');
const config = require(paths.config);

// Middleware
app.use(cors(config.server.cors));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));

// Rotas da API
app.use('/api', routes);

// Servir frontend usando paths resolvidos
app.use(express.static(paths.client.root));
app.use('/styles', express.static(paths.client.styles));
app.use('/images', express.static(paths.client.images));
app.use('/fonts', express.static(paths.client.fonts));

// SPA fallback usando path resolvido
app.get('*', (req, res) => {
  res.sendFile(paths.client.html.index);
});
```

## 2. Endpoints

### 2.1 Versionamento
Todas as rotas da API são versionadas com prefixo `/v1`:
```
/api/v1/health
/api/v1/cobol/bin
/api/v1/cobol/login
/api/v1/cobol/transaction
```

### 2.2 Autenticação

#### Login
```http
POST /api/v1/cobol/login
Content-Type: application/json

Request:
{
  "username": string,
  "password": string
}

Response:
{
  "success": boolean,
  "token": string,
  "user": {
    "id": number,
    "name": string,
    "balance": string
  }
}
```

### 2.3 Operações COBOL

#### Execução de Programa
```http
GET /api/v1/cobol/bin
Authorization: Bearer <token>

Response:
{
  "output": string
}
```

#### Realizar Transação
```http
POST /api/v1/cobol/transaction
Authorization: Bearer <token>
Content-Type: application/json

Request:
{
  "type": string,
  "amount": number
}

Response:
{
  "output": string
}
```

## 3. Integração COBOL

### 3.1 Execução de Programas
```javascript
const { exec } = require('child_process');
const config = require('../config');

// Exemplo de execução
exec(config.paths.COBOL.PROGRAMS.START, (error, stdout, stderr) => {
  if (error || stderr) {
    console.error(`Erro: ${error?.message || stderr}`);
    return;
  }
  // Processa stdout
});
```

### 3.2 Estrutura de Arquivos COBOL
```
Rules/api/cobol/
├── start/           # Inicialização
│   └── PGMSTART
├── login/          # Autenticação
│   └── PGMLOGIN
└── transaction/    # Operações
    ├── PGMTRANSF
    └── PGMBAL
```

## 4. Tratamento de Erros

### 4.1 Middleware de Erro Global
```javascript
app.use('/api', (err, req, res, next) => {
  console.error('Erro na API:', err.stack);
  res.status(500).json({ error: 'Algo deu errado!' });
});
```

### 4.2 Erros Específicos
```javascript
// Erro de autenticação
res.status(401).json({ 
  success: false,
  message: 'Credenciais inválidas'
});

// Erro de execução COBOL
res.status(500).json({ 
  success: false,
  message: 'Erro ao executar programa COBOL'
});
```

## 5. Ambiente de Desenvolvimento

### 5.1 Arquitetura do Servidor (Fase Atual - Desenvolvimento)
> **Nota: Fase Temporária de Desenvolvimento**
>
> Na fase atual do projeto, o servidor Express está configurado para servir tanto a API quanto o frontend em um único processo.
> Esta é uma configuração temporária para facilitar o desenvolvimento inicial e testes.
> 
> **Plano de Evolução:**
> - Fase 1 (Atual): Servidor único para API e frontend
> - Fase 2: Separação do backend em servidor independente
> - Fase 3: Implementação de comunicação via requisições HTTP entre frontend e backend
> - Fase 4: Possível containerização dos serviços

#### Configuração Atual
- Frontend servido como arquivos estáticos
- API disponível sob o prefixo `/api`
- Todas as outras rotas redirecionam para o SPA (frontend)
- Configuração centralizada via `config.json` e `config.js`

### 5.2 Inicialização
```bash
# Único comando para iniciar o servidor unificado
npm start
```

### 5.3 Endpoints Unificados (Fase Atual)
Todo o sistema roda em uma única porta (3000 por padrão):
- Frontend: http://localhost:3000
- API: http://localhost:3000/api
- Health Check: http://localhost:3000/api/v1/health

> **Nota sobre Evolução:**
> Esta estrutura unificada será modificada quando o backend for separado em seu próprio servidor.
> As URLs e portas serão atualizadas de acordo com a nova arquitetura.

## 6. Logs e Monitoramento

### 6.1 Logs da API
```javascript
// Logs de inicialização
console.log('Caminhos do cliente:');
console.log('Root:', paths.client.root);
console.log('Styles:', paths.client.styles);

// Logs de erro
console.error('Erro na API:', err.stack);
console.error('Erro ao executar COBOL:', error.message);
```

### 6.2 Estrutura de Logs
- **API**: `/Rules/api/log/api.log`
- **COBOL**: `/Rules/api/log/cobol.log`
- **Erros**: `/Rules/api/log/error.log`

## 7. Segurança

### 7.1 CORS
```javascript
app.use(cors(config.server.cors));
```
### 7.2 Validação de Entrada
- Sanitização de inputs
- Validação de tipos
- Proteção contra injeção

### 7.3 Autenticação
- Token JWT
- Validação de sessão
- Proteção de rotas
