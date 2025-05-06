# 🔍 Validação do Sistema de Logging

## 📋 Checklist de Pré-requisitos

### 1. Estrutura de Diretórios
- [ ] Verificar existência dos diretórios:
  ```
  Rules/api/
  ├── cobol/
  │   ├── DEV/
  │   │   └── bin/
  │   │       └── Enginev4TST
  │   └── processor/
  │       └── log-processor.js
  ├── routes/
  │   └── cobol.js
  ├── index.js
  └── utils/
      ├── paths.js
      └── logger/
          ├── config.js
          ├── console-logger.js
          ├── server.js
          └── index.js
  ```

### 2. Permissões e Executáveis
- [ ] Verificar permissões do Enginev4TST:
  ```bash
  chmod +x Rules/api/cobol/DEV/bin/Enginev4TST
  ```
- [ ] Testar execução direta do COBOL:
  ```bash
  cd Rules/api/cobol/DEV/bin
  ./Enginev4TST --test
  ```

### 3. Configurações
- [ ] Verificar variáveis de ambiente necessárias:
  ```bash
  DB_HOST=localhost
  DB_PORT=5432
  DB_NAME=cobolbd
  DB_TABLE=log_cobol_test
  ```

### 4. Estrutura de Portas
- [ ] Verificar disponibilidade das portas:
  ```
  3000 - Frontend
  3001 - Backend
  4000 - COBOL
  4001 - Logger
  ```

**Observação sobre as portas:**
- Portas 3xxx são usadas para serviços web (frontend, backend)
- Portas 4xxx são usadas para serviços internos (COBOL, logger)
- Esta organização facilita a gestão e identificação dos serviços

### 2. Resolução de Caminhos
- [ ] Verificar se paths.js está configurado corretamente:
  ```javascript
  module.exports = {
      paths: {
          config: resolveProjectPath('config.js'),
          logger: resolveProjectPath('Rules/api/utils/logger/console-logger.js'),
          // ... outros caminhos
      }
  };
  ```

- [ ] Verificar imports nos arquivos principais:
  ```javascript
  // Em routes/cobol.js
  const { logError } = require('../utils/logger/console-logger');

  // Em utils/logger/server.js
  const { paths } = require('../paths');
  const config = require(paths.config);

  // Em index.js (raiz da API)
  const { logRequest, logResponse, logError, logInfo } = require('./utils/logger/console-logger');
  ```

### 3. Configuração de Logging
- [ ] Verificar configurações de logging no config.js:
  ```javascript
  module.exports = {
      logging: {
          ignoreFrontend: false,        // Ignora logs do frontend
          ignoreStaticRequests: true,   // Ignora requisições de arquivos estáticos
          components: {                  // Componentes habilitados para logging
              backend: true,
              frontend: true,
              cobol: true
          }
      },
      filters: {
          staticPatterns: [             // Opcional: Padrões personalizados para arquivos estáticos
              /\.(css|js|jpg|jpeg|png|gif|ico|svg|woff|woff2|ttf|eot)$/i,
              /^\/static\//,
              /^\/assets\//
          ]
      }
  };
  ```

- [ ] Padrões default para arquivos estáticos (se não configurado):
  ```javascript
  const defaultStaticPatterns = [
      /\.(css|js|jpg|jpeg|png|gif|ico|svg|woff|woff2|ttf|eot)$/i,
      /^\/static\//,
      /^\/assets\//,
      /^\/images\//,
      /^\/fonts\//,
      /^\/styles\//
  ];
  ```

### 4. Tratamento de Configurações Ausentes
- [ ] O sistema deve funcionar mesmo sem todas as configurações definidas
- [ ] Usar optional chaining (?.) para acessar propriedades que podem não existir
- [ ] Fornecer valores default quando necessário
- [ ] Documentar quais configurações são opcionais

## 🧪 Cenários de Teste

### 1. Teste de Logging Básico
```javascript
const logger = require('./utils/logger');

// Teste INFO
logger.logInfo('Teste de mensagem INFO');

// Teste WARN
logger.logWarn('Teste de mensagem WARN');

// Teste ERROR
logger.logError(new Error('Teste de erro'));
```
✓ Deve exibir mensagens formatadas no console
✓ Deve respeitar as cores e ícones configurados

### 2. Teste de Requisição HTTP
```javascript
// Simular requisição
const req = {
    method: 'GET',
    url: '/api/test',
    body: {},
    startTime: Date.now()
};

// Simular resposta
const res = {
    statusCode: 200
};

logger.logRequest(req);
logger.logResponse(req, res, { success: true });
```
✓ Deve formatar corretamente os dados da requisição
✓ Deve calcular o tempo de resposta
✓ Deve enviar para processamento COBOL

### 3. Teste de Operação COBOL
```javascript
logger.logCobol('TEST_OPERATION', {
    session_id: 'TEST123',
    value: '100.00',
    status: 'OK'
});
```
✓ Deve formatar dados para COBOL
✓ Deve executar Enginev4TST
✓ Deve persistir no banco de dados

## 🔬 Validação de Integração

### 1. Fluxo Completo
1. Iniciar aplicação:
```bash
npm run dev
```

2. Verificar logs de inicialização:
```
✓ Log Processor inicializado
✓ Conexão com banco estabelecida
```

3. Executar requisição de teste:
```bash
curl http://localhost:3000/api/test
```

4. Verificar persistência:
```sql
SELECT * FROM log_cobol_test ORDER BY created_at DESC LIMIT 1;
```

### 2. Cenários de Erro
- [ ] Testar com banco indisponível
- [ ] Testar com COBOL engine inacessível
- [ ] Testar com dados inválidos
- [ ] Verificar tratamento de erros em cada camada

## 🚨 Problemas Conhecidos e Soluções

### 1. Erro: "Cannot find module '../../../scripts/logger'"
```
Error: Cannot find module '../../../scripts/logger'
Require stack:
- /srv/cb_Project/Rules/api/routes/cobol.js
```

**Causa:**
- Referências antigas ao logger no diretório scripts
- Caminhos relativos incorretos após reorganização do projeto

**Solução:**
1. Atualizar imports para usar o novo caminho do logger:
```javascript
// Incorreto
const { logError } = require('../../../scripts/logger');

// Correto
const { logError } = require('../utils/logger/console-logger');
```

2. Usar paths.js para resolução de caminhos:
```javascript
const { paths } = require('../utils/paths');
const logger = require(paths.logger);
```

### 2. Erro: "Cannot find module '/srv/cb_Project/Rules/config'"
```
Error: Cannot find module '/srv/cb_Project/Rules/config'
Require stack:
- /srv/cb_Project/Rules/api/utils/logger/server.js
```

**Causa:**
- Múltiplas formas de carregar o arquivo de configuração
- Uso de process.cwd() que pode variar dependendo de onde o processo é iniciado
- Caminhos absolutos hardcoded

**Solução:**
1. Usar apenas paths.js para carregar configurações:
```javascript
// Em utils/logger/server.js
const { paths } = require('../paths');
const config = require(paths.config);
```

## ✅ Verificação de Funcionamento

### 1. Inicialização dos Serviços
- [ ] Servidor de Logs (porta 4001):
  ```
  [LOG-SERVER] === Iniciando Servidor de Logs ===
  [LOG-SERVER] ✓ Processador COBOL inicializado
  [LOG-SERVER] ✓ Servidor iniciado na porta 4001
  ```

- [ ] Backend COBOL (porta 4000):
  ```
  [COBOL] Configuração carregada com sucesso
  [COBOL] ✓ Processador COBOL inicializado
  ```

- [ ] Backend Node.js (porta 3001):
  ```
  [BACKEND] Configuração carregada com sucesso
  [BACKEND] ✓ Processador COBOL inicializado
  ```

- [ ] Frontend (porta 3000):
  ```
  [FRONTEND] Configuração carregada com sucesso
  [FRONTEND] ✓ Processador COBOL inicializado
  ```

### 2. Arquivos Estáticos
- [ ] Estrutura de arquivos servida corretamente:
  ```
  /assets/design-system/...
  /js/...
  /components/...
  /services/...
  /styles/...
  ```

- [ ] Ordem correta de middleware:
  ```javascript
  app.use(express.static(paths.client.root));        // Primeiro: arquivos na raiz
  app.use('/assets', express.static(paths.client.assets));
  app.use('/js', express.static(paths.client.js));
  app.use('/components', express.static(paths.client.components));
  app.use('/services', express.static(paths.client.services));
  app.use('/styles', express.static(paths.client.styles));
  ```

### 3. Logging de Requisições
- [ ] Arquivos estáticos:
  ```
  [FRONTEND] → GET /assets/design-system/base/variables.css
  [FRONTEND] → GET /js/main.js
  ```

- [ ] Requisições de API:
  ```
  [BACKEND] → POST /api/auth/login
  [BACKEND] ← 200 OK (45ms)
  ```

- [ ] Operações COBOL:
  ```
  [COBOL] → EXEC OPERATION
  [COBOL] ← Result: OK
  ```

### 4. Portas e Endpoints
- [ ] Frontend: http://localhost:3000
- [ ] Backend API: http://localhost:3001/api
- [ ] COBOL Backend: http://localhost:4000/api
- [ ] Logger: http://localhost:4001

### 5. Processamento de Logs
- [ ] Arquivos estáticos: apenas log básico, sem processamento COBOL
- [ ] Requisições de API: log completo com processamento COBOL
- [ ] Operações COBOL: log completo com processamento COBOL
- [ ] Erros: log de erro com stack trace quando apropriado

## 🔄 Desacoplamento de Arquivos Estáticos e COBOL

### Problema Identificado
O sistema atual apresenta um acoplamento desnecessário entre o serviço de arquivos estáticos e o processador COBOL, causando:
- Atraso no carregamento de arquivos estáticos
- Dependência desnecessária do processador COBOL
- Sobrecarga do sistema de logging

### Solução Proposta

#### 1. Middleware Express Otimizado
```javascript
// Ordem correta dos middlewares
app.use(express.static(paths.client.root));        // 1º: Arquivos estáticos
app.use('/assets', express.static(paths.client.assets));
app.use('/js', express.static(paths.client.js));

// Depois: Middleware de logging e COBOL
app.use(loggerMiddleware);                         // 2º: Logging
app.use(cobolMiddleware);                          // 3º: COBOL
```

#### 2. Inicialização Lazy do COBOL
```javascript
class CobolProcessor {
    static instance = null;
    
    static async getInstance() {
        if (!this.instance) {
            this.instance = new CobolProcessor();
            await this.instance.initialize();
        }
        return this.instance;
    }
}

// Uso no middleware
const cobolMiddleware = async (req, res, next) => {
    if (isStaticFile(req.path)) {
        return next();
    }
    
    const processor = await CobolProcessor.getInstance();
    // ... processamento COBOL
};
```

#### 3. Configuração Atualizada
```javascript
module.exports = {
    logging: {
        static: {
            enabled: false,        // Desativa logging para arquivos estáticos
            patterns: [            // Padrões de arquivos estáticos
                /\.(css|js|jpg|jpeg|png|gif|ico|svg|woff|woff2|ttf|eot)$/i,
                /^\/static\//,
                /^\/assets\//
            ]
        },
        cobol: {
            lazyInit: true,       // Inicialização sob demanda
            required: false        // COBOL não é obrigatório para todos os endpoints
        }
    }
};
```

### Checklist de Implementação

#### 1. Estrutura de Arquivos
- [ ] Separar middlewares em arquivos distintos:
  ```
  Rules/api/middleware/
  ├── static.js
  ├── logger.js
  └── cobol.js
  ```

#### 2. Ordem de Inicialização
- [ ] Remover dependência do COBOL no startup
- [ ] Permitir início do frontend sem COBOL
- [ ] Implementar inicialização lazy do processador COBOL

#### 3. Logging
- [ ] Criar sistema de logging simplificado para arquivos estáticos
- [ ] Implementar filtros de logging baseados em tipo de requisição
- [ ] Separar logs de arquivos estáticos dos logs de processamento COBOL

### Verificação de Funcionamento

#### 1. Testes de Performance
- [ ] Medir tempo de resposta para arquivos estáticos antes/depois
- [ ] Verificar impacto no carregamento inicial da página
- [ ] Avaliar uso de memória do processador COBOL

#### 2. Testes de Independência
- [ ] Frontend deve funcionar sem processador COBOL
- [ ] Arquivos estáticos devem carregar mesmo com COBOL offline
- [ ] Logs não devem depender do estado do processador COBOL

#### 3. Monitoramento
- [ ] Implementar métricas de performance
- [ ] Adicionar logs de diagnóstico
- [ ] Criar dashboards de monitoramento

### Comandos de Verificação
```bash
# Verificar tempo de resposta dos arquivos estáticos
curl -w "%{time_total}\n" -o /dev/null -s http://localhost:3000/assets/style.css

# Testar funcionamento sem COBOL
systemctl stop cobol-processor
curl http://localhost:3000/

# Verificar logs independentes
tail -f logs/static-access.log
tail -f logs/cobol-processor.log
```

### Resultados Esperados
1. Arquivos estáticos servidos instantaneamente
2. Frontend funcional mesmo sem COBOL
3. Logs apropriados para cada tipo de requisição
4. Melhor utilização de recursos do sistema
5. Maior resiliência a falhas do processador COBOL

## 📝 Análise do Estado Atual

### Estrutura Existente
1. Diretório de Middlewares:
   ```
   Rules/api/middleware/
   └── validators.js
   ```

2. Configuração Atual (index.js):
   ```javascript
   // Inicialização precoce do COBOL
   (async () => {
       await logProcessor.initialize();
   })();

   // Logging global antes dos estáticos
   app.use((req, res, next) => {
       req.startTime = Date.now();
       logRequest(req);
       // ...
   });

   // Arquivos estáticos no final
   if (!process.env.BACKEND_ONLY) {
       app.use(express.static(paths.client.root));
       app.use('/assets', express.static(paths.client.assets));
       // ...
   }
   ```

### Problemas Identificados
1. **Inicialização Bloqueante:**
   - O processador COBOL é inicializado no startup
   - Bloqueia o início do servidor mesmo para arquivos estáticos
   - Não há lazy loading para funcionalidades COBOL

2. **Ordem Incorreta de Middlewares:**
   - Logging global afeta todas as requisições
   - Arquivos estáticos processados após logging
   - Sem distinção entre tipos de requisição

3. **Acoplamento Excessivo:**
   - Frontend depende da inicialização do COBOL
   - Logging misturado entre estáticos e dinâmicos
   - Sem separação clara de responsabilidades

## 🛠 Plano de Implementação

### ✅ Fase 1: Reorganização de Middlewares (Concluída)
1. Criados novos arquivos em `Rules/api/middleware/`:
   - `static.js`: Middleware otimizado para arquivos estáticos
     - Cache-Control automático
     - Logging simplificado
     - Métricas de acesso
   - `cobol.js`: Processador COBOL com lazy loading
     - Inicialização sob demanda
     - Gerenciamento de estado
     - Health check
   - `logger.js`: Sistema de logging separado
     - Métricas por tipo de requisição
     - Cleanup automático
     - Headers de performance

### 🔄 Fase 2: Atualização do index.js (Concluída)
1. **Remoção da Inicialização COBOL no Startup**
   - Removida inicialização síncrona do COBOL
   - Implementado lazy loading através do middleware

2. **Nova Ordem de Middlewares**
   ```javascript
   // 1. Segurança (primeiro, mais leve)
   app.use(helmet());
   app.use(cors());
   app.use(bodyParser);

   // 2. Arquivos Estáticos (antes do processamento pesado)
   app.use(staticMiddleware());
   app.use(express.static());

   // 3. Logging Otimizado
   app.use(loggerMiddleware());

   // 4. API com COBOL Lazy Loading
   app.use('/api', [
       apiLimiter,
       validationMiddleware,
       cobolMiddleware(),
       routes
   ]);
   ```

3. **Melhorias Adicionais**
   - Endpoint de métricas em desenvolvimento
   - Headers de performance
   - Logging separado por tipo
   - Health check do COBOL

### 🔄 Fase 3: Testes e Validação (Pendente)
- [ ] Implementar testes de carga
- [ ] Verificar métricas de performance
- [ ] Validar funcionamento sem COBOL

## 📝 Status de Implementação

### ✅ Fase 1: Reorganização de Middlewares (Concluída)
1. Criados novos arquivos em `Rules/api/middleware/`:
   - `static.js`: Middleware otimizado para arquivos estáticos
     - Cache-Control automático
     - Logging simplificado
     - Métricas de acesso
   - `cobol.js`: Processador COBOL com lazy loading
     - Inicialização sob demanda
     - Gerenciamento de estado
     - Health check
   - `logger.js`: Sistema de logging separado
     - Métricas por tipo de requisição
     - Cleanup automático
     - Headers de performance

### 🔄 Fase 2: Atualização do index.js (Concluída)
1. **Remoção da Inicialização COBOL no Startup**
   - Removida inicialização síncrona do COBOL
   - Implementado lazy loading através do middleware

2. **Nova Ordem de Middlewares**
   ```javascript
   // 1. Segurança (primeiro, mais leve)
   app.use(helmet());
   app.use(cors());
   app.use(bodyParser);

   // 2. Arquivos Estáticos (antes do processamento pesado)
   app.use(staticMiddleware());
   app.use(express.static());

   // 3. Logging Otimizado
   app.use(loggerMiddleware());

   // 4. API com COBOL Lazy Loading
   app.use('/api', [
       apiLimiter,
       validationMiddleware,
       cobolMiddleware(),
       routes
   ]);
   ```

3. **Melhorias Adicionais**
   - Endpoint de métricas em desenvolvimento
   - Headers de performance
   - Logging separado por tipo
   - Health check do COBOL

### 🔄 Fase 3: Testes e Validação (Pendente)
- [ ] Implementar testes de carga
- [ ] Verificar métricas de performance
- [ ] Validar funcionamento sem COBOL

## ✅ Critérios de Aceitação

### Performance
- [ ] Arquivos estáticos servidos em <50ms
- [ ] Servidor inicia em <2s
- [ ] Sem bloqueio por inicialização COBOL

### Funcionalidade
- [ ] Frontend funciona sem COBOL
- [ ] API mantém todas as funcionalidades
- [ ] Logs apropriados para cada tipo

### Monitoramento
- [ ] Métricas separadas para estáticos/dinâmicos
- [ ] Alertas para falhas do COBOL
- [ ] Dashboard de performance

## 📅 Próximos Passos

1. **Imediato:**
   - Criar os novos arquivos de middleware
   - Implementar lazy loading do COBOL
   - Atualizar ordem dos middlewares

2. **Curto Prazo:**
   - Implementar métricas de performance
   - Adicionar testes automatizados
   - Documentar mudanças

3. **Médio Prazo:**
   - Refinar sistema de logging
   - Otimizar cache de estáticos
   - Implementar health checks