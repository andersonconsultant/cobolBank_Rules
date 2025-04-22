# CobolBank API - Guia de Integração

## Visão Geral da API

A API do CobolBank serve como ponte entre a interface moderna e os sistemas COBOL legados, garantindo uma integração segura e eficiente.

## Estrutura do Projeto

```
Rules/api/
├── cobol/           # Programas COBOL
├── routes/          # Rotas da API
├── config.js        # Configurações do servidor
├── config.json      # Configurações do projeto
├── index.js         # Ponto de entrada da API
└── package.json     # Dependências e scripts
```

## Configuração do Projeto

### Configuração de Caminhos

1. **Estrutura de Diretórios**
   - Mantenha uma estrutura clara e organizada
   - Use caminhos relativos no `config.json` para maior portabilidade
   - Evite caminhos absolutos que possam quebrar em diferentes ambientes

2. **Configuração de Caminhos no config.json**
   ```json
   {
     "paths": {
       "client": {
         "root": "../../client",
         "styles": "../../client/styles",
         "images": "../../client/images",
         "fonts": "../../client/fonts",
         "html": {
           "index": "../../client/index.html"
         }
       }
     }
   }
   ```

3. **Resolução de Caminhos no index.js**
   ```javascript
   const path = require('path');
   
   // Resolve os caminhos do cliente
   const clientRoot = path.resolve(__dirname, config.paths.client.root);
   const clientStyles = path.resolve(__dirname, config.paths.client.styles);
   const clientImages = path.resolve(__dirname, config.paths.client.images);
   const clientFonts = path.resolve(__dirname, config.paths.client.fonts);
   const clientIndex = path.resolve(__dirname, config.paths.client.html.index);
   ```

### Servidor de Arquivos Estáticos

1. **Configuração do Express**
   ```javascript
   // Middleware
   app.use(cors(config.server.CORS_OPTIONS));
   app.use(bodyParser.json());
   app.use(bodyParser.urlencoded({ extended: true }));

   // Servir arquivos estáticos do frontend
   app.use(express.static(clientRoot));
   app.use('/styles', express.static(clientStyles));
   app.use('/images', express.static(clientImages));
   app.use('/fonts', express.static(clientFonts));
   ```

2. **Ordem das Rotas**
   - Defina a rota principal (`/`) antes das rotas da API
   - Use prefixo `/api` para todas as rotas da API
   - Mantenha o tratamento de erros no final

   ```javascript
   // Rota principal - serve o frontend
   app.get('/', (req, res) => {
     res.sendFile(clientIndex);
   });

   // Rotas da API (apenas para /api/*)
   app.use('/api', routes);

   // Tratamento de erros
   app.use((err, req, res, next) => {
     console.error(err.stack);
     res.status(500).send({ error: 'Algo deu errado!' });
   });
   ```

3. **Logs de Depuração**
   - Adicione logs para verificar os caminhos resolvidos
   - Inclua informações sobre o ambiente e URLs disponíveis

   ```javascript
   console.log('Caminhos do cliente:');
   console.log('Root:', clientRoot);
   console.log('Styles:', clientStyles);
   console.log('Images:', clientImages);
   console.log('Fonts:', clientFonts);
   console.log('Index:', clientIndex);

   app.listen(config.server.PORT, config.server.HOST, () => {
     console.log(`Servidor rodando em ${config.server.HOST}:${config.server.PORT}`);
     console.log(`Ambiente: ${config.environment}`);
     console.log(`Frontend: http://${config.server.HOST}:${config.server.PORT}`);
     console.log(`API: http://${config.server.HOST}:${config.server.PORT}/api`);
   });
   ```

## Rotas da API

### Estrutura das Rotas

1. **Prefixo Base**
   - Todas as rotas da API começam com `/api/v1`
   - Exemplo: `/api/v1/health`, `/api/v1/cobol/bin`

2. **Endpoints Disponíveis**
   ```javascript
   // Health Check
   GET /api/v1/health

   // COBOL
   GET /api/v1/cobol/bin
   POST /api/v1/cobol/login
   POST /api/v1/cobol/transaction
   ```

### Configuração de Rotas

1. **Arquivo de Rotas**
   - Localização: `routes/index.js`
   - Estrutura:
     ```javascript
     const express = require('express');
     const router = express.Router();
     
     // Health Check
     router.get('/v1/health', (req, res) => {
       res.json({ status: 'ok' });
     });
     
     // COBOL
     router.get('/v1/cobol/bin', (req, res) => {
       // Implementação
     });
     
     module.exports = router;
     ```

## Boas Práticas

1. **Configuração**
   - Use `config.json` para configurações do projeto
   - Use `config.js` para configurações do servidor
   - Mantenha as configurações organizadas e documentadas

2. **Caminhos**
   - Sempre use caminhos relativos
   - Use `path.resolve` para resolver caminhos
   - Mantenha a estrutura de diretórios consistente

3. **Rotas**
   - Use prefixos consistentes
   - Mantenha a ordem correta das rotas
   - Documente todos os endpoints

4. **Logs**
   - Adicione logs informativos
   - Inclua informações de depuração
   - Mantenha os logs organizados

5. **Tratamento de Erros**
   - Implemente tratamento de erros consistente
   - Retorne mensagens de erro claras
   - Registre erros no log

## Troubleshooting

1. **Problemas Comuns**
   - Caminhos incorretos: Verifique os logs de depuração
   - Rotas não encontradas: Verifique a ordem das rotas
   - Erros de servidor: Verifique os logs de erro

2. **Soluções**
   - Use os logs de depuração para identificar problemas
   - Verifique a configuração no `config.json`
   - Confirme a ordem das rotas no `index.js`

## Configuração e Instalação

1. **Instalar Dependências**
```bash
cd /srv/cobolBank_Rules/api
npm install
```

2. **Scripts Disponíveis**
```bash
# Iniciar o serviço de regras COBOL local
npm start

# (opcional) Gerenciar com PM2
pm2 start index.js --name cobol-rules-api
```

## API Endpoints

### Status da API
```http
GET /api/v1/health
```

**Resposta de Sucesso:**
```json
{
  "status": "ok",
  "environment": "development"
}
```

### Execução COBOL
```http
GET /api/v1/cobol/bin
```

**Resposta de Sucesso:**
```json
{
  "cobolOutput": "Resultado do programa COBOL..."
}
```

**Resposta de Erro:**
```json
{
  "error": "Erro ao executar o programa COBOL"
}
```

## Servindo Arquivos Estáticos

A API está configurada para servir arquivos estáticos do frontend automaticamente:

```javascript
// Configuração no index.js
app.use(express.static(path.join(__dirname, '../client')));
app.use('/styles', express.static(path.join(__dirname, '../client/styles')));
app.use('/images', express.static(path.join(__dirname, '../client/images')));
app.use('/fonts', express.static(path.join(__dirname, '../client/fonts')));
```

Acesso aos recursos:
- Frontend: `http://localhost:3000`
- Estilos: `http://localhost:3000/styles/style.css`
- Imagens: `http://localhost:3000/images/logo.png`
- Fontes: `http://localhost:3000/fonts/arimo.woff2`

## Integração COBOL

### Configuração
O sistema está configurado para executar programas COBOL localizados em `/srv/cobolBank_Rules/api/cobol/`:

```javascript
// Exemplo em routes/index.js
router.get('/cobol/bin', (req, res) => {
  exec(path.join(config.directories.rules.cobol, 'start'), 
    (error, stdout, stderr) => {
      if (error || stderr) {
        res.status(500).send({ error: 'Erro ao executar o programa COBOL' });
        return;
      }
      res.send({ cobolOutput: stdout });
    });
});
```

### Fluxo de Execução
1. Frontend faz requisição para `/api/v1/cobol/bin`
2. API localiza e executa o programa COBOL
3. Resultado é retornado em formato JSON
4. Frontend processa e exibe o resultado

## Tratamento de Erros

### API
```javascript
// Middleware de erro global
app.use((err, req, res, next) => {
  console.error(err.stack);
  res.status(500).send({ error: 'Algo deu errado!' });
});

// Rota 404
app.use((req, res) => {
  res.status(404).send({ error: 'Rota não encontrada' });
});
```

### Frontend
```javascript
try {
  const result = await callApi('/cobol/bin');
  // Processa sucesso
} catch (error) {
  // Trata erro
  alert('Erro ao executar programa COBOL');
}
```

## Logs e Monitoramento

### PM2 Logs
```bash
# Ver logs em tempo real
pm2 logs cobolbank-api

# Ver logs de erro
pm2 logs cobolbank-api --err
```

### Arquivos de Log
- Aplicação: `/var/log/cobolbank/api-out.log`
- Erros: `/var/log/cobolbank/api-error.log`

## Suporte

- Problemas API: backend@cobolbank.com
- Problemas COBOL: cobol@cobolbank.com
- Emergências: oncall@cobolbank.com

## Exposição HTTPS com Tailscale Funnel

### Configuração do Funnel

1. **Verificar Status do Tailscale**
   ```bash
   sudo tailscale status
   ```

2. **Expor a Aplicação com HTTPS**
   ```bash
   # Expor a aplicação com HTTPS via Funnel
   sudo tailscale funnel --bg --https 443 http://localhost:3000
   ```

3. **Verificar Status do Funnel**
   ```bash
   sudo tailscale funnel status
   ```
   Exemplo de saída:
   ```
   https://linux.taile65a90.ts.net (Funnel on)
   |-- / proxy http://localhost:3000
   ```

### Gerenciamento do Funnel

1. **Desativar o Funnel**
   ```bash
   sudo tailscale funnel --https=443 off
   ```

2. **Verificar Versão do Tailscale**
   ```bash
   tailscale version
   ```

### Acesso à Aplicação

- **HTTPS**: `https://linux.taile65a90.ts.net`
- A aplicação local continua acessível em: `http://localhost:3000`
- O Tailscale gerencia automaticamente:
  - Certificados SSL/TLS
  - Redirecionamento HTTP para HTTPS
  - Proxy reverso para a aplicação local

### Troubleshooting Funnel

1. **Verificar Status do Serviço**
   ```bash
   # Verificar se a aplicação está rodando localmente
   curl -v http://localhost:3000

   # Verificar status do Funnel
   sudo tailscale funnel status
   ```

2. **Problemas Comuns**
   - Certifique-se de que a aplicação está rodando na porta correta
   - Verifique se o Tailscale está conectado
   - Confirme que o Funnel está ativo e configurado corretamente

3. **Logs e Monitoramento**
   - Verifique os logs do Tailscale para diagnóstico
   - Monitore o status do Funnel regularmente 