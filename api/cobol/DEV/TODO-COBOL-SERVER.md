# 📋 TODO: COBOL Terminal Process Pool

Este TODO descreve como integrar nosso terminal COBOL interativo com Node.js.

## ✅ Fase 1: Terminal Interativo (IMPLEMENTADO)

Já implementamos com sucesso:

1. Programa COBOL interativo (`sSALDO.cbl`):
   ```cobol
   WORKING-STORAGE:
   01  DBNAME          PIC X(30).
   01  USERNAME        PIC X(30).
   01  PASSWD          PIC X(10).
   01  SALDO-RESULT    PIC 9(10)V99.
   01  TRANSFER-RESULT PIC X(500).
   01  WS-CMD          PIC X(10).

   PROCEDURE:
   - MAIN-RTN: Conecta ao banco
   - WAIT-CMD: Loop de comandos (saldo/extrato/exit)
   - GET-SALDO: Executa SELECT saldo()
   - GET-EXTRATO: Executa SELECT get_transfer()
   - FIM-PROGRAMA: Disconnect e finaliza
   ```

2. Comandos implementados:
   - `saldo`: Retorna "saldo\VALOR"
   - `extrato`: Retorna "transfer\RESULTADO"
   - `exit`: Encerra conexão e programa

## 🎯 Fase 2: Integração Node.js

1. Criar módulo para gerenciar processos COBOL:
   ```javascript
   const { spawn } = require('child_process');
   
   class CobolProcess {
     constructor() {
       this.process = null;
     }

     start() {
       this.process = spawn('./sSALDO');
       this.process.stdout.on('data', this.handleOutput);
     }

     async execute(command) {
       return new Promise((resolve) => {
         this.process.stdin.write(command + '\n');
         // Aguarda resposta específica do comando
       });
     }

     handleOutput(data) {
       // Processa saída do COBOL
       // "saldo\VALOR" ou "transfer\RESULTADO"
     }
   }
   ```

2. Pool de Processos:
   ```javascript
   class CobolPool {
     constructor(size = 5) {
       this.pool = new Map(); // sessionId -> CobolProcess
     }

     async getProcess(sessionId) {
       if (!this.pool.has(sessionId)) {
         const process = new CobolProcess();
         await process.start();
         this.pool.set(sessionId, process);
       }
       return this.pool.get(sessionId);
     }
   }
   ```

## 📝 Fase 3: API REST

1. Endpoints:
   ```javascript
   app.get('/saldo/:sessionId', async (req, res) => {
     const cobol = await pool.getProcess(req.params.sessionId);
     const result = await cobol.execute('saldo');
     res.json({ saldo: result });
   });

   app.get('/extrato/:sessionId', async (req, res) => {
     const cobol = await pool.getProcess(req.params.sessionId);
     const result = await cobol.execute('extrato');
     res.json({ extrato: result });
   });
   ```

## 🚀 Próximos Passos

1. Implementar pool de processos COBOL
2. Criar endpoints REST
3. Gerenciar timeout de sessões
4. Implementar reconexão automática

## 📊 Monitoramento

- Logs de execução dos comandos
- Número de processos ativos
- Tempo de resposta das queries
- Uso de memória por processo

---
**Obs.:** Esta abordagem é mais simples e robusta que TCP pois:
1. Usa recursos nativos do sistema operacional
2. Mantém o COBOL focado na lógica de negócio
3. Permite escalar horizontalmente com múltiplos processos
4. Facilita debug e monitoramento 