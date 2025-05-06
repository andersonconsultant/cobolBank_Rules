const { spawn } = require('child_process');

// Configuração do logger
const LOG_TYPES = {
    COBOL: 'COBOL',
    FRONTEND: 'FRONTEND',
    BACKEND: 'BACKEND'
};

class LogProcessor {
    constructor() {
        this.cobolProcess = null;
        this.isConnected = false;
    }

    // Inicializa a conexão com o COBOL
    async initialize() {
        console.log('=== Iniciando processador de logs ===\n');
        
        this.cobolProcess = spawn('./bin/Enginev4TST', [], { 
            cwd: '/srv/cb_Project/Rules/api/cobol/DEV',
            stdio: ['pipe', 'pipe', 'pipe']
        });

        // Processar erros do COBOL
        this.cobolProcess.stderr.on('data', (data) => {
            console.error('Erro COBOL:', data.toString());
        });

        // Processar saída do COBOL
        this.cobolProcess.stdout.on('data', (data) => {
            this.processCobolOutput(data.toString());
        });

        // Iniciar conexão com o banco
        await this.sendCommand('START');
        this.isConnected = true;
        console.log('Processador de logs inicializado e conectado ao banco');
    }

    // Processa a saída do COBOL
    processCobolOutput(line) {
        const trimmedLine = line.trim();
        if (!trimmedLine) return;

        if (trimmedLine.startsWith('log\\')) {
            const logData = trimmedLine.substring(4).split(';').map(item => item.trim());
            console.log('\nLog processado e salvo no banco:');
            console.log({
                session_id: logData[0],
                component: logData[1],
                action: logData[2],
                status: logData[3],
                response_time: logData[4],
                message: logData[5],
                endpoint: logData[6],
                method: logData[7],
                value: logData[8]
            });
            console.log('-------------------');
            return;
        }

        if (trimmedLine.includes('\\')) {
            const [key, value] = trimmedLine.split('\\');
            if (key.trim() === '1' || key.trim() === '0') {
                console.log(`COBOL Status: ${value.trim()}`);
            }
        } else {
            console.log('COBOL:', trimmedLine);
        }
    }

    // Envia comando para o COBOL
    async sendCommand(command) {
        return new Promise((resolve) => {
            console.log('\nEnviando comando:', command);
            this.cobolProcess.stdin.write(command + '\n');
            setTimeout(resolve, 500);
        });
    }

    // Processa um log recebido
    async processLog(logData) {
        if (!this.isConnected) {
            console.error('Processador não está conectado ao banco');
            return false;
        }

        try {
            await this.sendCommand('LOG');
            const logString = this.formatLogString(logData);
            await this.sendCommand(logString);
            return true;
        } catch (error) {
            console.error('Erro ao processar log:', error);
            return false;
        }
    }

    // Formata o log para o formato esperado pelo COBOL
    formatLogString(data) {
        return `${data.session_id};${data.component};${data.action};${data.status};${data.response_time};${data.message};${data.endpoint};${data.method};${data.value}`;
    }

    // Finaliza o processador
    async shutdown() {
        if (this.cobolProcess) {
            await this.sendCommand('EXIT');
            this.cobolProcess.kill();
            console.log('Processador de logs finalizado');
        }
    }
}

// Exporta o processador de logs
const logProcessor = new LogProcessor();
module.exports = logProcessor;

// Se executado diretamente, roda o teste
if (require.main === module) {
    // Função de teste
    async function runTest() {
        try {
            await logProcessor.initialize();

            // Exemplo de log
            const testLog = {
                session_id: "12345",
                component: "COBOL",
                action: "INSERT",
                status: "OK",
                response_time: "100",
                message: "Teste manual de log",
                endpoint: "/api/v1/test",
                method: "POST",
                value: "1500.75"
            };

            await logProcessor.processLog(testLog);
            await logProcessor.shutdown();
            process.exit(0);
        } catch (error) {
            console.error('Erro durante o teste:', error);
            process.exit(1);
        }
    }

    runTest();
} 