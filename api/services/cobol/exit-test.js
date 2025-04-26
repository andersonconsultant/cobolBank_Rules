const cobolService = require('./index');
const { logInfo } = require('../../../../scripts/logger');

async function testExit() {
    logInfo('Testing COBOL Exit Command', { timestamp: new Date() });

    try {
        // Primeiro vamos fazer uma operação simples para garantir que o processo está rodando
        const saldoResult = await cobolService.getSaldo();
        logInfo('Saldo before exit:', saldoResult);

        // Agora vamos executar o exit
        const exitResult = await cobolService.exit();
        logInfo('Exit result:', exitResult);

        // Vamos tentar fazer outra operação para confirmar que o processo foi encerrado
        try {
            await cobolService.getSaldo();
        } catch (error) {
            logInfo('Expected error after exit - process is closed', { error: error.message });
        }

    } catch (error) {
        logInfo('Test failed', { error: error.message });
        process.exit(1);
    }
}

// Executar o teste
testExit().then(() => {
    process.exit(0);
}).catch(error => {
    console.error('Test failed:', error);
    process.exit(1);
}); 