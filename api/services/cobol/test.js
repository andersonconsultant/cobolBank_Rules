const cobolService = require('./index');
const { logInfo } = require('../../../../scripts/logger');

async function runTests() {
    logInfo('Starting COBOL Extrato Test', { timestamp: new Date() });

    try {
        logInfo('Requesting Extrato...');
        const extratoResult = await cobolService.getExtrato();
        
        if (extratoResult.success) {
            logInfo('Extrato obtido com sucesso');
            logInfo('Extrato raw:', { raw: extratoResult.extrato });
            
            // Formatar o extrato para melhor visualização
            const lines = extratoResult.extrato.split('\n');
            const transactionLine = lines.find(line => line.includes('transfer'));
            
            if (transactionLine) {
                const transactions = transactionLine
                    .split('#')      // Separa as transações
                    .filter(t => t.trim() && t.includes('|'))  // Remove entradas vazias e garante que é uma transação
                    .map(transaction => {
                        const [num, conta, valor, data, tipo] = transaction.split('|');
                        return {
                            numero: num.replace('transfer\\', ''), // Remove o prefixo da primeira transação
                            conta: conta,
                            valor: parseFloat(valor).toLocaleString('pt-BR', { style: 'currency', currency: 'BRL' }),
                            data: data,
                            tipo: tipo
                        };
                    });
                
                logInfo('Transações do Extrato:', { transactions });
                logInfo('Número total de transações:', { count: transactions.length });
            } else {
                logInfo('Nenhuma transação encontrada no extrato');
            }
        } else {
            logInfo('Falha ao obter extrato', extratoResult);
        }
    } catch (error) {
        logInfo('Teste falhou', { error: error.message });
        process.exit(1);
    }
}

// Run tests if called directly
if (require.main === module) {
    runTests().then(() => {
        process.exit(0);
    }).catch(error => {
        console.error('Teste falhou:', error);
        process.exit(1);
    });
}

module.exports = runTests; 