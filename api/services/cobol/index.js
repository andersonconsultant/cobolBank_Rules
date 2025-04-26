const { spawn } = require('child_process');
const path = require('path');
const { logInfo, logError } = require('../../../../scripts/logger');

class CobolService {
    constructor() {
        this.process = null;
        this.currentResponse = null;
        this.responseResolver = null;
        this.isConnected = false;
    }

    start() {
        const cobolPath = path.join(__dirname, '../../cobol/DEV/Engine');
        this.process = spawn(cobolPath);
        
        logInfo('COBOL Process Started', { path: cobolPath });
        
        this.process.stdout.on('data', (data) => {
            const output = data.toString().trim();
            // Log apenas se não for uma linha de prompt
            if (!output.startsWith('#Digite o comando:')) {
                logInfo('COBOL Output', { output });
            }
            
            if (this.responseResolver) {
                this.responseResolver(output);
            }
        });

        this.process.stderr.on('data', (data) => {
            logError('COBOL Error', null, { error: data.toString() });
        });

        this.process.on('close', (code) => {
            logInfo('COBOL Process Closed', { code });
            this.process = null;
            this.isConnected = false;
        });
    }

    async execute(command) {
        if (!this.process) {
            this.start();
        }

        let accumulator = '';
        let responseFound = false;
        let resolveTimeout = null;
        
        return new Promise((resolve) => {
            const responseTimeout = setTimeout(() => {
                if (resolveTimeout) {
                    clearTimeout(resolveTimeout);
                }
                resolve(accumulator);
            }, 2000);

            const handleOutput = (output) => {
                accumulator += output + '\n';

                if (responseFound && resolveTimeout) {
                    return;
                }
                
                const lines = accumulator.split('\n');
                for (const line of lines) {
                    const trimmed = line.trim();
                    if (trimmed === '1\\' || trimmed === '0\\') {
                        responseFound = true;
                        if (resolveTimeout) {
                            clearTimeout(resolveTimeout);
                        }
                        resolveTimeout = setTimeout(() => {
                            clearTimeout(responseTimeout);
                            resolve(accumulator);
                        }, 200);
                        return;
                    }
                }
            };

            this.responseResolver = handleOutput;
            
            this.process.stdin.write(command + '\n');
        }).finally(() => {
            this.responseResolver = null;
        });
    }

    parseResponse(response) {
        if (!response) return { success: false, message: 'Sem resposta do servidor' };
        
        const lines = response.split('\n')
            .map(line => line.trim())
            .filter(line => line && !line.startsWith('#Digite o comando:'));
            
        const status = lines.find(line => line === '1\\' || line === '0\\');

        if (!status) {
            return { success: false, message: 'Resposta inválida' };
        }

        const statusIndex = lines.indexOf(status);
        const message = lines
            .slice(statusIndex + 1)
            .find(line => line.startsWith('#'))
            ?.substring(1)
            .trim() || 'Operação realizada';

        return {
            success: status === '1\\',
            message
        };
    }

    async startConnection() {
        try {
            if (this.isConnected) {
                return { success: true, message: 'Já conectado ao banco' };
            }

            const result = await this.execute('START');
            const response = this.parseResponse(result);
            if (response.success) {
                this.isConnected = true;
            }
            
            return response;
        } catch (error) {
            logError('Error starting connection', error);
            throw error;
        }
    }

    async getSaldo() {
        try {
            if (!this.isConnected) {
                return { success: false, message: 'Banco não conectado. Execute startConnection() primeiro' };
            }

            const result = await this.execute('SALDO');
            
            // Aceita resposta direta saldo\
            if (result.includes('saldo\\')) {
                const [_, rawValue] = result.split('\\');
                if (!rawValue) return { success: true, saldo: null };
                
                // Limpa a resposta removendo comentários e formatando o valor
                const value = rawValue
                    .split('#')[0]  // Remove tudo após #
                    .trim();        // Remove espaços

                return {
                    success: true,
                    saldo: value
                };
            }
            
            // Se não encontrou o formato direto, tenta o formato com status
            const lines = result.split('\n')
                .map(line => line.trim())
                .filter(line => line && !line.startsWith('#Digite o comando:'));

            const status = lines.find(line => line === '1\\' || line === '0\\');
            if (!status) {
                return { success: false, message: 'Resposta inválida' };
            }

            const saldoLine = lines.find(line => line.startsWith('#saldo:'));
            if (!saldoLine) {
                return { success: false, message: 'Formato de saldo inválido' };
            }

            const saldo = saldoLine.substring(7).trim();
            return {
                success: status === '1\\',
                saldo
            };
        } catch (error) {
            logError('Error getting saldo', error);
            throw error;
        }
    }

    async getExtrato() {
        try {
            if (!this.isConnected) {
                return { success: false, message: 'Banco não conectado. Execute startConnection() primeiro' };
            }

            const result = await this.execute('EXTRATO');
            
            // Aceita resposta direta transfer\
            if (result.includes('transfer\\')) {
                const [_, data] = result.split('\\');
                if (!data) return { success: true, extrato: [] };
                
                return {
                    success: true,
                    extrato: data.split('#')
                        .filter(t => t.trim() && t.includes('|'))
                        .map(transaction => {
                            const [num, conta, valor, data, tipo] = transaction.split('|');
                            return {
                                numero: num.trim(),
                                conta: conta.trim(),
                                valor: parseFloat(valor).toLocaleString('pt-BR', { 
                                    style: 'currency', 
                                    currency: 'BRL' 
                                }),
                                data: data.trim(),
                                tipo: tipo.trim()
                            };
                        })
                };
            }
            
            // Se não encontrou o formato direto, tenta o formato com status
            const lines = result.split('\n')
                .map(line => line.trim())
                .filter(line => line && !line.startsWith('#Digite o comando:'));

            const status = lines.find(line => line === '1\\' || line === '0\\');
            if (!status) {
                return { success: false, message: 'Resposta inválida' };
            }

            const transactions = lines
                .filter(line => line.startsWith('#trans:'))
                .map(line => {
                    const transData = line.substring(7).trim();
                    const [num, conta, valor, data, tipo] = transData.split('|');
                    return {
                        numero: num.trim(),
                        conta: conta.trim(),
                        valor: parseFloat(valor).toLocaleString('pt-BR', { 
                            style: 'currency', 
                            currency: 'BRL' 
                        }),
                        data: data.trim(),
                        tipo: tipo.trim()
                    };
                });

            return {
                success: status === '1\\',
                extrato: transactions
            };
        } catch (error) {
            logError('Error getting extrato', error);
            throw error;
        }
    }

    async showHelp() {
        try {
            const result = await this.execute('HELP');
            return {
                success: true,
                help: result
            };
        } catch (error) {
            logError('Error showing help', error);
            throw error;
        }
    }

    async exit() {
        try {
            if (!this.process) {
                return { success: false, message: 'Processo não está em execução' };
            }

            const result = await this.execute('EXIT');
            const response = this.parseResponse(result);
            
            if (response.success || response.message.includes('finalizada')) {
                this.isConnected = false;
                this.process = null;
            }
            
            return response;
        } catch (error) {
            logError('Error executing exit', error);
            throw error;
        }
    }
}

// Singleton instance
const cobolService = new CobolService();

module.exports = cobolService; 