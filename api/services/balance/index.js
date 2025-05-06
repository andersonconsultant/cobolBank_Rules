const { paths } = require('../../utils/paths');
const { logInfo, logError } = require('../../utils/logger/console-logger');
const config = require('../../../../config');
const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');

class BalanceService {
    constructor(useMock = false) {
        this.useMock = useMock;
        this._initializeMockData();

        // Caminho absoluto para o executável Engine
        this.enginePath = path.resolve(__dirname, '../../../..', config.executables.engine);

        try {
            fs.accessSync(this.enginePath, fs.constants.X_OK);
            logInfo('Engine', `Executável Engine encontrado e com permissões: ${this.enginePath}`);
        } catch (error) {
            logError('Engine', `Erro ao acessar executável Engine: ${error.message}`);
            throw new Error('Executável Engine não encontrado ou sem permissões');
        }
    }

    _initializeMockData() {
        this.mockData = {
            balance: 5432.10,
            previousBalance: 4982.10,
            lastUpdate: new Date(),
            variation: {
                percentage: 2.5,
                direction: 'up'
            }
        };
    }

    async getBalance() {
        try {
            if (this.useMock) {
                logInfo('Engine', 'Obtendo saldo do mock');
                return this.getMockBalance();
            }

            logInfo('Engine', 'Obtendo saldo do COBOL');
            return this.getRealBalance();
        } catch (error) {
            logError('Engine', `Erro ao obter saldo: ${error.message}`);
            throw error;
        }
    }

    async getMockBalance() {
        const { balance, previousBalance, lastUpdate, variation } = this.mockData;

        return {
            success: true,
            saldo: {
                atual: this.formatCurrency(balance),
                anterior: this.formatCurrency(previousBalance),
                raw: balance,
                variacao: {
                    percentual: variation.percentage,
                    direcao: variation.direction
                },
                ultimaAtualizacao: lastUpdate
            }
        };
    }

    async getRealBalance() {
        try {
            const cobolDir = path.dirname(this.enginePath);
            
            const output = execSync(`./${config.executables.name}`, {
                cwd: cobolDir,
                encoding: 'utf8',
                input: 'START\nSALDO\nEXIT\n'
            }).toString().trim();

            logInfo('Engine', `Saída COBOL: ${output}`);

            const match = output.match(/saldo\\(\d+,\d+)/);
            if (!match) {
                throw new Error(`Formato de saldo inválido. Saída: ${output}`);
            }

            const rawBalance = parseFloat(match[1].replace(',', '.'));
            logInfo('Engine', `Saldo convertido: ${rawBalance}`);

            return {
                success: true,
                saldo: {
                    atual: this.formatCurrency(rawBalance),
                    anterior: this.formatCurrency(rawBalance),
                    raw: rawBalance,
                    variacao: {
                        percentual: 0.0,
                        direcao: 'stable'
                    },
                    ultimaAtualizacao: new Date()
                }
            };
        } catch (error) {
            logError('Engine', `Erro ao executar Engine: ${error.message}`);
            throw new Error(`Erro ao obter saldo: ${error.message}`);
        }
    }

    formatCurrency(value) {
        return value.toLocaleString('pt-BR', {
            style: 'currency',
            currency: 'BRL'
        });
    }
}

module.exports = new BalanceService(false);
