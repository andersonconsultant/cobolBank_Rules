const { logInfo, logError } = require('../../../../scripts/logger');
const config = require('../../config');
const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');

/**
 * Serviço responsável por gerenciar operações relacionadas ao saldo
 * Este serviço é exclusivo do backend e não deve ser confundido
 * com os serviços do cliente em @client/services
 */
class BalanceService {
    constructor(useMock = false) {
        this.useMock = useMock;
        this._initializeMockData();
        
        // Caminho absoluto para o executável Engine
        this.enginePath = '/srv/cb_Project/Rules/api/cobol/DEV/Engine';
        
        // Verifica se o arquivo existe e tem permissões
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

    /**
     * Obtém o saldo atual
     * @returns {Promise<Object>} Objeto contendo informações do saldo
     */
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

    /**
     * Obtém saldo mockado para desenvolvimento
     * @private
     */
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

    /**
     * Obtém saldo real do sistema COBOL
     * @private
     */
    async getRealBalance() {
        try {
            // Muda para o diretório do Engine
            const cobolDir = path.dirname(this.enginePath);
            process.chdir(cobolDir);
            logInfo('Engine', `Diretório atual: ${process.cwd()}`);
            
            // Executa o Engine com comando SALDO
            const output = execSync('./Engine', {
                cwd: cobolDir,
                encoding: 'utf8',
                input: 'START\nSALDO\nEXIT\n'
            }).toString().trim();
            
            logInfo('Engine', `Saída COBOL: ${output}`);

            // Extrai o valor do saldo (formato: saldo\0000000775,54)
            const match = output.match(/saldo\\(\d+,\d+)/);
            if (!match) {
                throw new Error(`Formato de saldo inválido. Saída: ${output}`);
            }

            // Converte para número (troca vírgula por ponto)
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

    /**
     * Formata um valor para moeda brasileira
     * @private
     */
    formatCurrency(value) {
        return value.toLocaleString('pt-BR', {
            style: 'currency',
            currency: 'BRL'
        });
    }
}

// Exporta uma única instância do serviço
module.exports = new BalanceService(false); // Usando saldo real do COBOL 