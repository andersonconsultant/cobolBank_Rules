const path = require('path');
const { spawn } = require('child_process');
const loggerConfig = require('../../utils/logger/loggerConfig');

class LogProcessor {
    constructor() {
        this.enginePath = loggerConfig.cobol.enginePath;
        this.isInitialized = false;
        this.maxRetries = loggerConfig.cobol.maxRetries;
        this.commandTimeout = loggerConfig.cobol.commandTimeout;
        this.maxQueueSize = loggerConfig.cobol.maxQueueSize;
        this.batchSize = loggerConfig.cobol.batchSize;
        this.retryDelay = loggerConfig.cobol.retryDelay;
    }

    async initialize() {
        try {
            // Verifica se o executável existe
            const engineExists = require('fs').existsSync(this.enginePath);
            if (!engineExists) {
                throw new Error(`Engine COBOL não encontrado em: ${this.enginePath}`);
            }

            // Verifica se tem permissão de execução
            try {
                require('fs').accessSync(this.enginePath, require('fs').constants.X_OK);
            } catch (error) {
                throw new Error(`Sem permissão para executar Engine COBOL em: ${this.enginePath}`);
            }

            this.isInitialized = true;
            return true;
        } catch (error) {
            throw new Error(`Falha ao inicializar processador: ${error.message}`);
        }
    }

    async processLog(logData) {
        if (!this.isInitialized) {
            throw new Error('Processador não inicializado');
        }

        return new Promise((resolve, reject) => {
            // Valida os dados conforme configuração
            const validationErrors = this._validateLogData(logData);
            if (validationErrors.length > 0) {
                reject(new Error(`Dados inválidos: ${validationErrors.join(', ')}`));
                return;
            }

            // Formata dados para o COBOL
            const cobolInput = this._formatLogForCobol(logData);

            // Executa o programa COBOL com timeout
            const cobol = spawn(this.enginePath, ['--log']);
            let output = '';
            let error = '';

            const timeout = setTimeout(() => {
                cobol.kill();
                reject(new Error(`Timeout após ${this.commandTimeout}ms`));
            }, this.commandTimeout);

            cobol.stdout.on('data', (data) => {
                output += data.toString();
            });

            cobol.stderr.on('data', (data) => {
                error += data.toString();
            });

            cobol.on('close', (code) => {
                clearTimeout(timeout);
                if (code !== 0) {
                    reject(new Error(`Engine COBOL falhou com código ${code}: ${error}`));
                    return;
                }
                resolve(this._parseCobolOutput(output));
            });

            // Envia dados para o COBOL
            cobol.stdin.write(cobolInput);
            cobol.stdin.end();
        });
    }

    async queryLogs(filters) {
        if (!this.isInitialized) {
            throw new Error('Processador não inicializado');
        }

        return new Promise((resolve, reject) => {
            // Valida os filtros conforme configuração
            const validationErrors = this._validateFilters(filters);
            if (validationErrors.length > 0) {
                reject(new Error(`Filtros inválidos: ${validationErrors.join(', ')}`));
                return;
            }

            // Formata filtros para o COBOL
            const cobolInput = this._formatFiltersForCobol(filters);

            // Executa o programa COBOL com timeout
            const cobol = spawn(this.enginePath, ['--query']);
            let output = '';
            let error = '';

            const timeout = setTimeout(() => {
                cobol.kill();
                reject(new Error(`Timeout após ${this.commandTimeout}ms`));
            }, this.commandTimeout);

            cobol.stdout.on('data', (data) => {
                output += data.toString();
            });

            cobol.stderr.on('data', (data) => {
                error += data.toString();
            });

            cobol.on('close', (code) => {
                clearTimeout(timeout);
                if (code !== 0) {
                    reject(new Error(`Engine COBOL falhou com código ${code}: ${error}`));
                    return;
                }
                resolve(this._parseCobolOutput(output));
            });

            // Envia filtros para o COBOL
            cobol.stdin.write(cobolInput);
            cobol.stdin.end();
        });
    }

    _validateLogData(logData) {
        const errors = [];
        const maxLengths = loggerConfig.validation.maxLengths;

        // Valida comprimentos máximos
        for (const [field, maxLength] of Object.entries(maxLengths)) {
            if (logData[field] && logData[field].toString().length > maxLength) {
                errors.push(`Campo ${field} excede o tamanho máximo de ${maxLength} caracteres`);
            }
        }

        return errors;
    }

    _validateFilters(filters) {
        const errors = [];
        const maxLengths = loggerConfig.validation.maxLengths;

        // Valida comprimentos máximos dos filtros aplicáveis
        for (const [field, value] of Object.entries(filters)) {
            if (value && maxLengths[field] && value.toString().length > maxLengths[field]) {
                errors.push(`Filtro ${field} excede o tamanho máximo de ${maxLengths[field]} caracteres`);
            }
        }

        return errors;
    }

    _formatLogForCobol(logData) {
        // Formata os dados conforme esperado pelo COBOL
        const maxLengths = loggerConfig.validation.maxLengths;
        const fields = [
            (logData.session_id || '').toString().padEnd(maxLengths.session_id),
            (logData.component || '').toString().padEnd(maxLengths.component),
            (logData.action || '').toString().padEnd(maxLengths.action),
            (logData.status || '').toString().padEnd(maxLengths.status),
            (logData.response_time || '0').toString().padStart(6),
            (logData.message || '').toString().padEnd(maxLengths.message),
            (logData.endpoint || '').toString().padEnd(maxLengths.endpoint),
            (logData.method || '').toString().padEnd(maxLengths.method),
            (logData.value || '0').toString().padStart(12)
        ];
        
        return fields.join('') + '\n';
    }

    _formatFiltersForCobol(filters) {
        // Formata os filtros conforme esperado pelo COBOL
        const maxLengths = loggerConfig.validation.maxLengths;
        const fields = [
            (filters.session_id || '*').padEnd(maxLengths.session_id),
            (filters.status || '*').padEnd(maxLengths.status),
            (filters.endpoint || '*').padEnd(maxLengths.endpoint),
            (filters.start_date || '*').padEnd(10),
            (filters.end_date || '*').padEnd(10)
        ];
        
        return fields.join('') + '\n';
    }

    _parseCobolOutput(output) {
        try {
            // Remove espaços em branco extras e divide em linhas
            const lines = output.trim().split('\n');
            const maxLengths = loggerConfig.validation.maxLengths;
            
            // Processa cada linha
            return lines.map(line => {
                // Extrai campos conforme layout do COBOL
                return {
                    session_id: line.substring(0, maxLengths.session_id).trim(),
                    component: line.substring(maxLengths.session_id, maxLengths.session_id + maxLengths.component).trim(),
                    action: line.substring(maxLengths.session_id + maxLengths.component, maxLengths.session_id + maxLengths.component + maxLengths.action).trim(),
                    status: line.substring(maxLengths.session_id + maxLengths.component + maxLengths.action, maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status).trim(),
                    response_time: parseInt(line.substring(maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status, maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status + 6)),
                    message: line.substring(maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status + 6, maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status + 6 + maxLengths.message).trim(),
                    endpoint: line.substring(maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status + 6 + maxLengths.message, maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status + 6 + maxLengths.message + maxLengths.endpoint).trim(),
                    method: line.substring(maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status + 6 + maxLengths.message + maxLengths.endpoint, maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status + 6 + maxLengths.message + maxLengths.endpoint + maxLengths.method).trim(),
                    value: parseFloat(line.substring(maxLengths.session_id + maxLengths.component + maxLengths.action + maxLengths.status + 6 + maxLengths.message + maxLengths.endpoint + maxLengths.method).trim())
                };
            });
        } catch (error) {
            throw new Error(`Erro ao processar saída do COBOL: ${error.message}`);
        }
    }

    async shutdown() {
        // Cleanup se necessário
        this.isInitialized = false;
        return true;
    }
}

// Exporta uma única instância
module.exports = new LogProcessor(); 