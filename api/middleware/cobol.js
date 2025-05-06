/**
 * Middleware para processamento COBOL
 * Implementa lazy loading e gerenciamento de estado do processador
 */

const { isStaticFile } = require('../utils/paths');
const { logError, logInfo } = require('../utils/logger/console-logger');

class CobolProcessor {
    static instance = null;
    
    constructor() {
        this.isInitialized = false;
        this.initPromise = null;
        this.lastError = null;
        this.healthStatus = 'NOT_INITIALIZED';
        this.retryCount = 0;
        this.maxRetries = 3;
    }

    static async getInstance() {
        if (!this.instance) {
            this.instance = new CobolProcessor();
        }
        return this.instance;
    }

    async initialize() {
        if (this.isInitialized) {
            return true;
        }

        // Se já está tentando inicializar, retorna a promise existente
        if (this.initPromise) {
            return this.initPromise;
        }

        this.initPromise = new Promise(async (resolve) => {
            try {
                // Aqui importamos o processador real apenas quando necessário
                const logProcessor = require('../cobol/processor/log-processor');
                await logProcessor.initialize();
                
                this.isInitialized = true;
                this.healthStatus = 'HEALTHY';
                this.lastError = null;
                this.retryCount = 0;
                
                logInfo('✓ Processador COBOL inicializado com sucesso');
                resolve(true);
            } catch (error) {
                this.lastError = error;
                this.healthStatus = 'ERROR';
                this.retryCount++;
                
                logError('Erro ao inicializar processador COBOL:', error);
                
                if (this.retryCount < this.maxRetries) {
                    logInfo(`Tentando novamente (${this.retryCount}/${this.maxRetries})...`);
                    // Limpa a promise para permitir nova tentativa
                    this.initPromise = null;
                    resolve(false);
                } else {
                    logError('Número máximo de tentativas excedido');
                    resolve(false);
                }
            }
        });

        return this.initPromise;
    }

    getHealth() {
        return {
            status: this.healthStatus,
            initialized: this.isInitialized,
            lastError: this.lastError ? this.lastError.message : null,
            retryCount: this.retryCount
        };
    }
}

/**
 * Middleware para processamento COBOL
 * @param {Object} options Opções de configuração
 * @param {boolean} options.required Se true, retorna erro 503 se COBOL não disponível
 * @param {number} options.maxRetries Número máximo de tentativas de inicialização
 */
const cobolMiddleware = (options = {}) => {
    const {
        required = false,
        maxRetries = 3
    } = options;

    return async (req, res, next) => {
        // Ignora arquivos estáticos
        if (isStaticFile(req.path)) {
            return next();
        }

        try {
            const processor = await CobolProcessor.getInstance();
            
            // Se COBOL é requerido e não está inicializado, tenta inicializar
            if (required && !processor.isInitialized) {
                const initialized = await processor.initialize();
                if (!initialized) {
                    return res.status(503).json({
                        error: 'Serviço COBOL indisponível',
                        details: processor.getHealth()
                    });
                }
            }

            // Adiciona o processador à requisição para uso posterior
            req.cobolProcessor = processor;
            next();
        } catch (error) {
            logError('Erro no middleware COBOL:', error);
            next(error);
        }
    };
};

// Exporta o middleware e a classe do processador
module.exports = {
    cobolMiddleware,
    CobolProcessor
}; 