/**
 * Middleware para tratamento de arquivos estáticos
 * Implementa logging simplificado e otimizado para arquivos estáticos
 */

const { isStaticFile, STATIC_PATTERNS } = require('../utils/paths');

class StaticLogger {
    constructor() {
        this.staticHits = new Map();
        this.lastCleanup = Date.now();
        this.cleanupInterval = 1000 * 60 * 5; // 5 minutos
    }

    logStatic(req) {
        const now = Date.now();
        const path = req.path;
        
        // Cleanup antigo se necessário
        if (now - this.lastCleanup > this.cleanupInterval) {
            this.cleanup(now);
        }

        // Registra hit
        if (!this.staticHits.has(path)) {
            this.staticHits.set(path, {
                hits: 0,
                firstHit: now,
                lastHit: now
            });
        }

        const stats = this.staticHits.get(path);
        stats.hits++;
        stats.lastHit = now;
    }

    cleanup(now) {
        for (const [path, stats] of this.staticHits.entries()) {
            // Remove entradas mais antigas que 1 hora
            if (now - stats.lastHit > 1000 * 60 * 60) {
                this.staticHits.delete(path);
            }
        }
        this.lastCleanup = now;
    }

    getStats() {
        return Object.fromEntries(this.staticHits);
    }
}

// Singleton para estatísticas de arquivos estáticos
const staticLogger = new StaticLogger();

/**
 * Middleware para arquivos estáticos
 * @param {Object} options Opções de configuração
 * @param {boolean} options.enableLogging Habilita logging simplificado
 * @param {string[]} options.ignorePaths Caminhos a serem ignorados
 */
const staticMiddleware = (options = {}) => {
    const {
        enableLogging = true,
        ignorePaths = []
    } = options;

    return (req, res, next) => {
        // Marca o tempo inicial
        const startTime = Date.now();

        // Verifica se é arquivo estático e não está em ignorePaths
        if (isStaticFile(req.path) && !ignorePaths.some(path => req.path.startsWith(path))) {
            // Adiciona headers de cache otimizados
            res.setHeader('Cache-Control', 'public, max-age=86400'); // 24 horas
            
            // Log simplificado se habilitado
            if (enableLogging) {
                staticLogger.logStatic(req);
            }

            // Intercepta o método writeHead para adicionar o timing header
            const originalWriteHead = res.writeHead;
            res.writeHead = function(statusCode, statusMessage, headers) {
                // Calcula a duração
                const duration = Date.now() - startTime;
                
                // Adiciona o header de timing
                if (typeof statusMessage === 'object') {
                    headers = statusMessage;
                    statusMessage = undefined;
                }
                const finalHeaders = headers || {};
                finalHeaders['X-Static-Time'] = `${duration}ms`;
                
                // Chama o writeHead original com os headers atualizados
                return originalWriteHead.call(this, statusCode, statusMessage, finalHeaders);
            };
        }
        next();
    };
};

// Exporta o middleware e o logger para métricas
module.exports = {
    staticMiddleware,
    staticLogger
}; 