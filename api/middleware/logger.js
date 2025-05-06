/**
 * Middleware de logging otimizado
 * Implementa logging separado para diferentes tipos de requisições
 */

const { isStaticFile } = require('../utils/paths');
const { logRequest, logResponse, logError } = require('../utils/logger/console-logger');

class RequestMetrics {
    constructor() {
        this.metrics = {
            api: new Map(),
            static: new Map(),
            cobol: new Map()
        };
        this.lastCleanup = Date.now();
        this.cleanupInterval = 1000 * 60 * 15; // 15 minutos
    }

    recordMetric(type, path, duration, status) {
        const now = Date.now();
        
        // Cleanup se necessário
        if (now - this.lastCleanup > this.cleanupInterval) {
            this.cleanup();
        }

        // Registra métrica
        if (!this.metrics[type].has(path)) {
            this.metrics[type].set(path, {
                count: 0,
                totalDuration: 0,
                minDuration: duration,
                maxDuration: duration,
                errors: 0,
                lastAccess: now
            });
        }

        const metric = this.metrics[type].get(path);
        metric.count++;
        metric.totalDuration += duration;
        metric.minDuration = Math.min(metric.minDuration, duration);
        metric.maxDuration = Math.max(metric.maxDuration, duration);
        if (status >= 400) metric.errors++;
        metric.lastAccess = now;
    }

    cleanup() {
        const now = Date.now();
        const expiry = now - (1000 * 60 * 60); // 1 hora

        for (const type of Object.keys(this.metrics)) {
            for (const [path, metric] of this.metrics[type].entries()) {
                if (metric.lastAccess < expiry) {
                    this.metrics[type].delete(path);
                }
            }
        }

        this.lastCleanup = now;
    }

    getMetrics() {
        const result = {};
        for (const type of Object.keys(this.metrics)) {
            result[type] = Object.fromEntries(this.metrics[type]);
        }
        return result;
    }
}

// Singleton para métricas
const requestMetrics = new RequestMetrics();

/**
 * Middleware de logging
 * @param {Object} options Opções de configuração
 * @param {boolean} options.logStatic Se deve logar requisições estáticas
 * @param {boolean} options.logMetrics Se deve coletar métricas
 * @param {string[]} options.ignorePaths Caminhos a serem ignorados
 */
const loggerMiddleware = (options = {}) => {
    const {
        logStatic = false,
        logMetrics = true,
        ignorePaths = []
    } = options;

    return (req, res, next) => {
        // Marca tempo inicial
        req.startTime = Date.now();

        // Ignora caminhos configurados
        if (ignorePaths.some(path => req.path.startsWith(path))) {
            return next();
        }

        // Determina tipo de requisição
        const isStatic = isStaticFile(req.path);
        const isCobol = req.path.startsWith('/api/cobol');
        const type = isStatic ? 'static' : (isCobol ? 'cobol' : 'api');

        // Skip logging para estáticos se não configurado
        if (isStatic && !logStatic) {
            return next();
        }

        // Log da requisição
        logRequest(req);

        // Intercepta a resposta
        const oldEnd = res.end;
        res.end = function(...args) {
            // Calcula duração
            const duration = Date.now() - req.startTime;

            // Adiciona header de timing
            res.setHeader('X-Response-Time', `${duration}ms`);

            // Registra métricas se habilitado
            if (logMetrics) {
                requestMetrics.recordMetric(
                    type,
                    req.path,
                    duration,
                    res.statusCode
                );
            }

            // Log da resposta
            logResponse(req, res);

            oldEnd.apply(res, args);
        };

        next();
    };
};

// Middleware de erro
const errorLoggerMiddleware = () => {
    return (err, req, res, next) => {
        logError(err, req);
        next(err);
    };
};

// Exporta middlewares e métricas
module.exports = {
    loggerMiddleware,
    errorLoggerMiddleware,
    requestMetrics
}; 