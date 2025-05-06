const express = require('express');
const cors = require('cors');
const bodyParser = require('body-parser');
const rateLimit = require('express-rate-limit');
const helmet = require('helmet');
const { validationResult } = require('express-validator');
const { paths } = require('./utils/paths');
const config = require('../../config');
const routes = require('./routes');

// Importa os novos middlewares
const { staticMiddleware, staticLogger } = require('./middleware/static');
const { cobolMiddleware, CobolProcessor } = require('./middleware/cobol');
const { loggerMiddleware, errorLoggerMiddleware, requestMetrics } = require('./middleware/logger');

const app = express();

// Configurações básicas
app.set('trust proxy', 1);

// Rate Limiting para API
const apiLimiter = rateLimit({
    windowMs: 60 * 1000,
    max: 10,
    message: {
        success: false,
        error: 'Limite de requisições excedido',
        details: 'Para sua segurança, limitamos a 10 requisições por minuto',
        retryAfter: '1 minuto',
        code: 'RATE_LIMIT_EXCEEDED'
    },
    standardHeaders: true,
    legacyHeaders: false,
    handler: (req, res) => {
        const retryAfter = Math.ceil(req.rateLimit.resetTime - Date.now()) / 1000;
        res.status(429).json({
            success: false,
            error: 'Limite de requisições excedido',
            details: 'Para sua segurança, limitamos a 10 requisições por minuto',
            retryAfter: `${Math.ceil(retryAfter)} segundos`,
            code: 'RATE_LIMIT_EXCEEDED',
            limit: {
                max: 10,
                remaining: req.rateLimit.remaining,
                reset: Math.ceil(retryAfter)
            }
        });
    }
});

// 1. Middlewares de Segurança (primeiro, pois são mais leves e críticos)
app.use(helmet({
    contentSecurityPolicy: {
        directives: {
            defaultSrc: ["'self'"],
            scriptSrc: ["'self'", "'unsafe-inline'", "'unsafe-eval'", "https://cdn.jsdelivr.net"],
            styleSrc: ["'self'", "'unsafe-inline'", "https://cdn.jsdelivr.net", "https://cdnjs.cloudflare.com"],
            imgSrc: ["'self'", "data:", "https:"],
            fontSrc: ["'self'", "https://cdnjs.cloudflare.com"],
            connectSrc: ["'self'"],
            objectSrc: ["'none'"],
            mediaSrc: ["'self'"],
            frameSrc: ["'none'"]
        }
    },
    crossOriginEmbedderPolicy: false,
    crossOriginResourcePolicy: { policy: "cross-origin" }
}));
app.use(cors(config.server.cors));
app.use(bodyParser.json({ limit: '10kb' }));
app.use(bodyParser.urlencoded({ extended: true }));

// 2. Arquivos Estáticos (antes de qualquer processamento pesado)
if (!process.env.BACKEND_ONLY) {
    // Middleware de arquivos estáticos com logging simplificado
    app.use(staticMiddleware({
        enableLogging: true,
        ignorePaths: config.logging?.static?.ignorePaths || []
    }));

    // Configuração para servir arquivos estáticos
    const staticOptions = {
        setHeaders: (res, path) => {
            // Define o tipo MIME correto para módulos ES6
            if (path.endsWith('.js')) {
                res.set('Content-Type', 'application/javascript; charset=UTF-8');
            }
            if (path.endsWith('.mjs') || path.match(/\.js\?.*$/)) {
                res.set('Content-Type', 'application/javascript; charset=UTF-8');
            }
            // Cache control já é definido no staticMiddleware
        }
    };

    // Servir arquivos estáticos em ordem específica
    app.use('/assets', express.static(paths.client.assets, { ...staticOptions, index: false }));
    app.use('/js', express.static(paths.client.js, { ...staticOptions, index: false }));
    app.use('/components', express.static(paths.client.components, { ...staticOptions, index: false }));
    app.use('/services', express.static(paths.client.services, { ...staticOptions, index: false }));
    app.use('/styles', express.static(paths.client.styles, { ...staticOptions, index: false }));
    // Servir index.html e outros arquivos da raiz
    app.use(express.static(paths.client.root, staticOptions));
}

// 3. Logging para rotas dinâmicas
app.use(loggerMiddleware({
    logStatic: false, // Já temos logging otimizado para estáticos
    logMetrics: true,
    ignorePaths: config.logging?.ignorePaths || []
}));

// 4. Rotas da API com seus middlewares específicos
app.use('/api', [
    // Rate limiting primeiro
    apiLimiter,
    
    // Validação de requisições
    (req, res, next) => {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
            return res.status(400).json({ errors: errors.array() });
        }
        next();
    },
    
    // COBOL middleware (lazy loading)
    cobolMiddleware({
        required: false, // Não bloqueia se COBOL não estiver disponível
        maxRetries: config.cobol?.maxRetries || 3
    }),
    
    // Rotas
    routes
]);

// 5. Error handlers
app.use(errorLoggerMiddleware());
app.use('/api', (err, req, res, next) => {
    // Tratamento específico para erros de validação
    if (err.type === 'validation') {
        return res.status(400).json({
            error: 'Erro de validação',
            details: err.errors
        });
    }
    
    // Erro genérico com mais informações em desenvolvimento
    const error = process.env.NODE_ENV === 'production'
        ? 'Erro interno do servidor'
        : err.message || 'Algo deu errado!';
    
    res.status(err.status || 500).json({ error });
});

// Endpoint para métricas (apenas em desenvolvimento)
if (process.env.NODE_ENV === 'development') {
    app.get('/_metrics', (req, res) => {
        res.json({
            static: staticLogger.getStats(),
            requests: requestMetrics.getMetrics(),
            cobol: CobolProcessor.instance?.getHealth() || { status: 'NOT_INITIALIZED' }
        });
    });
}

// 6. Rota catch-all para SPA em modo não-backend (deve ser a última)
if (!process.env.BACKEND_ONLY) {
    app.get('*', (req, res, next) => {
        // Não enviar index.html para requisições de API
        if (req.path.startsWith('/api')) {
            return next();
        }
        // Envia index.html para todas as outras rotas
        res.sendFile(paths.client.html.index);
    });
}

// Inicia o servidor
const PORT = process.env.PORT || config.server.port || 3000;
const HOST = config.server.host || '0.0.0.0';

app.listen(PORT, HOST, () => {
    console.log(`
🚀 Servidor iniciado em http://${HOST}:${PORT}
📝 Modo: ${process.env.BACKEND_ONLY ? 'backend-only' : 'integrated'}
🔍 Endpoints:
   - API: http://localhost:${PORT}/api
   ${process.env.BACKEND_ONLY ? '' : '- Frontend: http://localhost:' + PORT}
   ${process.env.NODE_ENV === 'development' ? '- Métricas: http://localhost:' + PORT + '/_metrics' : ''}
`);
});