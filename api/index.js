const express = require('express');
const cors = require('cors');
const bodyParser = require('body-parser');
const rateLimit = require('express-rate-limit');
const helmet = require('helmet');
const { validationResult } = require('express-validator');
const { paths } = require('./utils/paths');
const config = require(paths.config);
const routes = require('./routes');
const { logRequest, logResponse, logError, logInfo } = require('../../scripts/logger');

const app = express();

// Rate Limiting
const apiLimiter = rateLimit({
    windowMs: 60 * 1000, // 1 minuto
    max: 10, // 10 requisições por minuto
    message: {
        success: false,
        error: 'Limite de requisições excedido',
        details: 'Para sua segurança, limitamos a 10 requisições por minuto',
        retryAfter: '1 minuto',
        code: 'RATE_LIMIT_EXCEEDED'
    },
    standardHeaders: true, // Retorna os headers `RateLimit-*`
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

// Middleware para logging de requisições
app.use((req, res, next) => {
    req.startTime = Date.now(); // Para calcular duração da requisição
    logRequest(req);
    
    // Intercepta a resposta para logging
    const oldSend = res.send;
    res.send = function(data) {
        logResponse(req, res, data);
        return oldSend.apply(res, arguments);
    };
    
    next();
});

// Middlewares de Segurança
app.use(helmet()); // Segurança básica de headers
app.use('/api', apiLimiter); // Rate limiting apenas para rotas da API
app.use(cors(config.server.cors));
app.use(bodyParser.json({
    limit: '10kb' // Limita tamanho do payload
}));
app.use(bodyParser.urlencoded({ extended: true }));

// Middleware de validação global
app.use((err, req, res, next) => {
    if (err instanceof SyntaxError && 'body' in err) {
        return res.status(400).json({ error: 'JSON inválido' });
    }
    
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return res.status(400).json({ errors: errors.array() });
    }
    
    next();
});

// Rotas da API (apenas para /api/*)
app.use('/api', routes);

// Tratamento de erros da API melhorado
app.use('/api', (err, req, res, next) => {
    logError(err, req);
    
    // Tratamento de erros mais específico
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

// Se não estiver em modo backend-only, serve os arquivos estáticos
if (!process.env.BACKEND_ONLY) {
    logInfo('Modo integrado - Servindo arquivos estáticos:', {
        root: paths.client.root,
        styles: paths.client.styles,
        images: paths.client.images,
        fonts: paths.client.fonts,
        index: paths.client.html.index
    });

    // Servir arquivos estáticos do frontend
    app.use(express.static(paths.client.root));
    app.use('/styles', express.static(paths.client.styles));
    app.use('/images', express.static(paths.client.images));
    app.use('/fonts', express.static(paths.client.fonts));

    // Todas as outras rotas servem o frontend (SPA)
    app.get('*', (req, res) => {
        res.sendFile(paths.client.html.index);
    });
} else {
    logInfo('Modo backend-only - Apenas API disponível');
}

// Inicia o servidor
const PORT = process.env.PORT || config.server.port || 3000;
const HOST = config.server.host || '0.0.0.0';
app.listen(PORT, HOST, () => {
    logInfo('🚀 Servidor iniciado', {
        url: `http://${HOST}:${PORT}`,
        mode: process.env.BACKEND_ONLY ? 'backend-only' : 'integrated',
        endpoints: {
            api: `http://localhost:${PORT}/api`,
            ...(process.env.BACKEND_ONLY ? {} : { frontend: `http://localhost:${PORT}` })
        }
    });
});