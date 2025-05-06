const express = require('express');
const logProcessor = require('../../cobol/processor/log-processor');
const { paths } = require('../paths');
const mainConfig = require('../../../../config');
const loggerConfig = require('./loggerConfig');

class LogServer {
    constructor() {
        this.app = express();
        this.port = process.env.LOG_SERVER_PORT || loggerConfig.server.port;
        this.server = null;
    }

    async initialize() {
        console.log('\n=== Iniciando Servidor de Logs ===');
        
        try {
            // Inicializa o processador COBOL
            await logProcessor.initialize();
            console.log('✓ Processador COBOL inicializado');

            // Configura rotas
            this.app.use(express.json());
            
            // Rota para consulta de logs
            this.app.get('/api/log', async (req, res) => {
                try {
                    const filters = {
                        session_id: req.query.session_id,
                        status: req.query.status,
                        endpoint: req.query.endpoint,
                        start_date: req.query.start_date,
                        end_date: req.query.end_date
                    };

                    // Validação dos comprimentos máximos
                    const maxLengths = loggerConfig.validation.maxLengths;
                    for (const [key, value] of Object.entries(filters)) {
                        if (value && maxLengths[key] && value.length > maxLengths[key]) {
                            return res.status(400).json({
                                success: false,
                                error: `Campo ${key} excede o tamanho máximo de ${maxLengths[key]} caracteres`
                            });
                        }
                    }

                    // Remove filtros vazios
                    Object.keys(filters).forEach(key => {
                        if (!filters[key]) {
                            delete filters[key];
                        }
                    });

                    console.log('Consultando logs com filtros:', filters);
                    const result = await logProcessor.queryLogs(filters);

                    res.json({
                        success: true,
                        filters: filters,
                        data: result || []
                    });
                } catch (error) {
                    console.error('Erro ao consultar logs:', error);
                    res.status(500).json({
                        success: false,
                        error: error.message,
                        filters: req.query
                    });
                }
            });

            // Rota para envio de logs
            this.app.post('/api/log', async (req, res) => {
                try {
                    const logData = req.body;

                    // Verifica se deve ignorar o log
                    if (loggerConfig.filters.ignoreStaticRequests && 
                        loggerConfig.filters.staticPatterns.some(pattern => 
                            pattern instanceof RegExp ? pattern.test(logData.endpoint) : logData.endpoint.includes(pattern))) {
                        return res.status(200).json({ success: true, ignored: true });
                    }

                    // Validação dos comprimentos máximos
                    const maxLengths = loggerConfig.validation.maxLengths;
                    for (const [key, value] of Object.entries(logData)) {
                        if (value && maxLengths[key] && value.length > maxLengths[key]) {
                            return res.status(400).json({
                                success: false,
                                error: `Campo ${key} excede o tamanho máximo de ${maxLengths[key]} caracteres`
                            });
                        }
                    }

                    // Processa o log
                    await logProcessor.processLog(logData);

                    res.json({
                        success: true,
                        message: 'Log processado com sucesso'
                    });
                } catch (error) {
                    console.error('Erro ao processar log:', error);
                    res.status(500).json({
                        success: false,
                        error: error.message
                    });
                }
            });

            // Rota para healthcheck
            this.app.get('/health', (req, res) => {
                res.json({ 
                    status: 'ok',
                    uptime: process.uptime(),
                    port: this.port,
                    config: {
                        ignoreStaticRequests: loggerConfig.filters.ignoreStaticRequests,
                        ignoredPaths: loggerConfig.filters.ignoredPaths
                    }
                });
            });
            
            // Inicia o servidor
            await this.startServer();
            
            console.log('✓ Servidor de logs pronto para receber dados');
            console.log(`✓ Endpoints disponíveis:`);
            console.log(`  - GET http://localhost:${this.port}/api/log`);
            console.log(`  - POST http://localhost:${this.port}/api/log`);
            console.log(`  - GET http://localhost:${this.port}/health`);

            // Configura shutdown limpo
            this.setupGracefulShutdown();

            return true;
        } catch (error) {
            console.error('Erro ao inicializar servidor de logs:', error);
            throw error;
        }
    }

    async startServer() {
        try {
            await this.tryPort();
            // Set environment variable for other processes
            process.env.LOG_SERVER_PORT = this.port.toString();
            console.log(`✓ Servidor iniciado na porta ${this.port}`);
        } catch (error) {
            if (error.code === 'EADDRINUSE') {
                throw new Error(`A porta ${this.port} já está em uso. Por favor, libere a porta ou especifique outra usando a variável de ambiente LOG_SERVER_PORT`);
            }
            throw error;
        }
    }

    tryPort() {
        return new Promise((resolve, reject) => {
            this.server = this.app.listen(this.port)
                .once('listening', () => resolve())
                .once('error', reject);
        });
    }

    setupGracefulShutdown() {
        const shutdown = async () => {
            console.log('\n=== Encerrando Servidor de Logs ===');

            if (this.server) {
                await new Promise(resolve => this.server.close(resolve));
                console.log('✓ Servidor HTTP encerrado');
            }

            await logProcessor.shutdown();
            console.log('✓ Processador COBOL encerrado');
            
            process.exit(0);
        };

        // Captura sinais de término
        process.on('SIGTERM', shutdown);
        process.on('SIGINT', shutdown);
        
        // Captura erros não tratados
        process.on('uncaughtException', async (error) => {
            console.error('Erro não tratado:', error);
            await shutdown();
        });
    }

    getPort() {
        return this.port;
    }
}

// Cria e exporta uma única instância do servidor
const logServer = new LogServer();

// Se executado diretamente, inicia o servidor
if (require.main === module) {
    logServer.initialize().catch(error => {
        console.error('Falha ao iniciar servidor:', error);
        process.exit(1);
    });
}

module.exports = logServer;