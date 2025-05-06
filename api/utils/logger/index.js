/**
 * Exporta todas as funcionalidades de logging centralizadas
 */

const consoleLogger = require('./console-logger');
const logServer = require('./server');
const config = require('./loggerConfig');

// Exporta a configuração e funcionalidades do logger
module.exports = {
    // Funções de logging para console
    ...consoleLogger,
    
    // Instância do servidor de logs
    server: logServer,
    
    // Configurações
    config,
    
    // Componentes disponíveis para logging
    COMPONENTS: config.components
}; 