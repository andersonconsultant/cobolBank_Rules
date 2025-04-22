const path = require('path');
const fs = require('fs');

// Carrega o arquivo de configuração central
const configPath = path.join(__dirname, '../../config.json');
console.log('Tentando ler o arquivo de configuração em:', configPath);
console.log('Diretório atual:', __dirname);

let config;
try {
    config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
    console.log('Configuração carregada com sucesso');
} catch (error) {
    console.error('Erro ao carregar configuração:', error);
    throw error;
}

// Obtém o ambiente atual do config
const environment = config.project.environment;

// Define caminhos do projeto usando path.join para garantir compatibilidade entre sistemas
const PROJECT_PATHS = {
    CLIENT: {
        ROOT: path.join(__dirname, config.paths.client.root),
        STYLES: path.join(__dirname, config.paths.client.styles),
        IMAGES: path.join(__dirname, config.paths.client.images),
        FONTS: path.join(__dirname, config.paths.client.fonts),
        HTML: {
            INDEX: path.join(__dirname, config.paths.client.html.index)
        }
    },
    COBOL: {
        ROOT: config.paths.cobol.root,
        PROGRAMS: {
            START: config.paths.cobol.programs.start,
            LOGIN: config.paths.cobol.programs.login,
            TRANSACTION: config.paths.cobol.programs.transaction
        }
    },
    LOGS: config.paths.logs
};

// Define endpoints da API
const API_ENDPOINTS = {
    BASE: config.endpoints.base,
    HEALTH: config.endpoints.health,
    COBOL: config.endpoints.cobol,
    AUTH: config.endpoints.auth
};

// Define configurações do servidor
const SERVER_CONFIG = {
    PORT: config.server.port,
    HOST: config.server.host,
    BASE_URL: config.server.baseUrl,
    CORS_OPTIONS: config.server.cors
};

module.exports = {
    environment,
    paths: PROJECT_PATHS,
    endpoints: API_ENDPOINTS,
    server: SERVER_CONFIG,
    project: config.project
}; 