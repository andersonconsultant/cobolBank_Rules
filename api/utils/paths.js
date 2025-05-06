const path = require('path');

// Função para resolver caminhos relativos ao projeto
const resolveProjectPath = (...segments) => path.join(__dirname, '..', '..', '..', ...segments);

// Padrões de arquivos estáticos
const staticPatterns = [
    /\.(css|js|jpg|jpeg|png|gif|ico|svg|woff|woff2|ttf|eot)$/i,
    /^\/static\//,
    /^\/assets\//,
    /^\/js\//,
    /^\/components\//,
    /^\/services\//,
    /^\/styles\//
];

// Função para verificar se um caminho é de arquivo estático
const isStaticFile = (path) => {
    return staticPatterns.some(pattern => {
        if (pattern instanceof RegExp) {
            return pattern.test(path);
        }
        return path.startsWith(pattern);
    });
};

// Caminhos da aplicação
const paths = {
    root: resolveProjectPath(),
    client: {
        root: resolveProjectPath('client'),
        assets: resolveProjectPath('client', 'assets'),
        js: resolveProjectPath('client', 'js'),
        components: resolveProjectPath('client', 'components'),
        services: resolveProjectPath('client', 'services'),
        styles: resolveProjectPath('client', 'styles'),
        html: {
            index: resolveProjectPath('client', 'index.html')
        }
    },
    api: {
        root: resolveProjectPath('Rules', 'api'),
        routes: resolveProjectPath('Rules', 'api', 'routes'),
        middleware: resolveProjectPath('Rules', 'api', 'middleware'),
        utils: resolveProjectPath('Rules', 'api', 'utils')
    }
};

module.exports = {
    paths,
    resolveProjectPath,
    isStaticFile
};