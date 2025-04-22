const path = require('path');

// Obtém o diretório raiz do projeto (2 níveis acima da pasta api)
const PROJECT_ROOT = path.resolve(__dirname, '../../../');

function resolveProjectPath(...segments) {
    return path.join(PROJECT_ROOT, ...segments);
}

module.exports = {
    PROJECT_ROOT,
    resolveProjectPath,
    paths: {
        config: resolveProjectPath('config.json'),
        client: {
            root: resolveProjectPath('client'),
            styles: resolveProjectPath('client/styles'),
            images: resolveProjectPath('client/images'),
            fonts: resolveProjectPath('client/fonts'),
            html: {
                index: resolveProjectPath('client/index.html')
            }
        },
        api: {
            root: resolveProjectPath('Rules/api'),
            cobol: resolveProjectPath('Rules/api/cobol')
        }
    }
}; 