/**
 * Console Logger
 * Implementação simplificada de logging para console
 */

const loggerConfig = require('./loggerConfig');

const colors = loggerConfig.console.colors;
const icons = loggerConfig.console.icons;

function formatMessage(type, message, data = null) {
    const timestamp = new Date().toISOString();
    const icon = icons[type] || '•';
    const color = colors[type] || '';
    const reset = colors.RESET;
    
    let output = `${color}[${timestamp}] ${icon} ${message}${reset}`;
    if (data) {
        output += '\n' + JSON.stringify(data, null, 2);
    }
    return output;
}

const logRequest = (req) => {
    const { method, path, body } = req;
    // Skip logging for static files if configured
    if (loggerConfig.filters.ignoreStaticRequests && 
        loggerConfig.filters.staticPatterns.some(pattern => 
            pattern instanceof RegExp ? pattern.test(path) : path.includes(pattern))) {
        return;
    }
    console.log(formatMessage('REQUEST', `${method} ${path}`, Object.keys(body || {}).length > 0 ? body : null));
};

const logResponse = (req, res) => {
    // Skip logging for static files if configured
    if (loggerConfig.filters.ignoreStaticRequests && 
        loggerConfig.filters.staticPatterns.some(pattern => 
            pattern instanceof RegExp ? pattern.test(req.path) : req.path.includes(pattern))) {
        return;
    }
    const duration = Date.now() - req.startTime;
    console.log(formatMessage('RESPONSE', `${res.statusCode} (${duration}ms)`));
};

const logError = (err, req = null) => {
    const message = req ? `Error in ${req.method} ${req.path}` : 'Error';
    console.error(formatMessage('ERROR', message, err));
};

const logInfo = (message, data = null) => {
    console.log(formatMessage('INFO', message, data));
};

const logWarn = (message, data = null) => {
    console.warn(formatMessage('WARN', message, data));
};

const logCobol = (operation, data = null) => {
    console.log(formatMessage('COBOL', `Operation: ${operation}`, data));
};

module.exports = {
    logRequest,
    logResponse,
    logError,
    logInfo,
    logWarn,
    logCobol
}; 