const path = require('path');

const config = {
    server: {
        port: process.env.LOG_SERVER_PORT || 4567,
        host: process.env.LOG_SERVER_HOST || 'localhost'
    },
    console: {
        colors: {
            REQUEST: '\x1b[36m',     // Cyan
            RESPONSE: '\x1b[32m',    // Green
            ERROR: '\x1b[31m',       // Red
            WARN: '\x1b[33m',        // Yellow
            INFO: '\x1b[34m',        // Blue
            COBOL: '\x1b[35m',       // Magenta
            RESET: '\x1b[0m'
        },
        icons: {
            REQUEST: '→',
            RESPONSE: '←',
            ERROR: '✖',
            WARN: '⚠',
            INFO: 'ℹ',
            COBOL: '⚙'
        }
    },
    components: {
        FRONTEND: 'FRONTEND',
        BACKEND: 'BACKEND',
        COBOL: 'COBOL'
    },
    cobol: {
        enginePath: path.join(__dirname, '../cobol/processor/log-processor.cbl'),
        maxRetries: 3,
        commandTimeout: 5000,
        maxQueueSize: 1000,
        batchSize: 100,
        retryDelay: 1000
    },
    database: {
        host: process.env.DB_HOST || 'localhost',
        port: process.env.DB_PORT || 5432,
        name: process.env.DB_NAME || 'cobolbd',
        table: process.env.DB_TABLE || 'log_cobol_test',
        user: process.env.DB_USER,
        password: process.env.DB_PASSWORD,
        pool: {
            max: 20,
            min: 4,
            idle: 10000
        }
    },
    validation: {
        maxLengths: {
            session_id: 10,
            component: 10,
            action: 10,
            status: 10,
            message: 100,
            endpoint: 30,
            method: 10,
            value: 12
        }
    },
    filters: {
        ignoreStaticRequests: true,
        ignoredPaths: [
            '/assets/',
            '/static/',
            '/images/',
            '/fonts/',
            '/health'
        ],
        staticPatterns: [
            /\.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$/i,
            /^\/assets\//,
            /^\/static\//,
            /^\/images\//,
            /^\/fonts\//
        ]
    }
};

module.exports = config; 