const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');
const { logInfo, logError } = require('../../utils/logger/console-logger');
const { paths } = require('../../utils/paths');
const config = require('../../../../config');

const cobolPath = path.resolve(__dirname, '../../../../', config.executables.engine);

class CobolService {
    constructor() {
        this.process = null;
        this.currentResponse = null;
        this.responseResolver = null;
        this.isConnected = false;
    }

    start() {
        if (!fs.existsSync(cobolPath)) {
            logError('COBOL Init', null, { error: `Executável não encontrado: ${cobolPath}` });
            throw new Error(`Executável não encontrado: ${cobolPath}`);
        }

        this.process = spawn(cobolPath);
        logInfo('COBOL Process Started', { path: cobolPath });

        this.process.stdout.on('data', (data) => {
            const output = data.toString().trim();
            if (!output.startsWith('#Digite o comando:')) {
                logInfo('COBOL Output', { output });
            }

            if (this.responseResolver) {
                this.responseResolver(output);
            }
        });

        this.process.stderr.on('data', (data) => {
            logError('COBOL Error', null, { error: data.toString() });
        });

        this.process.on('close', (code) => {
            logInfo('COBOL Process Closed', { code });
            this.process = null;
            this.isConnected = false;
        });
    }

    // ... (resto do código permanece como está)
}

// Singleton instance
const cobolService = new CobolService();

module.exports = cobolService;
