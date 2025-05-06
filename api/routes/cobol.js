const express = require('express');
const router = express.Router();
const cobolService = require('../services/cobol');
const { logError } = require('../utils/logger/console-logger');

// Health check
router.get('/health', (req, res) => {
    res.json({
        status: 'ok',
        message: 'COBOL service is running'
    });
});

// Iniciar conexão com o banco
router.post('/start', async (req, res) => {
    try {
        const result = await cobolService.startConnection();
        res.json(result);
    } catch (error) {
        logError('Error in /cobol/start', error);
        res.status(500).json({
            success: false,
            error: 'Erro ao iniciar conexão com o banco'
        });
    }
});

// Get saldo
router.get('/saldo', async (req, res) => {
    try {
        const result = await cobolService.getSaldo();
        res.json(result);
    } catch (error) {
        logError('Error in /cobol/saldo', error);
        res.status(500).json({
            success: false,
            error: 'Erro ao consultar saldo'
        });
    }
});

// Get extrato
router.get('/extrato', async (req, res) => {
    try {
        const result = await cobolService.getExtrato();
        res.json(result);
    } catch (error) {
        logError('Error in /cobol/extrato', error);
        res.status(500).json({
            success: false,
            error: 'Erro ao consultar extrato'
        });
    }
});

// Get help
router.get('/help', async (req, res) => {
    try {
        const result = await cobolService.showHelp();
        res.json(result);
    } catch (error) {
        logError('Error in /cobol/help', error);
        res.status(500).json({
            success: false,
            error: 'Erro ao exibir ajuda'
        });
    }
});

// Finalizar conexão
router.post('/exit', async (req, res) => {
    try {
        const result = await cobolService.exit();
        res.json(result);
    } catch (error) {
        logError('Error in /cobol/exit', error);
        res.status(500).json({
            success: false,
            error: 'Erro ao finalizar conexão'
        });
    }
});

module.exports = router; 