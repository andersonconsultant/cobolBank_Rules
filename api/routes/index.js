const express = require('express');
const router = express.Router();
const { exec } = require('child_process');
const config = require('../config');
const { logCobol, logInfo } = require('../../../scripts/logger');
const balanceService = require('../services/balance');
const { loginValidators, transactionValidators } = require('../middleware/validators');
const { validationResult } = require('express-validator');
const cobolRoutes = require('./cobol');

// Mock data (similar ao Overview.js)
const mockData = {
    balance: 5432.10,
    previousBalance: 4982.10,
    lastUpdate: new Date(),
    variation: {
        percentage: 2.5,
        direction: 'up'
    }
};

// Rota de saúde da API
router.get('/v1/health', (req, res) => {
    res.json({ status: 'ok', environment: config.environment });
});

// Rota para saldo
router.get('/v1/saldo', async (req, res) => {
    try {
        const result = await balanceService.getBalance();
        res.json(result);
    } catch (error) {
        logInfo('SALDO', `Erro ao consultar saldo: ${error.message}`);
        res.status(500).json({
            success: false,
            error: 'Erro ao consultar saldo',
            message: error.message
        });
    }
});

// Rota simplificada de saldo (para compatibilidade)
router.get('/saldo', (req, res) => {
    res.redirect('/api/v1/saldo');
});

// Rota para execução COBOL
router.get('/v1/cobol/bin', (req, res) => {
  exec(config.paths.COBOL.PROGRAMS.START, (error, stdout, stderr) => {
    if (error) {
      console.error(`Erro: ${error.message}`);
      res.status(500).send({ error: 'Erro ao executar o programa COBOL' });
      return;
    }
    if (stderr) {
      console.error(`Erro: ${stderr}`);
      res.status(500).send({ error: 'Erro ao executar o programa COBOL' });
      return;
    }
    res.send({ output: stdout });
  });
});

// Rota para login COBOL
router.post('/v1/cobol/login', loginValidators, (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return res.status(400).json({ 
            success: false,
            errors: errors.array()
        });
    }

    const { username, password } = req.body;
  
    // Para teste, vamos simular uma resposta de sucesso
    if (username === 'teste' && password === 'teste123') {
      res.json({
        success: true,
        token: 'mock-token-123',
        user: {
          id: 1,
          name: 'Usuário Teste',
          balance: '1000.00'
        }
      });
      return;
    }
  
    // Se as credenciais não forem as de teste, tenta executar o programa COBOL
    exec(`${config.paths.COBOL.PROGRAMS.LOGIN} ${username} ${password}`, (error, stdout, stderr) => {
      if (error) {
        console.error(`Erro: ${error.message}`);
        res.status(401).json({ 
          success: false,
          message: 'Credenciais inválidas'
        });
        return;
      }
      if (stderr) {
        console.error(`Erro: ${stderr}`);
        res.status(401).json({ 
          success: false,
          message: 'Erro ao validar credenciais'
        });
        return;
      }
      
      // Processa a saída do programa COBOL
      try {
        const output = JSON.parse(stdout);
        res.json({
          success: true,
          token: output.token,
          user: output.user
        });
      } catch (e) {
        console.error('Erro ao processar resposta do COBOL:', e);
        res.status(500).json({ 
          success: false,
          message: 'Erro ao processar resposta do servidor'
        });
      }
    });
});

// Rota para transação COBOL
router.post('/v1/cobol/transaction', transactionValidators, (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return res.status(400).json({ 
            success: false,
            errors: errors.array()
        });
    }

    const { type, amount } = req.body;
    exec(`${config.paths.COBOL.PROGRAMS.TRANSACTION} ${type} ${amount}`, (error, stdout, stderr) => {
      if (error) {
        console.error(`Erro: ${error.message}`);
        res.status(500).send({ error: 'Erro ao executar a transação COBOL' });
        return;
      }
      if (stderr) {
        console.error(`Erro: ${stderr}`);
        res.status(500).send({ error: 'Erro ao executar a transação COBOL' });
        return;
      }
      res.send({ output: stdout });
    });
});

// Registra as rotas COBOL em /api/cobol/*
router.use('/cobol', cobolRoutes);

module.exports = router; 