const express = require('express');
const router = express.Router();
const { exec } = require('child_process');
const config = require('../config');

// Rota de saúde da API
router.get('/v1/health', (req, res) => {
  res.json({ status: 'ok', environment: config.environment });
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
router.post('/v1/cobol/login', (req, res) => {
  const { username, password } = req.body;
  exec(`${config.paths.COBOL.PROGRAMS.LOGIN} ${username} ${password}`, (error, stdout, stderr) => {
    if (error) {
      console.error(`Erro: ${error.message}`);
      res.status(500).send({ error: 'Erro ao executar o login COBOL' });
      return;
    }
    if (stderr) {
      console.error(`Erro: ${stderr}`);
      res.status(500).send({ error: 'Erro ao executar o login COBOL' });
      return;
    }
    res.send({ output: stdout });
  });
});

// Rota para transação COBOL
router.post('/v1/cobol/transaction', (req, res) => {
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

module.exports = router; 