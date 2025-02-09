const express = require('express');
const https = require('https');  // Adicionado para HTTPS
const fs = require('fs');
const { exec } = require('child_process');
const iconv = require('iconv-lite');
const path = require('path');

const app = express();

// Middleware para servir arquivos estáticos
app.use(express.static(path.join(__dirname, '../client')));

// Rota principal
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, '../client/index.html'));
});

// Rota para executar o programa COBOL
app.get('/cobol', (req, res) => {
  exec(path.join(__dirname, 'start'), (error, stdout, stderr) => {
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
    res.send({ cobolOutput: iconv.decode(stdout, 'latin1') });
  });
});

app.listen(3000, '0.0.0.0', () => {
  console.log('API rodando na porta 3000 em http://localhost:3000');
});