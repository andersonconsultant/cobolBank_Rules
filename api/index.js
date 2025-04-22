const express = require('express');
const cors = require('cors');
const bodyParser = require('body-parser');
const { paths } = require('./utils/paths');
const config = require(paths.config);
const routes = require('./routes');

const app = express();

// Middleware
app.use(cors(config.server.cors));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));

console.log('Caminhos do cliente:');
console.log('Root:', paths.client.root);
console.log('Styles:', paths.client.styles);
console.log('Images:', paths.client.images);
console.log('Fonts:', paths.client.fonts);
console.log('Index:', paths.client.html.index);

// Rotas da API (apenas para /api/*)
app.use('/api', routes);

// Tratamento de erros da API
app.use('/api', (err, req, res, next) => {
  console.error('Erro na API:', err.stack);
  res.status(500).json({ error: 'Algo deu errado!' });
});

// Servir arquivos estáticos do frontend DEPOIS das rotas da API
app.use(express.static(paths.client.root));
app.use('/styles', express.static(paths.client.styles));
app.use('/images', express.static(paths.client.images));
app.use('/fonts', express.static(paths.client.fonts));

// Todas as outras rotas servem o frontend (SPA)
app.get('*', (req, res) => {
  res.sendFile(paths.client.html.index);
});

// Inicia o servidor
const PORT = process.env.PORT || config.server.port || 3000;
const HOST = config.server.host || '0.0.0.0';
app.listen(PORT, HOST, () => {
  console.log(`Servidor rodando em http://${HOST}:${PORT}`);
  console.log(`Frontend: http://localhost:${PORT}`);
  console.log(`API: http://localhost:${PORT}/api`);
});