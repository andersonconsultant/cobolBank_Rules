const express = require('express');
const cors = require('cors');
const bodyParser = require('body-parser');
const path = require('path');
const config = require('./config');
const routes = require('./routes');

const app = express();

// Middleware
app.use(cors(config.server.CORS_OPTIONS));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));

// Resolve os caminhos do cliente
const clientRoot = path.resolve(__dirname, config.paths.client.root);
const clientStyles = path.resolve(__dirname, config.paths.client.styles);
const clientImages = path.resolve(__dirname, config.paths.client.images);
const clientFonts = path.resolve(__dirname, config.paths.client.fonts);
const clientIndex = path.resolve(__dirname, config.paths.client.html.index);

console.log('Caminhos do cliente:');
console.log('Root:', clientRoot);
console.log('Styles:', clientStyles);
console.log('Images:', clientImages);
console.log('Fonts:', clientFonts);
console.log('Index:', clientIndex);

// Servir arquivos estáticos do frontend
app.use(express.static(clientRoot));
app.use('/styles', express.static(clientStyles));
app.use('/images', express.static(clientImages));
app.use('/fonts', express.static(clientFonts));

// Rota principal - serve o frontend
app.get('/', (req, res) => {
  res.sendFile(clientIndex);
});

// Rotas da API (apenas para /api/*)
app.use('/api', routes);

// Tratamento de erros
app.use((err, req, res, next) => {
  console.error(err.stack);
  res.status(500).send({ error: 'Algo deu errado!' });
});

// Rota 404
app.use((req, res) => {
  res.status(404).send({ error: 'Rota não encontrada' });
});

// Inicia o servidor
app.listen(config.server.PORT, config.server.HOST, () => {
  console.log(`Servidor rodando em ${config.server.HOST}:${config.server.PORT}`);
  console.log(`Ambiente: ${config.environment}`);
  console.log(`Frontend: http://${config.server.HOST}:${config.server.PORT}`);
  console.log(`API: http://${config.server.HOST}:${config.server.PORT}/api`);
});