# 🏦 CobolBank Rules Engine

🔄 Este repositório contém o motor de regras e a integração COBOL do projeto **CobolBank**. Para acessar o front-end e gateway:

👉 [Acesse o repositório principal com UI moderna](https://github.com/andersonconsultant/cobolBank_Project)


*[English](#english) | [Português](#português)*


---

<a name="english"></a>
# English

[![COBOL](https://img.shields.io/badge/COBOL-72%25-blue)](https://github.com/andersonconsultant/cobolBank_Rules) [![JavaScript](https://img.shields.io/badge/JavaScript-2.3%25-yellow)](https://github.com/andersonconsultant/cobolBank_Rules) [![API](https://img.shields.io/badge/API-REST-green)](https://github.com/andersonconsultant/cobolBank_Rules)

## The Bridge Between Legacy and Modern Banking

**CobolBank Rules Engine** is the core business logic component of the CobolBank project, providing a secure bridge between critical COBOL business rules and modern web applications.

## 🔑 Key Features

- **COBOL Business Logic Preservation**: Maintains decades of validated financial rules
- **Modern API Wrapper**: Exposes COBOL programs via RESTful HTTP endpoints
- **Zero-Risk Modernization**: Updates technology without risking business operations
- **Scalable Architecture**: Handles growing transaction volumes without rewrites

## 📋 Repository Structure

```
Rules/
├── api/                # API and Gateway
│   ├── cobol/         # COBOL Programs
│   │   ├── DEV/       # Development environment
│   │   ├── TST/       # Test environment
│   │   └── bin/       # Compiled binaries
│   ├── routes/        # API Routes
│   ├── config.js      # Server configurations
│   └── index.js       # Entry point
├── package.json       # Dependencies
└── GUIDE-RULES.md     # Business rules documentation
```

## 🚀 Quick Start

```bash
# Install dependencies
npm install

# Start the API server
npm start

# Run in development mode with hot-reload
npm run dev
```

## 📡 API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/v1/health` | GET | Health check for API status |
| `/v1/cobol/bin` | GET | Execute COBOL program |
| `/v1/cobol/login` | POST | User authentication |
| `/v1/cobol/transaction` | POST | Process financial transactions |

## 💼 Business Rules

This repository implements critical banking business rules including:

- **Authentication & Security**: Secure login flows with encryption
- **Customer Management**: Registration and profile validation
- **Transaction Processing**: Financial operations with integrity checks
- **Permission Management**: Role-based access control
- **Reporting**: Formatted financial data extraction

## 🔧 Development Environments

- **DEV**: Development environment with direct database access
- **TST**: Isolated testing environment with separate database

## 📚 Documentation

Comprehensive documentation is available in the repository:

- [Business Rules Guide](./GUIDE-RULES.md): Detailed business logic and test scenarios
- [API Integration Guide](./api/GUIDE-API.md): API usage and integration instructions

## 🔄 Integration

This repository is a sub-component of the larger CobolBank project, designed to work seamlessly with the front-end gateway while maintaining the integrity of legacy COBOL business logic.

## 👨‍💻 About the Developer

Anderson specializes in digital transformation with a focus on legacy system modernization. His background combines COBOL expertise with modern web development, enabling him to bridge critical business logic from legacy systems to modern platforms while minimizing business risk.

## 📫 Connect

[![LinkedIn](https://img.shields.io/badge/LinkedIn-Anderson%20Silva-0077B5?style=flat&logo=linkedin)](https://www.linkedin.com/in/andersonsilva-ds/)
[![GitHub](https://img.shields.io/badge/GitHub-andersonsilva--dev-181717?style=flat&logo=github)](https://github.com/andersonsilva-dev)
[![Email](https://img.shields.io/badge/Email-andersonsilvads26%40gmail.com-D14836?style=flat&logo=gmail)](mailto:andersonsilvads26@gmail.com)

---

<a name="português"></a>
# Português

[![COBOL](https://img.shields.io/badge/COBOL-72%25-blue)](https://github.com/andersonconsultant/cobolBank_Rules) [![JavaScript](https://img.shields.io/badge/JavaScript-2.3%25-yellow)](https://github.com/andersonconsultant/cobolBank_Rules) [![API](https://img.shields.io/badge/API-REST-green)](https://github.com/andersonconsultant/cobolBank_Rules)

## A Ponte Entre o Legado e o Banco Moderno

**CobolBank Rules Engine** é o componente central de lógica de negócios do projeto CobolBank, fornecendo uma ponte segura entre regras de negócio críticas em COBOL e aplicações web modernas.

## 🔑 Recursos Principais

- **Preservação da Lógica de Negócios COBOL**: Mantém décadas de regras financeiras validadas
- **Wrapper de API Moderna**: Expõe programas COBOL via endpoints HTTP RESTful
- **Modernização Sem Riscos**: Atualiza a tecnologia sem arriscar operações de negócios
- **Arquitetura Escalável**: Lida com volumes crescentes de transações sem reescritas

## 📋 Estrutura do Repositório

```
Rules/
├── api/                # API e Gateway
│   ├── cobol/         # Programas COBOL
│   │   ├── DEV/       # Ambiente de desenvolvimento
│   │   ├── TST/       # Ambiente de teste
│   │   └── bin/       # Binários compilados
│   ├── routes/        # Rotas da API
│   ├── config.js      # Configurações do servidor
│   └── index.js       # Ponto de entrada
├── package.json       # Dependências
└── GUIDE-RULES.md     # Documentação de regras de negócio
```

## 🚀 Início Rápido

```bash
# Instalar dependências
npm install

# Iniciar o servidor API
npm start

# Executar em modo de desenvolvimento com recarga automática
npm run dev
```

## 📡 Endpoints da API

| Endpoint | Método | Descrição |
|----------|--------|-----------|
| `/v1/health` | GET | Verificação de saúde para status da API |
| `/v1/cobol/bin` | GET | Executar programa COBOL |
| `/v1/cobol/login` | POST | Autenticação de usuário |
| `/v1/cobol/transaction` | POST | Processar transações financeiras |

## 💼 Regras de Negócio

Este repositório implementa regras de negócio bancárias críticas, incluindo:

- **Autenticação e Segurança**: Fluxos de login seguros com criptografia
- **Gestão de Clientes**: Registro e validação de perfil
- **Processamento de Transações**: Operações financeiras com verificações de integridade
- **Gestão de Permissões**: Controle de acesso baseado em funções
- **Relatórios**: Extração formatada de dados financeiros

## 🔧 Ambientes de Desenvolvimento

- **DEV**: Ambiente de desenvolvimento com acesso direto ao banco de dados
- **TST**: Ambiente de teste isolado com banco de dados separado

## 📚 Documentação

Documentação abrangente está disponível no repositório:

- [Guia de Regras de Negócio](./GUIDE-RULES.md): Lógica de negócios detalhada e cenários de teste
- [Guia de Integração da API](./api/GUIDE-API.md): Uso da API e instruções de integração

## 🔄 Integração

Este repositório é um subcomponente do projeto CobolBank mais amplo, projetado para trabalhar perfeitamente com o gateway front-end enquanto mantém a integridade da lógica de negócios legada em COBOL.

## 👨‍💻 Sobre o Desenvolvedor

Anderson atua na área de transformação digital com foco em modernização de sistemas legados. Sua formação combina expertise em COBOL com desenvolvimento web moderno, permitindo conectar lógica de negócios crítica de sistemas legados a plataformas modernas enquanto minimiza riscos aos negócios.

## 📫 Contato

[![LinkedIn](https://img.shields.io/badge/LinkedIn-Anderson%20Silva-0077B5?style=flat&logo=linkedin)](https://www.linkedin.com/in/andersonsilva-ds/)
[![GitHub](https://img.shields.io/badge/GitHub-andersonsilva--dev-181717?style=flat&logo=github)](https://github.com/andersonsilva-dev)
[![Email](https://img.shields.io/badge/Email-andersonsilvads26%40gmail.com-D14836?style=flat&logo=gmail)](mailto:andersonsilvads26@gmail.com)
