const { body, param, query } = require('express-validator');

// Validadores para rota de login
const loginValidators = [
    body('username')
        .trim()
        .notEmpty().withMessage('Username é obrigatório')
        .isLength({ min: 3 }).withMessage('Username deve ter no mínimo 3 caracteres')
        .escape(),
    body('password')
        .notEmpty().withMessage('Senha é obrigatória')
        .isLength({ min: 6 }).withMessage('Senha deve ter no mínimo 6 caracteres')
];

// Validadores para rota de transação
const transactionValidators = [
    body('type')
        .trim()
        .notEmpty().withMessage('Tipo de transação é obrigatório')
        .isIn(['credit', 'debit']).withMessage('Tipo de transação inválido'),
    body('amount')
        .notEmpty().withMessage('Valor é obrigatório')
        .isFloat({ min: 0.01 }).withMessage('Valor deve ser maior que zero')
        .custom((value) => {
            if (value > 1000000) {
                throw new Error('Valor máximo permitido é 1.000.000');
            }
            return true;
        })
];

module.exports = {
    loginValidators,
    transactionValidators
}; 