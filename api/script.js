// Configuração da API
const API_BASE_URL = '/api/v1';

// Função para fazer chamadas à API
async function callApi(endpoint, method = 'GET', body = null) {
  try {
    const options = {
      method,
      headers: {
        'Content-Type': 'application/json'
      }
    };

    if (body) {
      options.body = JSON.stringify(body);
    }

    const response = await fetch(`${API_BASE_URL}${endpoint}`, options);
    const data = await response.json();

    if (!response.ok) {
      throw new Error(data.error || 'Erro na chamada da API');
    }

    return data;
  } catch (error) {
    console.error('Erro na API:', error);
    throw error;
  }
}

// Botão de Visão Geral
document.getElementById('botao-click').addEventListener('click', async function() {
  try {
    const healthCheck = await callApi('/health');
    alert(`Sistema operacional! Ambiente: ${healthCheck.environment}`);
  } catch (error) {
    alert('Erro ao verificar status do sistema');
  }
});

// Botão de Pagamentos - Executa COBOL
document.querySelectorAll('#buttonStartCobol').forEach(button => {
  button.addEventListener('click', async () => {
    try {
      const result = await callApi('/cobol/bin');
      console.log('Resultado COBOL:', result);
      
      // Aqui você pode adicionar código para mostrar o resultado na interface
      alert(`Programa COBOL executado com sucesso!\nResultado: ${result.cobolOutput}`);
    } catch (error) {
      alert('Erro ao executar programa COBOL');
    }
  });
});
