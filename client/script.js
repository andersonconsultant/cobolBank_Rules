document.getElementById('botao-click').addEventListener('click', function() {
    alert('Você clicou no botão!');
  });

const initCobol = document.getElementById('buttonStartCobol');

initCobol.addEventListener('click', () => {
  fetch('/api/cobol/bin')
    .then(response => response.json())
    .then(data => {
      console.log(data);
      const cobolOutput = data.cobolOutput;
      console.log(cobolOutput);
    })
    .catch(error => console.error(error));
});
