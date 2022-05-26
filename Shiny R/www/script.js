document.addEventListener("DOMContentLoaded", function(){
});

function everBarTypeInputBoxes()
{
  let errorBarType = document.getElementById('errorBarType');
  let IVRow2 = document.getElementById('IVRow2');
  let upperMessage = document.getElementById('upperMessage');
  let lowerMessage = document.getElementById('lowerMessage');
  if (errorBarType.value === '95%CI') {
    IVRow2.removeAttribute('hidden');
    upperMessage.removeAttribute('hidden');
    lowerMessage.removeAttribute('hidden');
  } else {
    IVRow2.setAttribute('hidden', 'hidden');
    upperMessage.setAttribute('hidden', 'hidden');
    lowerMessage.setAttribute('hidden', 'hidden');
  }
}