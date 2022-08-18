function hexString(buffer) {
  const byteArray = new Uint8Array(buffer);
  const hexCodes = [...byteArray].map(value => {
    const hexCode = value.toString(16);
    const paddedHexCode = hexCode.padStart(2, '0');
    return paddedHexCode;
  });
  return hexCodes.join('');
}

function digestMessage(message) {
  const encoder = new TextEncoder();
  const data = encoder.encode(message);
  return window.crypto.subtle.digest('SHA-256', data);
}

function validateRegisterForm() 
{
  var valid = true;
  if(document.getElementById('user').value.length === 0)
  {
    valid = false;
    elem = document.getElementById('user');
    elem.className = 'form-control is-invalid';
    elem = document.getElementById('user-group');
    elem.className = 'form-floating is-invalid';
  }

  if(document.getElementById('email').value.length === 0) 
  {
    valid = false;
    elem = document.getElementById('email');
    elem.className = 'form-control is-invalid';
    elem = document.getElementById('email-group');
    elem.className = 'form-floating is-invalid';
  }

  if(document.getElementById('password').value.length === 0)
  {
    valid = false;
    elem = document.getElementById('password');
    elem.className = 'form-control is-invalid';
    elem = document.getElementById('password-group');
    elem.className = 'form-floating is-invalid';
  }

  return valid;
}


