function clickme(name) {
  //document.write(name);
  var xhr = createXHR();
  if(xhr) {
    xhr.open("GET", "inode?id=" + name, true);
  }
  xhr.send(null);
}

function createXHR() {
  try {
    return new XMLHttpRequest();
  } catch(e) {
    // No catching yet.
  }
  window.alert("XMLHttpRequest not supported");
  return null;
}
