function updateClocks() {
  document.querySelectorAll(".clock").forEach( (i) => i.innerHTML = timeNowToUnicode())
  document.getElementById("pagetime").innerHTML = timeNowToUnicode()
}

function startIconTime() {
  let interval = 300000
  var initialDelay = interval - (new Date().getTime() % interval)
  updateClocks()
  setTimeout(function(){
    updateClocks()
    setInterval(updateClocks, interval)
  }, initialDelay)
}

function timeNowToUnicode() {
  let d = new Date()
  let h = d.getHours() % 12
  let x = d.getMinutes()
  let m = x - (x % 5) // nearest 5 mins
  let offset = Math.floor((h * 12) + ((m / 60) * 12))
  let unicodeChar = 0xF0000 + offset
  return `&#${unicodeChar}`
}
