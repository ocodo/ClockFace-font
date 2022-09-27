function setIconTime() {
  document.querySelectorAll(".clock").forEach( (i) => i.innerHTML = timeNowToUnicode())
  document.getElementById("pagetime").innerHTML = timeNowToUnicode()
}

function startIconTime() {
  setIconTime()
  setInterval(setIconTime, 1000)
}

function timeNowToUnicode() {
  let d = new Date()
  let h = d.getHours() % 12
  let x = d.getMinutes()
  let m = x - (x % 5) // nearest 5 mins
  let offset = Math.floor((h * 12) + ((m / 60) * 12))
  let unicode = 0xE800 + offset

  // debug loggin
  // console.log(` H: ${h}\n M: ${m}\n Offset: ${offset}\n Unicode: ${unicode}\n`)

  return `&#${unicode}`
}
