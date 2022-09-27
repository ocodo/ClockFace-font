function setIconTime() {
    let clock = document.getElementById("pagetime")
    let unicode = timeNowToUnicode()
    clock.innerHTML = unicode
}

function startIconTime() {
  setIconTime()
  setInterval(setIconTime, 60 * 5 * 1000)
}

function timeNowToUnicode() {
  let d = new Date()
  let h = d.getHours() % 12
  let x = d.getMinutes()
  let m = x - (x % 5) // nearest 5 mins
  let offset = Math.floor((h * 12) + ((m / 60) * 12))
  let unicode = 0xE800 + offset

  console.log(` H: ${h}\n M: ${m}\n Offset: ${offset}\n Unicode: ${unicode}\n`)

  return `&#${unicode}`
}
