function setIconTime() {
  document.querySelectorAll(".clock").forEach( (i) => i.innerHTML = timeNowToUnicode())
  document.getElementById("pagetime").innerHTML = timeNowToUnicode()
}

const roundTo = roundTo => x => Math.round(x / roundTo) * roundTo; 
const roundTo5Minutes roundTo(300000)

function startIconTime() {
  setIconTime()
  let now = new Date()
  let nearest = roundTo5Minutes(now)
  setTimeout(i => setInterval(setIconTime, 300000), nearest)
}

function timeNowToUnicode() {
  let d = new Date()
  let h = d.getHours() % 12
  let x = d.getMinutes()
  let m = x - (x % 5) // nearest 5 mins
  let offset = Math.floor((h * 12) + ((m / 60) * 12))
  let unicode = 0xE800 + offset

  // debug uncomment
  // console.log(` H: ${h}\n M: ${m}\n Offset: ${offset}\n Unicode: ${unicode}\n`)

  return `&#${unicode}`
}
