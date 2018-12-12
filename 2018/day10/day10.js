'use strict';

// this does not work in chrome because of some
// cross origin request restriction bullshit
const readFile = (file, fail, success) => {
  console.log("FUCK");
  const xhr = new XMLHttpRequest();
  xhr.open("GET", file, true);
  xhr.setRequestHeader("Content-Type", "text/plain");
  xhr.onreadystatechange = () => {
    if (xhr.readyState === xhr.DONE) {
      if (xhr.status === 200) {
        success(xhr.responseText);
      } else {
        fail(xhr);
      }
    }
  };
  xhr.send();
};

const normalise = (v) => {
  return { position: { x: (v.position.x + 60000)/25,y: (v.position.y + 60000)/25}
           , velocity: {dx: v.velocity.dx/25,dy: v.velocity.dy/25}
         }
}

const parseRawInput = (input) => {
  console.log("HERE");
  //    document.getElementById('mytest').innerHTML = input;
  const lines = input.replace(/^\s+|\s+$/g, "").split('\n');
  //    for (let l of lines) console.log(l);
  return lines//.filter(s => s.match(/^position.*$/))
    .map(s => {
      //    console.log("HELLO "+s);
      const pat = /^position=<\s*(-?\d+),\s*(-?\d+)>\s*velocity=<\s*(-?\d+),\s*(-?\d+)>$/;
      const matches = s.match(pat);
      if (! matches ) return null; // console.log("it's fucked");
      //      console.log(matches);
      const v = { position: { x: parseInt(matches[1]), y: parseInt(matches[2]) }
                , velocity: { dx: parseInt(matches[3]), dy: parseInt(matches[4]) }
                };
      return normalise(v);
    }).filter(x => x != null);
  //  console.log(line);
};

const posnAtTime = ({position, velocity}, t) => {
  return { x: position.x + (velocity.dx*t),y: position.y + (velocity.dy*t)};
};

const posnAtTime1 = ({position, velocity}, t) => {
  return { x: position.x + (velocity.dx*t) - 2000,y: position.y + (velocity.dy*t)-2000};
};

const drawDot = (ctx, {x,y}) => {
  ctx.beginPath();
  ctx.arc(x, y, 5, 0, 2*Math.PI);
  ctx.fill();
};

const drawIt = (canvas, ctx, t, particles) => {
  ctx.clearRect(0,0, canvas.width, canvas.height);
  for (let p of particles) {
    const pt =posnAtTime1(p,t)
    console.log("draw: x="+ pt.x + " y="+pt.y);
    drawDot(ctx, pt);
  }
};

const doIt = () => {
  console.log("executing doIt");
  const canvas = document.getElementById("mycanvas")
  const ctx = canvas.getContext("2d");
  const slider = document.getElementById("mySlider");
  const info = document.getElementById("myinfo");

  readFile("input"
           , (x) => { throw new Error("cannot read input: "+ x); }
           , (rawInput) => {
             const particles = parseRawInput(rawInput);
             console.log(particles);
             slider.oninput = () => {
               info.innerHTML = "time: "+slider.value;
               drawIt(canvas,ctx, slider.value, particles);
             };
             console.log("added slider callback");
           }
          );
};
